unit pLuaRecord;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$TYPEDADDRESS ON}
{$ENDIF}

{$I pLua.inc}

interface

uses
  Classes, SysUtils, fgl,
  lua, pLua, uWordList, pLuaObject;
  
type
  PLuaRecordInfo = ^TLuaRecordInfo;
  PLuaRecordInstanceInfo = ^TLuaRecordInstanceInfo;

  plua_RecordMethodWrapper  = function(RecordPointer : pointer; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
  plua_RecordPropertyReader = plua_RecordMethodWrapper;
  plua_RecordPropertyWriter = plua_RecordMethodWrapper;
  plua_RecordConstructor    = function(l : Plua_State; paramidxstart, paramcount : integer; InstanceInfo : PLuaRecordInstanceInfo) : Pointer;
  plua_RecordDestructor     = procedure( RecordPointer : pointer; l : Plua_State );

  PLuaRecordProperty= ^TLuaRecordProperty;
  TLuaRecordProperty = record
    PropName : AnsiString;
    Reader   : plua_RecordPropertyReader;
    Writer   : plua_RecordPropertyWriter;
  end;

  TLuaRecId = type Pointer; //just a pointer is enough to identify "record-ness"

  TLuaRecordInfo = record
    RecId       : TLuaRecId;
    //Parent      : PLuaRecordInfo;
    RecordName  : AnsiString;
    PropHandlers: TWordList;
    New         : plua_RecordConstructor;
    Release     : plua_RecordDestructor;
    Properties  : Array of TLuaRecordProperty;
  end;
  
  TLuaRecordInstanceInfo = record
    OwnsInstance : Boolean;
    LuaRef       : Integer;
    RecordId     : TLuaRecId;
    l            : PLua_state;
    RecordPointer: Pointer;
  end;

  TRecIdObjMap = specialize TFPGMap<TLuaRecId, PLuaRecordInfo>;

  { TLuaRecordList }

  TLuaRecordList = class
    fItems : TList;

    //just an index to get PLuaClassInfo by RecordId quickly
    FRecordIdToRecordInfo:TRecIdObjMap;
  private
    function GetRecInfoById(Id : TLuaRecId): PLuaRecordInfo;
    function GetRecordInfo(index : integer): PLuaRecordInfo;
    function GetCount: integer;
    procedure FreeItem(I:PLuaRecordInfo);
  public
    constructor Create;
    destructor Destroy; override;

    function  GetPropReader(RecId: TLuaRecId; const aPropertyName : AnsiString) : plua_RecordPropertyReader;
    function  GetPropWriter(RecId: TLuaRecId; const aPropertyName : AnsiString; out ReadOnly : Boolean) : plua_RecordPropertyWriter;

    function  GetInfo(l : PLua_State; RecordPointer: Pointer) : PLuaRecordInstanceInfo;

    // becomes an owner of aRecordInfo!!!
    function  Add(aRecordInfo : PLuaRecordInfo) : Integer;

    procedure Remove(aRecordName : AnsiString);
    function  IndexOf(aRecordName : AnsiString) : Integer;
    procedure Clear;
    property  Count : integer read GetCount;
    property  RecordInfo[index : integer]:PLuaRecordInfo read GetRecordInfo; default;
    property  RecInfoById[Id : TLuaRecId]:PLuaRecordInfo read GetRecInfoById;
  end;

  { TLuaClassTypesList }

  TLuaRecordTypesList = class
    fItems : TWordList;
    fItemList : TList;
  private
    function GetCount: Integer;
    function GetIndexedItem(index : integer): PLuaRecordInfo;
    function GetItem(ItemName : AnsiString): PLuaRecordInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function  Add(ItemName : AnsiString {; LuaParent : PLuaRecordInfo = nil}) : PLuaRecordInfo;
    procedure Remove(ItemName : AnsiString);
    procedure Clear;

    procedure RegisterTo(L : PLua_State);

    property Item[ItemName : AnsiString] : PLuaRecordInfo read GetItem; default;
    property IndexedItem[index : integer] : PLuaRecordInfo read GetIndexedItem;
    property Count : Integer read GetCount;
  end;

// Important!!! Lua becomes RecordInfo owner, so leave it alone.
procedure plua_registerRecordType( l : PLua_State; RecordInfo : PLuaRecordInfo);

procedure plua_newRecordInfo( var RecordInfoPointer : PLuaRecordInfo);
procedure plua_initRecordInfo( var RecordInfo : TLuaRecordInfo);
procedure plua_releaseRecordInfo( var RecordInfo : PLuaRecordInfo);

procedure plua_RecordMarkFree(l: Plua_State; RecordPointer: Pointer);
function plua_registerExistingRecord( l : PLua_State; InstanceName : AnsiString;
                                      RecordPointer: Pointer;
                                      RecordInfo : PLuaRecordInfo;
                                      FreeOnGC : Boolean = false) : PLuaRecordInstanceInfo;

function plua_pushexisting( l : PLua_State;
                            RecordPointer: Pointer;
                            RecordId : TLuaRecId;
                            FreeOnGC : Boolean = false) : PLuaRecordInstanceInfo;

procedure plua_AddRecordProperty( var RecordInfo : TLuaRecordInfo;
                                 propertyName : AnsiString;
                                 Reader   : plua_RecordPropertyReader;
                                 Writer   : plua_RecordPropertyWriter );

function plua_getRecord( l : PLua_State; idx : Integer) : Pointer;
function plua_getRecordInfo( l : PLua_State; idx : Integer) : PLuaRecordInstanceInfo;
procedure plua_PushRecord(RecordInfo : PLuaRecordInstanceInfo);
function  plua_GetRecordInfo( l : PLua_State; RecordPointer : Pointer) : PLuaRecordInstanceInfo;

procedure plua_PushRecordToTable( L : PLua_State; RecordPointer : Pointer;
                                  RecordInfo : PLuaRecordInfo );

implementation
uses LuaWrapper;

function RecordMetaTableName(cinfo:PLuaRecordInfo):string;
begin
  //returning PChar(cinfo^.RecordName+'_mt') gives a memory leak :[
  Result:=cinfo^.RecordName+'_mt';
end;

function plua_gc_record(l : PLua_State) : integer; extdecl; forward;

function plua_index_record(l : PLua_State) : integer; extdecl;
var
  propName : AnsiString;
  propValueStart : Integer;
  rec      : pointer;
  rInfo    : PLuaRecordInstanceInfo;
  reader   : plua_RecordPropertyReader;
  pcount   : Integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  if not lua_istable(l, 1) then
    exit;

  rInfo := plua_GetRecordInfo(l, 1);
  if not assigned(rInfo) then
    exit;
  rec := rInfo^.RecordPointer;

  propName := plua_tostring(l, 2);
  propValueStart := 3;

  reader := LuaSelf(l).LuaRecords.GetPropReader( rInfo^.RecordId, propName);
  if assigned(reader) then
    result := reader(rec, l, propValueStart, pcount);
end;

function plua_newindex_record(l : PLua_State) : integer; extdecl;
var
  propName : AnsiString;
  propValueStart : Integer;
  rec      : pointer;
  rInfo    : PLuaRecordInstanceInfo;
  writer   : plua_RecordPropertyWriter;
  bReadOnly: Boolean;
  pcount   : Integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  if not lua_istable(l, 1) then
    exit;

  rInfo := plua_GetRecordInfo(l, 1);
  if not assigned(rInfo) then
    exit;
  rec := rInfo^.RecordPointer;

  propName := plua_tostring(l, 2);
  propValueStart := 3;
  writer := LuaSelf(l).LuaRecords.GetPropWriter(rInfo^.RecordId, propName, bReadOnly);
  if assigned(writer) then
    result := writer(rec, l, propValueStart, pcount)
  else
    begin
      if not bReadOnly then
        begin
          plua_pushstring(l, propName);
          lua_pushvalue(l, propValueStart);
          lua_rawset(l, 1);
        end;
    end;
end;

function plua_new_record(l : PLua_State) : integer; extdecl;
var
  n, tidx,
  oidx    : Integer;
  recordPTR: Pointer;
  recordId : TLuaRecId;
  rInfo   : PLuarecordInfo;
  instance: PLuaRecordInstanceInfo;
  pcount  : integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  n := lua_gettop(l);
  if (n < 1) or (not (lua_istable(l, 1))) then
    exit;

  tidx := 1;

  lua_pushstring(l, '__RecordId');
  lua_rawget(l, tidx);
  recordId := lua_touserdata(l, -1);
  lua_pop(l, 1);

  rInfo := LuaSelf(l).LuaRecords.RecInfoById[ recordId ];

  new(instance);
  instance^.OwnsInstance := true;
  instance^.RecordId := recordId;
  instance^.l := l;
  instance^.RecordPointer := rInfo^.New(l, 2, pcount, instance);
  LuaSelf(l).intLuaRecords.Add(pointer(instance));

  lua_newtable(L);
  instance^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti(l, LUA_REGISTRYINDEX, instance^.LuaRef);
  oidx := lua_gettop(L);

  lua_pushliteral(L, '__instance');
  lua_pushinteger(L, PtrUint(instance));
  lua_rawset(l, oidx);

  lua_pushstring(L, 'release');
  lua_pushcfunction(L, @plua_gc_record);
  lua_rawset(L, oidx);

  luaL_getmetatable(l, PChar(RecordMetaTableName(rInfo)));
  lua_setmetatable(l, -2);

  result := 1;
end;

function plua_getRecordInfoFromUserData(l: PLua_State; idx: Integer): PLuaRecordInstanceInfo;
var obj_user:^PLuaRecordInstanceInfo;
begin
  result := nil;

  obj_user:=lua_touserdata(l, plua_absindex(l, idx));
  if obj_user <> nil then
     Result:=PLuaRecordInstanceInfo(obj_user^);

  lua_pop(l, 1);
end;

procedure plua_ref_release(l : PLua_State; obj:PLuaRecordInstanceInfo);
begin
  if obj^.LuaRef <> LUA_NOREF then
    begin
      luaL_unref(L, LUA_REGISTRYINDEX, obj^.LuaRef);
      obj^.LuaRef:=LUA_NOREF;
    end;
end;

function plua_gc_record(l : PLua_State) : integer; extdecl;
var
  nfo : PLuaRecordInstanceInfo;
  Lua : TLuaInternalState;
begin
  //nfo := plua_GetRecordInfo(l, 1);
  nfo:=plua_getRecordInfoFromUserData(l, 1);
  if not assigned(nfo) then
    exit;

  Lua:=LuaSelf(l);
  Lua.intLuaRecords.Remove(nfo);

  LuaRecs_Free(Lua, nfo);
  result := 0;
end;

procedure plua_registerRecordType(l: PLua_State; RecordInfo: PLuaRecordInfo);
var
  lidx, tidx, midx : integer;
  ci   : PLuaRecordInfo;
  registered:boolean;
  S:TLuaInternalState;
  StartTop:Integer;
begin
  StartTop:=lua_gettop(l);
  try
    //already registered?
    luaL_getmetatable(l, PChar(RecordMetaTableName(RecordInfo)) );
    registered:=lua_istable(l, -1);
    lua_pop(l, 1);
    if registered then
      begin
        plua_releaseRecordInfo(RecordInfo);
        Exit;
      end;

    S:=LuaSelf(l);
    lidx:=S.LuaRecords.IndexOf( RecordInfo^.RecordName );
    if lidx <> -1 then
      begin
        plua_releaseRecordInfo(RecordInfo);
        Exit;
      end;
    lidx:=S.LuaRecords.Add(RecordInfo);

    plua_pushstring(l, RecordInfo^.RecordName);
    lua_newtable(l);

    luaL_newmetatable(l, PChar(RecordMetaTableName(RecordInfo)));
    lua_setmetatable(l, -2);
    lua_settable(l, LUA_GLOBALSINDEX);

    luaL_getmetatable(l, PChar(RecordMetaTableName(RecordInfo)));
    midx := lua_gettop(l);

    plua_pushstring(l, RecordInfo^.RecordName);
    lua_gettable(l, LUA_GLOBALSINDEX);
    tidx := lua_gettop(l);

    lua_pushstring(L, '__call');
    lua_pushcfunction(L, @plua_new_record);
    lua_rawset(L, midx);
    lua_pushstring(L, '__gc');
    lua_pushcfunction(L, @plua_gc_record);
    lua_rawset(L, midx);

    lua_pushstring(L, 'new');
    lua_pushcfunction(L, @plua_new_record);
    lua_rawset(L, tidx);

    lua_pushstring(L, '__RecordId');
    lua_pushlightuserdata(L, RecordInfo^.RecId);
    lua_rawset(L, tidx);

    lua_pushstring(L, '__index');
    lua_pushcfunction(L, @plua_index_record);
    lua_rawset(L, midx);
    lua_pushstring(L, '__newindex');
    lua_pushcfunction(L, @plua_newindex_record);
    lua_rawset(L, midx);
  finally
    plua_EnsureStackBalance(l, StartTop);
  end;
end;

procedure plua_newRecordInfo(var RecordInfoPointer: PLuaRecordInfo);
begin
  new(RecordInfoPointer);
  plua_initRecordInfo(RecordInfoPointer^);
end;

procedure plua_initRecordInfo(var RecordInfo: TLuaRecordInfo);
begin
  RecordInfo.RecordName := '';
  //RecordInfo.Parent     := nil;
  RecordInfo.PropHandlers := TWordList.Create;
  RecordInfo.New        := nil;
  RecordInfo.Release    := nil;
  SetLength(RecordInfo.Properties, 0);
end;

procedure plua_releaseRecordInfo(var RecordInfo: PLuaRecordInfo);
begin
  FreeAndNil( RecordInfo^.PropHandlers );
  Finalize( RecordInfo^.Properties );
  Finalize( RecordInfo^.RecordName );

  Freemem(RecordInfo);
  RecordInfo:=nil;
end;

procedure plua_RecordMarkFree(l: Plua_State; RecordPointer: Pointer);
var objinfo:PLuaRecordInstanceInfo;
begin
  objinfo := plua_getRecordInfo(l, RecordPointer);
  if objinfo = nil then
     raise LuaException.CreateFmt('Object $%P does not have record info', [Pointer(RecordPointer)]);

  //remove reference
  plua_ref_release(l, objinfo);
end;

function plua_registerExistingRecord(l: PLua_State; InstanceName: AnsiString;
  RecordPointer: Pointer; RecordInfo: PLuaRecordInfo; FreeOnGC: Boolean
  ): PLuaRecordInstanceInfo;
begin
  plua_pushstring(l, InstanceName);
  Result:=plua_pushexisting(l, RecordPointer, RecordInfo, FreeOnGC);
  lua_settable(l, LUA_GLOBALSINDEX );
end;

function plua_pushexisting(l: PLua_State; RecordPointer: Pointer;
  RecordId : TLuaRecId; FreeOnGC: Boolean): PLuaRecordInstanceInfo;
var
  oidx    : Integer;
  rInfo   : PLuaRecordInfo;
  obj_user: ^PLuaRecordInstanceInfo;
  Lua     : TLuaInternalState;
begin
  Result := plua_GetRecordInfo(l, RecordPointer);
  if assigned(Result) then
    begin
      plua_PushRecord(Result);
      exit;
    end;

  Lua:=LuaSelf(l);
  rInfo := Lua.LuaRecords.RecInfoById[ RecordId ];

  new(Result);
  Result^.OwnsInstance := FreeOnGC;
  Result^.RecordId := RecordId;
  Result^.l := l;
  Result^.RecordPointer := RecordPointer;

  Lua.intLuaRecords.Add(pointer(Result));

  lua_newtable(L);
  oidx := lua_gettop(L);

  lua_pushliteral(L, '__instance');
  lua_pushinteger(L, PtrUint(Result));
  lua_rawset(l, oidx);

  lua_pushliteral(L, '__instance2');
  obj_user:=lua_newuserdata(L, sizeof(obj_user));
  obj_user^:=Result;
  Result^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti(l, LUA_REGISTRYINDEX, Result^.LuaRef);
  luaL_getmetatable(l, PChar(RecordMetaTableName(rinfo)));
  lua_setmetatable(l, -2);
  lua_rawset(l, oidx);

  luaL_getmetatable(l, PChar(RecordMetaTableName(rinfo)));
  lua_setmetatable(l, -2);
end;

procedure plua_AddRecordProperty(var RecordInfo: TLuaRecordInfo;
  propertyName: AnsiString; Reader: plua_RecordPropertyReader;
  Writer: plua_RecordPropertyWriter);
var
  idx : integer;
begin
  idx := Length(RecordInfo.Properties);
  SetLength(RecordInfo.Properties, idx+1);
  RecordInfo.Properties[idx].PropName := propertyName;
  RecordInfo.Properties[idx].Reader   := Reader;
  RecordInfo.Properties[idx].Writer   := Writer;
  RecordInfo.PropHandlers.AddWord(propertyName)^.data := pointer(PtrUint(idx));
end;

function plua_getRecord(l: PLua_State; idx: Integer): Pointer;
var
  instance : PLuaRecordInstanceInfo;
begin
  result := nil;
  plua_pushstring(l, '__instance');
  lua_rawget(l, plua_absindex(l, idx));
  instance := PLuaRecordInstanceInfo(PtrUint(lua_tointeger(l, -1)));
  lua_pop(l, 1);
  if assigned(instance) and assigned(instance^.RecordPointer) then
    result := instance^.RecordPointer;
end;

function plua_getRecordInfo(l: PLua_State; idx: Integer
  ): PLuaRecordInstanceInfo;
begin
  result := nil;
  plua_pushstring(l, '__instance');
  lua_rawget(l, plua_absindex(l, idx));
  result := PLuaRecordInstanceInfo(PtrUint(lua_tointeger(l, -1)));
  lua_pop(l, 1);
end;

procedure plua_PushRecord(RecordInfo: PLuaRecordInstanceInfo);
begin
  lua_rawgeti(RecordInfo^.l, LUA_REGISTRYINDEX, RecordInfo^.LuaRef);
end;

function plua_GetRecordInfo(l : PLua_State; RecordPointer: Pointer): PLuaRecordInstanceInfo;
begin
  Result:=LuaSelf(l).LuaRecords.GetInfo(l, RecordPointer);
end;

procedure plua_PushRecordToTable(L: PLua_State; RecordPointer: Pointer;
  RecordInfo: PLuaRecordInfo);
var
  i, tblIdx : Integer;
begin
  lua_newtable(L);
  tblIdx := lua_gettop(L);
  for i := 0 to Length(RecordInfo^.Properties) -1 do
    if assigned(RecordInfo^.Properties[i].Writer) then
      begin
        plua_pushstring(L, RecordInfo^.Properties[i].PropName);
        RecordInfo^.Properties[i].Writer(RecordPointer, L, 0, 0);
        lua_settable(l, tblidx);
      end;
end;

{ TLuaRecordList }

function TLuaRecordList.GetRecInfoById(Id : TLuaRecId): PLuaRecordInfo;
begin
  Result:= FRecordIdToRecordInfo.KeyData[ Id ];
end;

function TLuaRecordList.GetRecordInfo(index: integer): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems[index]);
end;

function TLuaRecordList.GetCount: integer;
begin
  result := fItems.Count;
end;

procedure TLuaRecordList.FreeItem(I: PLuaRecordInfo);
begin
  FRecordIdToRecordInfo.Remove(I^.RecId);
  plua_releaseRecordInfo(I);
end;

constructor TLuaRecordList.Create;
begin
  fItems := TList.Create;
  FRecordIdToRecordInfo:=TRecIdObjMap.Create;
  FRecordIdToRecordInfo.Sorted:=True;
end;

destructor TLuaRecordList.Destroy;
begin
  Clear;
  fItems.Free;
  FRecordIdToRecordInfo.Free;
  inherited Destroy;
end;

function TLuaRecordList.GetPropReader(RecId: TLuaRecId; const aPropertyName: AnsiString): plua_RecordPropertyReader;
var
  pi : PtrInt;
  ei : PWordListSymbol;
  aRecordInfo: PLuaRecordInfo;
begin
  aRecordInfo:=RecInfoById[ RecId ];

// TODO - Add parent property calls in
  result := nil;
  ei := aRecordInfo^.PropHandlers.WordSymbol[aPropertyName];
  if not assigned(ei) then
    exit;
  pi := PtrInt(ei^.data);
  if (pi >= 0) and (pi < length(aRecordInfo^.Properties)) then
    result := aRecordInfo^.Properties[pi].Reader;
end;

function TLuaRecordList.GetPropWriter(RecId: TLuaRecId; const aPropertyName: AnsiString; out ReadOnly: Boolean): plua_RecordPropertyWriter;
var
  pi : PtrInt;
  ei : PWordListSymbol;
  aRecordInfo: PLuaRecordInfo;
begin
  aRecordInfo:=RecInfoById[ RecId ];

// TODO - Add parent property calls in
  ReadOnly := false;
  result := nil;
  ei := aRecordInfo^.PropHandlers.WordSymbol[aPropertyName];
  if not assigned(ei) then
    exit;
  pi := PtrInt(ei^.data);
  if (pi >= 0) and (pi < length(aRecordInfo^.Properties)) then
    begin
      ReadOnly := aRecordInfo^.Properties[pi].Writer = nil;
      result := aRecordInfo^.Properties[pi].Writer;
    end;
end;

function TLuaRecordList.GetInfo(l : PLua_State; RecordPointer: Pointer
  ): PLuaRecordInstanceInfo;
var
  i : Integer;
  S:TLuaInternalState;
begin
  result := nil;
  i := 0;
  S:=LuaSelf(l);

  while (result = nil) and (i < S.intLuaRecords.Count) do
    begin
      if (PLuaRecordInstanceInfo(S.intLuaRecords[i])^.RecordPointer = RecordPointer) and
         (PLuaRecordInstanceInfo(S.intLuaRecords[i])^.l = l) then
        result := PLuaRecordInstanceInfo(S.intLuaRecords[i]);
      inc(i);
    end;
end;

function TLuaRecordList.Add(aRecordInfo: PLuaRecordInfo): Integer;
var
  ri  : PLuaRecordInfo;
begin
  result := IndexOf(aRecordInfo^.RecordName);
  if result = -1 then
    begin
      result := fItems.Add(aRecordInfo);
    end
  else
    begin
      ri := RecordInfo[result];
      FreeItem(ri);
      fItems[result]:=aRecordInfo;
    end;

  FRecordIdToRecordInfo.Add( aRecordInfo^.RecId, aRecordInfo );
end;

procedure TLuaRecordList.Remove(aRecordName: AnsiString);
var
  idx : integer;
  ri  : PLuaRecordInfo;
begin
  idx := IndexOf(aRecordName);
  if idx > -1 then
    begin
      ri := RecordInfo[idx];
      FreeItem(ri);
      fItems.Delete(idx);
    end;
end;

function TLuaRecordList.IndexOf(aRecordName: AnsiString): Integer;
var
  i : Integer;
begin
  result := -1;
  i := 0;
  while (result = -1) and (i < count) do
    begin
      if CompareText(aRecordName, RecordInfo[i]^.RecordName) = 0 then
        result := i;
      inc(i);
    end;
end;

procedure TLuaRecordList.Clear;
var
  ri : PLuaRecordInfo;
begin
  FRecordIdToRecordInfo.Clear;
  while count > 0 do
    begin
      ri := RecordInfo[count-1];
      FreeItem(ri);
      fItems.Delete(count-1);
    end;
end;

{ TLuaRecordTypesList }

function TLuaRecordTypesList.GetCount: Integer;
begin
  result := fItemList.Count;
end;

function TLuaRecordTypesList.GetIndexedItem(index : integer): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItemList[index]);
end;

function TLuaRecordTypesList.GetItem(ItemName : AnsiString): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems.WordData[ItemName]);
end;

constructor TLuaRecordTypesList.Create;
begin
  fItems := TWordList.Create;
  fItemList := TList.Create;
end;

destructor TLuaRecordTypesList.Destroy;
begin
  Clear;
  fItems.Free;
  fItemList.Free;
  inherited Destroy;
end;

function TLuaRecordTypesList.Add(ItemName: AnsiString {; LuaParent : PLuaRecordInfo = nil}): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems.WordData[ItemName]);
  if not assigned(result) then
    begin
      plua_newRecordInfo(result);
      //result^.Parent := LuaParent;
      result^.RecordName := ItemName;
      fItems.AddWord(ItemName)^.data := result;
      fItemList.Add(result);
    end;
end;

procedure TLuaRecordTypesList.Remove(ItemName: AnsiString);
var
  wd : PWordListSymbol;
  ci : PLuaRecordInfo;
begin
  wd := fItems.WordSymbol[ItemName];
  if (assigned(wd)) and (assigned(wd^.data)) and (wd^.eow) then
    begin
      ci := PLuaRecordInfo(wd^.data);
      fItemList.Remove(wd^.data);
      wd^.data := nil;
      wd^.eow := false;
      plua_releaseRecordInfo(ci);
    end;
end;

procedure TLuaRecordTypesList.Clear;
begin
  while Count > 0 do
    Remove(IndexedItem[Count-1]^.RecordName);
end;

procedure TLuaRecordTypesList.RegisterTo(L: PLua_State);
var
  i : Integer;
begin
  for i := 0 to Count-1 do
    plua_registerRecordType(l, IndexedItem[i]);
end;

end.
