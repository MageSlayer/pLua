unit pLuaRecord;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua, pLua, uWordList, pLuaObject;
  
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

  TLuaRecordInfo = record
    Parent      : PLuaRecordInfo;
    RecordName  : AnsiString;
    PropHandlers: TWordList;
    New         : plua_RecordConstructor;
    Release     : plua_RecordDestructor;
    Properties  : Array of TLuaRecordProperty;
  end;
  
  TLuaRecordInstanceInfo = record
    OwnsInstance : Boolean;
    LuaRef       : Integer;
    RecordInfo   : PLuaRecordInfo;
    l            : PLua_state;
    RecordPointer: Pointer;
  end;

  { TLuaRecordList }

  TLuaRecordList = class
    fItems : TList;
  private
    function GetRecordInfo(index : integer): PLuaRecordInfo;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function  GetPropReader(aRecordInfo : PLuaRecordInfo; aPropertyName : AnsiString) : plua_RecordPropertyReader;
    function  GetPropWriter(aRecordInfo : PLuaRecordInfo; aPropertyName : AnsiString; out ReadOnly : Boolean) : plua_RecordPropertyWriter;

    function  GetInfo(l : PLua_State; RecordPointer: Pointer) : PLuaRecordInstanceInfo;

    function  Add(aRecordInfo : TLuaRecordInfo) : Integer;
    procedure Remove(aRecordName : AnsiString);
    function  IndexOf(aRecordName : AnsiString) : Integer;
    procedure Clear;
    property  Count : integer read GetCount;
    property  RecordInfo[index : integer]:PLuaRecordInfo read GetRecordInfo; default;
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

    function  Add(ItemName : AnsiString; LuaParent : PLuaRecordInfo = nil) : PLuaRecordInfo;
    procedure Remove(ItemName : AnsiString);
    procedure Clear;

    procedure RegisterTo(L : PLua_State);

    property Item[ItemName : AnsiString] : PLuaRecordInfo read GetItem; default;
    property IndexedItem[index : integer] : PLuaRecordInfo read GetIndexedItem;
    property Count : Integer read GetCount;
  end;

procedure plua_registerRecordType( l : PLua_State; const RecordInfo : TLuaRecordInfo);
procedure plua_newRecordInfo( var RecordInfoPointer : PLuaRecordInfo);
procedure plua_initRecordInfo( var RecordInfo : TLuaRecordInfo);
procedure plua_releaseRecordInfo( var RecordInfoPointer : PLuaRecordInfo);
procedure plua_releaseRecordInfo( var RecordInfo : TLuaRecordInfo);

procedure plua_RecordMarkFree(l: Plua_State; RecordPointer: Pointer);
function plua_registerExistingRecord( l : PLua_State; InstanceName : AnsiString;
                                      RecordPointer: Pointer;
                                      RecordInfo : PLuaRecordInfo;
                                      FreeOnGC : Boolean = false) : PLuaRecordInstanceInfo;

function plua_pushexisting( l : PLua_State;
                            RecordPointer: Pointer;
                            RecordInfo : PLuaRecordInfo;
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

procedure plua_ClearRecords( L : PLua_State );

implementation
uses LuaWrapper;

function RecordMetaTableName(cinfo:PLuaRecordInfo):string;
begin
  //returning PChar(cinfo^.RecordName+'_mt') gives a memory leak :[
  Result:=cinfo^.RecordName+'_mt';
end;

function plua_gc_record(l : PLua_State) : integer; cdecl; forward;

function plua_index_record(l : PLua_State) : integer; cdecl;
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
  reader := LuaSelf(l).LuaRecords.GetPropReader(rInfo^.recordInfo, propName);
  if assigned(reader) then
    result := reader(rec, l, propValueStart, pcount);
end;

function plua_newindex_record(l : PLua_State) : integer; cdecl;
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
  writer := LuaSelf(l).LuaRecords.GetPropWriter(rInfo^.recordInfo, propName, bReadOnly);
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

function plua_new_record(l : PLua_State) : integer; cdecl;
var
  n, tidx,
  oidx    : Integer;
  recordPTR: Pointer;
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

  lua_pushstring(l, '__recordPTR');
  lua_rawget(l, tidx);
  recordPTR := pointer(PtrInt(lua_tointeger(l, -1)));
  rInfo := PLuarecordInfo(recordPTR);
  lua_pop(l, 1);

  new(instance);
  instance^.OwnsInstance := true;
  instance^.recordInfo := rInfo;
  instance^.l := l;
  instance^.RecordPointer := rInfo^.New(l, 2, pcount, instance);
  LuaSelf(l).intLuaRecords.Add(pointer(instance));

  lua_newtable(L);
  instance^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti(l, LUA_REGISTRYINDEX, instance^.LuaRef);
  oidx := lua_gettop(L);

  lua_pushliteral(L, '__instance');
  lua_pushinteger(L, PtrInt(instance));
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

function plua_gc_record(l : PLua_State) : integer; cdecl;
var
  nfo : PLuaRecordInstanceInfo;
begin
  //nfo := plua_GetRecordInfo(l, 1);
  nfo:=plua_getRecordInfoFromUserData(l, 1);
  if not assigned(nfo) then
    exit;
  LuaSelf(l).intLuaRecords.Remove(nfo);
  if nfo^.OwnsInstance then
    begin
      nfo^.RecordInfo^.Release(nfo^.RecordPointer, l);
      nfo^.RecordPointer := nil;
    end;
  //luaL_unref(L, LUA_REGISTRYINDEX, nfo^.LuaRef);
  freemem(nfo);
  result := 0;
end;

procedure plua_registerRecordType(l: PLua_State; const RecordInfo: TLuaRecordInfo);
var
  lidx, tidx, midx : integer;
  ci   : PLuaRecordInfo;
  registered:boolean;
  S:TLuaInternalState;
begin
  //already registered?
  luaL_getmetatable(l, PChar(RecordMetaTableName(@RecordInfo)) );
  registered:=lua_istable(l, -1);
  lua_pop(l, 1);
  if registered then
    begin
      Exit;
    end;

  S:=LuaSelf(l);
  lidx:=S.LuaRecords.IndexOf( RecordInfo.RecordName );
  if lidx = -1 then
    begin
      lidx:=S.LuaRecords.Add(RecordInfo);
    end;

  plua_pushstring(l, RecordInfo.RecordName);
  lua_newtable(l);

  luaL_newmetatable(l, PChar(RecordMetaTableName(@RecordInfo)));
  lua_setmetatable(l, -2);
  lua_settable(l, LUA_GLOBALSINDEX);

  luaL_getmetatable(l, PChar(RecordMetaTableName(@RecordInfo)));
  midx := lua_gettop(l);

  plua_pushstring(l, RecordInfo.RecordName);
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

  lua_pushstring(L, '__recordID');
  lua_pushinteger(L, lidx);
  lua_rawset(L, tidx);
  lua_pushstring(L, '__recordPTR');
  ci := S.LuaRecords.RecordInfo[lidx];
  lua_pushinteger(L, PtrInt(ci));
  lua_rawset(L, tidx);

  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @plua_index_record);
  lua_rawset(L, midx);
  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @plua_newindex_record);
  lua_rawset(L, midx);
end;

procedure plua_newRecordInfo(var RecordInfoPointer: PLuaRecordInfo);
begin
  if RecordInfoPointer = nil then
    new(RecordInfoPointer);
  plua_initRecordInfo(RecordInfoPointer^);
end;

procedure plua_initRecordInfo(var RecordInfo: TLuaRecordInfo);
begin
  RecordInfo.RecordName := '';
  RecordInfo.Parent     := nil;
  RecordInfo.PropHandlers := TWordList.Create;
  RecordInfo.New        := nil;
  RecordInfo.Release    := nil;
  SetLength(RecordInfo.Properties, 0);
end;

procedure plua_releaseRecordInfo(var RecordInfo: TLuaRecordInfo);
begin
  FreeAndNil( RecordInfo.PropHandlers );
  Finalize( RecordInfo.Properties );
end;

procedure plua_releaseRecordInfo(var RecordInfoPointer: PLuaRecordInfo);
begin
  plua_releaseRecordInfo(RecordInfoPointer^);
  Freemem(RecordInfoPointer);
  RecordInfoPointer:=nil;
end;

procedure plua_RecordMarkFree(l: Plua_State; RecordPointer: Pointer);
var objinfo:PLuaRecordInstanceInfo;
begin
  objinfo := plua_getRecordInfo(l, RecordPointer);
  if objinfo = nil then
     raise LuaException.CreateFmt('Object $%x does not have record info', [PtrUInt(RecordPointer)]);

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
  RecordInfo: PLuaRecordInfo; FreeOnGC: Boolean): PLuaRecordInstanceInfo;
var
  oidx    : Integer;
  rInfo   : PLuaRecordInfo;
  obj_user: ^PLuaRecordInstanceInfo;
begin
  Result := plua_GetRecordInfo(l, RecordPointer);
  if assigned(Result) then
    begin
      plua_PushRecord(Result);
      exit;
    end;

  rInfo := RecordInfo;

  new(Result);
  Result^.OwnsInstance := FreeOnGC;
  Result^.RecordInfo := rInfo;
  Result^.l := l;
  Result^.RecordPointer := RecordPointer;

  LuaSelf(l).intLuaRecords.Add(pointer(Result));

  lua_newtable(L);
  oidx := lua_gettop(L);

  lua_pushliteral(L, '__instance');
  lua_pushinteger(L, PtrInt(Result));
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
  RecordInfo.PropHandlers.AddWord(propertyName)^.data := pointer(PtrInt(idx));
end;

function plua_getRecord(l: PLua_State; idx: Integer): Pointer;
var
  instance : PLuaRecordInstanceInfo;
begin
  result := nil;
  plua_pushstring(l, '__instance');
  lua_rawget(l, plua_absindex(l, idx));
  instance := PLuaRecordInstanceInfo(ptrint(lua_tointeger(l, -1)));
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
  result := PLuaRecordInstanceInfo(ptrint(lua_tointeger(l, -1)));
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

procedure plua_ClearRecords(L: PLua_State);
var
  i   : Integer;
  nfo : PLuaRecordInstanceInfo;
  S   : TLuaInternalState;
begin
  S:=LuaSelf(l);
  i := S.intLuaRecords.Count-1;
  while i > -1 do
    begin
      nfo := PLuaRecordInstanceInfo(S.intLuaRecords[i]);
      if nfo^.l = l then
        S.intLuaRecords.Remove(nfo);
      dec(i);
    end;
end;

{ TLuaRecordList }

function TLuaRecordList.GetRecordInfo(index: integer): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems[index]);
end;

function TLuaRecordList.GetCount: integer;
begin
  result := fItems.Count;
end;

constructor TLuaRecordList.Create;
begin
  fItems := TList.Create;
end;

destructor TLuaRecordList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

function TLuaRecordList.GetPropReader(aRecordInfo: PLuaRecordInfo;
  aPropertyName: AnsiString): plua_RecordPropertyReader;
var
  pi : PtrInt;
  ei : PWordListSymbol;
begin
// TODO - Add parent property calls in
  result := nil;
  ei := aRecordInfo^.PropHandlers.WordSymbol[aPropertyName];
  if not assigned(ei) then
    exit;
  pi := PtrInt(ei^.data);
  if (pi >= 0) and (pi < length(aRecordInfo^.Properties)) then
    result := aRecordInfo^.Properties[pi].Reader;
end;

function TLuaRecordList.GetPropWriter(aRecordInfo: PLuaRecordInfo;
  aPropertyName: AnsiString; out ReadOnly: Boolean): plua_RecordPropertyWriter;
var
  pi : PtrInt;
  ei : PWordListSymbol;
begin
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

function TLuaRecordList.Add(aRecordInfo: TLuaRecordInfo): Integer;
var
  ri  : PLuaRecordInfo;
begin
  result := IndexOf(aRecordInfo.RecordName);
  if result = -1 then
    begin
      new(ri);
      result := fItems.Add(ri);
    end
  else
    ri := RecordInfo[result];
  ri^ := aRecordInfo;
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
      fItems.Delete(idx);
      Freemem(ri);
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
  while count > 0 do
    begin
      ri := RecordInfo[count-1];
      fItems.Delete(count-1);
      Freemem(ri);
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

function TLuaRecordTypesList.Add(ItemName: AnsiString; LuaParent : PLuaRecordInfo = nil): PLuaRecordInfo;
begin
  result := PLuaRecordInfo(fItems.WordData[ItemName]);
  if not assigned(result) then
    begin
      plua_newRecordInfo(result);
      result^.Parent := LuaParent;
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
    plua_registerRecordType(l, IndexedItem[i]^);
end;

end.
