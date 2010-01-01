unit pLuaObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua, pLua, uWordList;

type
  TLuaObjectEventDelegate = class;
  PLuaInstanceInfo = ^TLuaInstanceInfo;
  plua_ClassMethodWrapper = function(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
  plua_PropertyReader   = plua_ClassMethodWrapper;
  plua_PropertyWriter   = plua_ClassMethodWrapper;
  plua_MethodWrapper    = plua_ClassMethodWrapper;
  plua_ClassConstructor = function(l : Plua_State; paramidxstart, paramcount : integer; InstanceInfo : PLuaInstanceInfo) : TObject;
  plua_ClassDestructor  = procedure(target : TObject; l : Plua_State);

  PLuaClassInfo = ^TLuaClassInfo;
  PLuaClassProperty = ^TLuaClassProperty;
  TLuaClassProperty = record
    PropName : AnsiString;
    Reader   : plua_PropertyReader;
    Writer   : plua_PropertyWriter;
  end;
  
  TLuaClassMethod = record
    MethodName : AnsiString;
    wrapper    : plua_MethodWrapper;
  end;
  
  TLuaClassInfo = record
    Parent      : PLuaClassInfo;
    ClassName   : AnsiString;
    New         : plua_ClassConstructor;
    Release     : plua_ClassDestructor;
    PropHandlers: TWordList;
    UnhandledReader : plua_PropertyReader;
    UnhandledWriter : plua_PropertyWriter;
    Properties  : Array of TLuaClassProperty;
    Methods     : Array of TLuaClassMethod;
  end;

  TLuaInstanceInfo = record
    OwnsObject: Boolean;
    LuaRef    : Integer;
    ClassInfo : PLuaClassInfo;
    l         : PLua_state;
    obj       : TObject;
    Delegate  : TLuaObjectEventDelegate;
  end;

  { TLuaClassList }

  TLuaClassList = class
    fItems : TList;
  private
    function GetClassInfo(index : integer): PLuaClassInfo;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function  GetPropReader(aClassInfo : PLuaClassInfo; aPropertyName : AnsiString) : plua_PropertyReader;
    function  GetPropWriter(aClassInfo : PLuaClassInfo; aPropertyName : AnsiString; out ReadOnly : Boolean) : plua_PropertyWriter;

    function  GetInfo(l : Plua_State; InstanceObject : TObject) : PLuaInstanceInfo;

    function  Add(aClassInfo : TLuaClassInfo) : Integer;
    procedure Remove(aClassName : AnsiString);
    function  IndexOf(aClassName : AnsiString) : Integer;
    procedure Clear;
    property  Count : integer read GetCount;
    property  ClassInfo[index : integer]:PLuaClassInfo read GetClassInfo; default;
  end;

  { TLuaObjectEventDelegate }

  TLuaObjectEventDelegate = class
  protected
    FInstanceInfo : PLuaInstanceInfo;
    FObj          : TObject;
    
    function  EventExists( EventName :AnsiString ) : Boolean;
    function  CallEvent( EventName :AnsiString ) : Integer; overload;
    function  CallEvent( EventName :AnsiString;
                         const Args: array of Variant ) : Integer; overload;
    function  CallEvent( EventName :AnsiString;
                         const Args: array of Variant;
                         Results : PVariantArray = nil):Integer; overload;
  public
    constructor Create(InstanceInfo : PLuaInstanceInfo; obj : TObject); virtual;
    destructor Destroy; override;
  end;

  { TLuaClassTypesList }

  TLuaClassTypesList = class
    fItems : TWordList;
    fItemList : TList;
  private
    function GetCount: Integer;
    function GetIndexedItem(index : integer): PLuaClassInfo;
    function GetItem(ItemName : AnsiString): PLuaClassInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function  Add(ItemName : AnsiString; LuaParent : PLuaClassInfo = nil) : PLuaClassInfo;
    procedure Remove(ItemName : AnsiString);
    procedure Clear;

    procedure RegisterTo(L : PLua_State);

    property Item[ItemName : AnsiString] : PLuaClassInfo read GetItem; default;
    property IndexedItem[index : integer] : PLuaClassInfo read GetIndexedItem;
    property Count : Integer read GetCount;
  end;

procedure plua_CheckStackBalance(l: PLua_State; TopToBe:Integer);
procedure plua_registerclass( l : PLua_State; classInfo : TLuaClassInfo);
procedure plua_newClassInfo( var ClassInfoPointer : PLuaClassInfo);
procedure plua_initClassInfo( var ClassInfo : TLuaClassInfo);

procedure plua_releaseClassInfo( var ClassInfoPointer : PLuaClassInfo);overload;
procedure plua_releaseClassInfo(var ClassInfoPointer: TLuaClassInfo);overload;

procedure plua_AddClassProperty( var ClassInfo : TLuaClassInfo;
                                 propertyName : AnsiString;
                                 Reader   : plua_PropertyReader;
                                 Writer   : plua_PropertyWriter );
procedure plua_AddClassMethod( var ClassInfo : TLuaClassInfo;
                               methodName : AnsiString;
                               wrapper : plua_MethodWrapper );

function plua_getObject( l : PLua_State; idx : Integer; PopValue:boolean = True) : TObject;
function plua_getObjectInfo( l : PLua_State; idx : Integer) : PLuaInstanceInfo;

function plua_registerExisting( l : PLua_State; InstanceName : AnsiString;
                                ObjectInstance : TObject;
                                classInfo : PLuaClassInfo;
                                FreeOnGC : Boolean = false) : PLuaInstanceInfo;
function plua_pushexisting( l : PLua_State;
                            ObjectInstance : TObject;
                            classInfo : PLuaClassInfo;
                            FreeOnGC : Boolean = false) : PLuaInstanceInfo;

function  plua_PushObject(ObjectInfo : PLuaInstanceInfo) : Boolean;
function  plua_GetObjectInfo(l : Plua_State; InstanceObject : TObject) : PLuaInstanceInfo;
function  plua_ObjectEventExists( ObjectInfo : PLuaInstanceInfo;
                                  EventName :AnsiString ) : Boolean;
function  plua_CallObjectEvent( ObjectInfo : PLuaInstanceInfo;
                                EventName :AnsiString;
                                const Args: array of Variant;
                                Results : PVariantArray = nil):Integer;

function  plua_GetEventDeletage( Obj : TObject ) : TLuaObjectEventDelegate;

procedure plua_ClearObjects(L : PLua_State);

var
  LuaClasses     : TLuaClassList;
  LuaDelegates   : TList;
  ClassTypesList : TLuaClassTypesList;
  LogFunction             : procedure (const Text:string) = nil;
  DumpStackTraceFunction  : procedure = nil;

implementation

uses
  typinfo;

type
  PObject = ^TObject;

var
  LuaObjects : TList;

procedure Log(const Text:string);
begin
  //if log handler assigned, then logging
  if @LogFunction <> nil then
    LogFunction( Text );
end;

procedure DumpStackTrace;
begin
  if @DumpStackTraceFunction <> nil then
    DumpStackTraceFunction;
end;

procedure Log(const TextFmt:string; Args:array of const);
begin
  {$IFDEF DEBUG_LUA}
  Log( Format(TextFmt, Args) );
  {$ENDIF}
end;

function plua_gc_class(l : PLua_State) : integer; cdecl; forward;

function plua_index_class(l : PLua_State) : integer; cdecl;
var
  propName : AnsiString;
  propValueStart : Integer;
  obj      : TObject;
  cInfo    : PLuaInstanceInfo;
  reader   : plua_PropertyReader;
  v        : variant;
  pcount   : Integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  if not lua_istable(l, 1) then
    exit;

  cInfo := plua_getObjectInfo(l, 1);
  if not assigned(cInfo) then
    exit;
  obj := cInfo^.obj;

  propName := plua_tostring(l, 2);
  propValueStart := 3;
  reader := LuaClasses.GetPropReader(cInfo^.ClassInfo, propName);
  if assigned(reader) then
    result := reader(obj, l, propValueStart, pcount)
  else
    begin
      if IsPublishedProp(obj, propName) then
        begin
          try
            v := GetPropValue(obj, propName);
            plua_pushvariant(l, v);
            result := 1;
          except
          end;
        end;
    end;
end;

function plua_newindex_class(l : PLua_State) : integer; cdecl;
var
  propName : AnsiString;
  propValueStart : Integer;
  obj      : TObject;
  cInfo    : PLuaInstanceInfo;
  writer   : plua_PropertyReader;
  bReadOnly: Boolean;
  pcount   : Integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  if not lua_istable(l, 1) then
    exit;

  cinfo := plua_getObjectInfo(l, 1);
  if not assigned(cInfo) then
    exit;
  obj := cInfo^.obj;

  propName := plua_tostring(l, 2);
  propValueStart := 3;
  writer := LuaClasses.GetPropWriter(cInfo^.ClassInfo, propName, bReadOnly);
  if assigned(writer) then
    result := writer(obj, l, propValueStart, pcount)
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

function plua_call_class_method(l : PLua_State) : integer; cdecl;
var
  method : plua_MethodWrapper;
  obj    : TObject;
  pcount   : Integer;
begin
  result := 0;
  pcount := lua_gettop(l);
  result := 0;
  obj := plua_getObject(l, 1);
  method := plua_MethodWrapper(PtrInt(lua_tointeger(l, lua_upvalueindex(1))));

  if assigned(obj) and assigned(method) then
    result := method(obj, l, 2, pcount);
end;

procedure LuaObjects_Add(instance:pointer);
begin
  LuaObjects.Add(pointer(instance));
  {$IFDEF DEBUG_LUA}
  DumpStackTrace;
  {$ENDIF}
end;

function plua_new_class(l : PLua_State) : integer; cdecl;
var
  i, n, tidx, midx, classID,
  oidx    : Integer;
  classPTR: Pointer;
  cInfo   : PLuaClassInfo;
  instance: PLuaInstanceInfo;
  pcount  : integer;
  obj_user: PObject;
begin
  pcount := lua_gettop(l);
  result := 0;
  n := lua_gettop(l);
  if (n < 1) or (not (lua_istable(l, 1))) then
    exit;

  tidx := 1;

  lua_pushstring(l, '__classPTR');
  lua_rawget(l, tidx);
  classPTR := pointer(PtrInt(lua_tointeger(l, -1)));
  cInfo := PLuaClassInfo(classPTR);
  lua_pop(l, 1);

  new(instance);
  instance^.OwnsObject := true;
  instance^.ClassInfo := cInfo;
  instance^.l := l;
  instance^.Delegate:=nil;
  if cInfo^.New <> nil then
    instance^.obj := cInfo^.New(l, 2, pcount, instance)
  else
    instance^.obj := TObject.Create;
  LuaObjects_Add(pointer(instance));

  lua_newtable(L);
  instance^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
  lua_rawgeti(l, LUA_REGISTRYINDEX, instance^.LuaRef);
  oidx := lua_gettop(L);

  obj_user:=lua_newuserdata(L, sizeof(PObject));
  obj_user^:=TObject(instance);

  luaL_getmetatable(l, PChar(cinfo^.ClassName+'_mt'));
  lua_setmetatable(l, -2);

  result := 1;
end;

function plua_gc_class(l : PLua_State) : integer; cdecl;
var
  nfo : PLuaInstanceInfo;
  d   : TLuaObjectEventDelegate;
begin
  nfo := plua_getObjectInfo(l, 1);
  if not assigned(nfo) then
    exit;
  LuaObjects.Remove(nfo);
  d := plua_GetEventDeletage(nfo^.obj);
  if assigned(d) then
    d.Free;
  if nfo^.OwnsObject then
    begin
      if assigned(nfo^.ClassInfo^.Release) then
        nfo^.ClassInfo^.Release(nfo^.obj, l)
      else
        nfo^.obj.Free;
    end;
  luaL_unref(L, LUA_REGISTRYINDEX, nfo^.LuaRef);
  freemem(nfo);
  result := 0;
end;

(*
procedure plua_registerclass(l: PLua_State; classInfo: TLuaClassInfo);
var
  lidx, {tidx, }midx, i, classtableidx : integer;
  ci   : PLuaClassInfo;
begin
  Log('Registering class %s.', [classInfo.ClassName]);

  //skip re-registering classes.
  if LuaClasses.IndexOf( classinfo.ClassName ) <> -1 then Exit;

  lidx := LuaClasses.Add(classInfo);

  {
  plua_pushstring(l, classInfo.ClassName);
  lua_newtable(l);
  classtableidx:=lua_gettop(l);
  }

  //assign metatable for class with name classInfo.ClassName
  luaL_newmetatable(l, PChar(classInfo.ClassName+'_mt'));
  //lua_setmetatable(l, -2);
  //lua_setmetatable(l, classtableidx);

  {lua_settable(l, LUA_GLOBALSINDEX);}

  {
  luaL_getmetatable(l, PChar(classInfo.ClassName+'_mt'));
  midx := lua_gettop(l);
  }

  {
  plua_pushstring(l, classInfo.ClassName);
  lua_gettable(l, LUA_GLOBALSINDEX);
  tidx := lua_gettop(l);
  }

  {
  lua_pushstring(L, '__call');
  lua_pushcfunction(L, @plua_new_class);
  lua_rawset(L, midx);
  }

  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @plua_gc_class);
  { lua_rawset(L, midx); }
  lua_settable(L, -3);

  {
  lua_pushstring(L, 'new');
  lua_pushcfunction(L, @plua_new_class);
  lua_rawset(L, tidx);

  lua_pushstring(L, '__classID');
  lua_pushinteger(L, lidx);
  lua_rawset(L, tidx);
  lua_pushstring(L, '__classPTR');
  ci := LuaClasses.ClassInfo[lidx];
  lua_pushinteger(L, PtrInt(ci));
  lua_rawset(L, tidx);

  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @plua_index_class);
  lua_rawset(L, midx);
  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @plua_newindex_class);
  lua_rawset(L, midx);

  Log('Registering class methods.');
  // TODO - Add parent method calls in
  for i := 0 to Length(classInfo.Methods)-1 do
    begin
      plua_pushstring(L, classInfo.Methods[i].MethodName);
      lua_pushinteger(l, PtrInt(classInfo.Methods[i].wrapper));
      lua_pushcclosure(L, @plua_call_class_method, 1);
      lua_rawset(l, -3);
    end;
  }
  Log('Registering class methods - done.');

  Log('Registering - done.');
end;
*)

(*
procedure plua_registerclass(l: PLua_State; classInfo: TLuaClassInfo);
const
  MetaMethods:array[0..1] of luaL_Reg =
  (
    (name : '__gc'; func: @plua_gc_class),
    (name : nil; func: nil)
  );
  ClassMethods:array[0..1] of luaL_Reg =
  (
    (name : 'new'; func: @plua_new_class),
    (name : nil; func: nil)
  );
begin
  Log('Registering class %s.', [classInfo.ClassName]);

  //skip re-registering classes.
  if LuaClasses.IndexOf( classinfo.ClassName ) <> -1 then Exit;

  {lidx := }LuaClasses.Add(classInfo);

  luaL_openlib(L, PChar(classInfo.ClassName), ClassMethods, 0);  //* create methods table, add it to the globals */

  luaL_newmetatable(l, PChar(classInfo.ClassName+'_mt'));

  luaL_openlib(L, nil, MetaMethods, 0);  //* fill metatable */
  lua_pushliteral(L, '__index');
  lua_pushvalue(L, -3);               //* dup methods table*/
  lua_rawset(L, -3);                  //* metatable.__index = methods */
  lua_pushliteral(L, '__metatable');
  lua_pushvalue(L, -3);               //* dup methods table*/
  lua_rawset(L, -3);                  //* hide metatable:
                                      //   metatable.__metatable = methods */
  lua_pop(L, 1);                      //* drop metatable */

  Log('Registering - done.');
end;
*)

procedure plua_CheckStackBalance(l: PLua_State; TopToBe:Integer);
var CurStack:Integer;
begin
  CurStack:=lua_gettop(l);
  if CurStack <> TopToBe then
    raise Exception.CreateFmt('Lua stack is unbalanced. %d, should be %d', [CurStack, TopToBe]);
end;

procedure plua_registerclass(l: PLua_State; classInfo: TLuaClassInfo);
var midx, StartTop : integer;
begin
  Log('Registering class %s.', [classInfo.ClassName]);

  StartTop := lua_gettop(l);
  try
    //skip re-registering classes.
    if LuaClasses.IndexOf( classinfo.ClassName ) <> -1 then Exit;

    {lidx := }LuaClasses.Add(classInfo);

    plua_pushstring(l, classInfo.ClassName);
    lua_newtable(l);

    if luaL_newmetatable(l, PChar(classInfo.ClassName+'_mt')) <> 1 then
      raise Exception.Create('Cannot create metatable');

    lua_setmetatable(l, -2);
    lua_settable(l, LUA_GLOBALSINDEX);

    luaL_getmetatable(l, PChar(classInfo.ClassName+'_mt'));
    midx := lua_gettop(l);

    lua_pushstring(L, '__call');
    lua_pushcfunction(L, @plua_new_class);
    lua_rawset(L, midx);
    lua_pushstring(L, '__gc');
    lua_pushcfunction(L, @plua_gc_class);
    lua_rawset(L, midx);

    lua_pop(l, 1);

  finally
    plua_CheckStackBalance(l, StartTop);
  end;

  Log('Registering - done.');
end;

procedure plua_newClassInfo(var ClassInfoPointer: PLuaClassInfo);
begin
  if ClassInfoPointer = nil then
    new(ClassInfoPointer);
  plua_initClassInfo(ClassInfoPointer^);
end;

procedure plua_initClassInfo( var ClassInfo: TLuaClassInfo);
begin
  ClassInfo.ClassName := '';
  ClassInfo.Parent    := nil;
  ClassInfo.New       := nil;
  ClassInfo.Release   := nil;
  ClassInfo.PropHandlers := TWordList.Create;
  ClassInfo.UnhandledReader := nil;
  ClassInfo.UnhandledWriter := nil;
  SetLength(ClassInfo.Properties, 0);
  SetLength(ClassInfo.Methods, 0);
end;

procedure plua_releaseClassInfo(var ClassInfoPointer: PLuaClassInfo);
begin
  plua_releaseClassInfo( ClassInfoPointer^ );

  Freemem(ClassInfoPointer);
  ClassInfoPointer:=nil;
end;

procedure plua_releaseClassInfo(var ClassInfoPointer: TLuaClassInfo);
begin
  Finalize(ClassInfoPointer.Properties);

  ClassInfoPointer.PropHandlers.Free;
  ClassInfoPointer.PropHandlers:=nil;
end;

procedure plua_AddClassProperty(var ClassInfo: TLuaClassInfo;
  propertyName: AnsiString; Reader: plua_PropertyReader;
  Writer: plua_PropertyWriter);
var
  idx : integer;
begin
  idx := Length(ClassInfo.Properties);
  SetLength(ClassInfo.Properties, idx+1);
  ClassInfo.Properties[idx].PropName := propertyName;
  ClassInfo.Properties[idx].Reader   := Reader;
  ClassInfo.Properties[idx].Writer   := Writer;
  ClassInfo.PropHandlers.AddWord(propertyName)^.data := pointer(PtrInt(idx));
end;

procedure plua_AddClassMethod(var ClassInfo: TLuaClassInfo;
  methodName: AnsiString; wrapper: plua_MethodWrapper);
var
  idx : integer;
begin
  idx := Length(ClassInfo.Methods);
  SetLength(ClassInfo.Methods, idx+1);
  ClassInfo.Methods[idx].MethodName := methodName;
  ClassInfo.Methods[idx].wrapper    := wrapper;
end;

function plua_getObject(l: PLua_State; idx: Integer; PopValue:boolean): TObject;
var
  obj_user:PObject;
  instance : PLuaInstanceInfo;
begin
  result := nil;
  instance:=nil;

  obj_user:= lua_touserdata(L, idx);
  if obj_user <> nil then
    instance := PLuaInstanceInfo(obj_user^);

  if PopValue then
    lua_pop(l, 1);
  if assigned(instance) and assigned(instance^.obj) then
    result := instance^.obj;
end;

function plua_getObjectInfo(l: PLua_State; idx: Integer): PLuaInstanceInfo;
var obj_user:PObject;
begin
  result := nil;

  obj_user:=lua_touserdata(l, plua_absindex(l, idx));
  if obj_user <> nil then
     Result:=PLuaInstanceInfo(obj_user^);

  lua_pop(l, 1);
end;

function plua_registerExisting(l: PLua_State; InstanceName: AnsiString;
  ObjectInstance: TObject; classInfo: PLuaClassInfo;
  FreeOnGC : Boolean = false) : PLuaInstanceInfo;
var
  i, n, tidx, midx, classID,
  oidx    : Integer;
  classPTR: Pointer;
  cInfo   : PLuaClassInfo;
  instance: PLuaInstanceInfo;
  obj_user: PObject;
begin
  instance := plua_GetObjectInfo(l, ObjectInstance);
  if assigned(instance) then
    begin
      plua_pushstring(l, InstanceName);
      plua_PushObject(instance);
      lua_settable(l, LUA_GLOBALSINDEX);
      exit;
    end;

  {$IFDEF DEBUG_LUA}
  Log('plua_registerExisting. Object $%x', [PtrInt(ObjectInstance)]);
  {$ENDIF}

  cInfo := classInfo;

  new(instance);
  result := instance;
  instance^.OwnsObject := FreeOnGC;
  instance^.ClassInfo := cInfo;
  instance^.l := l;
  instance^.obj := ObjectInstance;
  instance^.Delegate:=nil;

  LuaObjects_Add(pointer(instance));

  obj_user:=lua_newuserdata(L, sizeof(PObject));
  obj_user^:=TObject(instance);

  luaL_getmetatable(l, PChar(cinfo^.ClassName+'_mt'));
  lua_setmetatable(l, -2);

  lua_settable(l, LUA_GLOBALSINDEX );
end;

function plua_pushexisting(l: PLua_State; ObjectInstance: TObject;
  classInfo: PLuaClassInfo; FreeOnGC: Boolean): PLuaInstanceInfo;
var
  i, n, tidx, midx, classID,
  oidx, StartTop : Integer;
  classPTR: Pointer;
  cInfo   : PLuaClassInfo;
  instance: PLuaInstanceInfo;
  obj_user: PObject;
begin
  StartTop:=lua_gettop(l);
  try
    instance := plua_GetObjectInfo(l, ObjectInstance);
    if assigned(instance) then
      begin
        plua_PushObject(instance);
        exit;
      end;

    {$IFDEF DEBUG_LUA}
    Log('plua_pushexisting. Object $%x', [PtrInt(ObjectInstance)]);
    {$ENDIF}

    cInfo := classInfo;

    new(instance);
    result := instance;
    instance^.OwnsObject := FreeOnGC;
    instance^.ClassInfo := cInfo;
    instance^.l := l;
    instance^.obj := ObjectInstance;
    instance^.Delegate := nil;

    LuaObjects_Add(pointer(instance));

    obj_user:=lua_newuserdata(L, sizeof(PObject));
    obj_user^:=TObject(instance);

    luaL_getmetatable(l, PChar(cinfo^.ClassName+'_mt'));
    lua_setmetatable(l, -2);
  finally
    plua_CheckStackBalance(l, StartTop + 1);
  end;
end;

function plua_PushObject(ObjectInfo: PLuaInstanceInfo) : Boolean;
begin
  result := true;
  if assigned(ObjectInfo) then
    lua_rawgeti(ObjectInfo^.l, LUA_REGISTRYINDEX, ObjectInfo^.LuaRef)
  else
    result := false;
end;

function plua_GetObjectInfo(l : Plua_State; InstanceObject: TObject): PLuaInstanceInfo;
begin
  result := LuaClasses.GetInfo(l, InstanceObject);
end;

function plua_ObjectEventExists(ObjectInfo: PLuaInstanceInfo;
  EventName: AnsiString): Boolean;
var
  idx : Integer;
begin
  plua_PushObject(ObjectInfo);
  result := plua_functionexists(ObjectInfo^.l, EventName, lua_gettop(ObjectInfo^.l));
  lua_pop(ObjectInfo^.L, 1);
end;

function plua_CallObjectEvent(ObjectInfo: PLuaInstanceInfo;
  EventName: AnsiString; const Args: array of Variant; Results: PVariantArray
  ): Integer;
var
  idx : integer;
begin
  result := -1;
  if not plua_ObjectEventExists(ObjectInfo, EventName) then
    exit;
  plua_PushObject(ObjectInfo);
  idx := lua_gettop(ObjectInfo^.l);
  result := plua_callfunction(ObjectInfo^.l, EventName, args, results, idx);
end;

function plua_GetEventDeletage(Obj: TObject): TLuaObjectEventDelegate;
var
  d : TLuaObjectEventDelegate;
  i : Integer;
begin
  result := nil;
  i := 0;
  while (not assigned(result)) and (i < LuaDelegates.Count) do
    begin
      d := TLuaObjectEventDelegate(LuaDelegates[i]);
      if d.FInstanceInfo^.obj = obj then
        result := d;
      inc(i);
    end;
end;

procedure plua_ClearObjects(L: PLua_State);
var
  i   : Integer;
  nfo : PLuaInstanceInfo;
begin
  i := LuaObjects.Count-1;
  while i > -1 do
    begin
      nfo := PLuaInstanceInfo(LuaObjects[i]);
      if nfo^.l = l then
        LuaObjects.Remove(nfo);
      dec(i);
    end;
end;

{ TLuaClassList }

function TLuaClassList.GetClassInfo(index : integer): PLuaClassInfo;
begin
  result := PLuaClassInfo(fItems[index]);
end;

function TLuaClassList.GetCount: integer;
begin
  result := fItems.Count;
end;

constructor TLuaClassList.Create;
begin
  fItems := TList.Create;
end;

destructor TLuaClassList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

function TLuaClassList.GetPropReader(aClassInfo: PLuaClassInfo;
  aPropertyName: AnsiString): plua_PropertyReader;
var
  pi : PtrInt;
  ei : PWordListSymbol;
begin
// TODO - Add parent property calls in
  result := nil;
  ei := aClassInfo^.PropHandlers.WordSymbol[aPropertyName];
  if not assigned(ei) then
    begin
      if assigned(aClassInfo^.UnhandledReader) then
        result := aClassInfo^.UnhandledReader;
      exit;
    end;
  pi := PtrInt(ei^.data);
  if (pi >= 0) and (pi < length(aClassInfo^.Properties)) then
    result := aClassInfo^.Properties[pi].Reader;
end;

function TLuaClassList.GetPropWriter(aClassInfo: PLuaClassInfo;
  aPropertyName: AnsiString; out ReadOnly : Boolean): plua_PropertyWriter;
var
  pi : PtrInt;
  ei : PWordListSymbol;
begin
// TODO - Add parent property calls in
  ReadOnly := false;
  result := nil;
  ei := aClassInfo^.PropHandlers.WordSymbol[aPropertyName];
  if not assigned(ei) then
    begin
      if assigned(aClassInfo^.UnhandledWriter) then
        result := aClassInfo^.UnhandledWriter;
      exit;
    end;
  pi := PtrInt(ei^.data);
  if (pi >= 0) and (pi < length(aClassInfo^.Properties)) then
    begin
      ReadOnly := aClassInfo^.Properties[pi].Writer = nil;
      result := aClassInfo^.Properties[pi].Writer;
    end;
end;

function TLuaClassList.GetInfo(l : Plua_State; InstanceObject: TObject): PLuaInstanceInfo;
var
  i : Integer;
begin
  result := nil;
  i := 0;
  while (result = nil) and (i < LuaObjects.Count) do
    begin
      if (PLuaInstanceInfo(LuaObjects[i])^.obj = InstanceObject) and
         (PLuaInstanceInfo(LuaObjects[i])^.l = l) then
        result := PLuaInstanceInfo(LuaObjects[i]);
      inc(i);
    end;
end;

function TLuaClassList.Add(aClassInfo: TLuaClassInfo) : Integer;
var
  ci  : PLuaClassInfo;
begin
  result := IndexOf(aClassInfo.ClassName);
  if result = -1 then
    begin
      new(ci);
      result := fItems.Add(ci);
    end
  else
    ci := ClassInfo[result];
  ci^ := aClassInfo;
end;

procedure TLuaClassList.Remove(aClassName: AnsiString);
var
  idx : integer;
  ci  : PLuaClassInfo;
begin
  idx := IndexOf(aClassName);
  if idx > -1 then
    begin
      ci := ClassInfo[idx];
      plua_releaseClassInfo(ci);
      fItems.Delete(idx);
    end;
end;

function TLuaClassList.IndexOf(aClassName: AnsiString): Integer;
var
  i : Integer;
begin
  result := -1;
  i := 0;
  while (result = -1) and (i < count) do
    begin
      if CompareText(aClassName, ClassInfo[i]^.ClassName) = 0 then
        result := i;
      inc(i);
    end;
end;

procedure TLuaClassList.Clear;
var
  ci : PLuaClassInfo;
begin
  while count > 0 do
    begin
      ci := ClassInfo[count-1];
      plua_releaseClassInfo(ci);
      fItems.Delete(count-1);
    end;
end;

var
  instance : PLuaInstanceInfo;

{ TLuaObjectEventDelegate }

function TLuaObjectEventDelegate.EventExists(EventName: AnsiString): Boolean;
begin
  result := plua_ObjectEventExists(FInstanceInfo, EventName);
end;

function TLuaObjectEventDelegate.CallEvent(EventName: AnsiString): Integer;
begin
  result := CallEvent(EventName, [], nil);
end;

function TLuaObjectEventDelegate.CallEvent(EventName: AnsiString;
  const Args: array of Variant): Integer;
begin
  result := CallEvent(EventName, Args, nil);
end;

function TLuaObjectEventDelegate.CallEvent(EventName: AnsiString;
  const Args: array of Variant; Results: PVariantArray): Integer;
begin
  result := plua_CallObjectEvent(FInstanceInfo, EventName, Args, Results);
end;

constructor TLuaObjectEventDelegate.Create(InstanceInfo: PLuaInstanceInfo; obj : TObject);
begin
  LuaDelegates.Add(Self);
  FInstanceInfo := InstanceInfo;
  FObj := obj;
  InstanceInfo^.Delegate := self;
end;

destructor TLuaObjectEventDelegate.Destroy;
begin
  LuaDelegates.Remove(self);
  FInstanceInfo^.Delegate := nil;
  inherited Destroy;
end;

{ TLuaClassTypesList }

function TLuaClassTypesList.GetCount: Integer;
begin
  result := fItemList.Count;
end;

function TLuaClassTypesList.GetIndexedItem(index : integer): PLuaClassInfo;
begin
  result := PLuaClassInfo(fItemList[index]);
end;

function TLuaClassTypesList.GetItem(ItemName : AnsiString): PLuaClassInfo;
begin
  result := PLuaClassInfo(fItems.WordData[ItemName]);
end;

constructor TLuaClassTypesList.Create;
begin
  fItems := TWordList.Create;
  fItemList := TList.Create;
end;

destructor TLuaClassTypesList.Destroy;
begin
  Clear;
  fItems.Free;
  fItemList.Free;
  inherited Destroy;
end;

function TLuaClassTypesList.Add(ItemName: AnsiString; LuaParent : PLuaClassInfo = nil): PLuaClassInfo;
begin
  result := PLuaClassInfo(fItems.WordData[ItemName]);
  if not assigned(result) then
    begin
      plua_newClassInfo(result);
      result^.Parent := LuaParent;
      result^.ClassName := ItemName;
      fItems.AddWord(ItemName)^.data := result;
      fItemList.Add(result);
    end;
end;

procedure TLuaClassTypesList.Remove(ItemName: AnsiString);
var
  wd : PWordListSymbol;
  ci : PLuaClassInfo;
begin
  wd := fItems.WordSymbol[ItemName];
  if (assigned(wd)) and (assigned(wd^.data)) and (wd^.eow) then
    begin
      ci := PLuaClassInfo(wd^.data);
      fItemList.Remove(wd^.data);
      wd^.data := nil;
      wd^.eow := false;
      plua_releaseClassInfo(ci);
    end;
end;

procedure TLuaClassTypesList.Clear;
begin
  while Count > 0 do
    Remove(IndexedItem[Count-1]^.ClassName);
end;

procedure TLuaClassTypesList.RegisterTo(L: PLua_State);
var
  i : Integer;
begin
  for i := 0 to Count-1 do
    plua_registerclass(l, IndexedItem[i]^);
end;

initialization
  LuaClasses := TLuaClassList.Create;
  LuaObjects := TList.Create;
  LuaDelegates := TList.Create;
  ClassTypesList := TLuaClassTypesList.Create;

finalization
  if LuaObjects.Count > 0 then
    Log('Warning!!! %d objects left unfreed.', [LuaObjects.Count]);

  while LuaObjects.Count > 0 do
    begin
      instance := PLuaInstanceInfo(LuaObjects[LuaObjects.Count-1]);
      Log('Freeing class "%s" instance.', [instance^.obj.ClassName]);

      LuaObjects.Delete(LuaObjects.Count-1);
      if instance^.OwnsObject then
        begin
          if assigned(instance^.ClassInfo^.Release) then
            instance^.ClassInfo^.Release(instance^.obj, nil)
          else
            instance^.obj.Free;
        end;
      if Instance^.Delegate <> nil then
        Instance^.Delegate.Free;
      Freemem(instance);
    end;

  FreeAndNil(LuaObjects);
  FreeAndNil(ClassTypesList);
  FreeAndNil(LuaClasses);
  FreeAndNil(LuaDelegates);

end.
