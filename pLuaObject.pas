unit pLuaObject;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$TYPEDADDRESS ON}
{$ENDIF}

{$I pLua.inc}

interface

uses
  Classes, SysUtils, fgl,
  lua, pLua, uWordList;

type
  TLuaObjectEventDelegate = class;
  PLuaInstanceInfo = ^TLuaInstanceInfo;
  PPLuaInstanceInfo = ^PLuaInstanceInfo;

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
  
  TLuaClassId = type Pointer; //just a pointer is enough to identify "class-ness"
  TLuaClassInfo = record
    ClassId     : TLuaClassId;
    Parent      : PLuaClassInfo;
    ClassName   : AnsiString;
    New         : plua_ClassConstructor;
    Release     : plua_ClassDestructor;

    PropHandlers: TWordList;
    UnhandledReader : plua_PropertyReader;
    UnhandledWriter : plua_PropertyWriter;
    Properties  : Array of TLuaClassProperty;
    Methods     : Array of TLuaClassMethod;

    // information for representing special kinds of objects behaving a lot like Lua tables
    // they will support following operations
    //   - read value by key
    //   - insert key/value (first assignment of non-existing key)
    //   - modify value by key (ordinary assignment)
    //   - remove item by key (via nil assignment)
    //   - iterate all key/values (via __pairs metamethod support)
    OnRead               : plua_ClassMethodWrapper;   // receives the same arguments as __index metamethod
    OnReadByIndex        : plua_ClassMethodWrapper;   // receives single integer argument in [0..KeyCount-1] range, returns key/value pair
    OnInsertModifyRemove : plua_ClassMethodWrapper;   // receives the same arguments as __newindex metamethod
    OnGetKeyCount        : plua_ClassMethodWrapper;   // will return key count to initialize iteration via "for in" loop
  end;

  TLuaInstanceInfo = record
    OwnsObject: Boolean;
    LuaRef    : Integer;
    ClassId   : TLuaClassId;
    l         : PLua_state;
    obj       : TObject;
    Delegate  : TLuaObjectEventDelegate;
    {$IFDEF DEBUG}
    //in case some leakage/double freeing occurs, we'll know what class did that.
    ClassName:string;
    {$ENDIF}
  end;

  TClassIdObjMap = specialize TFPGMap<TLuaClassId, PLuaClassInfo>;

  { TLuaClassList }

  TLuaClassList = class
    fItems : TList;

    //just an index to get PLuaClassInfo by ClassId quickly
    FClassIdToClassInfo:TClassIdObjMap;
  private
    function GetClassInfo(index : integer): PLuaClassInfo;
    function GetClassInfoById(Id : TLuaClassId): PLuaClassInfo;
    function GetCount: integer;
    procedure FreeItem(ci: PLuaClassInfo);
  public
    constructor Create;
    destructor Destroy; override;

    function  GetPropReader( ClassId : TLuaClassId; const aPropertyName : AnsiString ) : plua_PropertyReader;
    function  GetPropWriter( ClassId : TLuaClassId; const aPropertyName : AnsiString; out ReadOnly : Boolean ) : plua_PropertyWriter;

    function  GetInfo(l : Plua_State; InstanceObject : TObject) : PLuaInstanceInfo;

    //AHTUNG!!! Do not try to use aClassInfo after Add!!!
    function  Add(aClassInfo : PLuaClassInfo) : Integer;

    procedure Remove(aClassName : AnsiString);
    function  IndexOf(aClassName : AnsiString) : Integer;
    procedure Clear;
    property  Count : integer read GetCount;
    property  ClassInfo[index : integer]:PLuaClassInfo read GetClassInfo; default;
    property  ClassInfoById[Id : TLuaClassId]:PLuaClassInfo read GetClassInfoById;
  end;

  { TLuaObjectEventDelegate }

  TLuaObjectEventDelegate = class
  protected
    FInstanceInfo : PLuaInstanceInfo;
    FObj          : TObject;
    FLua          : {TLuaInternalState} Pointer;
    
    function  EventExists( EventName :AnsiString ) : Boolean;
    function  CallEvent( EventName :AnsiString ) : Integer; overload;
    function  CallEvent( EventName :AnsiString;
                         const Args: array of Variant ) : Integer; overload;
    function  CallEvent( EventName :AnsiString;
                         const Args: array of Variant;
                         Results : PVariantArray = nil):Integer; overload;
  public
    constructor Create( Lua: {TLuaInternalState} Pointer; InstanceInfo : PLuaInstanceInfo; obj : TObject); virtual;
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

procedure plua_registerclass( l : PLua_State; classInfo : PLuaClassInfo);
procedure plua_newClassInfo( var ClassInfoPointer : PLuaClassInfo);
procedure plua_initClassInfo( var ClassInfo : TLuaClassInfo);

procedure plua_releaseClassInfo( var ClassInfoPointer : PLuaClassInfo);

procedure plua_AddClassProperty( var ClassInfo : TLuaClassInfo;
                                 propertyName : AnsiString;
                                 Reader   : plua_PropertyReader;
                                 Writer   : plua_PropertyWriter = nil // properties can be read-only
                                 );
procedure plua_AddClassMethod( var ClassInfo : TLuaClassInfo;
                               methodName : AnsiString;
                               wrapper : plua_MethodWrapper );

function plua_getObject( l : PLua_State; idx : Integer; PopValue:boolean = True) : TObject;
function plua_GlobalObjectGet( l : PLua_State; const varName:string) : TObject;

function plua_getObjectTable( l : PLua_State; idx : Integer; PopValue:boolean = True) : TObjArray;

function plua_getObjectInfo( l : PLua_State; idx : Integer) : PLuaInstanceInfo;

function plua_registerExisting( l : PLua_State; InstanceName : AnsiString;
                                ObjectInstance : TObject;
                                classId : TLuaClassId;
                                FreeOnGC : Boolean = false; KeepRef:boolean = True) : PLuaInstanceInfo;
//Function pushes an object into stack.
function plua_pushexisting( l : PLua_State;
                            ObjectInstance : TObject;
                            classId : TLuaClassId;
                            FreeOnGC : Boolean = false; //Call native destructor on garbage collecting?
                            KeepRef:boolean = True //Reference keeps garbage collector from freeing object, until application decides to release it.
                                                   //If KeepRef is true, then application itself is responsible for freeing Lua object using pLua_ObjectMarkFree
                            ) : PLuaInstanceInfo;

//special object registering
//e.g. Self for LuaWrapper.
//Does not adds objects into internal lists, as they don't exist yet.
//Chicken/egg problem avoidance.
function plua_pushexisting_special( l : PLua_State;
                            ObjectInstance : TObject;
                            classId: TLuaClassId; //Important!!! Can be nil here!
                            FreeOnGC : Boolean = false;  //Call native destructor on garbage collecting?
                            KeepRef:boolean = True //Reference keeps garbage collector from freeing object, until application decides to release it.
                                                   //If KeepRef is true, then application itself is responsible for freeing Lua object using pLua_ObjectMarkFree
                            ) : PLuaInstanceInfo;

//Pushes class object as opaque userdata. Useful e.g. for iterators which are pushed by Pascal as userdata+finalizer
//automatically calls Free on garbage collection
procedure plua_PushObjectAsUserData(l: Plua_State; O:TObject);

// only for freeing instances made by plua_pushexisting_special!
procedure plua_instance_free(l: Plua_State; var Instance: PLuaInstanceInfo);

function plua_ObjectMarkFree(l: Plua_State; ObjectInstance: TObject):boolean;
function plua_ref_release(l : PLua_State; obj:PLuaInstanceInfo):boolean;overload;

function  plua_PushObject(ObjectInfo : PLuaInstanceInfo) : Boolean;
function  plua_GetObjectInfo(l : Plua_State; InstanceObject : TObject) : PLuaInstanceInfo;
function  plua_ObjectEventExists( ObjectInfo : PLuaInstanceInfo;
                                  EventName :AnsiString ) : Boolean;
function  plua_CallObjectEvent( ObjectInfo : PLuaInstanceInfo;
                                EventName :AnsiString;
                                const Args: array of Variant;
                                Results : PVariantArray = nil):Integer;

function plua_AllocatedObjCount(L : PLua_State):Integer;

implementation
uses
  typinfo, LuaWrapper;

function  plua_GetEventDeletage(S: TLuaInternalState; Obj : TObject ) : TLuaObjectEventDelegate;forward;

function ClassMetaTableName(const ClassName:string):string;
begin
  Result:=ClassName+'_mt';
end;

function ClassMetaTableName(cinfo:PLuaClassInfo):string;
begin
  Result:=ClassMetaTableName(cinfo^.ClassName);
end;

function ClassProcsMetaTableName(const ClassName:string):string;
begin
  //Return metatable name for class methods metatable
  Result:=ClassName+'_mt_procs';
end;

function ClassProcsMetaTableName(cinfo:PLuaClassInfo):string;
begin
  //Return metatable name for class methods metatable
  Result:=ClassProcsMetaTableName(cinfo^.ClassName);
end;

function ClassPropsMetaTableName(const ClassName:string):string;
begin
  //Return metatable name for class properties metatable
  Result:=ClassName+'_mt_props';
end;

function ClassPropsMetaTableName(cinfo:PLuaClassInfo):string;
begin
  //Return metatable name for class properties metatable
  Result:=ClassPropsMetaTableName(cinfo^.ClassName);
end;

function plua_ref_release(l : PLua_State; obj_ref:Integer):boolean;overload;
begin
  Result:=obj_ref <> LUA_NOREF;
  if Result then
    begin
      luaL_unref(L, LUA_REGISTRYINDEX, obj_ref);
    end;
end;

function plua_ref_release(l : PLua_State; obj:PLuaInstanceInfo):boolean;overload;
begin
  Result:=plua_ref_release(l, obj^.LuaRef);
  obj^.LuaRef:=LUA_NOREF;
end;

function plua_instance_new: PLuaInstanceInfo;
begin
  New(Result);
  FillChar(Result^, Sizeof(Result^), 0);
  Result^.LuaRef:=LUA_NOREF;
end;

function plua_AllocatedObjCount(l : Plua_State):Integer;
var S:TLuaInternalState;
begin
  S:=LuaSelf(l);
  Result:=S.LuaObjects.Count;
end;

function plua_gc_class(l : PLua_State) : integer; extdecl; forward;

function plua_index_class(l : PLua_State) : integer; extdecl;
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

  cInfo := plua_getObjectInfo(l, 1);
  if not assigned(cInfo) then
    exit;
  obj := cInfo^.obj;

  propName := plua_tostring(l, 2);
  propValueStart := 3;

  //remove parameters from stack
  lua_pop(l, 2);

  reader := LuaSelf(l).LuaClasses.GetPropReader(cInfo^.ClassId, propName);
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

function plua_newindex_class(l : PLua_State) : integer; extdecl;
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

  cinfo := plua_getObjectInfo(l, 1);
  if not assigned(cInfo) then
    exit;
  obj := cInfo^.obj;

  propName := plua_tostring(l, 2);
  propValueStart := 3;

  writer := LuaSelf(l).LuaClasses.GetPropWriter(cInfo^.ClassId, propName, bReadOnly);
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

  //remove parameters from stack
  lua_pop(l, 2);
end;

function plua_call_class_method_act(l : PLua_State) : integer;
var
  method : plua_MethodWrapper;
  obj    : TObject;
  pcount : Integer;
begin
  result := 0;

  pcount := lua_gettop(l);
  obj := plua_getObject(l, 1, False);
  method := plua_MethodWrapper(lua_topointer(l, lua_upvalueindex(1)));

  if assigned(obj) and assigned(method) then
    result := method(obj, l, 2, pcount);
end;

// exception support
const pLuaExceptActual:TLuaNakedProc = @plua_call_class_method_act;
{$I pLuaExceptWrapper.inc}

function plua_new_class(l : PLua_State) : integer; extdecl;
var
  i, n, tidx, midx, oidx : Integer;
  classId : TLuaClassId;
  cInfo   : PLuaClassInfo;
  instance: PLuaInstanceInfo;
  pcount  : integer;
  obj_user: PPLuaInstanceInfo;
begin
  pcount := lua_gettop(l);
  result := 0;
  n := lua_gettop(l);
  if (n < 1) or (not (lua_istable(l, 1))) then
    exit;

  tidx := 1;

  lua_pushstring(l, '__ClassId');
  lua_rawget(l, tidx);
  classId := lua_topointer(l, -1);
  lua_pop(l, 1);
  if classId = nil then Exit;

  cInfo := LuaSelf(l).LuaClasses.ClassInfoById[ classId ];

  instance:=plua_instance_new;
  instance^.OwnsObject := true;
  instance^.ClassId := classId;
  instance^.l := l;
  if cInfo^.New <> nil then
    instance^.obj := cInfo^.New(l, 2, pcount, instance)
  else
    instance^.obj := TObject.Create;

  LuaObjects_Add( LuaSelf(l), instance );

  instance^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);

  obj_user:=lua_newuserdata(L, sizeof(instance));
  obj_user^:=instance;

  lua_getglobal(l, PChar(ClassMetaTableName(cinfo)) );
  lua_setmetatable(l, -2);

  result := 1;
end;

function plua_gc_class(l : PLua_State) : integer; extdecl;
var
  nfo : PLuaInstanceInfo;
  d   : TLuaObjectEventDelegate;
  ref : Integer;
begin
  result := 0;

  nfo := plua_getObjectInfo(l, 1);
  if not assigned(nfo) then
    exit;

  d := plua_GetEventDeletage(LuaSelf(l), nfo^.obj);
  if assigned(d) then
    d.Free;

  //to avoid double release,
  //first release object and only after release reference
  //ref:=nfo^.LuaRef;

  LuaObjects_Free( LuaSelf(l), nfo);

  //plua_ref_release(l, ref);
end;

procedure plua_pushmethod(l: PLua_State; const MethodName:string; wrapper:plua_ClassMethodWrapper);
//pushes wrapper method on Lua stack
begin
  plua_pushstring(L, MethodName);
  lua_pushlightuserdata(l, Pointer(wrapper));
  lua_pushcclosure(L, @plua_call_method, 1);
end;

procedure plua_pushmethod(l: PLua_State; wrapper:plua_ClassMethodWrapper);
//pushes wrapper method on Lua stack
begin
  lua_pushlightuserdata(l, Pointer(wrapper));
  lua_pushcclosure(L, @plua_call_method, 1);
end;

type
  TRegisterClassInfo = record
    ClassMetaTable : string;
    ProcsMetaTable : string;
    PropsMetaTable : string;
  end;

function plua_registerclass_createmt(l: PLua_State; const ClassName: string; ClassId : TLuaClassId; out R:TRegisterClassInfo):boolean;
// Creates metatable for given class info
var
  S:TLuaInternalState;
begin
  Result:=False;
  //already registered?
  R.ClassMetaTable:=ClassMetaTableName(ClassName);
  lua_getglobal( l, PChar(R.ClassMetaTable) );
  if lua_istable(l, -1) then
    begin
      LogDebug('Skipping registering class %s. Metatable already registered', [ClassName]);
      Exit;
    end;
  lua_pop(l, 1);

  S:=LuaSelf(l);
  //skip re-registering classes.
  if S.LuaClasses.IndexOf( ClassName ) <> -1 then
    begin
      LogDebug('Skipping registering class %s. Already registered', [ClassName]);
      Exit;
    end;

  plua_pushstring(l, ClassName);
  lua_newtable(l);

    //assign internal class id to class table
    lua_pushstring(L, '__ClassId');
    lua_pushlightuserdata(L, ClassId);
    lua_rawset(L, -3);

    //assign named metatable to class table
    pLua_TableGlobalCreate(l, R.ClassMetaTable);
    lua_getglobal(l, PChar(R.ClassMetaTable) );
    lua_setmetatable(l, -2);

  // create named class table
  lua_settable(l, LUA_GLOBALSINDEX);

  R.ProcsMetaTable:=ClassProcsMetaTableName(ClassName);
  R.PropsMetaTable:=ClassPropsMetaTableName(ClassName);

  Result:=True;
end;

procedure plua_registerclass(l: PLua_State; classInfo: PLuaClassInfo);

  {$REGION 'STDCLASS'}
  procedure CreateProps(const R:TRegisterClassInfo);
  var midx : integer;
  begin
    //create properties metatable
    pLua_TableGlobalCreate(l, R.PropsMetaTable);

    //attach property read/write handlers to properties metatable
    lua_getglobal(l, PChar(R.PropsMetaTable) );
    midx := lua_gettop(l);

    lua_pushstring(L, 'getprop');
    lua_pushcfunction(L, @plua_index_class);
    lua_rawset(L, midx);

    lua_pushstring(L, 'setprop');
    lua_pushcfunction(L, @plua_newindex_class);
    lua_rawset(L, midx);

    lua_pop(l, 1);
  end;

  procedure CreateMethods(const R:TRegisterClassInfo);
  var midx, i: integer;
  begin
    //create methods metatable
    pLua_TableGlobalCreate(l, R.ProcsMetaTable);

    //attach properties metatable to methods metatable
    //so properties metatable is searched only when no methods is found
    lua_getglobal(l, PChar(R.ProcsMetaTable) );
    midx := lua_gettop(l);

    //populate class methods metatable
    if Length(classInfo^.Methods) > 0 then
        begin
          LogDebug('Registering class methods.');
          // TODO - Add parent method calls in
          for i := 0 to High(classInfo^.Methods) do
            begin
              LogDebug('Registering class method %s.', [classInfo^.Methods[i].MethodName]);
              with classInfo^.Methods[i] do
                plua_pushmethod(L, MethodName, wrapper);
              lua_rawset(l, midx);
            end;
          LogDebug('Registering class methods - done.');
        end;
    lua_pop(l, 1);
  end;

  procedure CreateStdMetaMethods(const R:TRegisterClassInfo);
  var midx: integer;
  begin
    //populate main class metatable
    lua_getglobal(l, PChar(R.ClassMetaTable) );
    midx := lua_gettop(l);

    lua_pushstring(L, '__call');
    lua_pushcfunction(L, @plua_new_class);
    lua_rawset(L, midx);

    lua_pushstring(L, '__gc');
    lua_pushcfunction(L, @plua_gc_class);
    lua_rawset(L, midx);

    //pop class metatable
    lua_pop(l, 1);
 end;

  procedure CreateStdMethodPropSupport(const R:TRegisterClassInfo);
  var midx, err : integer;
  begin
    //populate main class metatable
    lua_getglobal(l, PChar(R.ClassMetaTable) );
    midx := lua_gettop(l);

    lua_pushstring(L, '__index');
    err:=plua_FunctionCompile(l,
            'function(v,x) ' + sLineBreak +
            '     if (%mt_procs%[x] ~= nil) then ' + sLineBreak +
            '        return %mt_procs%[x] ' + sLineBreak +
            '     else ' + sLineBreak +
            '        return (%mt_props%.getprop)(v, x) ' + sLineBreak +
            '     end ' + sLineBreak +
            'end',
            ['%mt_procs%', R.ProcsMetaTable,
             '%mt_props%', R.PropsMetaTable]);
    if err <> 0 then
      begin
        plua_RaiseException(l, 'Cannot compile __index function for class %s metatable', [classInfo^.ClassName]);
      end;
    lua_rawset(L, midx);

    lua_pushstring(L, '__newindex');
    err:=plua_FunctionCompile(l,
          'function(t,p,val) ' + sLineBreak +
          '  %mt_props%.setprop(t, p, val) ' + sLineBreak +
          'end',
          ['%mt_props%', R.PropsMetaTable]);
    if err <> 0 then
      begin
        plua_RaiseException(l, 'Cannot compile __newindex function for class %s metatable', [classInfo^.ClassName]);
      end;
    lua_rawset(L, midx);

    //pop class metatable
    lua_pop(l, 1);
  end;

  procedure CreateStdClass(const R:TRegisterClassInfo);
  begin
    //Pseudo-code for metatables created below
    // So, I just make a chain of metatables!
    //
    //  class_mt_props = {
    //     getprop = plua_index_class,
    //     setprop = plua_newindex_class
    //  }
    //
    //  class_mt_procs = {
    //     proc1 = plua_call_class_method,
    //     proc2 = plua_call_class_method
    //     ...
    //  }
    //
    //  class_mt = {
    //     __gc = plua_gc_class,
    //     __call = plua_new_class
    //     __index= function(v,x)
    //         if (mt_procs[x] ~= nil) then
    //           return mt_procs[x]
    //         else
    //           return (mt_props.getprop)(v, x)
    //         end
    //       end,
    //     __newindex= function(t,p,val)
    //        mt_props.setprop(t, p, val)
    //      end
    //  }
    // setmetatable(obj, class_mt)

    //create properties metatable
    CreateProps(R);

    //create methods metatable
    CreateMethods(R);

    CreateStdMetaMethods(R);
    CreateStdMethodPropSupport(R);
  end;
  {$ENDREGION}

  {$REGION 'TABLECLASS'}
  procedure CreateTableMetaSupport(const R:TRegisterClassInfo);
  var midx, err : integer;
  begin
    //populate main class metatable
    lua_getglobal(l, PChar(R.ClassMetaTable) );
    midx := lua_gettop(l);

    lua_pushstring(L, 'OnRead');
    plua_pushmethod(l, classInfo^.OnRead);
    lua_rawset(L, midx);

    lua_pushstring(L, '__index');
    err:=plua_FunctionCompile(l,
           'function(t,k)'                                         +sLineBreak+
           '  local mt = getmetatable(t)'                          +sLineBreak+
           '  if mt.OnReadWrapper == nil then'                     +sLineBreak+
           '    return mt.OnRead(t,k)'                             +sLineBreak+
           '  else'                                                +sLineBreak+
           '    return mt.OnReadWrapper( mt.OnRead, t, k )'        +sLineBreak+
           '  end'                                                 +sLineBreak+
           'end');
    if err <> 0 then
        plua_RaiseException(l, 'Cannot compile __index function for class %s metatable', [classInfo^.ClassName]);
    lua_rawset(L, midx);

    lua_pushstring(L, 'OnInsertModifyRemove');
    plua_pushmethod(l, classInfo^.OnInsertModifyRemove);
    lua_rawset(L, midx);

    lua_pushstring(L, '__newindex');
    err:=plua_FunctionCompile(l,
           'function(t,k,v)'                                             +sLineBreak+
           '  local mt = getmetatable(t)'                                +sLineBreak+
           '  if mt.OnInsertModifyRemoveWrapper == nil then'             +sLineBreak+
           '    mt.OnInsertModifyRemove(t,k,v)'                          +sLineBreak+
           '  else'                                                      +sLineBreak+
           '    mt.OnInsertModifyRemoveWrapper( mt.OnInsertModifyRemove, t, k, v )' +sLineBreak+
           '  end'                                                       +sLineBreak+
           'end');
    if err <> 0 then
        plua_RaiseException(l, 'Cannot compile __newindex function for class %s metatable', [classInfo^.ClassName]);
    lua_rawset(L, midx);

    lua_pushstring(L, '__len');
    plua_pushmethod(l, classInfo^.OnGetKeyCount);
    lua_rawset(L, midx);

    // publish OnReadByIndex for __pairs function
    lua_pushstring(L, 'OnReadByIndex');
    plua_pushmethod(l, classInfo^.OnReadByIndex);
    lua_rawset(L, midx);

    // publish OnGetKeyCount for __pairs function
    lua_pushstring(L, 'OnGetKeyCount');
    plua_pushmethod(l, classInfo^.OnGetKeyCount);
    lua_rawset(L, midx);

    lua_pushstring(L, '__pairs');
    err:=plua_FunctionCompile(l,
           'function(t)'                                              +sLineBreak+
           '  local i = -1'                                           +sLineBreak+
           '  local mt = getmetatable(t)'                             +sLineBreak+

           // additional checks
           '  if (mt == nil) or '                                     +sLineBreak+
           '     (mt.OnGetKeyCount == nil) or'                        +sLineBreak+
           '     (mt.OnReadByIndex == nil) then'                      +sLineBreak+
           '    return function()'                                    +sLineBreak+
           '      return nil, nil'                                    +sLineBreak+
           '    end, t, nil'                                          +sLineBreak+
           '  end'                                                    +sLineBreak+

           '  local count = mt.OnGetKeyCount(t)'                      +sLineBreak+
           '  return function()'                                      +sLineBreak+
           '    if (count == nil) or (i >= count-1) then'             +sLineBreak+
           '      return nil, nil'                                    +sLineBreak+
           '    end'                                                  +sLineBreak+
           '    i = i + 1'                                            +sLineBreak+

           '    if mt.OnReadByIndexWrapper == nil then'               +sLineBreak+
           '      return mt.OnReadByIndex(t,i)'                       +sLineBreak+
           '    else'                                                 +sLineBreak+
           '      return mt.OnReadByIndexWrapper( mt.OnReadByIndex, t, i )' +sLineBreak+
           '    end'                                                        +sLineBreak+
           '  end, t, nil'                                            +sLineBreak+
           'end');
    if err <> 0 then
        plua_RaiseException(l, 'Cannot compile __pairs function for class %s metatable', [classInfo^.ClassName]);
    lua_rawset(L, midx);

    //pop class metatable
    lua_pop(l, 1);
  end;

  procedure CreateTableClass(const R:TRegisterClassInfo);
  begin
    //Pseudo-code for metatable created below
    //
    //  class_mt = {
    //     __gc = plua_gc_class,
    //     __call = plua_new_class,
    //     __index= function(t,x)
    //         return OnRead(t, x)
    //       end,
    //     __newindex= function(t,p,val)
    //         OnInsertModifyRemove(t,p,val)
    //       end,
    //     __len= function(t)
    //         return OnGetKeyCount(t)
    //       end,
    //       OnReadByIndex = function(t,i)
    //         return OnReadByIndex(t,i)
    //       end,
    //     __pairs= function(t)
    //         local i = -1
    //         local mt = getmetatable(t)
    //         local count = mt.OnGetKeyCount(t)
    //         return function()
    //           if i >= count-1 then
    //             return
    //           end
    //           i = i + 1
    //           return i, mt.OnReadByIndex(t,i)
    //         end
    //       end
    //  }
    // setmetatable(obj, class_mt)

    with classInfo^ do
      Assert( (OnRead <> nil) and
              (OnReadByIndex <> nil) and
              (OnInsertModifyRemove <> nil) and
              (OnGetKeyCount <> nil),
              'All class as table meta info must be filled'
            );

    CreateStdMetaMethods(R);
    CreateTableMetaSupport(R);
  end;
  {$ENDREGION}

var StartTop: integer;
    R:TRegisterClassInfo;
    S:TLuaInternalState;
begin
  LogDebug('Registering class %s.', [classInfo^.ClassName]);

  StartTop := lua_gettop(l);

  if not plua_registerclass_createmt(l, classInfo^.ClassName, classInfo^.ClassId, R) then
    begin
      plua_EnsureStackBalance(l, StartTop);
      plua_releaseClassInfo(classInfo); // release class info as reference wasn't added to LuaClasses
      LogDebug('Registering - skipped, already exists.');
      Exit;
    end;

  S:=LuaSelf(l);
  S.LuaClasses.Add(classInfo); // now LuaClasses become the owner of classInfo

  if classInfo^.OnInsertModifyRemove = nil then
    CreateStdClass(R)
    else
    CreateTableClass(R);

  plua_CheckStackBalance(l, StartTop);

  LogDebug('Registering - done.');
end;

procedure plua_newClassInfo(var ClassInfoPointer: PLuaClassInfo);
begin
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
  ClassInfo.OnRead               := nil;
  ClassInfo.OnReadByIndex        := nil;
  ClassInfo.OnInsertModifyRemove := nil;
  ClassInfo.OnGetKeyCount        := nil;
end;

procedure plua_releaseClassInfo(var ClassInfoPointer: PLuaClassInfo);
begin
  Finalize(ClassInfoPointer^.Properties);
  Finalize(ClassInfoPointer^.Methods);

  ClassInfoPointer^.PropHandlers.Free;
  ClassInfoPointer^.PropHandlers:=nil;

  Freemem(ClassInfoPointer);
  ClassInfoPointer:=nil;
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
  ClassInfo.PropHandlers.AddWord(propertyName)^.data := pointer(PtrUint(idx));
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
  obj_user:PPLuaInstanceInfo;
  instance : PLuaInstanceInfo;
begin
  result := nil;
  instance:=nil;

  try
    if not lua_isuserdata(l, idx) then Exit;

    obj_user:= lua_touserdata(L, idx);
    if obj_user <> nil then
      instance := obj_user^;
  finally
    if PopValue then
      lua_pop(l, 1);
    if assigned(instance) and assigned(instance^.obj) then
      result := instance^.obj;
  end;
end;

function plua_GlobalObjectGet(l: PLua_State; const varName: string): TObject;
var StartTop:Integer;
begin
  Result:=nil;
  StartTop:=lua_gettop(l);
  try
    lua_pushstring(L, PChar(varName));
    lua_rawget(L, LUA_GLOBALSINDEX);

    if lua_isuserdata(L, -1) then
      begin
        Result:=plua_getObject(l, -1, False);
      end;
  finally
    lua_pop(L, 1);
    plua_CheckStackBalance(l, StartTop);
  end;
end;


function plua_getObjectTable(l: PLua_State; idx: Integer; PopValue: boolean
  ): TObjArray;
var
  obj_user:PPLuaInstanceInfo;
  instance : PLuaInstanceInfo;
  C:Integer;
begin
  SetLength(Result, 20);
  C:=0;

  //table traversal
  //http://www.lua.org/manual/5.0/manual.html#3.5
  // table is in the stack at index idx
  lua_pushnil(L);  // first key
  while (lua_next(L, idx) <> 0) do
  begin
    // key is at index -2 and value at index -1
     obj_user:= lua_touserdata(L, -1);
     if obj_user <> nil then
       begin
         instance := obj_user^;
         if assigned(instance) and assigned(instance^.obj) then
           begin
             Inc(C);
             if Length(Result) < C then
                SetLength(Result, Length(Result)+20);

             Result[C-1]:=instance^.obj;
           end;
       end;

     lua_pop(L, 1);  // removes value; keeps key for next iteration
  end;

  SetLength(Result, C);

  if PopValue then
    lua_pop(l, 1);
end;

function plua_getObjectInfo(l: PLua_State; idx: Integer): PLuaInstanceInfo;
var obj_user:PPLuaInstanceInfo;
    i:Integer;
begin
  result := nil;

  try
    i:=plua_absindex(l, idx);
    if not lua_isuserdata(l, i) then Exit;

    obj_user:=lua_touserdata(l, i);
    if obj_user <> nil then
       Result:=obj_user^;
  finally
    if lua_gettop(l) = idx then //злобный хак! Чтобы отказаться от него нужно потратить кучу времени на тестирование всего от него зависящего!
      lua_pop(l, 1);
  end;
end;

function plua_registerExisting(l: PLua_State; InstanceName: AnsiString;
  ObjectInstance: TObject; classId: TLuaClassId;
  FreeOnGC : Boolean; KeepRef:boolean) : PLuaInstanceInfo;
begin
  plua_pushstring(l, InstanceName);
  Result:=plua_pushexisting(l, ObjectInstance, classId, FreeOnGC, KeepRef);
  lua_settable(l, LUA_GLOBALSINDEX );
end;

function plua_pushexisting(l: PLua_State; ObjectInstance: TObject;
  classId: TLuaClassId; FreeOnGC: Boolean; KeepRef:boolean): PLuaInstanceInfo;
var
  StartTop : Integer;
  classinfo: PLuaClassInfo;
  Lua:TLuaInternalState;
begin
  StartTop:=lua_gettop(l);

  Result := plua_GetObjectInfo(l, ObjectInstance);
  if assigned(Result) and
     (Result^.LuaRef <> LUA_NOREF) // is it possible to push a reference to existing object instead of object itself?
    then
    begin
      plua_PushObject(Result);
    end
    else
    begin
      LogDebug('plua_pushexisting. Object $%P', [Pointer(ObjectInstance)]);

      Lua:=LuaSelf(l);

      Result:=plua_pushexisting_special(l, ObjectInstance, classId, FreeOnGC, KeepRef);
      LuaObjects_Add( Lua, Result );

      classinfo:=Lua.LuaClasses.ClassInfoById[ classId ];

      //assign metatable to simulate "classness"
      lua_getglobal(l, PChar(ClassMetaTableName(classinfo)) );
      lua_setmetatable(l, -2);
    end;
  plua_CheckStackBalance(l, StartTop + 1, LUA_TUSERDATA);
end;

function plua_pushexisting_special(l: PLua_State; ObjectInstance: TObject; classId: TLuaClassId; FreeOnGC: Boolean;
  KeepRef: boolean): PLuaInstanceInfo;
var obj_user:PPLuaInstanceInfo;
    StartTop : Integer;
begin
  StartTop:=lua_gettop(l);

  Result:=plua_instance_new;
  Result^.OwnsObject := FreeOnGC;
  Result^.ClassId := classId;
  Result^.l := l;
  Result^.obj := ObjectInstance;
  {$IFDEF DEBUG}
  Result^.ClassName:=ObjectInstance.ClassName;
  {$ENDIF}

  obj_user:=lua_newuserdata(L, sizeof(obj_user^));
  obj_user^:=Result;

  if not KeepRef then
    Result^.LuaRef:=LUA_NOREF
    else
    begin
      Result^.LuaRef := luaL_ref(L, LUA_REGISTRYINDEX);
      lua_rawgeti(Result^.l, LUA_REGISTRYINDEX, Result^.LuaRef);
    end;

  LogDebug('plua_pushexisting. Object $%P. LuaRef=%d', [ Pointer(ObjectInstance), Result^.LuaRef ]);

  plua_CheckStackBalance(l, StartTop + 1, LUA_TUSERDATA);
end;

function plua_gc_object_userdata(l : PLua_State) : integer; extdecl;
var obj_user:^TObject;
begin
  obj_user:=lua_touserdata(l, -1);
  obj_user^.Free;
  result := 0;
end;

procedure plua_PushObjectAsUserData(l: Plua_State; O: TObject);
var obj_user:^TObject;
    StartTop : Integer;
begin
  StartTop:=lua_gettop(l);

  obj_user:=lua_newuserdata(L, sizeof(obj_user^));
  obj_user^:=O;

  //attach metatable with finalizer
  lua_newtable(L);
  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @plua_gc_object_userdata);
  lua_rawset(L, -3);

  lua_setmetatable(L, -2);

  plua_CheckStackBalance(l, StartTop + 1, LUA_TUSERDATA);
end;

procedure plua_instance_free(l: Plua_State; var Instance: PLuaInstanceInfo);
begin
  //remove reference
  if l <> nil then
    plua_ref_release(l, Instance);

  Dispose(Instance);
  Instance:=nil;
end;

function plua_ObjectMarkFree(l: Plua_State; ObjectInstance: TObject):boolean;
var objinfo:PLuaInstanceInfo;
begin
  objinfo := plua_GetObjectInfo(l, ObjectInstance);
  if objinfo = nil then
     raise LuaException.CreateFmt('Object $%P does not have object info', [Pointer(ObjectInstance)]);

  //remove reference
  Result:=plua_ref_release(l, objinfo);

  if not objinfo^.OwnsObject then
    LuaObjects_Free( LuaSelf(l), objinfo );
end;

function plua_PushObject(ObjectInfo: PLuaInstanceInfo) : Boolean;
begin
  result := false;
  if assigned(ObjectInfo) then
    begin
      if ObjectInfo^.LuaRef = LUA_NOREF then
         raise LuaException.CreateFmt('Object $%P does not have Lua ref. Can'' push it on stack', [Pointer(ObjectInfo^.obj)]);

      lua_rawgeti(ObjectInfo^.l, LUA_REGISTRYINDEX, ObjectInfo^.LuaRef);
      result := true;
    end;
end;

function plua_GetObjectInfo(l : Plua_State; InstanceObject: TObject): PLuaInstanceInfo;
begin
  result := LuaSelf(l).LuaClasses.GetInfo(l, InstanceObject);
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

function plua_GetEventDeletage(S: TLuaInternalState; Obj: TObject): TLuaObjectEventDelegate;
var
  d : TLuaObjectEventDelegate;
  i : Integer;
begin
  result := nil;
  i := 0;
  while (not assigned(result)) and (i < S.LuaDelegates.Count) do
    begin
      d := TLuaObjectEventDelegate(S.LuaDelegates[i]);
      if d.FInstanceInfo^.obj = obj then
        result := d;
      inc(i);
    end;
end;

{ TLuaClassList }

function TLuaClassList.GetClassInfo(index : integer): PLuaClassInfo;
begin
  result := PLuaClassInfo(fItems[index]);
end;

function TLuaClassList.GetClassInfoById(Id : TLuaClassId): PLuaClassInfo;
begin
  Result:=FClassIdToClassInfo.KeyData[Id];
end;

function TLuaClassList.GetCount: integer;
begin
  result := fItems.Count;
end;

constructor TLuaClassList.Create;
begin
  fItems := TList.Create;
  FClassIdToClassInfo:=TClassIdObjMap.Create;
  FClassIdToClassInfo.Sorted:=True;
end;

destructor TLuaClassList.Destroy;
begin
  Clear;
  fItems.Free;
  FClassIdToClassInfo.Free;
  inherited Destroy;
end;

function TLuaClassList.GetPropReader(ClassId : TLuaClassId; const aPropertyName : AnsiString): plua_PropertyReader;
var
  pi : PtrInt;
  ei : PWordListSymbol;
  aClassInfo: PLuaClassInfo;
begin
  aClassInfo:=ClassInfoById[ ClassId ];

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

function TLuaClassList.GetPropWriter(ClassId : TLuaClassId; const aPropertyName : AnsiString; out ReadOnly : Boolean): plua_PropertyWriter;
var
  pi : PtrInt;
  ei : PWordListSymbol;
  aClassInfo: PLuaClassInfo;
begin
  aClassInfo:=ClassInfoById[ ClassId ];

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
  P : PLuaInstanceInfo;
  S:TLuaInternalState;
begin
  result := nil;
  S:=LuaSelf(l);

  for i:=0 to S.LuaObjects.Count-1 do
    begin
      P:=PLuaInstanceInfo(S.LuaObjects[i]);
      if (P^.obj = InstanceObject) and (P^.l = l)
         then
         begin
           result := P;
           break;
         end;
    end;
  {--
  i := 0;
  while (result = nil) and (i < LuaObjects.Count) do
    begin
      if (PLuaInstanceInfo(LuaObjects[i])^.obj = InstanceObject) and
         (PLuaInstanceInfo(LuaObjects[i])^.l = l) then
        result := PLuaInstanceInfo(LuaObjects[i]);
      inc(i);
    end;
  }
end;

function TLuaClassList.Add(aClassInfo: PLuaClassInfo) : Integer;
{
var
  ci  : PLuaClassInfo;
  }
begin
  result := IndexOf(aClassInfo^.ClassName);
  {
  if result = -1 then
    begin
      new(ci);

      result := fItems.Add(ci);
    end
  else
    ci := ClassInfo[result];
  ci^:= aClassInfo;
  }
  if result <> -1 then
    begin
      FreeItem( ClassInfo[result] );
      fItems[Result]:=aClassInfo;
    end
    else
    begin
      result := fItems.Add(aClassInfo);
    end;

  FClassIdToClassInfo.Add( aClassInfo^.ClassId, aClassInfo );
end;

procedure TLuaClassList.FreeItem(ci : PLuaClassInfo);
begin
  FClassIdToClassInfo.Remove( ci^.ClassId );
  plua_releaseClassInfo(ci);
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
      FreeItem(ci);
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
  FClassIdToClassInfo.Clear;
  while count > 0 do
    begin
      ci := ClassInfo[count-1];
      FreeItem(ci);
      fItems.Delete(count-1);
    end;
end;

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

constructor TLuaObjectEventDelegate.Create(Lua: {TLuaInternalState} Pointer; InstanceInfo: PLuaInstanceInfo; obj : TObject);
begin
  FLua:=Lua;
  TLuaInternalState(FLua).LuaDelegates.Add(Self);

  FInstanceInfo := InstanceInfo;
  FObj := obj;
  InstanceInfo^.Delegate := self;
end;

destructor TLuaObjectEventDelegate.Destroy;
begin
  TLuaInternalState(FLua).LuaDelegates.Remove(self);
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
    plua_registerclass(l, IndexedItem[i]);
end;

end.
