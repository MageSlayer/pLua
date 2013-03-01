unit LuaWrapper;

interface

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$TYPEDADDRESS ON}
{$ENDIF}

{$I pLua.inc}

{$DEFINE TLuaAsComponent}
{$DEFINE TLuaHandlersAsIsObjectType}

uses
  Classes,
  lua,
  pLua,
  pLuaObject, pLuaRecord;

type
  TLua = class;

  //function type which supports native Pascal exception handling
  TLuaProc = function (l : PLua_State; paramcount: Integer) : integer;

  TLuaOnException = procedure( Title: ansistring; Line: Integer; Msg: ansistring;
                               var handled : Boolean) {$IFDEF TLuaHandlersAsIsObjectType}of object{$ENDIF};
  TLuaOnLoadLibs  = procedure( LuaWrapper : TLua ) {$IFDEF TLuaHandlersAsIsObjectType}of object{$ENDIF};
  
  { TLUA }
  TLUA=class{$IFDEF TLuaAsComponent}(TComponent){$ENDIF}
  private
    FOnException: TLuaOnException;
    FOnLoadLibs: TLuaOnLoadLibs;
    FUseDebug: Boolean;
    L : Plua_State;
    FScript,
    FLibFile,
    FLibName: AnsiString;
    FMethods : TStringList;
    FErrHandler: Integer;

    //Lua internal objects
    FLuaObjects : TList;
    FLuaClasses : TLuaClassList;
    FLuaRecords : TLuaRecordList;
    FintLuaRecords : TList;
    FLuaDelegates : TList;
    FClassTypesList : TLuaClassTypesList;
    FRecordTypesList : TLuaRecordTypesList;

    FLuaSelf : PLuaInstanceInfo;

    procedure ClearObjects(LeakWarnings: boolean);
    procedure ClearRecords;
    function  GetLuaCPath: AnsiString;
    function  GetLuaPath: AnsiString;
    function  GetValue(valName : AnsiString): Variant;
    procedure SetLibName(const Value: AnsiString);
    procedure SetLuaCPath(const AValue: AnsiString);
    procedure SetLuaPath(const AValue: AnsiString);
    procedure OpenLibs;
    procedure SetOnException(const AValue: TLuaOnException);
    procedure SetOnLoadLibs(const AValue: TLuaOnLoadLibs);
    procedure SetUseDebug(const AValue: Boolean);
    procedure ErrorTest(errCode : Integer);
    procedure HandleException(E : LuaException);
    procedure SetValue(valName : AnsiString; const AValue: Variant);
    procedure ExecuteScript(NResults:integer);
  public
    constructor Create{$IFDEF TLuaAsComponent}(anOwner : TComponent); override;{$ENDIF}
    {$IFDEF TLuaAsComponent}constructor Create;{$ENDIF}
    destructor Destroy; override;

    procedure Close;
    procedure Open;

    function Owns(Obj:TObject):boolean;

    //mark given object as being ready for garbage collection
    function ObjMarkFree(Obj:TObject):boolean;overload;
    procedure ObjMarkFree(const Obj:TObjArray);overload;

    //test internal state (global variable, etc)
    procedure CheckIntegrity;

    procedure GarbageCollect;

    procedure LoadScript(const Script : AnsiString);
    procedure LoadFile(const FileName:AnsiString);

    //Loads function and saves it in lua state under given name.
    //It's purpose is to run it multiple times without reloading.
    procedure LoadFunctionFromFile(const FileName:string; const FunctionSaveAs:string);
    procedure LoadFunctionFromScript(const Script:string; const FunctionSaveAs:string);

    procedure Execute;

    function  ExecuteAsFunctionObj(const FunctionName:string):TObject;
    function  ExecuteAsFunctionObjList(const FunctionName:string):TObjArray;
    procedure ExecuteAsFunctionStrList(const Script: string; ResultTable: TStrings);

    procedure ExecuteCmd(Script:AnsiString);
    procedure ExecuteAsRepl(const Script:String; out ReplResult:string);
    procedure ExecuteFile(FileName : AnsiString);

    //simple registering of C-function. No proper support for exception handling!!!
    procedure RegisterLuaMethod(const aMethodName: AnsiString; Func: lua_CFunction);
    //registering of pascal-function. Proper support for exception handling!!!
    procedure RegisterLuaMethod(const aMethodName: AnsiString; Func: TLuaProc);
    procedure RegisterLuaMethod(const aPackage, aMethodName: AnsiString; Func: TLuaProc);

    procedure RegisterLuaTable(PropName: AnsiString; reader: lua_CFunction; writer : lua_CFunction = nil);
    function  FunctionExists(aMethodName:AnsiString) : Boolean;
    function  CallFunction( FunctionName :AnsiString; const Args: array of Variant;
                            Results : PVariantArray = nil):Integer;
    function  TableExists(const TableName):boolean;
    function  TableFunctionExists(TableName, FunctionName : AnsiString; out tblidx : Integer) : Boolean; overload;
    function  TableFunctionExists(TableName, FunctionName : AnsiString) : Boolean; overload;
    function  CallTableFunction( TableName, FunctionName :AnsiString;
                               const Args: array of Variant;
                               Results : PVariantArray = nil):Integer;

    procedure ObjArraySet(const varName:String; const A:TObjArray; C: TLuaClassId; FreeGC:boolean = False; KeepRef:boolean = True);
    procedure ObjSet(const varName:String; const O:TObject; C: TLuaClassId; FreeGC:boolean = False; KeepRef:boolean = True);
    procedure ObjSetEmpty(const varName:String);
    function  ObjGet(const varName:string):TObject;

    procedure GlobalObjClear;
    procedure GlobalVarClear(const varName:string);
    procedure GlobalObjectNames(List:TStrings; LuaType:Integer);

    property ScriptText: AnsiString read FScript write FScript;
    property ScriptFile: AnsiString read FLibFile write FLibFile;
    property LibName  : AnsiString read FLibName write SetLibName;
    property LuaState : Plua_State read L;
    property LuaPath  : AnsiString read GetLuaPath write SetLuaPath;
    property LuaCPath : AnsiString read GetLuaCPath write SetLuaCPath;
    property UseDebug : Boolean read FUseDebug write SetUseDebug;
    property Value[valName : AnsiString] : Variant read GetValue write SetValue; default;
    property OnException : TLuaOnException read FOnException write SetOnException;
    property OnLoadLibs : TLuaOnLoadLibs read FOnLoadLibs write SetOnLoadLibs;
  end;

  TLuaInternalState = class(TLua)
    //class to be used by Lua handlers
    public
      property LuaObjects : TList read FLuaObjects;
      property LuaClasses : TLuaClassList read FLuaClasses;
      property intLuaRecords : TList read FintLuaRecords;
      property LuaDelegates : TList read FLuaDelegates;
      property LuaRecords : TLuaRecordList read FLuaRecords;
      property ClassTypesList : TLuaClassTypesList read FClassTypesList;
      property RecordTypesList : TLuaRecordTypesList read FRecordTypesList;
  end;

  { TLUAThread }
  TLUAThread=class
  private
    FMaster : TLUA;
    FMethodName: AnsiString;
    FTableName: AnsiString;
    L : PLua_State;
    FThreadName : AnsiString;
    function GetIsValid: Boolean;
  public
    constructor Create(LUAInstance: TLUA; ThreadName : AnsiString);
    destructor Destroy; override;

    function Start(TableName : AnsiString; AMethodName : AnsiString; const ArgNames: array of AnsiString; var ErrorString : AnsiString) : Boolean;
    function Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString) : Boolean;

    property LuaState : Plua_State read L;
  published
    property IsValid : Boolean read GetIsValid;
    property ThreadName : AnsiString read FThreadName;
    property MethodName : AnsiString read FMethodName;
    property TableName  : AnsiString read FTableName;
  end;

  { TLUAThreadList }
  TLUAThreadList=class
  private
    FThreads : TList;
    FLUAInstance : TLUA;
    function GetCount: Integer;
    function GetThread(index: integer): TLUAThread;
  public
    constructor Create(LUAInstance: TLUA);
    destructor Destroy; override;

    procedure Process(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString);

    function SpinUp(TableName, AMethodName, ThreadName : AnsiString; var ErrorString : AnsiString) : Boolean;
    function IndexOf(ThreadName : AnsiString): Integer;
    procedure Release(ThreadIndex : Integer);

    property Thread[index:integer]: TLUAThread read GetThread;
  published
    property Count : Integer read GetCount;
  end;

  //To be used in Lua handlers
  //Use carefully, it is global for Lua instance!!!
  function LuaSelf(L : PLua_State):TLuaInternalState;
  procedure LuaObjects_Free(S:TLuaInternalState; instance:PLuaInstanceInfo);
  procedure LuaObjects_Add(S:TLuaInternalState; instance:PLuaInstanceInfo);
  procedure LuaRecs_Free(S:TLuaInternalState; instance:PLuaRecordInstanceInfo);

implementation

uses
  Variants,
  SysUtils;

const
  Lua_Self = '__LuaWrapperSelf';

constructor TLUA.Create{$IFDEF TLuaAsComponent}(anOwner: TComponent){$ENDIF};
begin
  {$IFDEF TLuaAsComponent}inherited;{$ENDIF}
  FUseDebug := false;
  FMethods := TStringList.Create;

  FLuaObjects := TList.Create;
  FLuaClasses := TLuaClassList.Create;
  FintLuaRecords := TList.Create;
  FLuaDelegates := TList.Create;
  FLuaRecords := TLuaRecordList.Create;
  FClassTypesList := TLuaClassTypesList.Create;
  FRecordTypesList := TLuaRecordTypesList.Create;
  FLibName:='main';

  Open;
end;

{$IFDEF TLuaAsComponent}
constructor TLUA.Create;
begin
  Create(nil);
end;
{$ENDIF}

procedure TLUA.ClearObjects(LeakWarnings:boolean);
// Frees/unregistered manually-tracked objects
var
  i   : Integer;
  nfo : PLuaInstanceInfo;
begin
  if FLuaObjects.Count > 0 then
    Log('Warning!!! %d objects left unfreed.', [FLuaObjects.Count]);

  i := FLuaObjects.Count-1;
  while i > -1 do
    begin
      nfo := PLuaInstanceInfo(FLuaObjects[i]);
      if nfo^.l = l then
        begin
          if LeakWarnings then
            begin
              //we cannot trust nfo^.obj is still valid here, so do not try to read ClassName via nfo^.obj.
              //use only debug info when accesible.
              if nfo^.LuaRef <> LUA_NOREF then
                 LogDebug('Lua object $%P (%s) has lua ref (%d) unfreed.',
                          [ Pointer(nfo^.obj), {$IFDEF DEBUG}nfo^.ClassName{$ELSE}'unknown'{$ENDIF}, nfo^.LuaRef ]);

              LogDebug('Lua object $%P (%s) memory leak. Freeing...', [ Pointer(nfo^.obj), {$IFDEF DEBUG}nfo^.ClassName{$ELSE}'unknown'{$ENDIF} ]);
            end;

          plua_ref_release( l, nfo );
          LuaObjects_Free( TLuaInternalState(Self), nfo );
        end;
      dec(i);
    end;
end;

procedure TLUA.ClearRecords;
var
  i   : Integer;
  nfo : PLuaRecordInstanceInfo;
begin
  i := FintLuaRecords.Count-1;
  while i > -1 do
    begin
      nfo := PLuaRecordInstanceInfo(FintLuaRecords[i]);
      if nfo^.l = l then
        FintLuaRecords.Remove(nfo);
      dec(i);
    end;
end;

destructor TLUA.Destroy;
var instance:PLuaInstanceInfo;
    rec_instance:PLuaRecordInstanceInfo;
begin
  Close;
  FMethods.Free;
  FreeAndNil(FLuaObjects);
  FreeAndNil(FLuaClasses);
  FreeAndNil(FintLuaRecords);

  FreeAndNil(FLuaDelegates);
  FreeAndNil(FLuaRecords);
  FreeAndNil(FClassTypesList);
  FreeAndNil(FRecordTypesList);

  inherited;
end;

procedure TLUA.ExecuteScript(NResults: integer);
var errCode:Integer;
    msg:string;
begin
  if L = nil then
    Open;

  if (lua_gettop(l) <= 0) or
     (lua_type(L,-1) <> LUA_TFUNCTION) then
     raise Exception.Create('No script is loaded at stack');

  //ErrorTest(lua_pcall(L, 0, NResults, 0));

  errCode:=lua_pcall(L, 0, NResults, FErrHandler);
  if errCode <> 0 then
    begin
      msg := plua_tostring(l, -1);
      HandleException(LuaException.Create(msg));
    end;
end;

procedure TLUA.Execute;
begin
  ExecuteScript(0);
end;

function TLUA.ExecuteAsFunctionObj(const FunctionName:string): TObject;
var tix:Integer;
    StartTop:integer;
begin
  Result:=nil;
  StartTop:=lua_gettop(l);

  //load function with name FunctionName on stack
  lua_getglobal(l, PChar(FunctionName));

  ExecuteScript(LUA_MULTRET);
  tix:=lua_gettop(l);
  if tix > 0 then
    begin
      if lua_type(L,-1) = LUA_TUSERDATA then
        Result:=plua_getObject(l, tix)
        else
        lua_pop(l, 1);
    end;

  plua_CheckStackBalance(l, StartTop);
end;

function TLUA.ExecuteAsFunctionObjList(const FunctionName: string): TObjArray;
var tix:Integer;
    StartTop:integer;
    O:TObject;
begin
  SetLength(Result,0);
  StartTop:=lua_gettop(l);

  //load function with name FunctionName on stack
  lua_getglobal(l, PChar(FunctionName));

  ExecuteScript(LUA_MULTRET);
  tix:=lua_gettop(l);
  if tix > 0 then
    begin
      case lua_type(L,-1) of
        LUA_TUSERDATA:
          begin
            O:=plua_getObject(l, tix);
            SetLength(Result, 1);
            Result[0]:=O;
          end;

        LUA_TTABLE:
          begin
            Result:=plua_getObjectTable(l, tix);
          end;

        else
          lua_pop(l, 1);
      end;
    end;

  plua_CheckStackBalance(l, StartTop);
end;

procedure TLUA.ExecuteAsFunctionStrList(const Script: string; ResultTable:TStrings) ;
var Val:PChar;
    StartTop, idx:integer;
begin
  ResultTable.Create;
  StartTop:=lua_gettop(l);

  try
    luaL_loadstring(l, PChar(Script));
    ExecuteScript(LUA_MULTRET);
    if lua_type(L,-1) = LUA_TTABLE then
      begin
        //read table into TString object

        idx:=lua_gettop(l);
        //table traversal
        //http://www.lua.org/manual/5.0/manual.html#3.5
        // table is in the stack at index idx
        lua_pushnil(L);  // first key
        while (lua_next(L, idx) <> 0) do
        begin
           if lua_type(L, -1) <> LUA_TSTRING then
             raise LuaException.Create('ExecuteAsFunctionStrList requires to be all table values to be strings');

           // key is at index -2 and value at index -1
           Val:= lua_tostring(L, -1);
           if Val <> nil then
             ResultTable.Add( String(Val) );

           lua_pop(L, 1);  // removes value; keeps key for next iteration
        end;
      end;
  finally
    plua_EnsureStackBalance(l, StartTop);
  end;
end;

procedure TLUA.ExecuteCmd(Script: AnsiString);
var StartTop:Integer;
begin
  if L= nil then
    Open;

  StartTop:=lua_gettop(l);
  try
    ErrorTest(luaL_loadbuffer(L, PChar(Script), Length(Script), PChar(LibName)));
    ExecuteScript(0);
  finally
    plua_EnsureStackBalance(l, StartTop);
  end;
end;

procedure TLUA.ExecuteAsRepl(const Script: String; out ReplResult: string);
var StartTop:Integer;
    r:String;
    ExceptThrown:boolean;
begin
  ExceptThrown:=false;
  ReplResult:='';
  if L= nil then
    Open;
  StartTop:=lua_gettop(l);

  try
    ErrorTest(luaL_loadbuffer(L, PChar(Script), Length(Script), PChar(LibName)));
    ExecuteScript(LUA_MULTRET); // LUA_MULTRET - нас интересуют _все_ результаты
  except
    on E:Exception do
      begin
        ReplResult:=E.Message + sLineBreak;
        ExceptThrown:=true;
      end;
  end;

  //пока есть результаты - продолжаем выталкивать из стека
  while lua_gettop(l) <> StartTop do
  begin
    if (not ExceptThrown) and (lua_type(L,-1) = LUA_TSTRING) then
      begin
        r := plua_tostring(l, -1);
        ReplResult:=r + sLineBreak + ReplResult; //последний результат лежит на верху стека
      end;
    lua_pop(l, 1);
  end;

  plua_CheckStackBalance(l, StartTop);
end;

procedure TLUA.ExecuteFile(FileName: AnsiString);
var
  Script : AnsiString;
  sl     : TStringList;
begin
  if L = nil then
    Open;

  ErrorTest(luaL_loadfile(L, PChar(FileName)));
  ExecuteScript(0);
end;

procedure TLUA.SetLuaPath(const AValue: AnsiString);
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'path');
  lua_pushstring(L, PChar(AValue));
  lua_settable(L, -3);
end;

procedure TLUA.LoadFile(const FileName: AnsiString);
var StartTop:integer;
begin
  if L = nil then
    Open;

  StartTop:=lua_gettop(l);

  FLibFile := FileName;
  FScript := '';
  ErrorTest( luaL_loadfile(L, PChar(FileName)) );

  plua_CheckStackBalance(l, StartTop+1);
end;

procedure TLUA.LoadFunctionFromFile(const FileName: string; const FunctionSaveAs:string);
var StartTop:integer;
begin
  StartTop:=lua_gettop(l);

  LoadFile(FileName);
  lua_setglobal(L, PChar(FunctionSaveAs));

  plua_CheckStackBalance(l, StartTop);
end;

procedure TLUA.LoadFunctionFromScript(const Script: string;
  const FunctionSaveAs: string);
var StartTop:integer;
begin
  StartTop:=lua_gettop(l);

  LoadScript(Script);
  lua_setglobal(L, PChar(FunctionSaveAs));

  plua_CheckStackBalance(l, StartTop);
end;

procedure TLUA.LoadScript(const Script: AnsiString);
var StartTop:integer;
begin
  if FScript <> Script then
    Close;

  if L = nil then
    Open;

  StartTop:=lua_gettop(l);

  FScript := Trim(Script);
  FLibFile := '';
  if FScript <> '' then
    luaL_loadbuffer(L, PChar(Script), length(Script), PChar(LibName));

  plua_CheckStackBalance(l, StartTop);
end;

function TLUA.FunctionExists(aMethodName: AnsiString): Boolean;
begin
  lua_pushstring(L, PChar(aMethodName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := (not lua_isnil(L, -1)) and lua_isfunction(L, -1);
  lua_pop(L, 1);
end;

procedure TLUA.RegisterLUAMethod(const aMethodName: AnsiString; Func: lua_CFunction);
begin
  if L = nil then
    Open;
  lua_register(L, PChar(aMethodName), Func);
  if FMethods.IndexOf(aMethodName) = -1 then
    FMethods.AddObject(aMethodName, TObject(@Func))
  else
    FMethods.Objects[FMethods.IndexOf(aMethodName)] := TObject(@Func);
end;

function plua_call_method_except_wrapper(l : PLua_State; out exc_message:pchar) : integer;
var
  method : TLuaProc;
  pcount : Integer;
begin
  result := 0;
  exc_message:=nil;
  try
    pcount := lua_gettop(l);
    method := TLuaProc(lua_topointer(l, lua_upvalueindex(1)));

    if assigned(method) then
      result := method(l, pcount);
  except
    on E:Exception do
      begin
        exc_message:=StrToPChar(E.Message);
      end;
  end;
end;

{$IMPLICITEXCEPTIONS OFF}
function plua_call_method(l : PLua_State) : integer; extdecl;
var
  exc_message:PChar;
  curtop : Integer;
begin
  {
    Using lua_error in functions having automatically-generated "finally" code causes access violations.
    So all exceptions are caught in plua_call_class_method_except_wrapper function and
    returned as a simple PChar strings to avoid any automatic release.
    This function forcibly lacks "finally" code - {$IMPLICITEXCEPTIONS OFF}
  }
  Result:=plua_call_method_except_wrapper(l, exc_message);

  if exc_message <> nil then
    begin
      Result:=0;
      curtop:=lua_gettop(l);
      lua_pop(l, curtop); //remove both parameters and any non-complete return values.

      lua_pushstring(L, exc_message);
      StrDispose(exc_message);

      lua_error(L); //does longjmp, almost the same as exception raising
    end;
end;
{$IMPLICITEXCEPTIONS ON}

procedure TLUA.RegisterLuaMethod(const aMethodName: AnsiString; Func: TLuaProc);
begin
  if L = nil then
    Open;

  plua_pushstring(L, aMethodName);
  lua_pushlightuserdata(l, Pointer(Func));
  lua_pushcclosure(L, @plua_call_method, 1);
  lua_rawset(l, LUA_GLOBALSINDEX);
end;

procedure TLUA.RegisterLuaMethod(const aPackage, aMethodName: AnsiString; Func: TLuaProc);
var StartTop:Integer;
begin
  if L = nil then
    Open;

  StartTop:=lua_gettop(L);
  try
    if not TableExists(aPackage) then
      begin
        pLua_TableGlobalCreate(L, aPackage);
      end;

    //push aPackage table onto stack
    plua_pushstring(L, aPackage);
    lua_rawget(L, LUA_GLOBALSINDEX);

    //write a method into aPackage table
    plua_pushstring(L, aMethodName);
    lua_pushlightuserdata(l, Pointer(Func));
    lua_pushcclosure(L, @plua_call_method, 1);
    lua_rawset(l, -3);
  finally
    plua_EnsureStackBalance(L, StartTop);
  end;
end;

procedure TLUA.RegisterLuaTable(PropName: AnsiString; reader: lua_CFunction;
  writer: lua_CFunction);
begin
  plua_RegisterLuaTable(l, PropName, reader, writer);
end;

procedure TLUA.SetLibName(const Value: AnsiString);
begin
  FLibName := Value;
end;

procedure TLUA.SetLuaCPath(const AValue: AnsiString);
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'cpath');
  lua_pushstring(L, PChar(AValue));
  lua_settable(L, -3);
end;

function TLUA.GetLuaPath: AnsiString;
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'path');
  lua_rawget(L, -2);
  result := AnsiString(lua_tostring(L, -1));
end;

function TLUA.GetValue(valName : AnsiString): Variant;
var StartTop:Integer;
begin
  StartTop:=lua_gettop(l);

  result := NULL;
  lua_pushstring(l, PChar(valName));
  lua_rawget(l, LUA_GLOBALSINDEX);
  try
    result := plua_tovariant(l, -1);
  finally
    lua_pop(l, 1);
  end;

  plua_CheckStackBalance(l, StartTop);
end;

function TLUA.GetLuaCPath: AnsiString;
begin
  lua_pushstring(L, 'package');
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, 'cpath');
  lua_rawget(L, -2);
  result := AnsiString(lua_tostring(L, -1));
end;

function TLUA.CallFunction(FunctionName: AnsiString;
  const Args: array of Variant; Results: PVariantArray = nil): Integer;
begin
  try
    if FunctionExists(FunctionName) then
      result := plua_callfunction(L, FunctionName, Args, Results)
    else
      result := -1;
  except
    on E:LuaException do
      HandleException(E);
  end;
end;

function TLUA.TableExists(const TableName): boolean;
var StartTop:Integer;
begin
  StartTop:=lua_gettop(L);
  try
    lua_pushstring(L, PChar(TableName));
    lua_rawget(L, LUA_GLOBALSINDEX);
    result := lua_istable(L, -1);
  finally
    plua_EnsureStackBalance(L, StartTop);
  end;
end;

procedure TLUA.Close;
begin
  FErrHandler:=-1;
  if L <> nil then
    begin
      ClearObjects(True);
      ClearRecords;
      lua_close(L);

      if FLuaSelf <> nil then
        begin
          plua_instance_free(nil, FLuaSelf);
        end;
    end;
  L := nil;

  FLibFile:='';
  FScript:='';
end;

procedure TLUA.Open;
begin
  if L <> nil then
    Close;
  L := lua_open;
  OpenLibs;

  //Register self in Lua VM to be able call from Lua code/native handlers
  FLuaSelf:=plua_pushexisting_special(l, Self, nil, False);
  lua_setglobal( L, PChar(Lua_Self) );

  //push error handler on stack to be able to reference it everywhere
  // http://tinylittlelife.org/?p=254
  lua_getglobal(L, 'debug');
  //-1 is the top of the stack
  lua_getfield(L, -1, 'traceback');
  //traceback is on top, remove debug from 2nd spot
  lua_remove(L, -2);
  FErrHandler:=lua_gettop(L);
end;

function LuaSelf(L: PLua_State): TLuaInternalState;
var StartTop:Integer;
begin
  StartTop:=lua_gettop(l);
  Result:=TLuaInternalState(plua_GlobalObjectGet(l, Lua_Self));

  //global VM object MUST BE registered, otherwise something really wrong.
  if Result = nil then
    plua_RaiseException(l, StartTop, 'Global Lua VM Self pointer is not registered');

  if Result.ClassType <> TLUA then
    plua_RaiseException(l, StartTop, 'Global Lua VM Self must be of type %s, but has type %s', [TLUA.ClassName, Result.ClassName]);

  plua_CheckStackBalance(l, StartTop);
end;

procedure LuaObjects_Add(S:TLuaInternalState; instance:PLuaInstanceInfo);
begin
  S.LuaObjects.Add( instance );
  {$IFDEF DEBUG_LUA}
  DumpStackTrace;
  {$ENDIF}
end;

procedure LuaRecs_Free(S: TLuaInternalState; instance: PLuaRecordInstanceInfo);
var i:integer;
    C:PLuaClassInfo;
    RecInfo:PLuaRecordInfo;
begin
  if instance^.OwnsInstance then
    begin
      RecInfo:=S.LuaRecords.RecInfoById[ instance^.RecordId ];
      RecInfo^.Release(instance^.RecordPointer, nil);
    end;
  Freemem(instance);
end;

procedure LuaObjects_Free(S:TLuaInternalState; instance:PLuaInstanceInfo);
var i:integer;
    C:PLuaClassInfo;
begin
  i:=S.LuaObjects.IndexOf(instance);
  if i <> -1 then
    begin
      if instance^.OwnsObject then
        begin
          C:= S.LuaClasses.ClassInfoById[ instance^.ClassId ];
          if assigned(C^.Release) then
            C^.Release(instance^.obj, nil)
          else
            instance^.obj.Free;
        end;

      if Instance^.Delegate <> nil then
        Instance^.Delegate.Free;
      Dispose(instance);

      S.LuaObjects.Delete(i);
    end;
end;

function TLUA.Owns(Obj: TObject): boolean;
var info:PLuaInstanceInfo;
begin
  Result:=False;

  info:=plua_GetObjectInfo(l, Obj);
  if info = nil then Exit;

  Result:=info^.OwnsObject;
end;

function TLUA.ObjMarkFree(Obj: TObject):boolean;
begin
  Result:=False;
  if l <> nil then
    Result:=plua_ObjectMarkFree(l, Obj);
end;

procedure TLUA.ObjMarkFree(const Obj: TObjArray);
var n:integer;
begin
  if l <> nil then
    for n:=0 to High(Obj) do
      plua_ObjectMarkFree(l, Obj[n]);
end;

procedure TLUA.CheckIntegrity;
begin
  //Lua Self has its own checks
  LuaSelf(l);
end;

procedure TLUA.GarbageCollect;
begin
  if l <> nil then
    lua_gc(l, LUA_GCCOLLECT, 0);
end;

procedure TLUA.OpenLibs;
var
  I : Integer;
begin
  luaL_openlibs(L);
  if UseDebug then
    luaopen_debug(L);
  lua_settop(L, 0);

  for I := 0 to FMethods.Count -1 do
    RegisterLUAMethod(FMethods[I], lua_CFunction(Pointer(FMethods.Objects[I])));

  FRecordTypesList.RegisterTo(L);
  FClassTypesList.RegisterTo(L);

  if assigned(FOnLoadLibs) then
    FOnLoadLibs(self);
end;

procedure TLUA.SetOnException(const AValue: TLuaOnException);
begin
  if FOnException=AValue then exit;
  FOnException:=AValue;
end;

procedure TLUA.SetOnLoadLibs(const AValue: TLuaOnLoadLibs);
begin
  if FOnLoadLibs=AValue then exit;
  FOnLoadLibs:=AValue;
  if (L <> nil) and (FOnLoadLibs <> nil) then
    FOnLoadLibs(self);
end;

procedure TLUA.SetUseDebug(const AValue: Boolean);
begin
  if FUseDebug=AValue then exit;
  FUseDebug:=AValue;
end;

procedure TLUA.ErrorTest(errCode: Integer);
var
  msg : AnsiString;
begin
  if errCode <> 0 then
    begin
      msg := plua_tostring(l, -1);
      lua_pop(l, 1);
      HandleException(LuaException.Create(msg));
    end;
end;

procedure TLUA.HandleException(E: LuaException);
var
  title, msg : AnsiString;
  line       : Integer;
  handled    : Boolean;
begin
  handled := false;
  if assigned(FOnException) then
    begin
      plua_spliterrormessage(e.Message, title, line, msg);
      FOnException(title, line, msg, handled);
    end;
  if not handled then
    raise E;
end;

procedure TLUA.SetValue(valName : AnsiString; const AValue: Variant);
var StartTop:Integer;
begin
  StartTop:=lua_gettop(l);

  if VarIsType(AValue, varString) then
    begin
      lua_pushliteral(l, PChar(valName));
      lua_pushstring(l, PChar(AnsiString(AValue)));
      lua_settable(L, LUA_GLOBALSINDEX);
    end
  else
    begin
      lua_pushliteral(l, PChar(valName));
      plua_pushvariant(l, AValue);
      lua_settable(L, LUA_GLOBALSINDEX);
    end;

  plua_CheckStackBalance(l, StartTop);
end;

function TLUA.CallTableFunction(TableName, FunctionName: AnsiString;
  const Args: array of Variant; Results: PVariantArray): Integer;
var
  tblidx : integer;
begin
  try
    if TableFunctionExists(TableName, FunctionName, tblidx) then
      begin
        lua_pushvalue(l, tblidx);
        tblidx := lua_gettop(l);
        result := plua_callfunction(l, FunctionName, args, results, tblidx)
      end
    else
      result := -1;
  except
    on E: LuaException do
      HandleException(E);
  end;
end;

procedure TLUA.ObjArraySet(const varName: String; const A: TObjArray; C: TLuaClassId; FreeGC:boolean; KeepRef:boolean);
var n, tix:integer;
    StartTop:integer;
begin
  StartTop:=lua_gettop(l);

  {
  //if global var already exists ...
  lua_getglobal(L, PChar(varName) );
  if not lua_isnil(L, -1) then
    begin
      // ... remove it
      lua_pushnil( L );
      lua_setglobal( L, PChar(varName) );
    end;
  lua_pop(L, -1); //balance stack
  }
  lua_newtable(L); // table
  for n:=0 to High(A) do
    begin
      lua_pushinteger(L, n+1); // table,key
      pLuaObject.plua_pushexisting(l, A[n], C, FreeGC, KeepRef);
      lua_settable(L,-3); // table
    end;
  lua_setglobal( L, PChar(varName) );

  plua_CheckStackBalance(l, StartTop);
end;

procedure TLUA.ObjSet(const varName: String; const O: TObject;
  C: TLuaClassId; FreeGC: boolean; KeepRef:boolean);
begin
  pLuaObject.plua_pushexisting(l, O, C, FreeGC, KeepRef);
  lua_setglobal( L, PChar(varName) );
end;

procedure TLUA.ObjSetEmpty(const varName: String);
begin
  lua_pushnil(l);
  lua_setglobal( L, PChar(varName) );
end;

function TLUA.ObjGet(const varName: string): TObject;
begin
  Result:=plua_GlobalObjectGet(l, varName);
end;

procedure TLUA.GlobalObjClear;
begin
  ClearObjects(False);
end;

procedure TLUA.GlobalVarClear(const varName: string);
begin
  lua_pushstring(l, PChar(varName));
  lua_pushnil(l);
  lua_settable(L, LUA_GLOBALSINDEX);
end;

procedure TLUA.GlobalObjectNames(List: TStrings; LuaType: Integer);
begin
  List.Clear;
  if LuaType = LUA_TFUNCTION then
    begin
      ExecuteAsFunctionStrList('return lua_info.global_functions()', List);
    end;
end;

function TLUA.TableFunctionExists(TableName,
  FunctionName: AnsiString; out tblidx : Integer): Boolean;
begin
  lua_pushstring(L, PChar(TableName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := lua_istable(L, -1);
  if result then
    begin
      tblidx := lua_gettop(L);
      lua_pushstring(L, PChar(FunctionName));
      lua_rawget(L, -2);
      result := lua_isfunction(L, -1);
      lua_pop(L, 1);
    end
  else
    begin
      tblidx := -1;
      lua_pop(L, 1);
    end;
end;

function TLUA.TableFunctionExists(TableName, FunctionName: AnsiString
  ): Boolean;
var
  tblidx : Integer;
begin
  result := TableFunctionExists(TableName, FunctionName, tblidx);
  if result then
    lua_pop(L, 1);
end;

{ TLUAThread }

function TLUAThread.GetIsValid: Boolean;
begin
  lua_getglobal(L, PChar(FThreadName));
  result := not lua_isnil(L, 1);
  lua_pop(L, 1);
end;

constructor TLUAThread.Create(LUAInstance: TLUA; ThreadName: AnsiString);
begin
  L := lua_newthread(LUAInstance.LuaState);
  FThreadName := ThreadName;
  lua_setglobal(LUAInstance.LuaState, PChar(ThreadName));
  FMaster := LUAInstance;
end;

destructor TLUAThread.Destroy;
begin
  lua_pushnil(FMaster.LuaState);
  lua_setglobal(FMaster.LuaState, PChar(FThreadName));
  inherited;
end;

function luaResume(L : PLua_State; NArgs:Integer; out Res : Integer) : Boolean;
begin
  Res := lua_resume(L, NArgs);
  result := Res <> 0;
end;

function TLUAThread.Start(TableName : AnsiString; AMethodName : AnsiString; const ArgNames: array of AnsiString; var ErrorString : AnsiString) : Boolean;
var
  i,
  rres : Integer;
begin
  FTableName := TableName;
  FMethodName := AMethodName;
  if TableName <> '' then
    begin
      lua_pushstring(L, PChar(TableName));
      lua_gettable(L, LUA_GLOBALSINDEX);
      plua_pushstring(L, PChar(AMethodName));
      lua_rawget(L, -2);
    end
  else
    lua_getglobal(L, PChar(AMethodName));

  for i := 0 to Length(ArgNames)-1 do
    lua_getglobal(L, PChar(ArgNames[i]));

  if luaResume(L, Length(ArgNames), rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      result := false;
      exit;
    end
  else
    result := true;
end;

function TLUAThread.Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString) : Boolean;
var
  rres,
  i : Integer;
  msg : AnsiString;
begin
  lua_pushnumber(L, EllapsedTime);
  for i := 0 to Length(Args)-1 do
    plua_pushvariant(L, Args[i]);
  if luaResume(L, Length(Args)+1, rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      msg := 'Error ('+IntToStr(rres)+'): '+ErrorString;
      result := false;
      raise exception.Create(msg);
    end
  else
    result := true;
end;

{ TLUAThreadList }

function TLUAThreadList.GetCount: Integer;
begin
  result := FThreads.Count;
end;

function TLUAThreadList.GetThread(index: integer): TLUAThread;
begin
  result := TLUAThread(FThreads[index]);
end;

constructor TLUAThreadList.Create(LUAInstance: TLUA);
begin
  FLUAInstance := LUAInstance;
  FThreads := TList.Create;
end;

destructor TLUAThreadList.Destroy;
var
  T : TLUAThread;
begin
  while FThreads.Count > 0 do
    begin
      T := TLUAThread(FThreads[FThreads.Count-1]);
      FThreads.Remove(T);
      T.Free;
    end;
  FThreads.Free;
  inherited;
end;

procedure TLUAThreadList.Process(EllapsedTime: lua_Number; Args : array of Variant;
  var ErrorString: AnsiString);
var
  i : Integer;
begin
  i := 0;
  while i < Count do
    begin
      if not TLUAThread(FThreads[I]).Resume(EllapsedTime, Args, ErrorString) then
        Release(i)
      else
        inc(i);
    end;
end;

function TLUAThreadList.SpinUp(TableName, AMethodName, ThreadName: AnsiString; var ErrorString : AnsiString) : Boolean;
var
  T : TLUAThread;
begin
  T := TLUAThread.Create(FLUAInstance, ThreadName);
  FThreads.Add(T);
  result := T.Start(TableName, AMethodName, [], ErrorString);
end;

function TLUAThreadList.IndexOf(ThreadName: AnsiString): Integer;
var
  i : Integer;
begin
  result := -1;
  i := 0;
  while (result = -1) and (i<FThreads.Count) do
    begin
      if CompareText(ThreadName, TLUAThread(FThreads[i]).ThreadName) = 0 then
        result := i;
      inc(i);
    end;
end;

procedure TLUAThreadList.Release(ThreadIndex: Integer);
var
  T : TLUAThread;
begin
  if (ThreadIndex < Count) and (ThreadIndex > -1) then
    begin
      T := TLUAThread(FThreads[ThreadIndex]);
      FThreads.Delete(ThreadIndex);
      T.Free;
    end;
end;

initialization

finalization

end.
