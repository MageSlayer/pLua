unit pLua;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$ENDIF}

{$I pLua.inc}

interface

uses
  SysUtils, Classes, lua, variants;

type
  TVariantArray =array of Variant;
  PVariantArray =^TVariantArray;
  TObjArray = array of TObject;

  //Lua object type. Do not change enum values, except for addition of new types!!!
  TLuaObjectType = ( lotFunction = 1,        //all accessible functions
                     lotFunctionSource = 2,  //all accessible functions with sources available
                     lotGlobalVars = 3       //all global vars
                     );
  TLuaObjectTypes = set of TLuaObjectType;

  LuaException = class(Exception)
  end;

  //function type which supports native Pascal exception handling
  TLuaProc = function (l : PLua_State; paramcount: Integer) : integer;
  TLuaNakedProc = function (l : PLua_State) : integer;

  TLuaCdataHandler = function (Cdata:Pointer):Variant;
  TLuaVariantHandler = function (l : Plua_State; const V:Variant) : boolean;
  TLuaVariantFinalizer = procedure (var V:Variant);

{$IFDEF LUA_LPEG} // as it links statically, not everybody can need it.
const
  {$IFDEF WINDOWS}
    {$IFDEF CPU32}
    LpegLib = 'lpeg.dll'
    {$ENDIF}
    {$IFDEF CPU64}
    LpegLib = 'lpeg-x64.dll'
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF CPU32}
    LpegLib = 'lpeg.so'
    {$ENDIF}
    {$IFDEF CPU64}
    LpegLib = 'lpeg-x64.so'
    {$ENDIF}
  {$ENDIF}
  ;
//register Lpeg in Lua instance
function luaopen_lpeg (L: PLua_State):Integer;cdecl;external LpegLib;
{$ENDIF}

function  plua_tostring(L: PLua_State; Index: Integer): ansistring;
procedure plua_pushstring(L: PLua_State; const AString : AnsiString);
function StrToPChar(const S:string):PChar;

procedure plua_RegisterLuaTable( l:PLua_State; Name : AnsiString;
                                 Reader : lua_CFunction = nil;
                                 Writer : lua_CFunction = nil;
                                 TableIndex : Integer = LUA_GLOBALSINDEX);

function plua_functionexists( L: PLua_State; FunctionName : AnsiString;
                              TableIndex : Integer = LUA_GLOBALSINDEX) : boolean;

type
  TLuaParamPushProc = function (L:Plua_State):Integer is nested;
function plua_callfunction( L: PLua_State; FunctionName : AnsiString;
                            const args : array of Variant;
                            results : PVariantArray = nil;
                            TableIndex : Integer = LUA_GLOBALSINDEX;
                            ErrHandler: Integer = 0;
                            ParamsToPush:TLuaParamPushProc = nil;
                            VariantHandler:TLuaVariantHandler = nil;
                            CdataHandler:TLuaCdataHandler = nil) : Integer;

procedure plua_pushvariant( L : PLua_State; const v : Variant; VariantHandler:TLuaVariantHandler = nil);
procedure plua_pushvariants( L : PLua_State; const v : array of Variant; ReverseOrder:boolean; VariantHandler:TLuaVariantHandler = nil);
function  plua_popvariant( L : PLua_State; CdataHandler:TLuaCdataHandler = nil ):Variant;
procedure plua_pushstrings( L : PLua_State; S : TStrings );
procedure plua_popstrings( L : PLua_State; S : TStrings; Keys:TStrings = nil );
function  plua_popvariants( L : PLua_State; count:integer; ReverseOrder:boolean; CdataHandler:TLuaCdataHandler = nil ):TVariantArray;

//dumps function on top of stack to string (string.dump analog)
{$IFDEF LUAJIT_DUMPX}
const varLuaFunction = CFirstUserType+1; // type id for variant
function plua_toFuncDump( L : PLua_State; Index:Integer; strip: Boolean ):string;
{$ENDIF}
function plua_popFuncDump( L : PLua_State ):string;

//deprecated. Do not use. Use plua_TableToVariant instead
function  plua_TableToVariantArray( L: Plua_State; Index: Integer;
                                    Keys : TStrings = nil) : variant;

procedure pLua_TableGlobalCreate(L : Plua_State; const TableName:string);
function pLua_TableExists(L : Plua_State; const TableName:string):boolean;

procedure plua_PushTable(L: PLua_State; const v:variant; VariantHandler:TLuaVariantHandler = nil);overload;
procedure plua_PushTable(L: PLua_State; const values, keys:array of variant; VariantHandler:TLuaVariantHandler = nil);overload;
function plua_TableToVariant( L: Plua_State; Index: Integer; CdataHandler:TLuaCdataHandler = nil ) : variant;
function plua_tovariant(L: Plua_State; Index: Integer; CdataHandler:TLuaCdataHandler = nil): Variant;

function plua_absindex(L: Plua_State; Index: Integer): integer;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: ansistring; out Line: Integer; out Msg: ansistring);

procedure plua_CopyTable(L: Plua_State; IdxFrom, IdxTo : Integer);

procedure plua_RegisterMethod( l : Plua_State; aMethodName : AnsiString;
                               MethodPtr : lua_CFunction;
                               totable : Integer = LUA_GLOBALSINDEX);overload;
procedure plua_RegisterMethod(l : PLua_State; const aMethodName:string; Func:TLuaProc);overload;
procedure plua_RegisterMethod(l : PLua_State; const aPackage, aMethodName:string; Func:TLuaProc);overload;

//assign metatable for userdata ( it cannot be done directly in Lua )
//must be exported to Lua manually
function plua_helper_setmetatable(l : PLua_State; paramcount: Integer) : integer;

//create a dummy userdata with zero size.
//useful, e.g. for attaching metatable to ordinary Lua tables
//must be exported to Lua manually
function plua_helper_userdata_dummy(l : PLua_State; paramcount: Integer) : integer;

procedure plua_GetTableKey( l : PLua_State; TableIndex : Integer; KeyName : AnsiString );

//parses full function name (with dots) into package + simple name
procedure plua_FuncNameParse(const FuncName:String; out Package, FName:string);

function plua_typename(l : Plua_State; luatype:Integer ):string;

procedure plua_CheckStackBalance(l: PLua_State; TopToBe:Integer; TypeOnTop:integer = LUA_TNONE);

//pops all values from stack until stack is at TopToBe
procedure plua_EnsureStackBalance(l: PLua_State; TopToBe:Integer);

//pops all values from stack
procedure plua_ClearStack(l: PLua_State);

//Balance Lua stack and throw exception
procedure plua_RaiseException(l: PLua_State; const ErrMes:string);overload;
procedure plua_RaiseException(l: PLua_State; const ErrMes:string; const Params:array of const);overload;
procedure plua_RaiseException(l: PLua_State; TopToBe:Integer; const ErrMes:string);overload;
procedure plua_RaiseException(l: PLua_State; TopToBe:Integer; const ErrMes:string; const Params:array of const);overload;

//compiles function text FuncCode and pushes its chunk on stack
function plua_FunctionCompile(l: PLua_State; const FuncCode:string):integer;overload;
//same as above, but substitutes text in FuncCode
function plua_FunctionCompile(l: PLua_State; const FuncCode:string; const Substs:array of const):integer;overload;

//report error from lua called functions
//can't deal properly with Pascal exceptions under x86/32bit platforms!!!
//Use plua_RaiseException instead
procedure lua_reporterror(l : PLua_State; const ErrMes:string);
procedure lua_reporterror(l : PLua_State; const ErrMes:string; const Params:array of const);

procedure VarToStrings(const V:variant; Values:TStrings; Keys:TStrings = nil);

var
  LogFunction             : procedure (const Text:string) = nil;
  DumpStackTraceFunction  : procedure = nil;

procedure Log(const Text:string);inline;overload;
procedure LogFmt(const TextFmt:string; Args:array of const);overload;
procedure Log(const TextFmt:string; Args:array of const);overload;
procedure DumpStackTrace;
procedure LogDebug(const TextFmt:string; Args:array of const);inline;overload;
procedure LogDebug(const Text:string);inline;overload;

//lua stack logging
procedure lua_logstacktypes(const LogPrefix:string; L: PLua_State);
procedure lua_logstack(const LogPrefix: string; L: PLua_State);

implementation

uses
  math;

{$IFDEF LUAJIT_EXCEPTION_SUPPORT}
procedure lua_reporterror(l : PLua_State; const ErrMes:string);
begin
  //LuaJit wants native exceptions, not longjmp!
  raise LuaException.Create(ErrMes);
end;
{$ENDIF}

{$IFNDEF LUAJIT_EXCEPTION_SUPPORT}
{$IMPLICITEXCEPTIONS OFF}
procedure lua_reporterror(l : PLua_State; const ErrMes:string);
begin
  assert(L <> nil, 'Lua state is nil');
  lua_pushstring(L, PChar(ErrMes));
  lua_error(L); //does longjmp, almost the same as exception raising
end;
{$IMPLICITEXCEPTIONS ON}
{$ENDIF}

procedure lua_reporterror(l: PLua_State; const ErrMes: string; const Params: array of const);
begin
  lua_reporterror(l, Format(ErrMes, Params));
end;

function plua_tostring(L: PLua_State; Index: Integer): ansistring;
var
  Size: size_t;
  S:PChar;
begin
  Result:='';
  if not lua_isstring(L, Index) then Exit;

  S := lua_tolstring(L, Index, @Size);
  if S = nil then Exit;

  SetLength(Result, Size);
  if (Size > 0) then
    Move(S^, Pchar(@Result[1])^, Size);
end;

procedure plua_pushstring(L: PLua_State; const AString: AnsiString);
begin
  //do not use lua_pushstring
  //as it does not deal properly with Pascal strings containing zeroes
  lua_pushlstring(l, PChar(AString), Length(AString));
end;

function StrToPChar(const S:string):PChar;
//allocates memory for PChar and copies contents of S, should be freed using StrDispose afterwards
//does not return nil!
begin
  Result:=StrAlloc(Length(S)+1);
  StrPCopy(Result, S);
end;

{$IFDEF LUAJIT_DUMPX}
type

{ TLuaFunctionDump }

// type for dealing with lua function dumps in variants
TLuaFunctionDump = class(TCustomVariantType)
  public
    constructor Create;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
end;

{ TLuaFunctionDump }

constructor TLuaFunctionDump.Create;
begin
  inherited Create(varLuaFunction);
end;

procedure TLuaFunctionDump.Clear(var V: TVarData);
begin
  if V.vType <> varLuaFunction then
    raise Exception.CreateFmt('Only varLuaFunction type is supported. %d (TLuaFunctionDump.Clear)', [V.vType]);
  String(V.vstring):='';
end;

procedure TLuaFunctionDump.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
var s:string;
begin
  s:=String(Source.vstring);
  Dest.vString := nil;
  String(Dest.vstring):=s;
  Dest.vtype:=varLuaFunction;
end;

procedure TLuaFunctionDump.Cast(var Dest: TVarData; const Source: TVarData);
begin
  if Source.vType <> varstring then
    raise Exception.CreateFmt('Cast from %s to varLuaFunction is not supported', [Source.vType]);
  Copy(Dest, Source, true);
end;
{$ENDIF}

procedure plua_RegisterLuaTable(l: PLua_State; Name: AnsiString;
  Reader: lua_CFunction; Writer: lua_CFunction; TableIndex: Integer);
var
  tidx, midx : Integer;
begin
  lua_gettable(l, TableIndex);
  if (lua_type(L, -1) <> LUA_TTABLE) then
    begin
      lua_pushliteral(L, PChar(Name));
      lua_newtable(L);
      tidx := lua_gettop(L);
      
      lua_newtable(L);
      midx := lua_gettop(L);
      
      lua_pushstring(L, '__index');
      lua_pushcfunction(L, Reader);
      lua_rawset(L, midx);
      lua_pushstring(L, '__newindex');
      lua_pushcfunction(L, Writer);
      lua_rawset(L, midx);
      lua_setmetatable(l, tidx);
      lua_settable(l, TableIndex);
    end;
end;

function plua_functionexists(L: PLua_State; FunctionName: AnsiString;
  TableIndex: Integer): boolean;
begin
  plua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  result := lua_isfunction(L, lua_gettop(L));
  if result then
    begin
      result := not lua_iscfunction(L, lua_gettop(L));
      lua_pop(L, 1);
    end;
end;

function plua_callfunction( L: PLua_State; FunctionName : AnsiString;
                            const args : array of Variant;
                            results : PVariantArray;
                            TableIndex : Integer;
                            ErrHandler : Integer;
                            ParamsToPush:TLuaParamPushProc;
                            VariantHandler:TLuaVariantHandler;
                            CdataHandler:TLuaCdataHandler) : Integer;
var
   NArgs, offset,
   i :Integer;
   msg : AnsiString;
begin
  offset := lua_gettop(l);
  plua_pushstring(L, FunctionName);
  lua_rawget(L, TableIndex);
  if lua_isnil(L, -1) then
    raise LuaException.CreateFmt('Function %s not found', [FunctionName]);

  if ParamsToPush <> nil then
    begin
      NArgs:=ParamsToPush(l) - 1;
    end
    else
    begin
      plua_pushvariants(l, args, False, VariantHandler);
      NArgs := High(Args);
    end;
  if lua_pcall(l, NArgs+1, LUA_MULTRET, ErrHandler) <> 0 then
    begin
      msg := plua_tostring(l, -1);
      lua_pop(l, 1);
      raise LuaException.create(msg);
    end;
  result := lua_gettop(l) - offset;
  if (Results<>Nil) then
    begin
      Results^:=plua_popvariants(l, result, True, CdataHandler);
    end;
end;

procedure plua_PushTable(L: PLua_State; const v:variant; VariantHandler:TLuaVariantHandler);
var
  i, h: Integer;
  keys, values : array of variant;
begin
  // lua table to be pushed contains of two elements (see plua_TableToVariant)
  h := VarArrayHighBound(v, 1);
  assert( h = 1, 'Invalid array passed to plua_PushTable' );
  // first containts a variant array of values
  values:=v[0];
  // second containts a variant array of keys
  keys:=v[1];

  plua_PushTable(l, values, keys, VariantHandler);
end;

procedure plua_PushTable(L: PLua_State; const values, keys: array of variant; VariantHandler: TLuaVariantHandler);
var
  i: Integer;
begin
  assert( Length(keys) = Length(values), 'Keys/values passed to plua_PushTable do not match' );

  lua_newtable(L);
  for i := 0 to High(keys) do
    begin
      plua_pushvariant(L, keys[i], VariantHandler);
      plua_pushvariant(L, values[i], VariantHandler);
      lua_settable(L, -3);
    end;
end;

procedure plua_pushvariant(L: PLua_State; const v: Variant; VariantHandler:TLuaVariantHandler);
var c, h, err:Integer;
  {$IFDEF LUAJIT_DUMPX}
  s:string;
  {$ENDIF}
begin
  if (VariantHandler = nil) or    //if variant handler is not defined
     (not VariantHandler(l, v))   //or it could not push the value on stack
    then                          //then fallback to standard push implementation
    case VarType(v) of
      varEmpty,
      varNull    : lua_pushnil(L);

      varBoolean : lua_pushboolean(L, v);

      varStrArg,
      varOleStr,
      varString  : plua_pushstring(L, v);

      varDate    : plua_pushstring(L, DateTimeToStr(VarToDateTime(v)));

      varsmallint,
      varinteger,
      varsingle,
      varint64,   // TODO: LuaJIT supports 64bit integers as cdata
      vardecimal,
      vardouble  :
                   lua_pushnumber(L, Double(VarAsType(v, varDouble)));

      varArray   : begin
                     h := VarArrayHighBound(v, 1);
                     lua_newtable(L);
                     for c := 0 to h do
                       begin
                         lua_pushinteger(L, c+1);
                         plua_pushvariant(L, v[c]);
                         lua_settable(L, -3);
                       end;
                   end;

      vararray + varvariant: //array of variant
                   begin
                     plua_pushtable(l, v, VariantHandler);
                   end;
      {$IFDEF LUAJIT_DUMPX}
      // custom/patched LuaJIT needed
      varLuaFunction:
                   begin
                     s:=String(tvardata(v).vstring);
                     err:=luaL_loadbuffer(l, PChar(s), Length(s), 'luaFunction');
                     if err <> 0 then
                       raise LuaException.CreateFmt('Error loading lua function %d (plua_pushvariant)', [err]);
                   end;
      {$ENDIF}
      else
        raise LuaException.CreateFmt('Unsupported type (%d) passed to plua_pushvariant', [VarType(v)]);
    end;
end;

procedure plua_pushvariants(L: PLua_State; const v: array of Variant; ReverseOrder:boolean; VariantHandler:TLuaVariantHandler);
var i:Integer;
begin
  if ReverseOrder then
    for i:=High(v) downto 0 do
      plua_pushvariant(l, v[i], VariantHandler)
  else
    for i:=0 to High(v) do
      plua_pushvariant(l, v[i], VariantHandler);
end;

function plua_popvariant(L: PLua_State; CdataHandler:TLuaCdataHandler): Variant;
begin
  Result:=plua_tovariant(l, -1, CdataHandler);

  //remove value from stack
  lua_pop(l, 1);
end;

procedure plua_pushstrings(L: PLua_State; S: TStrings);
var n:Integer;
begin
  lua_newtable(L);
  for n := 0 to S.Count-1 do
    begin
      lua_pushinteger(L, n+1);
      plua_pushstring(L, S.Strings[n]);
      lua_settable(L, -3);
    end;
end;

procedure plua_popstrings(L: PLua_State; S: TStrings; Keys: TStrings);
var idx:integer;
    Val:string;
begin
  if Keys <> nil then
    Keys.Clear;

  S.Clear;
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
         Val:= plua_tostring(L, -1);
         if Val <> '' then
           begin
             S.Add( Val );
             if Keys <> nil then
               Keys.Add( plua_tostring(L, -2) );
           end;

         lua_pop(L, 1);  // removes value; keeps key for next iteration
      end;
    end;
end;

function plua_popvariants(L: PLua_State; count: integer; ReverseOrder:boolean; CdataHandler:TLuaCdataHandler): TVariantArray;
//pops 'count' elements from stack into TVariantArray
//reverses element order
//supports count = 0
var i:Integer;
begin
  SetLength(Result, count);
  if ReverseOrder then
    for i:=0 to count-1 do
      begin
        Result[count - 1 - i]:=plua_popvariant(L, CdataHandler);
      end
    else
    for i:=0 to count-1 do
      begin
        Result[i]:=plua_popvariant(L, CdataHandler);
      end;
end;

type
  TStrWrite = record
     s:String;
     real_len:NativeInt;
  end;
  PStrWrite = ^TStrWrite;
function StrWriter(L : Plua_State; p : Pointer; sz : size_t; ud : Pointer) : Integer; extdecl;
var s:PStrWrite;
begin
  s:=PStrWrite(ud);
  if s^.real_len + sz > Length(s^.s) then
    SetLength(s^.s, max(2*Length(s^.s), s^.real_len + sz));
  Move(p^, (@s^.s[s^.real_len+1])^, sz);
  Inc(s^.real_len, sz);
  Result:=0;
end;

{$IFDEF LUAJIT_DUMPX}
function plua_toFuncDump( L : PLua_State; Index:Integer; strip:boolean ):string;
//dumps function internal representation to string
//string.dump analog
var err:Integer;
    s:TStrWrite;
begin
  if lua_type(l, Index) <> LUA_TFUNCTION then
    raise LuaException.Create('plua_FuncDump requires function to dump');

  s.s:='';
  s.real_len:=0;
  err:=lua_dumpx(L, Index, @StrWriter, @s, Integer(strip));
  if err <> 0 then
    raise LuaException.CreateFmt('dumpx failed. Error %s', [err]);

  SetLength(s.s, s.real_len);
  Result:=s.s;
end;
{$ENDIF}

function plua_popFuncDump(L: PLua_State): string;
//dumps function internal representation to string
//string.dump analog
var StackTop:Integer;
    nargs:integer;
begin
  StackTop:=lua_gettop(l);
  try
    if lua_type(l, -1) <> LUA_TFUNCTION then
      raise LuaException.Create('plua_popFuncDump requires function on stack top');

    lua_getglobal(l, 'string');
    lua_getfield(l, -1, 'dump');

    if lua_isnil(l, -1) then
      raise LuaException.Create('plua_popFuncDump string.dump not found');

    //remove 'string' global table
    lua_remove(l, lua_gettop(l) - 1);

    //move string.dump function before function on stack
    lua_insert(l, lua_gettop(l) - 1);

    nargs:=1;
    {$IFDEF LUAJIT}
    //LuaJIT has an additional parameter to drop debug information
    lua_pushboolean(l, true);
    Inc(nargs);
    {$ENDIF}

    if lua_pcall(l, nargs, 1, 0) <> 0 then
      raise LuaException.Create('plua_popFuncDump string.dump error');

    Result:=plua_tostring(l, -1);
    lua_pop(l, 1);
  finally
    plua_EnsureStackBalance(l, StackTop);
  end;
end;

function  plua_TableToVariantArray( L: Plua_State; Index: Integer;
                                    Keys : TStrings = nil) : variant;
var
  cnt : Integer;
  va : array of Variant;
begin
  Index := plua_absindex(L, Index);
  if Assigned(Keys) then
    Keys.Clear;

  lua_pushnil(L);
  cnt := 0;
  while (lua_next(L, Index) <> 0) do
    begin
      SetLength(va, cnt+1);
      if assigned(Keys) then
        Keys.Add(plua_tostring(L, -2));
      va[cnt] := plua_tovariant(l, -1);
      lua_pop(L, 1);
      inc(cnt);
    end;

  if cnt > 0 then
    begin
      result := VarArrayCreate([0,cnt-1], varvariant);
      while cnt > 0 do
        begin
          dec(cnt);
          result[cnt] := va[cnt];
        end;
    end
  else
    result := VarArrayCreate([0,0], varvariant);
end;

procedure VarToStrings(const V:variant; Values:TStrings; Keys:TStrings);
var vkeys, vvalues : Variant;
    h, n:Integer;
begin
  Values.Clear;
  if Keys <> nil then
    Keys.Clear;

  //see plua_TableToVariant for details
  if VarArrayDimCount(V) <> 1 then
    raise LuaException.Create('Invalid array passed to VarToStrings');

  h:=VarArrayHighBound(V, 1);
  if h <> 1 then
    raise LuaException.Create('Invalid array passed to VarToStrings');

  vvalues:=V[0];
  vkeys:=V[1];
  h:=VarArrayHighBound(vvalues, 1);
  for n:=0 to h do
    begin
      Values.Add( vvalues[n] );
      if Keys <> nil then
        Keys.Add( vkeys[n] );
    end;
end;

procedure pLua_TableGlobalCreate(L: Plua_State; const TableName: string);
begin
  plua_pushstring(l, TableName);
  lua_newtable(l);
  lua_settable(l, LUA_GLOBALSINDEX);
end;

function pLua_TableExists(L: Plua_State; const TableName: string): boolean;
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

function  plua_TableToVariant( L: Plua_State; Index: Integer; CdataHandler:TLuaCdataHandler ) : variant;
// gets Lua table recursively
// table are returned variant of two elements.
// values in first subarray and keys in second subarray

function VariantArrayToVarArray(const A:array of variant; realcount:Integer):Variant;
var i:Integer;
begin
  result := VarArrayCreate([0,realcount-1], varvariant);
  for i:=0 to realcount-1 do
    begin
      result[i] := A[i];
    end;
end;

var
  i , realcount: Integer;
  keys, values : array of Variant;
begin
  Index := plua_absindex(L, Index);

  realcount:=0;
  SetLength(keys, 10);
  SetLength(values, 10);

  lua_pushnil(L);
  i := 0;
  while (lua_next(L, Index) <> 0) do
    begin
      Inc(realcount);
      if realcount > Length(keys) then
        begin
          SetLength(keys, realcount + 10);
          SetLength(values, realcount + 10);
        end;

      keys[i]  :=plua_tovariant(L, -2, CdataHandler);
      values[i]:=plua_tovariant(L, -1, CdataHandler);   // recursive call is here (tables inside tables)

      lua_pop(L, 1);
      inc(i);
    end;

  //pack Lua table into two-element variant array
  result := VarArrayCreate([0,1], varvariant);
  result[0] := VariantArrayToVarArray(values, realcount);
  result[1] := VariantArrayToVarArray(keys, realcount);
end;

function plua_tovariant(L: Plua_State; Index: Integer; CdataHandler:TLuaCdataHandler): Variant;
Var
  dataType :Integer;
  dataNum  :Double;
  {$IFDEF LUAJIT}
  p        :Pointer;
  ctypeid  :LuaJIT_CTypeID;
  {$ENDIF}
  {$IFDEF LUAJIT_DUMPX}
  s:String;
  {$ENDIF}
begin
  dataType :=lua_type(L, Index);
  case dataType of
    LUA_TSTRING          : Result := VarAsType(plua_tostring(L, Index), varString);
    LUA_TUSERDATA,
    LUA_TLIGHTUSERDATA   : Result := VarAsType(PtrInt(lua_touserdata(L, Index)), varInteger);
    LUA_TNONE,
    LUA_TNIL             : Result := Null;
    LUA_TBOOLEAN         : Result := VarAsType(lua_toboolean(L, Index), varBoolean);
    LUA_TNUMBER          : begin
                             dataNum :=lua_tonumber(L, Index);
                             if (Abs(dataNum)>MAXINT) then
                               Result :=VarAsType(dataNum, varDouble)
                             else
                               begin
                                 if (Frac(dataNum)<>0) then
                                   Result :=VarAsType(dataNum, varDouble)
                                 else
                                   Result :=VarAsType(Trunc(dataNum), varInteger);
                               end;
                           end;
    //LUA_TTABLE           : result := plua_TableToVariantArray(L, Index);
    LUA_TTABLE           : result := plua_TableToVariant(L, Index, CdataHandler);
    {$IFDEF LUAJIT_DUMPX}
    // custom/patched LuaJIT allows to dump any value on stack vs. only the top one in standard Lua
    LUA_TFUNCTION        : begin
                             s:=plua_toFuncDump(L, Index, true);
                             result := VarAsType(s, varLuaFunction);
                           end;
    {$ENDIF}

    {$IFDEF LUAJIT}
    LUA_TCDATA:
      begin
        p := luajit_tocdata(l, Index, ctypeid);
        if p = nil then
           raise LuaException.Create('Cannot pop nil cdata from stack. plua_tovariant');

        // check for 64bit number cdata
        if ctypeid = LuaJIT_CTYPEDID_INT64 then
          Result:=PInt64(p)^
        else
        begin
          if CdataHandler = nil then
             raise LuaException.Create('Cannot pop cdata from stack. plua_tovariant')
             else
             Result:=CdataHandler( p );
        end;
      end;
    {$ENDIF}
  else
    result := NULL;
  end;
end;

function plua_absindex(L: Plua_State; Index: Integer): integer;
begin
  if (index > -1) or ((index = LUA_GLOBALSINDEX) or (index = LUA_REGISTRYINDEX)) then
    result := index
  else
    result := index + lua_gettop(L) + 1
end;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: ansistring; out Line: Integer; out Msg: ansistring);
const
  Term = #$00;
  function S(Index: Integer): Char;
  begin
    if (Index <= Length(ErrMsg)) then
      Result := ErrMsg[Index]
    else
      Result := Term;
  end;
  function IsDigit(C: Char): Boolean;
  begin
    Result := ('0' <= C) and (C <= '9');
  end;
  function PP(var Index: Integer): Integer;
  begin
    Inc(Index);
    Result := Index;
  end;
var
  I, Start, Stop: Integer;
  LS: string;
  Find: Boolean;
begin
  Title := '';
  Line := 0;
  Msg := ErrMsg;
  Find := False;
  I := 1 - 1;
  Stop := 0;
  repeat
    while (S(PP(I)) <> ':') do
      if (S(I) = Term) then
        Exit;
    Start := I;
    if (not IsDigit(S(PP(I)))) then
      Continue;
    while (IsDigit(S(PP(I)))) do
      if (S(I - 1) = Term) then
        Exit;
    Stop := I;
    if (S(I) = ':') then
      Find := True;
  until (Find);
  Title := Copy(ErrMsg, 1, Start - 1);
  LS := Copy(ErrMsg, Start + 1, Stop - Start - 1);
  Line := StrToIntDef(LS, 0);
  Msg := Copy(ErrMsg, Stop + 1, Length(ErrMsg));
end;

procedure plua_CopyTable(L: Plua_State; IdxFrom, IdxTo: Integer);
var
  id:Integer;
  key : AnsiString;
  cf : lua_CFunction;
begin
  lua_pushnil(L);
  while(lua_next(L, IdxFrom)<>0)do
    begin
      key := plua_tostring(L, -2);
      case lua_type(L, -1) of
        LUA_TTABLE    : begin
          id := lua_gettop(L);
          plua_CopyTable(L, id, IdxTo);
        end;
      else
        lua_pushliteral(l, PChar(key));
        lua_pushvalue(l, -2);
        lua_rawset(L, IdxTo);
      end;
      lua_pop(L, 1);
    end;
end;

procedure plua_RegisterMethod(l: Plua_State; aMethodName: AnsiString;
  MethodPtr: lua_CFunction; totable : Integer);
begin
  lua_pushliteral(l, PChar(aMethodName));
  lua_pushcfunction(l, MethodPtr);
  lua_settable(l, totable);
end;

function plua_helper_setmetatable(l: PLua_State; paramcount: Integer): integer;
begin
  result := 0;
  if (paramcount <> 2) then
    plua_RaiseException(l, 'Parameter number must be 2 (plua_helper_setmetatable)')
    else
    begin
      // parameter order is the same as for setmetatable of Lua - (x, mt)
      if lua_type(l, -1) <> LUA_TTABLE then
        plua_RaiseException(l, 'Parameter #2 must be metatable (plua_helper_setmetatable)');
      if lua_type(l, -2) <> LUA_TUSERDATA then
        plua_RaiseException(l, 'Parameter #1 must be userdata (plua_helper_setmetatable)');

      lua_setmetatable(l, -2);

      //remove x from stack
      lua_pop(l, 1);
    end;
end;

function plua_helper_userdata_dummy(l: PLua_State; paramcount: Integer): integer;
begin
  result := 0;
  if (paramcount <> 0) then
    plua_RaiseException(l, 'Parameter number must be 0 (plua_helper_userdata_dummy)')
    else
    begin
      lua_newuserdata(l, 0);
      result := 1;
    end;
end;

procedure plua_GetTableKey(l: PLua_State; TableIndex: Integer;
  KeyName: AnsiString);
begin
  TableIndex := plua_absindex(l, TableIndex);
  plua_pushstring(l, KeyName);
  lua_gettable(l, TableIndex);
end;

procedure plua_FuncNameParse(const FuncName: String; out Package, FName: string);
var n:Integer;
begin
  n:=Pos('.', FuncName);
  if n = 0 then
    begin
      Package:='';
      FName:=FuncName;
    end
    else
    begin
      Package:=Copy(FuncName, 1, n-1);
      FName  :=Copy(FuncName, n+1, Length(FuncName) - n);
    end;
end;

function plua_typename(l: Plua_State; luatype: Integer): string;
begin
  Result:=String( lua_typename(l, luatype) );
end;

procedure plua_CheckStackBalance(l: PLua_State; TopToBe:Integer; TypeOnTop:integer = LUA_TNONE);
var CurStack:Integer;
    ActualTypeOnTop:Integer;
begin
  CurStack:=lua_gettop(l);
  if CurStack <> TopToBe then
    raise Exception.CreateFmt('Lua stack is unbalanced. %d, should be %d', [CurStack, TopToBe]);

  if (TypeOnTop <> LUA_TNONE) then
    begin
      ActualTypeOnTop:=lua_type(l, -1);
      if ActualTypeOnTop <> TypeOnTop then
        raise Exception.CreateFmt('Wrong type pushed (%d)', [ActualTypeOnTop]);
    end;
end;

procedure plua_EnsureStackBalance(l: PLua_State; TopToBe: Integer);
begin
  while lua_gettop(l) > TopToBe do
  begin
    lua_pop(l, 1);
  end;
end;

procedure plua_ClearStack(l: PLua_State);
var
  curtop : Integer;
begin
  curtop:=lua_gettop(l);
  if curtop > 0 then
    lua_pop(l, curtop); //remove all stack values.
end;

procedure plua_RaiseException(l: PLua_State; const ErrMes: string);
begin
  raise LuaException.Create(ErrMes);
  //lua_reporterror(l, ErrMes);
end;

procedure plua_RaiseException(l: PLua_State; const ErrMes: string; const Params: array of const);
begin
  plua_RaiseException(l, Format(ErrMes, Params));
end;

procedure plua_RaiseException(l: PLua_State; TopToBe: Integer; const ErrMes: string);
begin
  if l <> nil then plua_EnsureStackBalance(l, TopToBe);
  plua_RaiseException(l, ErrMes);
end;

procedure plua_RaiseException(l: PLua_State; TopToBe: Integer; const ErrMes: string;
  const Params: array of const);
begin
  plua_RaiseException(l, TopToBe, Format(ErrMes, Params));
end;

function plua_FunctionCompile(l: PLua_State; const FuncCode: string): integer;
var S:string;
begin
  S:=Format('return (%s)(...)', [FuncCode]);
  Result:=luaL_loadstring(l, PChar(S));
end;

function plua_FunctionCompile(l: PLua_State; const FuncCode: string; const Substs: array of const): integer;
var S, StrFrom, StrTo:string;
    i:Integer;
begin
  S:=FuncCode;

  if Length(Substs) mod 2 <> 0 then
    begin
      //function expects pairs of values to replace
      Result:=LUA_ERRERR;
      Exit;
    end;

  i:=0;
  while i < Length(Substs) do
  begin
    //read string to substitute
    with Substs[i] do
      case VType of
        vtString:     StrFrom:=VString^;
        vtAnsiString: StrFrom:=AnsiString(VAnsiString);
        else
          begin
            Result:=LUA_ERRERR;
            Exit;
          end;
      end;

    //read value to substitute
    with Substs[i+1] do
      case VType of
        vtString:     StrTo:=VString^;
        vtAnsiString: StrTo:=AnsiString(VAnsiString);
        vtInt64:      StrTo:=IntToStr(VInt64^);
        vtInteger:    StrTo:=IntToStr(VInteger);
        else
          begin
            Result:=LUA_ERRERR;
            Exit;
          end;
      end;

    S:=StringReplace(S, StrFrom, StrTo, [rfReplaceAll]);

    //move to next pair
    Inc(i, 2);
  end;

  Result:=plua_FunctionCompile(l, S);
end;

procedure Log(const Text:string);inline;
begin
  //if log handler assigned, then logging
  if @LogFunction <> nil then
    LogFunction( Text );
end;

procedure LogFmt(const TextFmt:string; Args:array of const);
begin
  Log( Format(TextFmt, Args) );
end;

procedure Log(const TextFmt:string; Args:array of const);
begin
  LogFmt( TextFmt, Args );
end;

procedure DumpStackTrace;
begin
  if @DumpStackTraceFunction <> nil then
    DumpStackTraceFunction;
end;

procedure LogDebug(const TextFmt:string; Args:array of const);inline;
begin
  {$IFDEF DEBUG_LUA}
  LogFmt( TextFmt, Args );
  {$ENDIF}
end;

procedure LogDebug(const Text:string);inline;
begin
  {$IFDEF DEBUG_LUA}
  Log( Text );
  {$ENDIF}
end;

procedure lua_logstacktypes(const LogPrefix:string; L: PLua_State);
var n:Integer;
begin
  for n:=1 to lua_gettop(l) do
    begin
      Log(Format('%s [%d] - %s', [LogPrefix, n, plua_typename(l, lua_type(l, n))]) );
    end;
end;

procedure lua_logstack(const LogPrefix: string; L: PLua_State);
var n:Integer;
    val:string;
    luat:Integer;
begin
  Log(Format('%s top=%d', [LogPrefix, lua_gettop(l)]) );
  for n:=1 to lua_gettop(l) do
    begin
      luat:=lua_type(l, n);
      case luat of
         LUA_TNIL:       val:='nil';
         LUA_TBOOLEAN:   val:=BoolToStr(lua_toboolean(l, n), true);
         LUA_TNUMBER:    val:=FloatToStr(lua_tonumber(l, n));
         LUA_TSTRING:    val:=lua_tostring(l, n);
       else
         val:='()';
      end;

      Log(Format('%s [%d] - value:%s, type:%s', [LogPrefix, n, val, plua_typename(l, luat)]) );
    end;
end;

function plua_call_method_act(l : PLua_State) : integer;
var
  method : TLuaProc;
  pcount : Integer;
begin
  result := 0;

  pcount := lua_gettop(l);
  method := TLuaProc(lua_topointer(l, lua_upvalueindex(1)));

  if assigned(method) then
    result := method(l, pcount);
end;

// exception support
const pLuaExceptActual:TLuaNakedProc = @plua_call_method_act;
{$I pLuaExceptWrapper.inc}

procedure plua_RegisterMethod(l : PLua_State; const aMethodName:string; Func:TLuaProc);
begin
  plua_pushstring(L, aMethodName);
  lua_pushlightuserdata(l, Pointer(Func));
  lua_pushcclosure(L, @plua_call_method, 1);
  lua_rawset(l, LUA_GLOBALSINDEX);
end;

procedure plua_RegisterMethod(l : PLua_State; const aPackage, aMethodName:string; Func:TLuaProc);
var StartTop:Integer;
begin
  StartTop:=lua_gettop(L);
  try
    if not plua_TableExists(l, aPackage) then
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

{$IFDEF LUAJIT_DUMPX}
var D:TLuaFunctionDump;
initialization
  D:=TLuaFunctionDump.Create;
finalization
  FreeAndNil(D);
{$ENDIF}

end.

