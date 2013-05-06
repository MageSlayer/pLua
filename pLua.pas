unit pLua;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$ENDIF}

{$I pLua.inc}

interface

uses
  SysUtils, Classes, lua;

type
  TVariantArray =array of Variant;
  PVariantArray =^TVariantArray;
  TObjArray = array of TObject;

  LuaException = class(Exception)
  end;

  //function type which supports native Pascal exception handling
  TLuaProc = function (l : PLua_State; paramcount: Integer) : integer;
  TLuaNakedProc = function (l : PLua_State) : integer;

{$IFDEF LUA_LPEG} // as it links statically, not everybody can need it.
const
  {$IFDEF WIN32}
    LpegLib = 'lpeg.dll'
  {$ENDIF}
  {$IFDEF UNIX}
    LpegLib = 'liblpeg.so'
  {$ENDIF}
  ;
//register Lpeg in Lua instance
function luaopen_lpeg (L: PLua_State):Integer;cdecl;external LpegLib;
{$ENDIF}

function  plua_tostring(L: PLua_State; Index: Integer): ansistring;
procedure plua_pushstring(L: PLua_State; AString : AnsiString);
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
                            const args : Array of Variant;
                            results : PVariantArray = nil;
                            TableIndex : Integer = LUA_GLOBALSINDEX;
                            ParamsToPush:TLuaParamPushProc = nil) : Integer;

procedure plua_pushvariant( L : PLua_State; v : Variant);
procedure plua_pushstrings( L : PLua_State; S : TStrings );

function  plua_TableToVariantArray( L: Plua_State; Index: Integer;
                                    Keys : TStrings = nil) : variant;

procedure pLua_TableGlobalCreate(L : Plua_State; const TableName:string);
function pLua_TableExists(L : Plua_State; const TableName:string):boolean;

function plua_tovariant(L: Plua_State; Index: Integer): Variant;

function plua_absindex(L: Plua_State; Index: Integer): integer;

procedure plua_spliterrormessage(const ErrMsg: string; out Title: ansistring; out Line: Integer; out Msg: ansistring);

procedure plua_CopyTable(L: Plua_State; IdxFrom, IdxTo : Integer);

procedure plua_RegisterMethod( l : Plua_State; aMethodName : AnsiString;
                               MethodPtr : lua_CFunction;
                               totable : Integer = LUA_GLOBALSINDEX);overload;
procedure plua_RegisterMethod(l : PLua_State; const aMethodName:string; Func:TLuaProc);overload;
procedure plua_RegisterMethod(l : PLua_State; const aPackage, aMethodName:string; Func:TLuaProc);overload;

procedure plua_GetTableKey( l : PLua_State; TableIndex : Integer; KeyName : AnsiString );

function plua_typename(l : Plua_State; luatype:Integer ):string;

procedure plua_CheckStackBalance(l: PLua_State; TopToBe:Integer; TypeOnTop:integer = LUA_TNONE);

//pops all values from stack until stack is at TopToBe
procedure plua_EnsureStackBalance(l: PLua_State; TopToBe:Integer);

//pops all values from stack
procedure plua_ClearStack(l: PLua_State);

//Balance Lua stacka and throw exception
procedure plua_RaiseException(l: PLua_State; const ErrMes:string);overload;
procedure plua_RaiseException(l: PLua_State; const ErrMes:string; const Params:array of const);overload;
procedure plua_RaiseException(l: PLua_State; TopToBe:Integer; const ErrMes:string);overload;
procedure plua_RaiseException(l: PLua_State; TopToBe:Integer; const ErrMes:string; const Params:array of const);overload;

//compiles function text FuncCode and pushes its chunk on stack
function plua_FunctionCompile(l: PLua_State; const FuncCode:string):integer;overload;
//same as above, but substitutes text in FuncCode
function plua_FunctionCompile(l: PLua_State; const FuncCode:string; const Substs:array of const):integer;overload;

//report error from lua called functions
procedure lua_reporterror(l : PLua_State; const ErrMes:string);

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
  Variants;

procedure lua_reporterror(l : PLua_State; const ErrMes:string);
begin
  {$IFDEF LUAJIT_EXCEPTION_SUPPORT}
  //LuaJit wants native exceptions, not longjmp!
  raise LuaException.Create(ErrMes);
  {$ELSE}
  assert(L <> nil, 'Lua state is nil');
  lua_pushstring(L, PChar(ErrMes));
  lua_error(L); //does longjmp, almost the same as exception raising
  {$ENDIF}
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

procedure plua_pushstring(L: PLua_State; AString: AnsiString);
begin
  lua_pushstring(l, pchar(AString));
end;

function StrToPChar(const S:string):PChar;
//allocates memory for PChar and copies contents of S, should be freed using StrDispose afterwards
//does not return nil!
begin
  Result:=StrAlloc(Length(S)+1);
  StrPCopy(Result, S);
end;

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
                            const args : Array of Variant;
                            results : PVariantArray;
                            TableIndex : Integer;
                            ParamsToPush:TLuaParamPushProc) : Integer;
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

  if @ParamsToPush <> nil then
    begin
      NArgs:=ParamsToPush(l) - 1;
    end
    else
    begin
      NArgs := High(Args);
      for i:=0 to NArgs do
        plua_pushvariant(l, args[i]);
    end;
  if lua_pcall(l, NArgs+1, LUA_MULTRET, 0) <> 0 then
    begin
      msg := plua_tostring(l, -1);
      lua_pop(l, 1);
      raise LuaException.create(msg);
    end;
  result := lua_gettop(l) - offset;
  if (Results<>Nil) then
    begin
      SetLength(Results^, Result);
      for i:=0 to Result-1 do
        Results^[Result-i-1] := plua_tovariant(L, -(i+1));
    end;
end;

procedure plua_pushvariant(L: PLua_State; v: Variant);
var
  h, c : Integer;
begin
  case VarType(v) of
    varEmpty,
    varNull    : lua_pushnil(L);
    varBoolean : lua_pushboolean(L, v);
    varStrArg,
    varOleStr,
    varString  : plua_pushstring(L, v);
    varDate    : plua_pushstring(L, DateTimeToStr(VarToDateTime(v)));
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
  else
    lua_pushnumber(L, Double(VarAsType(v, varDouble)));
  end;
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

function plua_tovariant(L: Plua_State; Index: Integer): Variant;
Var
  dataType :Integer;
  dataNum  :Double;
begin
  dataType :=lua_type(L, Index);
  case dataType of
    LUA_TSTRING          : Result := VarAsType(plua_tostring(L, Index), varString);
    LUA_TUSERDATA,
    LUA_TLIGHTUSERDATA   : Result := VarAsType(PtrInt(lua_touserdata(L, Index)), varInteger);
    LUA_TNONE,
    LUA_TNIL             : Result := varNull;
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
                                   Result :=VarAsType(Trunc(dataNum), varDouble);
                               end;
                           end;
    LUA_TTABLE           : result := plua_TableToVariantArray(L, Index);
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

procedure plua_GetTableKey(l: PLua_State; TableIndex: Integer;
  KeyName: AnsiString);
begin
  TableIndex := plua_absindex(l, TableIndex);
  plua_pushstring(l, KeyName);
  lua_gettable(l, TableIndex);
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
  //raise LuaException.Create(ErrMes);
  lua_reporterror(l, ErrMes);
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

end.

