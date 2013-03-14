unit pLuaUtils;

{$mode objfpc}{$H+}

{$I pLua.inc}

interface
uses Classes, SysUtils,
     Lua, pLua;

//fs namespace support
procedure plua_fs_register(L:Plua_State);

//dbg namespace support
procedure plua_dbg_register(L:Plua_State);

implementation
uses Forms, gstack, pLuaObject;

const
  Package_fs = 'fs';
  Package_dbg = 'dbg';

const
  AllMask = {$IFDEF WINDOWS}'*.*'{$ELSE}'*'{$ENDIF};

type
  TFindFilesIteratorParams = record
    Dir, WildCard:string;
    Recursive:boolean
  end;

  { TFindFilesIterator }

  TFindFilesIteratorState = (stInit, stFindDirectoriesLoop, stFindDirectoriesNext, stFindFiles, stFindFilesLoop, stFindFilesNext, stComplete);
  TFindFilesIteratorStackFrame = record
    SR:TSearchRec;
    Dir:string;
    State:TFindFilesIteratorState;
  end;
  PFindFilesIteratorStackFrame = ^TFindFilesIteratorStackFrame;
  TFindFilesIteratorStack = specialize TStack<PFindFilesIteratorStackFrame>;

  TFindFilesIterator = class
    private
      FParams:TFindFilesIteratorParams;
      FStack:TFindFilesIteratorStack;

      procedure Clear;
      function ProcessDir: string;
      procedure PushFrame(const Dir:string);
      procedure PopFrame(Force: boolean=False);
    public
      constructor Create(const Dir, WildCard:string; Recursive:boolean);
      destructor Destroy;override;

      function FetchNext:string;
  end;

procedure FindFilesToList(const Dir, WildCard:string; Recursive:boolean; L:TStrings);
var Iter:TFindFilesIterator;
    f:string;
begin
  L.Clear;
  Iter:=TFindFilesIterator.Create(Dir, WildCard, Recursive);
  try
    while true do
    begin
      f:=Iter.FetchNext;
      if f = '' then break;
      L.Add(f);
    end;
  finally
    Iter.Free;
  end;
end;

procedure FindFilesArgs(l : Plua_State; paramcount: Integer;
                        out Dir:string; out Recursive:boolean; out Mask:string );
begin
  if not (paramcount in [2,3]) then
     lua_reporterror(l, 'Dir, Recursive, [Mask] are expected.');

  if paramcount < 3 then
    begin
      Mask:=AllMask;
    end
    else
    begin
      Mask:=plua_tostring(l, -1);
      lua_pop(l, 1);
    end;
  Recursive:=lua_toboolean(l, -1);
  lua_pop(l, 1);

  Dir:=plua_tostring(l, -1);
  lua_pop(l, 1);
end;

function plua_findfiles(l : PLua_State; paramcount: Integer) : integer;
var S:TStringList;
    Recursive:boolean;
    Dir, Mask:string;
begin
  Result:=0;
  FindFilesArgs(l, paramcount, Dir, Recursive, Mask);

  S:=TStringList.Create;
  try
    FindFilesToList(Dir, Mask, Recursive, S);

    plua_pushstrings(l, S);
    Result:=1;
  finally
    S.Free;
  end;
end;

function plua_findfiles_iterator(l : PLua_State) : integer; extdecl;
var paramcount:Integer;
    user_obj:^TObject;
    f:String;
begin
  paramcount:=lua_gettop(l);
  if paramcount <> 2 then
    lua_reporterror(l, 'Invalid findfiles_iterator params');

  //second param is not used actually
  lua_pop(l, 1);

  if not lua_isuserdata(l, -1) then
    lua_reporterror(l, 'Invalid findfiles_iterator param1');

  user_obj:=lua_touserdata(l, -1);
  f:=TFindFilesIterator(user_obj^).FetchNext;

  if f = '' then
    lua_pushnil(l)
    else
    plua_pushstring(l, f);

  Result:=1;
end;

function plua_iterfiles(l : PLua_State; paramcount: Integer) : integer;
var Iter:TFindFilesIterator;
    f:string;
    Recursive:boolean;
    Dir, Mask:string;
begin
  Result:=0;
  FindFilesArgs(l, paramcount, Dir, Recursive, Mask);

  Iter:=TFindFilesIterator.Create(Dir, Mask, Recursive);
  f:=Iter.FetchNext;

  lua_pushcfunction(l, @plua_findfiles_iterator);
  plua_PushObjectAsUserData(l, Iter);

  if f = '' then
      lua_pushnil(l)
    else
      plua_pushstring(l, f);

  Result:=3;
end;

function plua_process_messages(l : PLua_State; paramcount: Integer) : integer;
begin
  Application.ProcessMessages;
  plua_EnsureStackBalance(l, 0);
  Result:=0;
end;

procedure plua_fs_register(L: Plua_State);
begin
  plua_RegisterMethod(l, Package_fs, 'findfiles', @plua_findfiles);
  plua_RegisterMethod(l, Package_fs, 'iterfiles', @plua_iterfiles);
end;

procedure plua_dbg_register(L: Plua_State);
begin
  plua_RegisterMethod(l, Package_dbg, 'ProcessMessages', @plua_process_messages);
end;

{ TFindFilesIterator }

procedure TFindFilesIterator.Clear;
begin
  while not FStack.IsEmpty do
  begin
    PopFrame;
  end;
end;

constructor TFindFilesIterator.Create(const Dir, WildCard: string;
  Recursive: boolean);
var F:TFindFilesIteratorStackFrame;
begin
  FStack:=TFindFilesIteratorStack.Create;

  FParams.Dir:=Dir;
  FParams.WildCard:=WildCard;
  FParams.Recursive:=Recursive;

  PushFrame(Dir);
end;

destructor TFindFilesIterator.Destroy;
begin
  Clear;
  FStack.Free;
  inherited Destroy;
end;

procedure TFindFilesIterator.PushFrame(const Dir: string);
var F:PFindFilesIteratorStackFrame;
begin
  New(F);
  F^.Dir:=Dir;
  F^.State:=stInit;
  FillByte(F^.SR, SizeOf(F^.SR), 0);
  FStack.Push(F);
end;

procedure TFindFilesIterator.PopFrame(Force:boolean=False);
var Frame:PFindFilesIteratorStackFrame;
begin
  Frame:=FStack.Top();

  if Force or not (Frame^.State in [stInit, stComplete]) then
    FindClose(Frame^.SR);

  Dispose(Frame);
  FStack.Pop();
end;

function TFindFilesIterator.ProcessDir:string;
label
  NextIter;
var Found:Integer;
    Frame:PFindFilesIteratorStackFrame;
begin

NextIter:
  Result:='';
  Frame:=FStack.Top();

  while true do
  case Frame^.State of
    stInit:
      begin
        //first pass - directories
        Frame^.State:=stFindFiles;
        if FParams.Recursive then
          begin
            Found:=FindFirst(Frame^.Dir + DirectorySeparator + AllMask, faDirectory, Frame^.SR);
            if Found = 0 then
              Frame^.State:=stFindDirectoriesLoop;
          end;
      end;

    stFindDirectoriesLoop:
      begin
        Frame^.State:=stFindDirectoriesNext;
        with Frame^.SR do
        begin
          if (Name <> '.') and (Name <> '..') then
            if (Attr and faDirectory) > 0 then
              begin
                PushFrame(Frame^.Dir + DirectorySeparator + Name);
                goto NextIter;
              end;
        end;
      end;

    stFindDirectoriesNext:
      begin
        Found:=FindNext(Frame^.SR);
        if Found = 0 then
          Frame^.State:=stFindDirectoriesLoop
          else
          begin
            FindClose(Frame^.SR);
            Frame^.State:=stFindFiles;
          end;
      end;

    stFindFiles:
      begin
        //second pass - files
        Found:=FindFirst(Frame^.Dir + DirectorySeparator + FParams.WildCard, faAnyFile, Frame^.SR);
        if Found = 0 then
          Frame^.State:=stFindFilesLoop
          else
          Frame^.State:=stComplete;
      end;

    stFindFilesLoop:
      begin
        Frame^.State:=stFindFilesNext;

        with Frame^.SR do
        begin
          if (Name <> '.') and (Name <> '..') then
            if (Attr and faDirectory) = 0 then
              begin
                Result:=Frame^.Dir + DirectorySeparator + Name;
                break;
              end;
        end;
      end;

    stFindFilesNext:
      begin
        Found:=FindNext(Frame^.SR);
        if Found = 0 then
          Frame^.State:=stFindFilesLoop
          else
          Frame^.State:=stComplete;
      end;

    stComplete:
      begin
        PopFrame(True);
        if FStack.IsEmpty() then
          break;
        goto NextIter;
      end;
   end;
end;

function TFindFilesIterator.FetchNext: string;
begin
  Result:='';
  if not FStack.IsEmpty() then
    Result:=ProcessDir;
end;

end.

