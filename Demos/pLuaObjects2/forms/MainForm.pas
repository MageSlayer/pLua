unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LuaWrapper,
  StdCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Lua : TLua;
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  lua, plua, LuaButton, pLuaObject;

function lua_ShowMessage(l : PLua_State) : integer; cdecl;
var
  n, i : Integer;
  msg : AnsiString;
begin
  result := 0;
  n := lua_gettop(l);
  if n > 0 then
    begin
      msg := '';
      for i := 1 to n do
        msg := msg + plua_tostring(L, i);
      ShowMessage(msg);
    end;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Lua := TLua.Create(self);
  Lua.RegisterLuaMethod('ShowMessage', @lua_ShowMessage);
  RegisterLuaButton(Lua.LuaState);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  RegisterExistingButton(Lua.LuaState, 'btn', btn);
  if FileExists('script.lua') then
    begin
      Lua.LoadFile('script.lua');
      Lua.Execute;
    end;
end;

initialization
  {$I MainForm.lrs}

end.

