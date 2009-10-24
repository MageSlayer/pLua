unit dmScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, lua, plua, LuaWrapper,
  pLuaObject, Graphics, ExtCtrls;

type

  { TScriptEngine }

  TScriptEngine = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    function GetCanvas: TCanvas;
    function GetImage: TImage;
    { private declarations }
    procedure LoadLibs(LuaWrapper : TLua);
    procedure ScriptExceptionHandler( Title: ansistring; Line: Integer; Msg: ansistring;
                                      var handled : Boolean);
  public
    { public declarations }
    Lua : TLua;
    procedure Execute(Source : AnsiString);
    property Canvas : TCanvas read GetCanvas;
    property Image  : TImage read GetImage;
  end; 

var
  ScriptEngine: TScriptEngine;

implementation

uses
  MainForm, Turtle;

function lua_Clear(L : Plua_State) : Integer; cdecl;
var
  n, c : Integer;
begin
  result := 0;
  n := lua_gettop(l);
  if (n > 0) and (lua_isnumber(l, 1)) then
    c := lua_tointeger(l, 1)
  else
    c := Integer(clWhite);
  ScriptEngine.Canvas.Brush.Color := c;
  ScriptEngine.Canvas.Pen.Color := c;
  ScriptEngine.Canvas.Rectangle(0, 0, ScriptEngine.Image.Width, ScriptEngine.Image.Height);
end;

function lua_print(L : Plua_state) : integer; cdecl;
var
  i, n : Integer;
  sl : TStringList;
  s  : AnsiString;
begin
  result := 0;
  n := lua_gettop(l);
  s := '';
  sl := TStringList.Create;
  try
    for i := 1 to n do
      s := s + lua_tostring(l, i);
    sl.Text := s;
    frmMain.memOutput.Lines.AddStrings(sl);
  finally
    sl.Free;
  end;
end;

function lua_HexToInt(L : Plua_state) : integer; cdecl;
var
  val : AnsiString;
begin
  result := 0;
  val := '$' + plua_tostring(l, 1);
  lua_pushinteger(l, StrToInt(val));
  result := 1;
end;

function lua_SetCanvasSize(L : Plua_state) : integer; cdecl;
var
  w, h : integer;
begin
  result := 0;
  w := lua_tointeger(l, 1);
  h := lua_tointeger(l, 2);
  ScriptEngine.Image.Width := w;
  ScriptEngine.Image.Height := h;
  ScriptEngine.Image.Picture.Bitmap.Width := w;
  ScriptEngine.Image.Picture.Bitmap.Height := h;
end;

{ TScriptEngine }

procedure TScriptEngine.DataModuleCreate(Sender: TObject);
begin
  Lua := TLua.Create(self);
  Lua.LibName := 'Script';
  Lua.OnLoadLibs := @LoadLibs;
  Lua.OnException := @ScriptExceptionHandler;
end;

function TScriptEngine.GetCanvas: TCanvas;
begin
  result := frmMain.imgImage.Canvas;
end;

function TScriptEngine.GetImage: TImage;
begin
  result := frmMain.imgImage;
end;

procedure TScriptEngine.LoadLibs(LuaWrapper : TLua);
begin
  LuaWrapper.RegisterLuaMethod('Clear', @lua_Clear);
  LuaWrapper.RegisterLuaMethod('print', @lua_print);
  LuaWrapper.RegisterLuaMethod('HexToInt', @lua_HexToInt);
  LuaWrapper.RegisterLuaMethod('SetCanvasSize', @lua_SetCanvasSize);
  RegisterTurtle(LuaWrapper);
end;

procedure TScriptEngine.ScriptExceptionHandler(Title: ansistring;
  Line: Integer; Msg: ansistring; var handled: Boolean);
begin
  Handled := true;
  frmMain.memOutput.Lines.Add(format('%s (%d): %s', [Title, Line, msg]));
end;

procedure TScriptEngine.Execute(Source: AnsiString);
begin
  TurtleCanvas := Canvas;
  Lua.LoadScript(Source);
  Lua.Execute;
end;

initialization
  {$I dmScriptEngine.lrs}

end.

