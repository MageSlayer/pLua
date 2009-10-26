unit Turtle;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, lua, plua, pLuaObject, LuaWrapper;

procedure RegisterTurtle(LuaInstance : TLua);

var
  TurtleCanvas : TCanvas;

implementation

var
  TurtleInfo : TLuaClassInfo;

type

  { TTurtle }

  TTurtle = class
  private
    FColor: TColor;
    FPenDown: Boolean;
    FX: integer;
    FY: integer;
    procedure SetColor(const AValue: TColor);
    procedure SetPenDown(const AValue: Boolean);
    procedure SetX(const AValue: integer);
    procedure SetY(const AValue: integer);
  public
    property Color : TColor read FColor write SetColor;
    property X : integer read FX write SetX;
    property Y : integer read FY write SetY;
    property PenDown : Boolean read FPenDown write SetPenDown;
  end;

{ TTurtle }

procedure TTurtle.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
end;

procedure TTurtle.SetPenDown(const AValue: Boolean);
begin
  if FPenDown=AValue then exit;
  FPenDown:=AValue;
end;

procedure TTurtle.SetX(const AValue: integer);
begin
  if FX=AValue then exit;
  FX:=AValue;
end;

procedure TTurtle.SetY(const AValue: integer);
begin
  if FY=AValue then exit;
  FY:=AValue;
end;

function newTurtle(l : Plua_State; paramidxstart, paramcount : integer; InstanceInfo : PLuaInstanceInfo) : TObject;
begin
  result := TTurtle.Create;
end;

procedure releaseTurtle(target : TObject; l : Plua_State);
begin
end;

function GetColor(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  turtle : TTurtle;
begin
  turtle := TTurtle(target);
  lua_pushinteger(l, turtle.Color);
  result := 1;
end;

function SetColor(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  turtle : TTurtle;
begin
  turtle := TTurtle(target);
  turtle.Color := lua_tointeger(l, paramidxstart);
  result := 0;
end;

function GetPenDown(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  turtle : TTurtle;
begin
  turtle := TTurtle(target);
  lua_pushboolean(l, turtle.PenDown);
  result := 1;
end;

function SetPenDown(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  turtle : TTurtle;
begin
  turtle := TTurtle(target);
  turtle.PenDown := lua_toboolean(l, paramidxstart);
  result := 0;
end;

function MoveTo(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  turtle : TTurtle;
  tx, ty : Integer;
begin
  turtle := TTurtle(target);
  tX := lua_tointeger(l, paramidxstart);
  tY := lua_tointeger(l, paramidxstart+1);
  if turtle.PenDown then
    begin
      TurtleCanvas.Pen.Color := turtle.Color;
      if (abs(tx-turtle.X)>1) or (abs(ty-turtle.Y)>1) then
        begin
          TurtleCanvas.Line(turtle.X, turtle.Y, tX, tY);
        end
      else
        begin
          TurtleCanvas.Pixels[turtle.X, turtle.Y] := turtle.Color;
          TurtleCanvas.Pixels[tX, tY] := turtle.Color;
        end;
    end;
  turtle.X := tX;
  turtle.Y := tY;
  result := 0;
end;

function LineTo(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  turtle : TTurtle;
  tx, ty : Integer;
begin
  turtle := TTurtle(target);
  tX := lua_tointeger(l, paramidxstart);
  tY := lua_tointeger(l, paramidxstart+1);
  TurtleCanvas.Pen.Color := turtle.Color;
  TurtleCanvas.Line(turtle.X, turtle.Y, tX, tY);
  turtle.X := tX;
  turtle.Y := tY;
  result := 0;
end;

function setTurtleInfo : TLuaClassInfo;
begin
  plua_initClassInfo(result);
  result.ClassName := 'Turtle';
  result.New := @newTurtle;
  result.Release := @releaseTurtle;
  plua_AddClassProperty(result, 'Color', @GetColor, @SetColor);
  plua_AddClassProperty(result, 'PenDown', @GetPenDown, @SetPenDown);
  plua_AddClassMethod(result, 'MoveTo', @MoveTo);
  plua_AddClassMethod(result, 'LineTo', @LineTo);
end;

procedure RegisterTurtle(LuaInstance: TLua);
var
  L : PLua_State;
begin
  L := LuaInstance.LuaState;
  plua_registerclass(L, TurtleInfo);
end;

initialization
  TurtleInfo := setTurtleInfo;
  
finalization

end.

