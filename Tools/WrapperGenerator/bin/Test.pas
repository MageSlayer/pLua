unit Test; 

{$mode objfpc}{$H+}

// This is just a simple little test file for testing the Generator

interface

uses
  Classes, SysUtils; 

type
  TMyRecord = record
    v1, v2 : Variant;
  end;
  
  { TMyObject }
  TMyObject = class
  public
    procedure Test;
    function  DoIt(Val1, Val2 : integer) : ansistring;
  end;
  
  { TMyObjectSub }
  TMyObjectSub = class(TMyObject)
  private
    FSomeProp: Double;
    procedure SetSomeProp(const AValue: Double);
  public
    property SomeProp : Double read FSomeProp write SetSomeProp;
  end;

function Test2(Val1 : Integer; Val2 : String) : Double;
procedure ProcTest;

var
  v1, v2 : Variant;
  v3     : integer;

const
  c1 = 'Test constant value 1';
  c2 : AnsiString = 'Test constant value 2';

implementation

function Test2(Val1: Integer; Val2: String): Double;
begin

end;

procedure ProcTest;
begin

end;

{ TMyObject }

procedure TMyObject.Test;
begin

end;

function TMyObject.DoIt(Val1, Val2: integer): ansistring;
begin

end;

{ TMyObjectSub }

procedure TMyObjectSub.SetSomeProp(const AValue: Double);
begin
  if FSomeProp=AValue then exit;
  FSomeProp:=AValue;
end;

end.

