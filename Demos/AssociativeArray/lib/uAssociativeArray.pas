unit uAssociativeArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  PAssocArrayItem = ^TAssocArrayItem;
  TAssocArrayItem = packed record
    Name : AnsiString;
    Value: Variant;
    Next : PAssocArrayItem;
  end;
  
  { TAssociativeArray }

  TAssociativeArray = class
  private
    ffirst : PAssocArrayItem;
    function   GetItem(S: AnsiString): Variant;
    procedure  SetItem(S: AnsiString; V: Variant);
  public
    constructor Create;
    destructor Destroy; override;

    property Values[Index: AnsiString]: Variant read GetItem write SetItem; default;
  end;
  
implementation

uses
  Variants;

{ TAssociativeArray }

function TAssociativeArray.GetItem(S: AnsiString): Variant;
var
  itm : PAssocArrayItem;
begin
  result := NULL;
  itm := ffirst;
  while (result = NULL) and (assigned(itm)) do
    begin
      if AnsiCompareText(S, itm^.Name) = 0 then
        result := itm^.Value;
      itm := itm^.Next;
    end;
end;

procedure TAssociativeArray.SetItem(S: AnsiString; V: Variant);
var
  workingitm,
  itm : PAssocArrayItem;
begin
  itm := nil;
  workingitm := ffirst;
  while (assigned(workingitm)) do
    begin
      itm := workingitm;
      if AnsiCompareText(S, workingitm^.Name) = 0 then
        begin
          workingitm^.Value := V;
          exit;
        end;
      workingitm := workingitm^.Next;
    end;

// if itm isn't assigned we can't have a first pointer yet, so create it
  if not assigned(itm) then
    begin
      new(ffirst);
      ffirst^.Next := nil;
      itm := ffirst;
    end
  else
    begin
      new(itm^.Next); // create a new working node
      itm := itm^.Next;
      itm^.Next := nil;
    end;
  itm^.Name := S;
  itm^.Value := V;
end;

constructor TAssociativeArray.Create;
begin
  ffirst := nil;
end;

destructor TAssociativeArray.Destroy;
var
  itm : PAssocArrayItem;
begin
  while assigned(ffirst) do
    begin
      itm := ffirst;
      ffirst := ffirst^.Next;
      Freemem(itm);
    end;
  inherited Destroy;
end;

end.

