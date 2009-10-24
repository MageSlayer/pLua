unit parserTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, PasTree, PParser;

type

  { TWraperTreeContainer }

  TWraperTreeContainer = class(TPasTreeContainer)
  private
    fElements : TStringList;
    function GetCount: Integer;
    function GetItem(index: integer): TPasElement;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure DumpToFile(FileName : AnsiString);
    procedure Clear;

    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
    
    property Count : Integer read GetCount;
    property Item[index:integer] : TPasElement read GetItem;
  end;

implementation

function TWraperTreeContainer.GetCount: Integer;
begin
  result := fElements.Count;
end;

function TWraperTreeContainer.GetItem(index: integer): TPasElement;
begin
  result := TPasElement(fElements.Objects[index]);
end;

constructor TWraperTreeContainer.Create;
begin
  fElements := TStringList.Create;
end;

destructor  TWraperTreeContainer.Destroy;
begin
  fElements.Free;
  inherited Destroy;
end;

procedure TWraperTreeContainer.Clear;
begin
  fElements.Clear;
end;

function TWraperTreeContainer.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  result := AClass.Create(AName, AParent);
  result.Visibility := AVisibility;
  result.SourceFilename := ASourceFilename;
  result.SourceLinenumber := ASourceLinenumber;
  fElements.AddObject(AName, result);
end;

function TWraperTreeContainer.FindElement(const AName: String): TPasElement;
var
  idx : Integer;
begin
  idx := fElements.IndexOf(AName);
  if idx > -1 then
    result := TPasElement(fElements.Objects[idx])
  else
    result := nil;
end;

procedure TWraperTreeContainer.DumpToFile(FileName : AnsiString);
begin
  fElements.SaveToFile(FileName);
end;

end.
