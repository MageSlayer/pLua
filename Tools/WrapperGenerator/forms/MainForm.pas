unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  LuaWrapper, StdCtrls, parserTree, ComCtrls, ExtCtrls;

const
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};

type
  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edCommandLineArgs: TEdit;
    edFullName: TEdit;
    edName: TEdit;
    edPathName: TEdit;
    edTPTreeElement: TEdit;
    edTypeName: TEdit;
    edDeclaration: TEdit;
    edDeclarationFull: TEdit;
    edRefCount: TEdit;
    edVisibility: TEdit;
    edSourceLine: TEdit;
    FileNameEdit1: TFileNameEdit;
    ilTreeViewStateImages: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    tvParseTree: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure tvParseTreeClick(Sender: TObject);
    procedure tvParseTreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    lua : TLua;
  public
    { public declarations }
    procedure ProcessFile(WhatFile : AnsiString);
    procedure ShowTree(Container : TWraperTreeContainer);
    function  IsNodeChecked(WhatNode : TTreeNode) : Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  PParser, PasTree, luaParserNode, plua, lua;

{ TfrmMain }

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ProcessFile(FileNameEdit1.FileName);
end;

procedure TfrmMain.ProcessFile(WhatFile : AnsiString);
var
  Container : TWraperTreeContainer;
begin
  try
    Container := TWraperTreeContainer.Create;
    try
      if trim(edCommandLineArgs.Text) <> '' then
        ParseSource(Container, WhatFile + #32 + trim(edCommandLineArgs.Text), OSTarget, CPUTarget)
      else
        ParseSource(Container, WhatFile, OSTarget, CPUTarget);
      ShowTree(Container);
    finally
      Container.Free;
    end;
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  ndx,
  i   : Integer;
  procedure DumpNode(ANode : TTreeNode);
  var
    c : Integer;
  begin
    if not IsNodeChecked(ANode) then
      exit;
    if assigned(ANode.Parent) then
      if not IsNodeChecked(ANode.Parent) then
        exit;
    lua_pushinteger(lua.LuaState, ndx);
    PushExistingNode(lua.LuaState, TPasElement(ANode.Data));
    lua_settable(lua.LuaState, -3);
    inc(ndx);
    for c := 0 to ANode.Count -1 do
      DumpNode(ANode.Items[c]);
  end;
begin
  if lua = nil then
    lua := TLua.Create(self)
  else
    lua.Close;
  try
    try
      lua.LoadFile('exporter.lua');
      lua.Value['SourceFile'] := FileNameEdit1.FileName;
      lua.Value['FilePath'] := ExtractFilePath(FileNameEdit1.FileName);
      lua.Value['FileName'] := ExtractFileName(FileNameEdit1.FileName);
      lua.Value['FileBase'] := ChangeFileExt(ExtractFileName(FileNameEdit1.FileName), '');
      lua_pushliteral(Lua.LuaState, 'nodes');
      lua_newtable(Lua.LuaState);
      ndx := 1;
      for i := 0 to tvParseTree.Items.Count-1 do
        if tvParseTree.Items[i].Parent = nil then
          DumpNode(tvParseTree.Items[i]);
      lua_settable(Lua.LuaState, LUA_GLOBALSINDEX);
      lua.Execute;
    except
      on e : exception do
        ShowMessage(e.Message);
    end;
  finally
    //lua.Free;
  end;
end;

procedure TfrmMain.FileNameEdit1AcceptFileName(Sender: TObject;
  var Value: String);
begin
  try
    ProcessFile(Value);
  except
    on e:Exception do
      ShowMessage(e.Message);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  edCommandLineArgs.Text := '';
end;

procedure TfrmMain.tvParseTreeClick(Sender: TObject);
var
  p : TPoint;
begin
  p := tvParseTree.ScreenToClient(mouse.CursorPos);
  if (htOnStateIcon in tvParseTree.GetHitTestInfoAt(P.X, P.Y)) then
    case tvParseTree.Selected.StateIndex of
      2 : tvParseTree.Selected.StateIndex := 3;
      3 : tvParseTree.Selected.StateIndex := 2;
      4 : tvParseTree.Selected.StateIndex := 5;
      5 : tvParseTree.Selected.StateIndex := 4;
    end;
end;

procedure TfrmMain.tvParseTreeSelectionChanged(Sender: TObject);
var
  itm : TPasElement;
begin
  edName.Text := '';
  edFullName.Text := '';
  edPathName.Text := '';
  edTypeName.Text := '';
  edDeclaration.Text := '';
  edDeclarationFull.Text := '';
  edRefCount.Text := '';
  edVisibility.Text := '';
  edSourceLine.Text := '';
  edTPTreeElement.Text := '';

  try
    if assigned(tvParseTree.Selected) then
      begin
        itm := TPasElement(tvParseTree.Selected.Data);
        if assigned(itm) then
          begin
            edName.Text     := itm.Name;
            edFullName.Text := itm.FullName;
            edPathName.Text := itm.PathName;
            edTypeName.Text := itm.ElementTypeName;
            edDeclaration.Text := itm.GetDeclaration(false);
            edDeclarationFull.Text := itm.GetDeclaration(true);
            edRefCount.Text := IntToStr(itm.RefCount);
            edVisibility.Text := VisibilityNames[itm.Visibility];
            edSourceLine.Text := IntToStr(itm.SourceLinenumber);
            edTPTreeElement.Text := itm.ClassName;
          end;
      end;
  except
    on e:Exception do
      begin
      end;
  end;
end;

procedure TfrmMain.ShowTree(Container: TWraperTreeContainer);
var
  i : Integer;
  procedure ScanChildren(ParentNode : TTreeNode; ParentItem : TPasElement);
  var
    c : Integer;
  begin
    ParentNode.Data := ParentItem;
    ParentNode.StateIndex := 2;
    if TObject(ParentNode.Data) is TPasUnresolvedTypeRef then
      ParentNode.StateIndex := 6;
    for c := 0 to Container.Count -1 do
      if (Container.Item[c].Parent = ParentItem) and (not (Container.Item[c].Visibility in [visPrivate, visProtected])) then// and (not (Container.Item[c] is TPasUnresolvedTypeRef)) then
        ScanChildren(tvParseTree.Items.AddChild(ParentNode, Container.Item[c].Name), Container.Item[c]);
  end;
begin
  tvParseTree.Items.BeginUpdate;
  try
    tvParseTree.Items.Clear;
    for i := 0 to Container.Count -1 do
      if (Container.Item[i].Parent = nil) and (not (Container.Item[i].Visibility in [visPrivate, visProtected])) then// and (not (Container.Item[i] is TPasUnresolvedTypeRef)) then
        ScanChildren(tvParseTree.Items.AddChild(nil, Container.Item[i].Name), Container.Item[i]);
  finally
    tvParseTree.Items.EndUpdate;
  end;
end;

function TfrmMain.IsNodeChecked(WhatNode: TTreeNode): Boolean;
begin
  if WhatNode.StateIndex in [2..5] then
    result := WhatNode.StateIndex in [2,4]
  else
    result := true;
end;

initialization
  {$I MainForm.lrs}

end.

