unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, SynEdit, SynUniHighlighter, dmScriptEngine, ActnList;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actNew: TAction;
    actOpen: TAction;
    actSave: TAction;
    actRun: TAction;
    ActionList1: TActionList;
    imgImage: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    memOutput: TMemo;
    nbProject: TNotebook;
    OpenDialog1: TOpenDialog;
    pnlMessages: TPanel;
    pgSource: TPage;
    pgImage: TPage;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    seSource: TSynEdit;
    SynUniSyn1: TSynUniSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  nbProject.Align := alClient;
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    seSource.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmMain.actNewExecute(Sender: TObject);
begin
  seSource.Lines.Clear;
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  memOutput.Clear;
  nbProject.PageIndex := pgImage.PageIndex;
  ScriptEngine.Execute(seSource.Lines.Text);
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    seSource.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if FileExists('lua.hgl') then
    begin
      SynUniSyn1.LoadFromFile('lua.hgl');
      SynUniSyn1.Enabled := true;
      seSource.Highlighter := SynUniSyn1;
    end;
end;

initialization
  {$I MainForm.lrs}

end.

