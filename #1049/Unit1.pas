unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees;

type
  TForm1 = class(TForm)
    edIndex: TEdit;
    btnAdd: TButton;
    btnRemove: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edName: TEdit;
    VirtualStringTree1: TVirtualStringTree;
    chkBeginEndUpdate: TCheckBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    procedure CreateColumn(const Title: string; Position: Integer);
    procedure DestroyColumn(AColumnIndex: TColumnIndex);
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  CurrentColumn: TVirtualTreeColumn;
begin
  if Column < 0 then exit;
  CurrentColumn := VirtualStringTree1.Header.Columns[Column];
  CellText := 'R:' + IntToStr(Node.Index)+ ', C:' + IntToStr(Column) +
    ', I:' + IntToStr(CurrentColumn.Index) +
    ', P:' + IntToStr(CurrentColumn.Position);
end;

procedure TForm1.CreateColumn(const Title: string; Position: Integer);
var
  Column: TVirtualTreeColumn;
begin
  Column := VirtualStringTree1.Header.Columns.Add;
  Column.Text := Title;
  Column.Width := 100;
  Column.Position := Position;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateColumn('4th', 0);
  CreateColumn('3rd', 0);
  CreateColumn('2nd', 0);
  CreateColumn('1st', 0);

  VirtualStringTree1.RootNodeCount := 5;
end;

procedure TForm1.DestroyColumn(AColumnIndex: TColumnIndex);
begin
  if AColumnIndex >= 0 then
  begin
    VirtualStringTree1.Header.Columns.Delete(AColumnIndex);
  end;
end;

procedure TForm1.btnAddClick(Sender: TObject);
begin
  CreateColumn(edName.Text, StrToInt(edIndex.Text));
end;

procedure TForm1.btnRemoveClick(Sender: TObject);
var
  Column: TVirtualTreeColumn;
  Position: Cardinal;
  i: Integer;
begin
  Position := StrToUInt(edIndex.Text);

  if chkBeginEndUpdate.Checked then
    VirtualStringTree1.BeginUpdate;
  try
    for i := 0 to VirtualStringTree1.Header.Columns.Count - 1 do
    begin
      Column := VirtualStringTree1.Header.Columns[i];
      if Column.Position = Position then
      begin
        DestroyColumn(Column.Index);
        Break;
      end;
    end;
  finally
    if chkBeginEndUpdate.Checked then
      VirtualStringTree1.EndUpdate;
  end;
end;

end.
