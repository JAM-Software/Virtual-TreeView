unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ImgList, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    tvSrc: TVirtualDrawTree;
    PrjImages, ImageListHot: TImageList;
    lbl1: TLabel;
    grpList: TGroupBox;
    chkListState: TCheckBox;
    chkListOthers: TCheckBox;
    grpIndex: TGroupBox;
    chkIndexState: TCheckBox;
    chkIndexOthers: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure TuneTree(Sender: TObject);
  private
    { Private declarations }
    procedure SrcDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
    procedure SrcGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex{Integer});
    procedure SrcGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure DoOnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
   public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i: integer; n: PVirtualNode;
begin
  for i := 0 to 9 do begin
    n := tvSrc.AddChild(nil);
  end;
  tvSrc.OnInitNode := DoOnInitNode;
  tvSrc.OnGetImageIndex := SrcGetImageIndex;
  tvSrc.OnDrawNode := SrcDrawNode;
end;


(****
  tvSrc.OnGetNodeDataSize := DoGetNodeDataSize;
  tvSrc.OnBeforeItemErase := SrcBeforeItemErase;
  tvSrc.OnUpdating := SrcUpdating;
  tvSrc.OnChange := SrcChanged;
  tvSrc.OnFocusChanged := SrcFocusChanged;
  tvSrc.OnChecking := SrcChecking;
  tvSrc.OnChecked  := SrcChecked;
  tvSrc.OnMouseDown := SrcMouseDown;
  tvSrc.OnMouseUp := SrcMouseUp;
  tvSrc.OnEditing := SrcEditing;
  tvSrc.OnCreateEditor := SrcCreateEditor;
  tvSrc.OnEnter := SrcEnter;
  tvSrc.OnExit := SrcExit;
  tvSrc.OnKeyAction := SrcKeyAction;
  tvSrc.OnExpanding := SrcExpanding;
***)

procedure TForm1.SrcDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var S: string; cl: TColor; NodeFocused: boolean; R: TRect;
begin
  SrcGetText( Sender, PaintInfo.Node, PaintInfo.Column, ttNormal, S );
  NodeFocused := (Sender.FocusedNode = PaintInfo.Node);
  if NodeFocused
     then cl := clGreen
     else cl := clFuchsia;

  R := PaintInfo.ContentRect;
  PaintInfo.Canvas.Font.Color := cl;
  PaintInfo.Canvas.TextRect(R, S);
end;

procedure TForm1.SrcGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var ImageIndex: TImageIndex);
begin
  if Kind = ikState then begin
    if chkIndexState.Checked
       then ImageIndex := Node.Index
       else ImageIndex := -1;
  end else begin
    if chkIndexOthers.Checked
       then ImageIndex := Node.Index
       else ImageIndex := -1;
  end;
end;

procedure TForm1.SrcGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  i: integer; S: string; C: Char;
begin
  i := Node.Index;
  i := i xor Column;
  C := Char(Ord('0') + i);
  S := StringOfChar(C, 10 + 4*Column);

  CellText := S;
end;

procedure TForm1.DoOnInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin

end;

procedure TForm1.TuneTree(Sender: TObject);
begin

  if chkListState.Checked
     then tvSrc.StateImages := ImageListHot
     else tvSrc.StateImages := nil;

  if chkListOthers.Checked
     then tvSrc.Images := PrjImages
     else tvSrc.Images := nil;

end;

end.
