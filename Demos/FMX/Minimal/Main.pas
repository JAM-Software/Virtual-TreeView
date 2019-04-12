unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, VirtualTrees, VirtualTrees.FMX, FMX.StdCtrls, FMX.Edit,
  FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    AddOneButton: TButton;
    Button1: TButton;
    ClearButton: TButton;
    CloseButton: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure AddOneButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    VST: TVirtualStringTree;


    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

type
  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: WideString;
  end;

procedure TMainForm.AddOneButtonClick(Sender: TObject);
var
  Count: Cardinal;
  Start: Cardinal;
  No   : Integer;
begin
  // Add some nodes to the treeview.
  //Screen.Cursor := crHourGlass;

  try
    Start := TThread.GetTickCount;
    No:= (Sender as TButton).Tag;
    case No of
      0: // add to root
        begin
          Count := StrToInt(Edit1.Text);
          VST.RootNodeCount := VST.RootNodeCount + Count;
        end;
      1: // add as child
        if Assigned(VST.FocusedNode) then
        begin
          Count := StrToInt(Edit1.Text);
          VST.ChildCount[VST.FocusedNode] := VST.ChildCount[VST.FocusedNode] + Count;
          VST.Expanded[VST.FocusedNode] := True;
          VST.InvalidateToBottom(VST.FocusedNode);
        end;
    end;
    Label1.Text := Format('Last operation duration: %d ms', [TThread.GetTickCount - Start]);
  finally
    //Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
var
  Start: Cardinal;

begin
  //Screen.Cursor := crHourGlass;
  try
    Start := TThread.GetTickCount;
    VST.Clear;
    Label1.Text := Format('Last operation duration: %d ms', [TThread.GetTickCount - Start]);
  finally
    //Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
Var col: TVirtualTreeColumn;
begin
  VST:= TVirtualStringTree.Create(Self);
  VST.Parent:= Self;
  VST.Fill.Color:= TAlphaColorRec.White;
  //VST.AlignWithMargins:= true;
  VST.Height:= 262;
  VST.Align:= TAlignLayout.Top;
  VST.Colors.BorderColor:= clWindowText;
  VST.Colors.HotColor := clBlack;
  //VST.DragMode := TDragMode.dmAutomatic;
  VST.DragType := dtVCL;
  VST.Header.AutoSizeIndex := -1;
  //VST.Header.Font.Charset := DEFAULT_CHARSET;
  //VST.Header.Font.Color := clWindowText;
  VST.Header.Font.Size := 10;
  VST.Header.Font.Family := 'Tahoma';
  VST.Header.Font.Style := [];
  VST.Header.Options := [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible];
  //VST.HintAnimation := hatNone;
  VST.IncrementalSearch := isAll;
  //VST.ParentBiDiMode := False;
  VST.TabOrder := 0;
  VST.TreeOptions.AnimationOptions := [toAnimatedToggle];
  VST.TreeOptions.MiscOptions := [toEditable, toInitOnSave, toToggleOnDblClick, toWheelPanning];
  VST.TreeOptions.PaintOptions := [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages];
  VST.TreeOptions.SelectionOptions := [toMultiSelect];
  VST.OnFreeNode := VSTFreeNode;
  VST.OnGetText := VSTGetText;
  VST.OnInitNode := VSTInitNode;
  col:= VST.Header.Columns.Add;
  col.Position := 0;
  col.Width := 300;
  col.Text := 'Name';


  // Let the tree know how much data space we need.
  VST.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  VST.RootNodeCount := 20;
end;

procedure TMainForm.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PMyRec;

begin
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  Finalize(Data^);
end;

procedure TMainForm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  Data: PMyRec;

begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data.Caption;
end;

procedure TMainForm.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PMyRec;

begin
  Data := Sender.GetNodeData(Node);
  // Construct a node caption. This event is triggered once for each node but
  // appears asynchronously, which means when the node is displayed not when it is added.
  Data.Caption := Format('Level %d, Index %d', [Sender.GetNodeLevel(Node), Node.Index]);
end;

end.
