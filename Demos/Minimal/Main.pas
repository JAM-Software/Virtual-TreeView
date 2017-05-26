unit Main;

// Demonstration project for TVirtualStringTree to generally show how to get started.
// Written by Mike Lischke.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    VST: TVirtualStringTree;
    ClearButton: TButton;
    AddOneButton: TButton;
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    CloseButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure CloseButtonClick(Sender: TObject);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  end;

var
  MainForm: TMainForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.DFM}

type
  // This is a very simple record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: WideString;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  // Let the tree know how much data space we need.
  VST.NodeDataSize := SizeOf(TMyRec);
  // Set an initial number of nodes.
  VST.RootNodeCount := 20;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ClearButtonClick(Sender: TObject);

var
  Start: Cardinal;

begin
  Screen.Cursor := crHourGlass;
  try
    Start := GetTickCount;
    VST.Clear;
    Label1.Caption := Format('Last operation duration: %d ms', [GetTickCount - Start]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.AddButtonClick(Sender: TObject);

var
  Count: Cardinal;
  Start: Cardinal;

begin
  // Add some nodes to the treeview.
  Screen.Cursor := crHourGlass;
  with VST do
  try
    Start := GetTickCount;
    case (Sender as TButton).Tag of
      0: // add to root
        begin
          Count := StrToInt(Edit1.Text);
          RootNodeCount := RootNodeCount + Count;
        end;
      1: // add as child
        if Assigned(FocusedNode) then
        begin
          Count := StrToInt(Edit1.Text);
          ChildCount[FocusedNode] := ChildCount[FocusedNode] + Count;
          Expanded[FocusedNode] := True;
          InvalidateToBottom(FocusedNode);
        end;
    end;
    Label1.Caption := Format('Last operation duration: %d ms', [GetTickCount - Start]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

var
  Data: PMyRec;

begin
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    CellText := Data.Caption;
end;

//----------------------------------------------------------------------------------------------------------------------

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

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PMyRec;

begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node is displayed not when it is added.
    Data.Caption := Format('Level %d, Index %d', [GetNodeLevel(Node), Node.Index]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CloseButtonClick(Sender: TObject);

begin
  Close;
end;


end.


