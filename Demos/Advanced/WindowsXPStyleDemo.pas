unit WindowsXPStyleDemo;

// Virtual Treeview sample form demonstrating following features:
//   - Windows XP style treeview.
// Written by Mike Lischke.

interface

{$warn UNSAFE_TYPE off}
{$warn UNSAFE_CAST off}
{$warn UNSAFE_CODE off}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ImgList, ComCtrls, ToolWin, Menus, StdCtrls, UITypes,
  System.ImageList, VirtualTrees.Types;

type
  TWindowsXPForm = class(TForm)
    XPTree: TVirtualStringTree;
    LargeImages: TImageList;
    SmallImages: TImageList;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Label1: TLabel;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    PrintDialog: TPrintDialog;
    procedure XPTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex);
    procedure FormCreate(Sender: TObject);
    procedure XPTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure XPTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure XPTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure XPTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure XPTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure XPTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure Label4Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure XPTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure XPTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  end;

var
  WindowsXPForm: TWindowsXPForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Main, ShellAPI, Printers, States, Vcl.GraphUtil;

{$R *.dfm}

type
  PEntry = ^TEntry;
  TEntry = record
    Caption: UnicodeString;
    Image: Integer;
    Size: Int64;
  end;

var
  TreeEntries: array[0..17] of TEntry = (
    (Caption: 'My Computer'; Image: 0; Size: 0),
    (Caption: 'Network Places'; Image: 1; Size: 0),
    (Caption: 'Recycle Bin'; Image: 2; Size: 0),
    (Caption: 'My Documents'; Image: 3; Size: 0),
    (Caption: 'My Music'; Image: 4; Size: 0),
    (Caption: 'My Pictures'; Image: 5; Size: 0),
    (Caption: 'Control Panel'; Image: 6; Size: 0),
    (Caption: 'Help'; Image: 7; Size: 0),
    (Caption: 'Help Document'; Image: 8; Size: 0),
    (Caption: 'User Accounts'; Image: 9; Size: 0),
    (Caption: 'Internet'; Image: 10; Size: 0),
    (Caption: 'Network Group'; Image: 11; Size: 0),
    (Caption: 'Folder'; Image: 12; Size: 0),
    (Caption: 'Window'; Image: 13; Size: 0),
    (Caption: 'Warning'; Image: 14; Size: 0),
    (Caption: 'Information'; Image: 15; Size: 0),
    (Caption: 'Critical'; Image: 16; Size: 0),
    (Caption: 'Security'; Image: 17; Size: 0)
  );

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex);

var
  Data: PEntry;

begin
  Data := Sender.GetNodeData(Node);
  case Kind of
    ikNormal, ikSelected:
      if (Column = 0) and (Sender.NodeParent[Node] = nil) then
        Index := Data.Image;
    ikState:
      case Column of
        0:
          if Sender.NodeParent[Node] <> nil then
            Index := 21;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.FormCreate(Sender: TObject);

begin
  XPTree.NodeDataSize := SizeOf(TEntry);
  XPTree.HintMode := hmTooltip;
  ConvertToHighColor(LargeImages);
  ConvertToHighColor(SmallImages);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PEntry;

begin
  if ParentNode = nil then
  begin
    Include(InitialStates, ivsHasChildren);
    Data := Sender.GetNodeData(Node);
    Data^ := TreeEntries[Node.Index mod 18];
    Data.Size := Random(100000);
    Node.CheckType := ctCheckBox;
  end
  else
    Node.CheckType := ctRadioButton;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);

begin
  ChildCount := 5;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);

var
  Data: PEntry;

begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0:
      if Sender.NodeParent[Node] = nil then
        CellText := Data.Caption
      else
        CellText := 'More entries';
    1:
      if Sender.NodeParent[Node] = nil then
        CellText := FloatToStr(Data.Size / 1000) + ' MB';
    2:
      if Sender.NodeParent[Node] = nil then
        CellText := 'System Folder';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

begin
  if HitInfo.Button = mbLeft then
  begin
    with Sender do
    begin
      if SortColumn > NoColumn then
        Columns[SortColumn].Options := Columns[SortColumn].Options + [TVTColumnOption.coParentColor];

      // Do not sort the last column, it contains nothing to sort.
      if HitInfo.Column = 2 then
        SortColumn := NoColumn
      else
      begin
        if (SortColumn = NoColumn) or (SortColumn <> HitInfo.Column) then
        begin
          SortColumn := HitInfo.Column;
          SortDirection := sdAscending;
        end
        else
          if SortDirection = sdAscending then
            SortDirection := sdDescending
          else
            SortDirection := sdAscending;

        if SortColumn <> NoColumn then begin
          Columns[SortColumn].Color := GetShadowColor(ColorToRGB(XPTree.Colors.BackGroundColor), -32);
        end;
        TBaseVirtualTree(Sender.Treeview).SortTree(SortColumn, SortDirection, True);

      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  Data1, Data2: PEntry;

begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0:
      Result := CompareText(Data1.Caption, Data2.Caption);
    1:
      Result := Data1.Size - Data2.Size;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

begin
  // Show only a dummy hint. It is just to demonstrate how to do it.
  HintText := 'Size larger than 536 MB' + #13 +
    'Folders: addins, AppPatch, Config, Connection Wizard, ...' + #13 +
    'Files: 1280.bmp, 1280x1024.bmp, 2001 94 mars.bmp, ac3api.ini, ...';
  LineBreakStyle := TVTTooltipLineBreakStyle.hlbForceMultiLine;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.Label4Click(Sender: TObject);

begin
    ShellExecute(0, 'open', 'https://groups.google.com/forum/#!forum/virtual-treeview', nil, nil, SW_SHOW);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.ToolButton9Click(Sender: TObject);

begin
  if PrintDialog.Execute then
    XPTree.Print(Printer, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PEntry;

begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

end.
