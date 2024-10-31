unit GeneralAbilitiesDemo;

// Virtual Treeview sample form demonstrating following features:
//   - General use and feel of TVirtualStringTree.
//   - Themed/non-themed painting.
//   - Node button styles.
//   - Selection rectangle styles.
//   - Multiple columns, header, customize column backgrounds, header popup.
//   - Unicode strings.
//   - OLE drag'n drop image.
//   - Switchable main column.
//   - Right click select and drag.
//   - Node specific popup menu.
//   - Save tree content as text file.
// Written by Mike Lischke.

interface

// For some things to work we need code, which is classified as being unsafe for .NET.
{$warn UNSAFE_TYPE off}
{$warn UNSAFE_CAST off}
{$warn UNSAFE_CODE off}
{$if CompilerVersion >= 20}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ifend}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, VirtualTrees, ComCtrls, ExtCtrls, ImgList, Menus,
  StdActns, ActnList, VirtualTrees.HeaderPopup, UITypes, System.ImageList, VirtualTrees.BaseTree,
  VirtualTrees.Types;

type
  TGeneralForm = class(TForm)
    VST2: TVirtualStringTree;
    TreeImages: TImageList;
    FontDialog1: TFontDialog;
    PopupMenu1: TPopupMenu;
    Onemenuitem1: TMenuItem;
    forrightclickselection1: TMenuItem;
    withpopupmenu1: TMenuItem;
    VTHPopup: TVTHeaderPopupMenu;
    SaveDialog: TSaveDialog;
    ImageList1: TImageList;
    Panel1: TPanel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    ThemeRadioGroup: TRadioGroup;
    SaveButton: TBitBtn;
    GroupBox1: TGroupBox;
    Label19: TLabel;
    MainColumnUpDown: TUpDown;
    ScrollBox1: TScrollBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure VST2InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VST2InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VST2NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure VST2GetCellText(Sender: TCustomVirtualStringTree;
      var E: TVSTGetCellTextEventArgs);
    procedure VST2PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VST2GetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure FormCreate(Sender: TObject);
    procedure MainColumnUpDownChanging(Sender: TObject; var AllowChange: Boolean);
    procedure VST2GetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
      var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure VST2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RadioGroup1Click(Sender: TObject);
    procedure VST2FocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure RadioGroup2Click(Sender: TObject);
    procedure ThemeRadioGroupClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure VST2StateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
    procedure VST2DragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
      Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure VST2GetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex;
      var ImageList: TCustomImageList);
    procedure VST2FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  end;

var
  GeneralForm: TGeneralForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ShellAPI, Main, States;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

const
  LevelCount = 5;
  
type
  PNodeData2 = ^TNodeData2;
  TNodeData2 = record
    Caption,
    StaticText,
    ForeignText: UnicodeString;
    ImageIndex,
    Level: Integer;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Determine if we are running on Windows XP or higher.
  ThemeRadioGroup.Enabled := CheckWin32Version(5, 1);
  if ThemeRadioGroup.Enabled then
    ThemeRadioGroup.ItemIndex := 0;

  // Add a second line of hint text for column headers (not possible in the object inspector).
  with VST2.Header do
    for I := 0 to Columns.Count - 1 do
      Columns[I].Hint := Columns[I].Hint + #10 + '(Can show further information in hints too.)';

  ConvertToHighColor(TreeImages);

  VST2.InitRecursive(nil); // Without this statement, the scrollbar will be wrong and correct itself when scrolling down. See issue #597
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2GetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);

// Returns the size of a node record. Since this size is fixed already at
// creation time it would make sense to avoid this event and assign the value
// in OnCreate of the form (see there for the other trees). But this is a
// demo program, so I want to show this way too. Note the -1 value in
// VST2.NodeDataSize which primarily causes this event to be fired.

begin
  NodeDataSize := SizeOf(TNodeData2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);

var
  Data: PNodeData2;

begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0: // main column
      case TextType of
        ttNormal:
          if Data.Level = 0 then
            TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
        ttStatic:
          begin
            if Node = Sender.HotNode then
              TargetCanvas.Font.Color := clRed
            else
              TargetCanvas.Font.Color := clBlue;
            TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
          end;
      end;
    1: // image column (there is no text)
      ;
    2: // language column (no customization)
      ;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2GetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);

// Returns the text as it is stored in the nodes data record.

var
  Data: PNodeData2;

begin
  Data := Sender.GetNodeData(E.Node);
  case E.Column of
    0: // main column (has two different captions)
      begin
        E.CellText := Data.Caption;
        E.StaticText := Data.StaticText;
        if Sender.GetNodeLevel(E.Node) > 0 then
          E.StaticTextAlignment := TAlignment.taRightJustify;
      end;
    1,2:
      E.CellText := Data.ForeignText;
  else
    E.CellText := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

const
  LevelToCheckType: array[0..5] of TCheckType = (
    ctButton, ctRadioButton, ctTriStateCheckBox, ctTriStateCheckBox, ctCheckBox, ctNone
  );

var
  Data: PNodeData2;

begin
  Data := Sender.GetNodeData(Node);
  with Data^ do
  begin
    Level := Sender.GetNodeLevel(Node);
    if Level < LevelCount then
    begin
      Include(InitialStates, ivsHasChildren);
      if Level = 0 then
        Include(InitialStates, ivsExpanded);
    end;

    Caption := Format('Level %d, Index %d', [Level, Node.Index]);
    if Level in [0, 2, 3] then
      StaticText := '(static text)';

    ForeignText := '';
    case Data.Level of
      1:
        begin
          ForeignText := WideChar($2200);
          ForeignText := ForeignText + WideChar($2202) + WideChar($221C) + WideChar($221E) + WideChar($2230) +
            WideChar($2233) + WideChar($2257) + WideChar($225D) + WideChar($22B6) + WideChar($22BF);
        end;
      2:
        begin
          ForeignText := WideChar($32E5);
          ForeignText := ForeignText + WideChar($32E6) + WideChar($32E7) + WideChar($32E8) + WideChar($32E9);
        end;
      3:
        begin
          ForeignText := WideChar($03B1);
          ForeignText := ForeignText + WideChar($03B2) + WideChar($03B3) + WideChar($03B4) + WideChar($03B5) +
            WideChar($03B6) + WideChar($03B7) + WideChar($03B8) + WideChar($03B9);
        end;
      4:
        begin
          ForeignText := WideChar($20AC);
          ForeignText := 'nichts ist unmöglich ' + ForeignText;
        end;
      5:
        begin
          ForeignText := 'Deepest level';
        end;
    end;
    Node.CheckType := LevelToCheckType[Data.Level];
    // use enabled and disabled checkboxes
    case (Data.Level mod 5) of
      0,1,2,3: Sender.CheckState[Node] := csCheckedNormal;
      4: Sender.CheckState[Node] := csCheckedDisabled;
    end;//case
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2GetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex; var ImageList: TCustomImageList);

var
  Data: PNodeData2;

begin
  // For this demo only the normal image is shown, you can easily
  // change this for the state and overlay images.
  case Kind of
    ikNormal, ikSelected:
      begin
        Data := Sender.GetNodeData(Node);
        Ghosted := Node.Index = 1;
        case Column of
          -1, // general case
          0:  // main column
            ImageIndex := Data.Level + 7;
          1: // image only column
            if Sender.FocusedNode = Node then
              ImageIndex := 6;
        end;
      end;
    ikOverlay:
      begin
        // Enable this code to show an arbitrary overlay for each image.
        // Note the high overlay index. Standard overlays only go up to 15.
        // Virtual Treeview allows for any number.
        // ImageList := ImageList1;
        // ImageIndex := 58;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

// The tree is set to 5 levels a 5 children (~4000 nodes).

var
  Data: PNodeData2;
  
begin
  Data := Sender.GetNodeData(Node);
  if Data.Level < LevelCount then
    ChildCount := 5;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: string);

// The caption of a node has been changed, keep this in the node record.

var
  Data: PNodeData2;

begin
  Data := Sender.GetNodeData(Node);
  Data.Caption := NewText;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.BitBtn1Click(Sender: TObject);

begin
  with FontDialog1 do
  begin
    Font := VST2.Font;
    if Execute then
      VST2.Font := Font;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.MainColumnUpDownChanging(Sender: TObject; var AllowChange: Boolean);

begin
  VST2.Header.MainColumn := MainColumnUpDown.Position;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2GetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);

begin
  case Column of
    0:
      PopupMenu := PopupMenu1
  else
    PopupMenu := nil;
  end;                       
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if ssCtrl in Shift then
    case Key of
      Ord('C'):
        VST2.CopyToClipboard;
      Ord('X'):
        VST2.CutToClipboard;
    end
  else
    case Key of
      VK_DELETE:
        if Assigned(VST2.FocusedNode) then
          VST2.DeleteNode(VST2.FocusedNode);
      VK_INSERT:
        if Assigned(VST2.FocusedNode) then
          VST2.AddChild(VST2.FocusedNode);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.RadioGroup1Click(Sender: TObject);

begin
  with Sender as TRadioGroup do
    if ItemIndex = 0 then
    begin
      VST2.TreeOptions.PaintOptions := VST2.TreeOptions.PaintOptions + [TVTPaintOption.toShowTreeLines];
      VST2.ButtonStyle := bsRectangle;
    end
    else
    begin
      VST2.TreeOptions.PaintOptions := VST2.TreeOptions.PaintOptions - [TVTPaintOption.toShowTreeLines];
      VST2.ButtonStyle := bsTriangle;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2FocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);

begin
  Allowed := (NewColumn <= 0) or (NewColumn = 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.RadioGroup2Click(Sender: TObject);

begin                                     
  with Sender as TRadioGroup do
    if ItemIndex = 0 then
    begin
      VST2.DrawSelectionMode := smDottedRectangle;
    end
    else
    begin
      VST2.DrawSelectionMode := smBlendedRectangle;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.ThemeRadioGroupClick(Sender: TObject);

begin
  with VST2.TreeOptions do
    if ThemeRadioGroup.ItemIndex = 0 then
    begin
      PaintOptions := PaintOptions + [TVTPaintOption.toThemeAware];
      VST2.CheckImageKind := ckSystemDefault;
    end
    else
      PaintOptions := PaintOptions - [TVTPaintOption.toThemeAware];

  RadioGroup1.Enabled := ThemeRadioGroup.ItemIndex = 1;
  RadioGroup2.Enabled := ThemeRadioGroup.ItemIndex = 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.SaveButtonClick(Sender: TObject);

const
  HTMLHead : String = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">'#13#10 +
    '<html>'#13#10 +
    '  <head>'#13#10 +
    '    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">'#13#10 +
    '    <title>Virtual Treeview export</title>'#13#10 +
    '  </head>'#13#10 +
    '<body>'#13#10;

var
  lText: String;
  lTargetName: string;

  procedure Save(pEconding: TEncoding);
  begin
    With TStreamWriter.Create(lTargetName, False, pEconding) do
      try
        Write(lText);
      finally
        Free;
      end;
  end;


begin
  with SaveDialog do
  begin
    if Execute then
    begin
      lTargetName := FileName;
      case FilterIndex of
      1: // HTML
        begin
          if Pos('.', lTargetName) = 0 then
            lTargetName := lTargetName + '.html';
          lText := HTMLHead + VST2.ContentToHTML(tstVisible) + '</body></html>';
          Save(TEncoding.UTF8);
        end;//HTML
      2: // Unicode UTF-16 text file
        begin
          lTargetName := ChangeFileExt(lTargetName, '.uni');
          lText := VST2.ContentToText(tstVisible, #9);
          Save(TEncoding.Unicode);
        end;
      3: // Rich text file
        begin
          lTargetName := ChangeFileExt(lTargetName, '.rtf');
          With TStreamWriter.Create(lTargetName, False, TEncoding.ASCII) do
            try
              Write(VST2.ContentToRTF(tstVisible));
            finally
              Free;
            end;
        end;
      4: // Comma separated values ANSI text file
        begin
          lTargetName := ChangeFileExt(lTargetName, '.csv');
          lText := VST2.ContentToText(tstVisible, FormatSettings.ListSeparator);
          Save(TEncoding.UTF8);
        end;
      else
        // Plain text file
        lTargetName := ChangeFileExt(lTargetName, '.txt');
        lText := VST2.ContentToText(tstVisible, #9);
        Save(TEncoding.ANSI);
      end;//case
    end;//if
  end;//With
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2StateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2DragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
  Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

begin
  Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGeneralForm.VST2FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PNodeData2;

begin
  Data := Sender.GetNodeData(Node);
  Finalize(data^);
end;

end.
