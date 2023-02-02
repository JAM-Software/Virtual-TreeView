unit AlignDemo;

// Virtual Treeview sample form demonstrating following features:
//   - Header with images and different glyph and column alignment.
//   - Header popup with images.
//   - Multilingual treeview with english, greek, hebrew and arabic texts.
//   - Interaction between column alignment and column directionality (bidi).
// Written by Mike Lischke.

interface

// For some things to work we need code, which is classified as being unsafe for .NET.
{$warn UNSAFE_TYPE off}
{$warn UNSAFE_CAST off}
{$warn UNSAFE_CODE off}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, VirtualTrees, ComCtrls, ExtCtrls, ImgList, Menus, UITypes, VirtualTrees.Types, System.ImageList;

type
  TAlignForm = class(TForm)
    AlignTree: TVirtualStringTree;
    Label8: TLabel;
    TreeImages: TImageList;
    HeaderImages: TImageList;
    IconPopup: TPopupMenu;
    Label1: TLabel;
    AlignCombo0: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    AlignCombo1: TComboBox;
    Label4: TLabel;
    AlignCombo2: TComboBox;
    BidiGroup0: TRadioGroup;
    BidiGroup1: TRadioGroup;
    BidiGroup2: TRadioGroup;
    GroupBox1: TGroupBox;
    ShowGlyphsOptionBox: TCheckBox;
    HotTrackOptionBox: TCheckBox;
    ShowTextOptionBox: TCheckBox;
    VisibleOptionBox: TCheckBox;
    EnabledOptionBox: TCheckBox;
    Label5: TLabel;
    LayoutCombo: TComboBox;
    procedure AlignTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: TImageIndex);
    procedure AlignTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure AlignTreePaintText(Sender: TBaseVirtualTree; const Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure AlignTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure AlignTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure AlignTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IconPopupPopup(Sender: TObject);
    procedure AlignComboChange(Sender: TObject);
    procedure BidiGroupClick(Sender: TObject);
    procedure AlignTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure OptionBoxClick(Sender: TObject);
    procedure LayoutComboChange(Sender: TObject);
    procedure AlignTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure AlignTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure AlignTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FArabicFont,
    FHebrewFont: TFont;
    procedure ChangeHeaderText;
    procedure MeasureIconItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure MenuItemClick(Sender: TObject);
  end;

var
  AlignForm: TAlignForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Main, States;
  
{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

const
  DefaultHintColumn0 = 'Text is initially centered and has a left-to-right directionality.';
  DefaultHintColumn1 = 'Text is initially left aligned and has a left-to-right directionality.';
  DefaultHintColumn2 = 'Text is initially left aligned and has a right-to-left directionality.';
  CommonHeaderHint = 'Right click to pick a column glyph. Left click to switch sort glyph (no sorting is performed).';

type
  PAlignData = ^TAlignData;
  TAlignData = record
    MainColumnText,
    GreekText,
    RTLText: UnicodeString;
    ImageIndex: Integer;
  end;

// These arrays store some text which is originally displayed right-to-left, so it supports the demonstration of
// alignment even more than normal text. This text will be filled at runtime from a resource file.
// Additionally, some greek text for another column is stored here too just because I like how it looks (the text,
// not the storage ;-)).
var
  GreekStrings: array[0..8] of UnicodeString;
  ArabicStrings: array[0..3] of UnicodeString;
  HebrewStrings: array[0..2] of UnicodeString;

//----------------------------------------------------------------------------------------------------------------------

procedure LoadStrings;

// Helper routine to load Unicode strings from the resource. Putting these strings directly into the
// source code does not work, since Delphi does not support Unicode source code.

begin
  // Take the first arabic string as identification whether we have already loaded the strings or not.
  if Length(ArabicStrings[0]) = 0 then
  begin
    LoadUnicodeStrings('Greek', GreekStrings);
    LoadUnicodeStrings('Arabic', ArabicStrings);
    LoadUnicodeStrings('Hebrew', HebrewStrings);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);

// Node data size can also be set at design time (if you know the size of the record) or in FormCreate.
// We do it here just because to show this third way too.

begin
  NodeDataSize := SizeOf(TAlignData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreePaintText(Sender: TBaseVirtualTree; const Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);

begin
  // In order to display arabic and hebrew texts with a nice font we have assign one explicitely. Otherwise the
  // system picks one and this often leads to non-ideal results.
  case Column of
    1:
      // Make the second column lighter.
      Canvas.Font.Color := clSilver;
    2:
      begin
        if not Odd(Sender.NodeParent[Node].Index) then
          Canvas.Font := FArabicFont
        else
          Canvas.Font := FHebrewFont;
      end;
  end;

  // Reset the text color for selected and drop target nodes.
  if ((Node = Sender.DropTargetNode) or (vsSelected in Node.States)) and (Column = Sender.FocusedColumn) then
    Canvas.Font.Color := clHighlightText;
  if Sender.NodeParent[Node] = nil then
    Canvas.Font.Style := [fsBold];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);

var
  Data: PAlignData;

begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0: // left alignd column
      CellText := Data.MainColumnText;
    1: // centered column
      CellText := Data.GreekText;
    2: // right aligned column
      CellText := Data.RTLText;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex);

var
  Data: PAlignData;

begin
  if Kind in [ikNormal, ikSelected] then
  begin
    Data := Sender.GetNodeData(Node);
    Index := Data.ImageIndex;
  end;
  if (Kind = ikState) and (Column = Sender.Header.MainColumn) then
    Index := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PAlignData;

begin
  // intialize caption strings
  LoadStrings;

  Data := Sender.GetNodeData(Node);

  Data.ImageIndex := 0;                          
  if ParentNode = nil then
  begin
    with Data^ do
    begin
      if not Odd(Node.Index) then
        MainColumnText := 'Arabic texts'
      else
        MainColumnText := 'Hebrew texts';

      GreekText := GreekStrings[(Node.Index and 1) * 5];
      RTLText := '';
    end;
    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
  end
  else
  begin
    if not Odd(ParentNode.Index) then
    begin
      with Data^ do
      begin
        MainColumnText := Format('Arabic text %d', [Node.Index]);
        GreekText := GreekStrings[Node.Index + 1];
        RTLText := ArabicStrings[Node.Index];
      end;
    end
    else
    begin
      with Data^ do
      begin
        MainColumnText := Format('Hebrew text %d', [Node.Index]);
        GreekText := GreekStrings[6 + Node.Index];
        RTLText := HebrewStrings[Node.Index];
      end;
    end;
  end;
  Node.CheckType := TCheckType.ctTriStateCheckBox;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

begin
  if not Odd(Node.Index) then
    ChildCount := 4 // arabic text
  else
    ChildCount := 3; // hebrew text
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.FormCreate(Sender: TObject);

var
 I: Integer;
 NewItem: TMenuItem;

begin
  // High color image lists look much better.
  ConvertToHighColor(TreeImages);
  ConvertToHighColor(HeaderImages);

  // To display the various texts in a nice manner we use some specialized fonts of the system.
  // We could directly assign the font names used here in the OnPaintText event, but since this
  // would then be the only reference for the font it would cause the font to be recreated every
  // time it is used (a font is reference counted in Graphics.pas). In order to avoid this overhead
  // it is better to create the fonts once and for all.
  // Note: if the fonts used here are not installed on the target system then the font mapper of Windows
  //       will pick similar fonts which are capable of rendering the required glyphs (however Arial and Times New Roman
  //       should be available on any Windows system).
  FArabicFont := TFont.Create;
  with FArabicFont do
  begin
    if Screen.Fonts.IndexOf('Traditional Arabic') > -1 then
    begin
      Name := 'Traditional Arabic';
      Size := 12;
    end
    else
    begin
      Name := 'Arial';
      Size := 14;
    end;
    Color := $FF6B43;
    if Handle = 0 then
      Beep;
  end;
  FHebrewFont := TFont.Create;
  with FHebrewFont do
  begin
    Name := 'Times New Roman';
    Size := 14;
    Color := $436BFF;
  end;

  // To demonstrate header clicks together with the header menu a glyph picker menu is provided.
  with IconPopup do
  begin
    for I := 0 to HeaderImages.Count - 1 do
    begin
      NewItem := TMenuItem.Create(Self);
      NewItem.Caption := '';
      NewItem.ImageIndex := I;
      NewItem.RadioItem := True;
      NewItem.OnClick := MenuItemClick;
      if (I mod 10) = 0 then
        NewItem.Break := mbBreak;
      NewItem.OnMeasureItem := MeasureIconItem;
      Items.Add(NewItem);
    end;
  end;

  // Add some additional info to the column hints. This can only be done in code as the object inspector does not
  // allow to enter multiline strings (it does not allow to edit wide strings correctly at all).
  with AlignTree.Header do
  begin
    Columns[0].Hint := DefaultHintColumn0 + #13 + CommonHeaderHint;
    Columns[1].Hint := DefaultHintColumn1 + #13 + CommonHeaderHint;
    Columns[2].Hint := DefaultHintColumn2 + #13 + CommonHeaderHint;
  end;

  // Set up the initial values of the alignment and bidi-mode pickers as well as layout and options.
  with AlignTree.Header do
  begin
    // Alignment and bidi
    AlignCombo0.ItemIndex := Ord(Columns[0].Alignment);
    BidiGroup0.ItemIndex := Ord(Columns[0].BidiMode <> bdLeftToRight);
    AlignCombo1.ItemIndex := Ord(Columns[1].Alignment);
    BidiGroup1.ItemIndex := Ord(Columns[1].BidiMode <> bdLeftToRight);
    AlignCombo2.ItemIndex := Ord(Columns[2].Alignment);
    BidiGroup2.ItemIndex := Ord(Columns[2].BidiMode <> bdLeftToRight);

    // Button layout
    LayoutCombo.ItemIndex := Ord(Columns[0].Layout);
    if not (TVTHeaderOption.hoShowImages in Options) then
      Height := 24
    else
      if Columns[0].Layout in [TVTHeaderColumnLayout.blGlyphTop, TVTHeaderColumnLayout.blGlyphBottom] then
        Height := 64
      else
        Height := 40;

    // Options
    ShowGlyphsOptionBox.Checked := TVTHeaderOption.hoShowImages in Options;
    HotTrackOptionBox.Checked := TVTHeaderOption.hoHotTrack in Options;
    ShowTextOptionBox.Checked := True;
    ChangeHeaderText;
    VisibleOptionBox.Checked := TVTHeaderOption.hoVisible in Options;
    EnabledOptionBox.Checked := TVTColumnOption.coEnabled in Columns[0].Options;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.FormDestroy(Sender: TObject);

begin
  FArabicFont.Free;
  FHebrewFont.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.ChangeHeaderText;

// Sets or clears the text of all columns depending on the state of SetTextOptionBox.

begin
  with AlignTree.Header do
    if ShowTextOptionBox.Checked then
    begin
      Columns[0].Text := 'English text column';
      Columns[1].Text := 'Greek text column';
      Columns[2].Text := 'Hebrew/arabic text column';
    end
    else
    begin
      Columns[0].Text := '';
      Columns[1].Text := '';
      Columns[2].Text := '';
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.MeasureIconItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

// Used to tell the popup menu how large it is. I don't want menu item captions so the menu item size is
// made as small as possible here.

begin
  // The icons are 32 bits wide but some extra space will be added implicitely.
  Width := 24;
  Height := 36;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.MenuItemClick(Sender: TObject);

// During the the right click on the header the clicked column is recorded in Tree.Header.Columns.ClickIndex.
// We can use this information to determine to which column the new image index must be assigned. 

var
  Index: Integer;

begin
  with AlignTree.Header do
  begin
    Index := Columns.ClickIndex;
    if Index > NoColumn then
    begin
      (Sender as TMenuItem).Checked := True;
      Columns[Index].ImageIndex := (Sender as TMenuItem).ImageIndex;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.IconPopupPopup(Sender: TObject);

// Mark the selected image before presenting the popup to the user.

var
  Index: Integer;

begin
  with AlignTree.Header do
  begin
    Index := Columns.ClickIndex;
    if Index > NoColumn then
      (Sender as TPopupMenu).Items[Columns[Index].ImageIndex].Checked := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignComboChange(Sender: TObject);

begin
  with Sender as TComboBox do
    case Tag of
      0:
        AlignTree.Header.Columns[0].Alignment := TAlignment(AlignCombo0.ItemIndex);
      1:
        AlignTree.Header.Columns[1].Alignment := TAlignment(AlignCombo1.ItemIndex);
      2:
        AlignTree.Header.Columns[2].Alignment := TAlignment(AlignCombo2.ItemIndex);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.BidiGroupClick(Sender: TObject);

begin
  with Sender as TRadioGroup do
    case Tag of
      0:
        AlignTree.Header.Columns[0].BidiMode := TBidiMode(BidiGroup0.ItemIndex);
      1:
        AlignTree.Header.Columns[1].BidiMode := TBidiMode(BidiGroup1.ItemIndex);
      2:
        AlignTree.Header.Columns[2].BidiMode := TBidiMode(BidiGroup2.ItemIndex);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

// This method sets sort column and direction on a header click.
// Note: this is only to show the header layout. There gets nothing really sorted.

begin
  if HitInfo.Button = mbLeft then
  begin
    with Sender do
    begin
      if SortColumn <> HitInfo.Column then
      begin
        SortColumn := HitInfo.Column;
        SortDirection := sdAscending;
      end
      else
      case SortDirection of
        sdAscending:
          SortDirection := sdDescending;
        sdDescending:
          SortColumn := NoColumn;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.OptionBoxClick(Sender: TObject);

var
  I: Integer;
  
begin
  with Sender as TCheckBox, AlignTree.Header do
    case Tag of
      0:
        if Checked then
        begin
          Options := Options + [TVTHeaderOption.hoShowImages];
          if Columns[0].Layout in [TVTHeaderColumnLayout.blGlyphTop, TVTHeaderColumnLayout.blGlyphBottom] then
            Height := 64
          else
            Height := 40;
        end
        else
        begin
          Options := Options - [TVTHeaderOption.hoShowImages];
          Height := 24;
        end;
      1:
        if Checked then
          Options := Options + [TVTHeaderOption.hoHotTrack]
        else
          Options := Options - [TVTHeaderOption.hoHotTrack];
      2:
        ChangeHeaderText;
      3:
        if Checked then
          Options := Options + [TVTHeaderOption.hoVisible]
        else
          Options := Options - [TVTHeaderOption.hoVisible];
      4:
        for I := 0 to Columns.Count - 1 do
          if Checked then
            Columns[I].Options := Columns[I].Options + [TVTColumnOption.coEnabled]
          else
            Columns[I].Options := Columns[I].Options - [TVTColumnOption.coEnabled];
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.LayoutComboChange(Sender: TObject);

var
  I: Integer;

begin
  with Sender as TComboBox, AlignTree.Header do
  begin
    for I := 0 to Columns.Count - 1 do
      Columns[I].Layout := TVTHeaderColumnLayout(ItemIndex);

    if not (TVTHeaderOption.hoShowImages in Options) then
      Height := 24
    else
      if Columns[0].Layout in [TVTHeaderColumnLayout.blGlyphTop, TVTHeaderColumnLayout.blGlyphBottom] then
        Height := 64
      else
        Height := 40;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

const
  FocusedText = #13'Text of focused node is: ';

var
  Data: PAlignData;

begin
  if Assigned(Node) then
  begin
    Data := Sender.GetNodeData(Node);
    with AlignTree.Header do
    begin
      Columns[0].Hint := DefaultHintColumn0 + #13 + CommonHeaderHint + FocusedText + Data.MainColumnText;
      Columns[1].Hint := DefaultHintColumn1 + #13 + CommonHeaderHint + FocusedText + Data.GreekText;
      Columns[2].Hint := DefaultHintColumn2 + #13 + CommonHeaderHint + FocusedText + Data.RTLText;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAlignForm.AlignTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PAlignData;

begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

end.
