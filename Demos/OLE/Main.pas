unit Main;
 
// Virtual Treeview sample application demonstrating clipboard and drag'n drop operations.
// The treeview uses OLE for these operations but can also issue and accept VCL drag'n drop.
// Written by Mike Lischke.

interface

uses 
  Windows, Messages, ActiveX, SysUtils, Forms, Dialogs, Graphics, 
  VirtualTrees, ActnList, ComCtrls, ExtCtrls, StdCtrls, Controls, Classes,
  ImgList, System.Actions;

type
  TMainForm = class(TForm)
    ActionList1: TActionList;
    CutAction: TAction;
    CopyAction: TAction;
    PasteAction: TAction;
    FontDialog: TFontDialog;
    Panel3: TPanel;
    Label6: TLabel;
    Button1: TButton;
    Button3: TButton;
    Tree2: TVirtualStringTree;
    Label1: TLabel;
    Tree1: TVirtualStringTree;
    Label2: TLabel;
    PageControl1: TPageControl;
    LogTabSheet: TTabSheet;
    RichTextTabSheet: TTabSheet;
    LogListBox: TListBox;
    RichEdit1: TRichEdit;
    Label3: TLabel;
    Label7: TLabel;
    Button2: TButton;
    TabSheet1: TTabSheet;
    Label8: TLabel;
    TabSheet2: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    TreeImages: TImageList;
    procedure Button1Click(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure Tree1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure FormCreate(Sender: TObject);
    procedure TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure Button2Click(Sender: TObject);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure Tree1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure Button3Click(Sender: TObject);
    procedure Tree2DragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
      Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure Tree2BeforeItemErase(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
  private
    procedure AddUnicodeText(DataObject: IDataObject; Target: TVirtualStringTree; Mode: TVTNodeAttachMode);
    procedure AddVCLText(Target: TVirtualStringTree; const Text: UnicodeString; Mode: TVTNodeAttachMode);
    function FindCPFormatDescription(CPFormat: Word): string;
    procedure InsertData(Sender: TVirtualStringTree; DataObject: IDataObject; Formats: TFormatArray; Effect: Integer;
      Mode: TVTNodeAttachMode);
  end;

var
  MainForm: TMainForm;

//----------------------------------------------------------------------------------------------------------------------

implementation                          

uses
  TypInfo, ShlObj, UrlMon, VirtualTrees.ClipBoard;
  
{$R *.DFM}
{$R Res\Extra.res}  // Contains a little rich text for the rich edit control and a XP manifest.

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Caption: UnicodeString;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Button1Click(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CutActionExecute(Sender: TObject);

begin
  if ActiveControl = Tree1 then
    Tree1.CutToClipboard
  else
    if ActiveControl = Tree2 then
      Tree2.CutToClipboard
    else                                                           
      if ActiveControl = RichEdit1 then
        RichEdit1.CutToClipboard;                               
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CopyActionExecute(Sender: TObject);

begin
  if ActiveControl = Tree1 then
    Tree1.CopyToClipboard
  else
    if ActiveControl = Tree2 then
      Tree2.CopyToClipboard
    else
      if ActiveControl = RichEdit1 then
        RichEdit1.CopyToClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PasteActionExecute(Sender: TObject);

var
  DataObject: IDataObject;
  EnumFormat: IEnumFormatEtc;
  Format: TFormatEtc;
  Formats: TFormatArray;
  Fetched: Integer;
  Tree: TVirtualStringTree;
  
begin
  if ActiveControl is TVirtualStringTree then
  begin
    Tree := ActiveControl as TVirtualStringTree;
    
    if LogListBox.Items.Count > 0 then
      LogListBox.Items.Add('');
    if ActiveControl = Tree1 then
      LogListBox.Items.Add('----- Tree 1')
    else
      LogListBox.Items.Add('----- Tree 2');

    if Tree.PasteFromClipboard then
      LogListBox.Items.Add('Native tree data pasted.')
    else
    begin
      LogListBox.Items.Add('Other data pasted.');
      // Some other data was pasted. Enumerate the available formats and try to add the data.
      // 1) Get a data object for the data.
      OLEGetClipboard(DataObject);
      // 2) Enumerate all offered formats and create a format array from it which can be used in InsertData.
      if Succeeded(DataObject.EnumFormatEtc(DATADIR_GET, EnumFormat)) then
      begin
        EnumFormat.Reset;
        while EnumFormat.Next(1, Format, @Fetched) = S_OK do
        begin
          SetLength(Formats, Length(Formats) + 1);
          Formats[High(Formats)] := Format.cfFormat;
        end;

        InsertData(Tree, DataObject, Formats, DROPEFFECT_COPY, Tree.DefaultPasteMode);
      end;
    end;
  end
  else
    if ActiveControl = RichEdit1 then
      RichEdit1.PasteFromClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Tree1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

var
  Data: PNodeData;

begin
  if TextType = ttNormal then
  begin
    Data := Sender.GetNodeData(Node);
    CellText := Data.Caption;
  end
  else
    Text := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

var
  Stream: TResourceStream;

begin
  Tree1.NodeDataSize := SizeOf(TNodeData);
  Tree1.RootNodeCount := 30;
  Tree2.NodeDataSize := SizeOf(TNodeData);
  Tree2.RootNodeCount := 30;

  // There is a small RTF text stored in the resource to have something to display in the rich edit control.
  Stream := TResourceStream.Create(HInstance, 'RTF', 'RCDATA');
  try
    RichEdit1.Lines.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.AddUnicodeText(DataObject: IDataObject; Target: TVirtualStringTree; Mode: TVTNodeAttachMode);

// This method is called when the drop handler gets called with Unicode text as only
// understandable clipboard format. This text is retrieved and splitted in lines.
// Every line is then added as new node.

var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  OLEData,
  Head, Tail: PWideChar;
  TargetNode,
  Node: PVirtualNode;
  Data: PNodeData;
  
begin
  if Mode <> amNowhere then
  begin
    // fill the structure used to get the Unicode string
    with FormatEtc do
    begin
      cfFormat := CF_UNICODETEXT;
      // no specific target device
      ptd := nil;
      // normal content to render
      dwAspect := DVASPECT_CONTENT;
      // no specific page of multipage data
      lindex := -1;
      // pass the data via memory
      tymed := TYMED_HGLOBAL;
    end;

    // Check if we can get the Unicode text data.
    if DataObject.QueryGetData(FormatEtc) = S_OK then
    begin
      // Data is accessible so finally get a pointer to it
      if DataObject.GetData(FormatEtc, Medium) = S_OK then
      begin
        OLEData := GlobalLock(Medium.hGlobal);
        if Assigned(OLEData) then
        begin
          Target.BeginUpdate;
          TargetNode := Target.DropTargetNode;
          if TargetNode = nil then
            TargetNode := Target.FocusedNode;
            
          Head := OLEData;
          try
            while Head^ <> #0 do
            begin
              Tail := Head;
              while not CharInSet(Tail^, [WideChar(#0), WideChar(#13), WideChar(#10), WideChar(#9)]) do
                Inc(Tail);
              if Head <> Tail then
              begin
                // add a new node if we got a non-empty caption
                Node := Target.InsertNode(TargetNode, Mode);
                Data := Target.GetNodeData(Node);
                SetString(Data.Caption, Head, Tail - Head);
              end;
              // Skip any tab.
              if Tail^ = #9 then
                Inc(Tail);
              // skip line separators
              if Tail^ = #13 then
                Inc(Tail);
              if Tail^ = #10 then
                Inc(Tail);
              Head := Tail;
            end;
          finally
            GlobalUnlock(Medium.hGlobal);
            Target.EndUpdate;
          end;
        end;
        // never forget to free the storage medium
        ReleaseStgMedium(Medium);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.AddVCLText(Target: TVirtualStringTree; const Text: UnicodeString; Mode: TVTNodeAttachMode);

// This method is called when the drop handler gets called with a VCL drag source.
// The given text is retrieved and splitted in lines.

var
  Head, Tail: PWideChar;
  TargetNode,
  Node: PVirtualNode;
  Data: PNodeData;
  
begin
  if Mode <> amNowhere then
  begin
    Target.BeginUpdate;
    try
      TargetNode := Target.DropTargetNode;
      if TargetNode = nil then
        TargetNode := Target.FocusedNode;

      Head := PWideChar(Text);
      while Head^ <> #0 do
      begin
        Tail := Head;
        while not CharInSet(Tail^, [WideChar(#0), WideChar(#13), WideChar(#10)]) do
          Inc(Tail);
        if Head <> Tail then
        begin
          // add a new node if we got a non-empty caption
          Node := Target.InsertNode(TargetNode, Mode);
          Data := Target.GetNodeData(Node);
          SetString(Data.Caption, Head, Tail - Head);
        end;
        // skip line separators
        if Tail^ = #13 then
          Inc(Tail);
        if Tail^ = #10 then
          Inc(Tail);
        Head := Tail;
      end;
    finally
      Target.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMainForm.FindCPFormatDescription(CPFormat: Word): string;

var
  Buffer: array[0..2048] of Char;

begin
  // Try the formats support the by Virtual Treeview first.
  Result := GetVTClipboardFormatDescription(CPFormat);

  // Retrieve additional formats from system.
  if Length(Result) = 0 then
  begin
    if GetClipboardFormatName(CPFormat, @Buffer, 2048) > 0 then
      Result := '  - ' + Buffer
    else
      Result := Format('  - unknown format (%d)', [CPFormat]);
  end
  else
    Result := '  - ' + Result;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);

  //--------------- local function --------------------------------------------

  procedure DetermineEffect;

  // Determine the drop effect to use if the source is a Virtual Treeview.

  begin
    // In the case the source is a Virtual Treeview we know 'move' is the default if dragging within
    // the same tree and copy if dragging to another tree. Set Effect accordingly.
    if Shift = [] then
    begin
      // No modifier key, so use standard action.
      if Source = Sender then
        Effect := DROPEFFECT_MOVE
      else
        Effect := DROPEFFECT_COPY;     
    end
    else
    begin
      // A modifier key is pressed, hence use this to determine action.
      if (Shift = [ssAlt]) or (Shift = [ssCtrl, ssAlt]) then
        Effect := DROPEFFECT_LINK
      else
        if Shift = [ssCtrl] then
          Effect := DROPEFFECT_COPY
        else
          Effect := DROPEFFECT_MOVE;
    end;
  end;

  //--------------- end local function ----------------------------------------

var
  S: string;
  Attachmode: TVTNodeAttachMode;
  Nodes: TNodeArray;
  I: Integer;

begin
  Nodes := nil;
  
  if LogListBox.Items.Count > 0 then
    LogListBox.Items.Add('');
  if Sender = Tree1 then
    LogListBox.Items.Add('----- Tree 1')
  else
    LogListBox.Items.Add('----- Tree 2');
    
  if DataObject = nil then
    LogListBox.Items.Add('VCL drop arrived')
  else
    LogListBox.Items.Add('OLE drop arrived');

  S := 'Drop actions allowed:';
  if Boolean(DROPEFFECT_COPY and Effect) then
    S := S + ' copy';
  if Boolean(DROPEFFECT_MOVE and Effect) then
    S := S + ' move';
  if Boolean(DROPEFFECT_LINK and Effect) then
    S := S + ' link'; 
  LogListBox.Items.Add(S);

  S := 'Drop mode: ' + GetEnumName(TypeInfo(TDropMode), Ord(Mode));
  LogListBox.Items.Add(S);

  // Translate the drop position into an node attach mode.
  case Mode of
    dmAbove:
      AttachMode := amInsertBefore;
    dmOnNode:
      AttachMode := amAddChildLast;
    dmBelow:
      AttachMode := amInsertAfter;
  else
    AttachMode := amNowhere;
  end;

  if DataObject = nil then
  begin
    // VCL drag'n drop. Handling this requires detailed knowledge about the sender and its data. This is one reason
    // why it was a bad decision by Borland to implement something own instead using the system's way.
    // In this demo we have two known sources of VCL dd data: Tree2 and LogListBox. 
    if Source = Tree2 then
    begin
      // Since we know this is a Virtual Treeview we can ignore the drop event entirely and use VT mechanisms.
      DetermineEffect;
      Nodes := Tree2.GetSortedSelection(True);
      if Effect = DROPEFFECT_COPY then
      begin
        for I := 0 to High(Nodes) do
          Tree2.CopyTo(Nodes[I], Sender.DropTargetNode, AttachMode, False);
      end
      else
        for I := 0 to High(Nodes) do
          Tree2.MoveTo(Nodes[I], Sender.DropTargetNode, AttachMode, False);
    end
    else
    begin
      // One long string (one node) is added, containing all text currently in the list box.
      AddVCLText(Sender as TVirtualStringTree, LogListBox.Items.CommaText, AttachMode);
      LogListBox.Items.Add('List box data accepted as string.');
    end;
  end
  else
  begin
    // OLE drag'n drop. Perform full processing.
    
    LogListBox.Items.Add('There are ' + IntToStr(Length(Formats)) + ' formats available:');

    // Determine action in advance even if we don't use the dropped data.
    // Note: The Effect parameter is a variable which must be set to the action we
    //       will actually take, to notify the sender of the drag operation about remaining actions.
    //       This value determines what the caller will do after the method returns,
    //       e.g. if DROPEFFECT_MOVE is returned then the source data will be deleted.
    if Source is TBaseVirtualTree then
    begin
      DetermineEffect;
    end
    else
      // Prefer copy if allowed for every other drag source. Alone from Effect you cannot determine the standard action
      // of the sender, but we assume if copy is allowed then it is also the standard action
      // (e.g. as in TRichEdit).
      if Boolean(Effect and DROPEFFECT_COPY) then
        Effect := DROPEFFECT_COPY
      else
        Effect := DROPEFFECT_MOVE;

    InsertData(Sender as TVirtualStringTree, DataObject, Formats, Effect, AttachMode);
  end;

  // scroll last added entry into view
  LogListBox.ItemIndex := LogListBox.Items.Count - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Button2Click(Sender: TObject);

begin
  LogListBox.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PNodeData;

begin
  Data := Sender.GetNodeData(Node);
  // set a generic caption only if there is not already one (e.g. from drag operations)
  if Length(Data.Caption) = 0 then
    Data.Caption := Format('Node Index %d', [Node.Index]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Tree1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);

var
  Data: PNodeData;

// Tree1 as well as Tree2 use the soSaveCaptions StringOption which enables automatic caption store action
// when tree data is serialized into memory (e.g. for drag'n drop). Restoring the caption is done by triggering
// this event for each loaded node.
// This mechanism frees us from implementing a SaveNode and LoadNode event since we have only the caption to store.

begin
  Data := Sender.GetNodeData(Node);
  Data.Caption := NewText;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Button3Click(Sender: TObject);

begin
  with FontDialog do
  begin
    Font := Tree1.Font;
    if Execute then
    begin
      Tree1.Font := Font;
      Tree2.Font := Font;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Tree2DragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

// Tree 2 uses manual drag start to tell which node might be dragged.
 
begin
  Allowed := Odd(Node.Index);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
  Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

begin
  Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Tree2BeforeItemErase(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);

// The second tree uses manual drag and we want to show the lines which are allowed to start a drag operation by
// a colored background.

begin
  if Odd(Node.Index) then
  begin
    ItemColor := $FFEEEE;
    EraseAction := eaColor;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.InsertData(Sender: TVirtualStringTree; DataObject: IDataObject; Formats: TFormatArray;
  Effect: Integer; Mode: TVTNodeAttachMode);

var
  FormatAccepted: Boolean;
  I: Integer;
  
begin
  // Go through each available format and see if we can make sense of it.
  FormatAccepted := False;
  for I := 0 to High(Formats) do
  begin
    case Formats[I] of
      // standard clipboard formats
      CF_UNICODETEXT:
        begin
          LogListBox.Items.Add('  - Unicode text');

          // As demonstration for non-tree data here an implementation for Unicode text.
          // Formats are placed in preferred order in the formats parameter. Hence if
          // there is native tree data involved in this drop operation then it has been
          // caught earlier in the loop and FormatAccepted is already True.
          if not FormatAccepted then
          begin
            // Unicode text data was dropped (e.g. from RichEdit1) add this line by line
            // as new nodes.
            AddUnicodeText(DataObject, Sender as TVirtualStringTree, Mode);
            LogListBox.Items.Add('+ Unicode accepted');
            FormatAccepted := True;
          end;
        end;
    else
      if Formats[I] = CF_VIRTUALTREE then
      begin
        // this is our native tree format
        LogListBox.Items.Add('  - native Virtual Treeview data');

        if not FormatAccepted then
        begin
          Sender.ProcessDrop(DataObject, Sender.DropTargetNode, Effect, Mode);
          LogListBox.Items.Add('+ native Virtual Treeview data accepted');
          // Indicate that we found a format we accepted so the data is not used twice.
          FormatAccepted := True;
        end;
      end
      else
        if Formats[I] = CF_VTREFERENCE then
          LogListBox.Items.Add('  - Virtual Treeview reference')
        else
        begin
          // Predefined, shell specific, MIME specific or application specific clipboard data.
          LogListBox.Items.Add(FindCPFormatDescription(Formats[I]));
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.






