unit PropertiesDemo;

// Virtual Treeview sample form demonstrating following features:
//   - Property page like string tree with individual node editors.
//   - Incremental search.
// Written by Mike Lischke.
{$WARN UNSAFE_CODE OFF} // Prevent warnins that are not applicable 

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, ImgList, ExtCtrls, UITypes, VirtualTrees.BaseTree, System.ImageList,
  VirtualTrees.Types;

const
  // Helper message to decouple node change handling from edit handling.
  WM_STARTEDITING = WM_USER + 778;
  
type
  TPropertiesForm = class(TForm)
    VST3: TVirtualStringTree;
    Label9: TLabel;
    Label10: TLabel;
    TreeImages: TImageList;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure VST3Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VST3CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VST3Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure VST3GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure VST3GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: TImageIndex);
    procedure VST3GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure VST3InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VST3InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VST3PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VST3IncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string;
      var Result: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure VST3StateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure VST3FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    procedure WMStartEditing(var Message: TMessage); message WM_STARTEDITING;
  end;

var
  PropertiesForm: TPropertiesForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Editors, Math, Main, States;

{$R *.DFM}

//----------------- TPropertiesForm ------------------------------------------------------------------------------------

procedure TPropertiesForm.FormCreate(Sender: TObject);

begin
  // We assign these handlers manually to keep the demo source code compatible
  // with older Delphi versions after using UnicodeString instead of WideString.
  //VST3.OnGetText := VST3GetText;
  //VST3.OnGetHint := VST3GetHint;
  //VST3.OnIncrementalSearch := VST3IncrementalSearch;

  // Always tell the tree how much data space per node it must allocated for us. We can do this here, in the
  // object inspector or in the OnGetNodeDataSize event.
  VST3.NodeDataSize := SizeOf(TPropertyData);
  // The VCL (D7 and lower) still uses 16 color image lists. We create a high color version explicitely because it
  // looks so much nicer.
  ConvertToHighColor(TreeImages);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

begin
  case Node.Index of
    0:
      ChildCount := 13;
    1:
      ChildCount := 8;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PPropertyData;

begin
  if ParentNode = nil then
    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded]
  else
  begin
    Data := Sender.GetNodeData(Node);
    Data.ValueType := ValueTypes[ParentNode.Index, Node.Index];
    if Data.ValueType = vtDate then
      Data.Value := DateToStr(Now)
    else
      Data.Value := DefaultValue[ParentNode.Index, Node.Index];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);

var
  Data: PPropertyData;

begin
  if TextType = ttNormal then
    case Column of
      0:
        if Sender.NodeParent[Node] = nil then
        begin
          // root nodes
          if Node.Index = 0 then
            CellText := 'Description'
          else
            CellText := 'Origin';
        end
        else
          CellText := PropertyTexts[Sender.NodeParent[Node].Index, Node.Index, ptkText];
      1:
        begin
          Data := Sender.GetNodeData(Node);
          CellText := Data.Value;
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

begin
  // Add a dummy hint to the normal hint to demonstrate multiline hints.
  if (Column = 0) and (Sender.NodeParent[Node] <> nil) then
  begin
    HintText := PropertyTexts[Sender.NodeParent[Node].Index, Node.Index, ptkHint];
    { Related to #Issue 623
      Observed when solving issue #623. For hmToolTip, the multi-line mode
      depends on the node's multi-lin emode. Hence, append a line only
      if not hmToolTip. Otherwise, if you must append lines, force the
      lineBreakStyle := hlbForceMultiLine for hmToolTip.
    }
    if (Sender as TVirtualStringTree).Hintmode <> hmTooltip then
       HintText := HintText
          + #13 + '(Multiline hints are supported too).'
          ;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex);

var
  Data: PPropertyData;

begin
  if (Kind in [ikNormal, ikSelected]) and (Column = 0) then
  begin
    if Sender.NodeParent[Node] = nil then
      Index := 12 // root nodes, this is an open folder
    else
    begin
      Data := Sender.GetNodeData(Node);
      if Data.ValueType <> vtNone then
        Index := 14
      else
        Index := 13;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

var
  Data: PPropertyData;
  
begin
  with Sender do
  begin
    Data := GetNodeData(Node);
    Allowed := (Sender.NodeParent[Node] <> nil) and (Column = 1) and (Data.ValueType <> vtNone);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3Change(Sender: TBaseVirtualTree; Node: PVirtualNode);

begin
  with Sender do
  begin
    // Start immediate editing as soon as another node gets focused.
    if Assigned(Node) and (Sender.NodeParent[Node] <> nil) and not (tsIncrementalSearching in TreeStates) then
    begin
      // We want to start editing the currently selected node. However it might well happen that this change event
      // here is caused by the node editor if another node is currently being edited. It causes trouble
      // to start a new edit operation if the last one is still in progress. So we post us a special message and
      // in the message handler we then can start editing the new node. This works because the posted message
      // is first executed *after* this event and the message, which triggered it is finished.
      PostMessage(Self.Handle, WM_STARTEDITING, WPARAM(Node), 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);
  
// This is the callback of the tree control to ask for an application defined edit link. Providing one here allows
// us to control the editing process up to which actual control will be created.
// TPropertyEditLink implements an interface and hence benefits from reference counting. We don't need to keep a
// reference to free it. As soon as the tree finished editing the class will be destroyed automatically.

begin
  EditLink := TPropertyEditLink.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);

var
  Data: PPropertyData;

begin
  // Make the root nodes underlined and draw changed nodes in bold style.
  if Sender.NodeParent[Node] = nil then
    TargetCanvas.Font.Style := [fsUnderline]
  else
  begin
    Data := Sender.GetNodeData(Node);
    if Data.Changed then
      TargetCanvas.Font.Style := [fsBold]
    else
      TargetCanvas.Font.Style := [];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3IncrementalSearch(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: string;
  var Result: Integer);

var
  S,
  PropText: string;

begin
  S := SearchText;
  SetStatusbarText('Searching for: ' + S);

  if Sender.NodeParent[Node] = nil then
  begin
    // root nodes
    if Node.Index = 0 then
      PropText := 'Description'
    else
      PropText := 'Origin';
  end
  else
  begin
    PropText := PropertyTexts[Sender.NodeParent[Node].Index, Node.Index, ptkText];
  end;

  // By using StrLIComp we can specify a maximum length to compare. This allows us to find also nodes
  // which match only partially. Don't forget to specify the shorter string length as search length.
  Result := StrLIComp(PChar(S), PChar(PropText), Min(Length(S), Length(PropText)))
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.RadioGroup1Click(Sender: TObject);

begin
  with Sender as TRadioGroup do
    if ItemIndex = 0 then
      VST3.IncrementalSearchDirection := sdForward
    else
      VST3.IncrementalSearchDirection := sdBackward;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3StateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if tsIncrementalSearching in Enter then
    // Note: Unicode will be converted to ANSI here, but for demonstration purposes we accept that for now.
    SetStatusbarText('Searching for: ' + Sender.SearchBuffer);
  if tsIncrementalSearching in Leave then
    SetStatusbarText('');

  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.WMStartEditing(var Message: TMessage);

// This message was posted by ourselves from the node change handler above to decouple that change event and our
// intention to start editing a node. This is necessary to avoid interferences between nodes editors potentially created
// for an old edit action and the new one we start here.

var
  Node: PVirtualNode;

begin
  Node := Pointer(Message.WParam);
  // Note: the test whether a node can really be edited is done in the OnEditing event.
  VST3.EditNode(Node, 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PPropertyData;

begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

end.
