unit VTCellSelectionTests.VTSelectionTestForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees,
  VirtualTrees.Types, VirtualTrees.ClipBoard, ActiveX, Vcl.ExtCtrls;

type

  TDataRec = record
    Name: String;
    Desp: String;
    Loc: String;
  end;
  PDataRec = ^TDataRec;

  TSelectionTestForm = class(TForm)
    VSTA: TVirtualStringTree;
    procedure VSTAGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTADragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure VSTADragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure VSTADragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure VSTAFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormShow(Sender: TObject);
    procedure VSTAChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTAClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectionTestForm: TSelectionTestForm;

implementation

{$R *.dfm}

procedure TSelectionTestForm.FormShow(Sender: TObject);
var
  Data: PDataRec;
  PNode: PVirtualNode;
begin
  VSTA.BeginUpdate;
  New(Data);
  FillChar(Data^, SizeOf(TDataRec), 0);
  Data.Name := 'Name-0';
  Data.Desp := 'Desp-0';
  Data.Loc := 'Loc-0';
  PNode := VSTA.AddChild(nil, Data);

  New(Data);
  FillChar(Data^, SizeOf(TDataRec), 0);
  Data.Name := 'Name-1';
  Data.Desp := 'Desp-1';
  Data.Loc := 'Loc-1';
  VSTA.AddChild(PNode, Data);

  New(Data);
  FillChar(Data^, SizeOf(TDataRec), 0);
  Data.Name := 'Name-2';
  Data.Desp := 'Desp-2';
  Data.Loc := 'Loc-2';
  VSTA.AddChild(PNode, Data);
  VSTA.Expanded[PNode] := True;

  New(Data);
  FillChar(Data^, SizeOf(TDataRec), 0);
  Data.Name := 'Name-3';
  Data.Desp := 'Desp-3';
  Data.Loc := 'Loc-3';
  VSTA.AddChild(nil, Data);
  VSTA.EndUpdate;
end;

procedure TSelectionTestForm.VSTAChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Beep;
end;

procedure TSelectionTestForm.VSTAClick(Sender: TObject);
begin
//  Beep;
end;

procedure TSelectionTestForm.VSTADragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := true;
end;

procedure TSelectionTestForm.VSTADragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  I: Integer;
  AttachMode: TVTNodeAttachMode;

begin
  if Length(Formats) > 0 then
  begin
    // OLE drag'n drop
    // If the native tree format is listed then use this and accept the drop, otherwise recject (ignore) it.
    // It is recommend by Microsoft to order available clipboard formats in decreasing detail richness so
    // the first best format which we can accept is usually the best format we can get at all.
    for I := 0 to High(Formats) do
      if Formats[I] = CF_VIRTUALTREE then
      begin
        case Mode of
          dmAbove:
            AttachMode := amInsertBefore;
          dmOnNode:
            AttachMode := amAddChildLast;
          dmBelow:
            AttachMode := amInsertAfter;
        else
          if Assigned(Source) and (Source is TBaseVirtualTree) and (Sender <> Source) then
            AttachMode := amInsertBefore
          else
            AttachMode := amNowhere;
        end;
        // in the case the drop target does an optimized move Effect is set to DROPEFFECT_NONE
        // to indicate this also to the drag source (so the source doesn't need to take any further action)
        Sender.ProcessDrop(DataObject, Sender.DropTargetNode, Effect, AttachMode);
        Sender.Expanded[Sender.DropTargetNode] := True;
        Break;
      end;
  end
  else
  begin
    // VCL drag'n drop, Effects contains by default both move and copy effect suggestion,
    // as usual the application has to find out what operation is finally to do
    Beep;
  end;
end;

procedure TSelectionTestForm.VSTADragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
begin
  Accept := true;
//  Accept := (Source = Sender);
end;

procedure TSelectionTestForm.VSTAFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PDataRec;
begin
  Data := PPointer(Sender.GetNodeData(Node))^;
  Dispose(Data);
end;

procedure TSelectionTestForm.VSTAGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PDataRec;
begin
  Data := PPointer(Sender.GetNodeData(Node))^;
  if TextType = ttNormal then begin
    case Column of
      -1,
      0: CellText := Data.Name;
      1: CellText := Data.Desp;
      2: CellText := Data.Loc;
      else
        CellText := '';
    end;
  end;
end;

end.
