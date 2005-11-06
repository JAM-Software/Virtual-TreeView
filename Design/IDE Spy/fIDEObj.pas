unit fIDEObj;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ImgList, ExtCtrls, Contnrs, VirtualTrees, TypInfo, DesignIntf, Menus,
  VTEditors, StdCtrls, DesignEditors, VCLEditors, IniFiles, ActnList;

type
  TfrmIDEObjects = class(TForm, IDesigner)
    tvComps: TTreeView;
    ImageList1: TImageList;
    Splitter1: TSplitter;
    pumTree: TPopupMenu;
    tcProps: TTabControl;
    vtProps: TVirtualStringTree;
    Panel1: TPanel;
    Label1: TLabel;
    meHierarchy: TMemo;
    Splitter2: TSplitter;
    Actions: TActionList;
    actFreeObject: TAction;
    FreeObject1: TMenuItem;
    procedure tvCompsGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure vtPropsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtPropsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure tvCompsChange(Sender: TObject; Node: TTreeNode);
    procedure vtPropsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vtPropsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtPropsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtPropsAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure pumTreePopup(Sender: TObject);
    procedure tvCompsDblClick(Sender: TObject);
    procedure vtPropsCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vtPropsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure tcPropsChange(Sender: TObject);
    procedure vtPropsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtPropsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure actFreeObjectUpdate(Sender: TObject);
    procedure actFreeObjectExecute(Sender: TObject);
  private
    FObjects: TObjectList;
    FCurrentEditor: IComponentEditor;
    procedure ns(const Name: string = '');
    procedure ProcessTopLevelComponent(AOwner: TComponent; Node: TTreeNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure Run;
  protected
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean;
    procedure PaintGrid;

    function UniqueName(const BaseName: string): string;
    function GetRoot: TComponent;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read GetCustomForm write SetCustomForm;

    function CreateMethod(const Name: string; TypeData: PTypeData): TMethod;
    function GetMethodName(const Method: TMethod): string;
    procedure GetMethods(TypeData: PTypeData; Proc: TGetStrProc);
    function GetPrivateDirectory: string;
    procedure GetSelections(const List: IDesignerSelections);
    function MethodExists(const Name: string): Boolean;
    procedure RenameMethod(const CurName, NewName: string);
    procedure SelectComponent(Instance: TPersistent);
    procedure SetSelections(const List: IDesignerSelections);
    procedure ShowMethod(const Name: string);
    procedure GetComponentNames(TypeData: PTypeData; Proc: TGetStrProc);
    function GetComponent(const Name: string): TComponent;
    function GetComponentName(Component: TComponent): string;
    function GetObject(const Name: string): TPersistent;
    function GetObjectName(Instance: TPersistent): string;
    procedure GetObjectNames(TypeData: PTypeData; Proc: TGetStrProc);
    function MethodFromAncestor(const Method: TMethod): Boolean;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent;
    function IsComponentLinkable(Component: TComponent): Boolean;
    procedure MakeComponentLinkable(Component: TComponent);
    procedure Revert(Instance: TPersistent; PropInfo: PPropInfo);
    function GetIsDormant: Boolean;
    function HasInterface: Boolean;
    function HasInterfaceMember(const Name: string): Boolean;
    procedure AddToInterface(InvKind: Integer; const Name: string; VT: Word;
      const TypeInfo: string);
    procedure GetProjectModules(Proc: TGetModuleProc);
    function GetAncestorDesigner: IDesigner;
    function IsSourceReadOnly: Boolean;
    function GetContainerWindow: TWinControl;
    procedure SetContainerWindow(const NewContainer: TWinControl);
    function GetScrollRanges(const ScrollPosition: TPoint): TPoint;
    procedure Edit(const Component: TComponent);
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string;
      TypeData: PTypeData);
    procedure CopySelection;
    procedure CutSelection;
    function CanPaste: Boolean;
    procedure PasteSelection;
    procedure ClearSelection;
    procedure NoSelection;
    procedure ModuleFileNames(var ImplFileName, IntfFileName, FormFileName: string);
    function GetRootClassName: string;
    procedure Modified;
    procedure DsgnerNotification(AnObject: TPersistent; Operation: TOperation);

    function GetActiveClassGroup: TPersistentClass;
    function FindRootAncestor(const AClassName: string): TComponent;

    function GetPathAndBaseExeName: string;
    function GetBaseRegKey: string;
    function GetIDEOptions: TCustomIniFile;
    function CreateCurrentComponent(Parent: TComponent; const Rect: TRect): TComponent;
    function IsComponentHidden(Component: TComponent): Boolean;
    procedure DeleteSelection(ADoAll: Boolean = False);
    function GetShiftState: TShiftState;
    procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);
    procedure SelectItemName(const PropertyName: string);
    procedure Resurrect;
  end;

implementation

{$R *.DFM}

type
  TCompList = class(TComponentList)
  public
    procedure AddChild(Child: TComponent);
  end;

  TPropEditList = class(TInterfaceList)
  public
    procedure AddProp(const Prop: IProperty);
  end;

procedure TPropEditList.AddProp(const Prop: IProperty);
begin
  Add(Prop);
end;

procedure TCompList.AddChild(Child: TComponent);
begin
  if IndexOf(Child) < 0 then Add(Child);
end;

type
  TComponentCrack = class(TComponent);

{ TfrmIDEObjects }

procedure TfrmIDEObjects.ProcessTopLevelComponent(AOwner: TComponent; Node: TTreeNode);
var
  Comps: TComponentList;
  I: Integer;

  procedure ProcessComp(Comp: TComponent; Node: TTreeNode);
  var
    L: TCompList;
    I: Integer;
    N: TTreeNode;

    procedure DoList(L: TCompList);
    var
      I: Integer;
    begin
      for I := 0 to L.Count - 1 do
        Comps.Remove(L[I]);
      for I := 0 to L.Count - 1 do
      begin
        N := TTreeView(Node.TreeView).Items.AddChild(Node, L[I].Name + ': ' + L[I].ClassName);
        if L[I] is TWinControl then
          N.Cut := not TWinControl(L[I]).Showing
        else if L[I] is TControl then
          N.Cut := not TControl(L[I]).Visible;
        N.Data := L[I];
        FObjects.Add(L[I]);
        ProcessComp(L[I], N);
      end;
    end;

  begin
    L := TCompList.Create(False);
    try
      if Comp is TWinControl then
        for I := 0 to TWinControl(Comp).ControlCount - 1 do
          L.Add(TWinControl(Comp).Controls[I]);
      TComponentCrack(Comp).GetChildren(L.AddChild, AOwner);
      DoList(L);
    finally
      L.Free;
    end;
  end;

begin
  Node.Text := AOwner.Name + ': ' + AOwner.ClassName;
  Node.Data := AOwner;
  Comps := TComponentList.Create(False);
  try
    for I := 0 to AOwner.ComponentCount - 1 do
      Comps.Add(AOwner.Components[I]);
    ProcessComp(AOwner, Node);
    for I := 0 to Comps.Count - 1 do
    begin
      TTreeView(Node.TreeView).Items.AddChild(Node, Comps[I].Name + ': ' + Comps[I].ClassName).Data := Comps[I];
      FObjects.Add(Comps[I]);
    end;
  finally
    Comps.Free;
  end;
end;

class procedure TfrmIDEObjects.Run;
var
  Node, N: TTreeNode;
  I: Integer;
  L: TList;
begin
  L := nil;
  with Create(Application) do
  try
    L := TList.Create;
    for I := 0 to Application.ComponentCount - 1 do
      L.Add(Application.Components[I]);

    Node := tvComps.Items.Add(nil, 'Application');
    with Node do
    begin

      Data := Application;
      N := tvComps.Items.AddChild(Node, 'Forms');
      for I := 0 to Screen.FormCount - 1 do
      begin
        L.Remove(Screen.Forms[I]);
        ProcessTopLevelComponent(Screen.Forms[I], tvComps.Items.AddChild(N, ''));
      end;

      N := tvComps.Items.AddChild(Node, 'DataModules');
      for I := 0 to Screen.DataModuleCount - 1 do
      begin
        L.Remove(Screen.DataModules[I]);
        ProcessTopLevelComponent(Screen.DataModules[I], tvComps.Items.AddChild(N, ''));
      end;

      for I := L.Count - 1 downto 0 do
        if TObject(L[I]) is TDataModule then
        begin
          ProcessTopLevelComponent(TComponent(L[I]), tvComps.Items.AddChild(N, ''));
          L.Delete(I);
        end;


      if L.Count > 0 then
      begin
        N := tvComps.Items.AddChild(Node, 'Other');
        for I := 0 to L.Count - 1 do
          ProcessTopLevelComponent(TComponent(L[I]), tvComps.Items.AddChild(N, ''));
      end;
    end;
    ShowModal;
  finally
    Free;
    L.Free;
  end;
end;

procedure TfrmIDEObjects.tvCompsGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Data = nil then
    Node.ImageIndex := 0
  else if TObject(Node.Data) is TForm then
    Node.ImageIndex := 1
  else if TObject(Node.Data) is TControl then
    Node.ImageIndex := 2
  else
    Node.ImageIndex := 3;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TfrmIDEObjects.vtPropsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(Pointer);
end;

procedure TfrmIDEObjects.vtPropsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  IInterface(Sender.GetNodeData(Node)^)._Release;
end;

procedure TfrmIDEObjects.tvCompsChange(Sender: TObject; Node: TTreeNode);
var
  C: TComponent;
  P: TClass;
  L: TPropEditList;
  I: Integer;
  CL: IDesignerSelections;
  Filter: TTypeKinds;
  S: string;
  Nodes: TList;
begin
  Nodes := nil;
  vtProps.BeginUpdate;
  try
    Nodes := TList.Create;
    vtProps.Clear;
    meHierarchy.Clear;
    tvComps.GetSelections(Nodes);
    if Assigned(Node) then
    begin
      C := TComponent(Node.Data);
      if not Assigned(C) then Exit;
      P := C.ClassType;
      S := P.ClassName;
      P := P.ClassParent;
      while Assigned(P) and (P <> TObject) do
      begin
        S := S + #13#10 + P.ClassName;
        P := P.ClassParent;
      end;
      meHierarchy.Text := S;
      CL := nil;
      L := TPropEditList.Create;
      try
        CL := CreateSelectionList;
        for I := 0 to Nodes.Count - 1 do
          if TObject(TTreeNode(Nodes[I]).Data) is TPersistent then
            CL.Add(TPersistent(TTreeNode(Nodes[I]).Data));
        if tcProps.TabIndex = 0 then Filter := tkProperties else Filter := [tkMethod];
        GetComponentProperties(CL, Filter, Self, L.AddProp);
        for I := 0 to L.Count - 1 do
        begin
          vtProps.AddChild(nil, Pointer(L[i] as IProperty));
          L[I]._AddRef;
        end;
      finally
        L.Free;
      end;
    end;
  finally
    Nodes.Free;
    vtProps.EndUpdate;
  end;
end;

procedure TfrmIDEObjects.vtPropsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  S: string;
begin
  try
    if TextType <> ttNormal then Exit;
    with IProperty(Sender.GetNodeData(Node)^) do
      case Column of
        -1, 0: CellText := GetName;
        1:     if GetEditValue(S) then CellText := S else CellText := GetValue;
      end;
  except
    on E: Exception do
      CellText := E.Message;
  end;
end;

procedure TfrmIDEObjects.vtPropsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  with IProperty(Sender.GetNodeData(Node)^) do
    if paSubProperties in GetAttributes then
      Include(InitialStates, ivsHasChildren);
end;

procedure TfrmIDEObjects.vtPropsInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  L: TPropEditList;
  I: Integer;
begin
  with IProperty(Sender.GetNodeData(Node)^) do
  begin
    if not (paSubProperties in GetAttributes) then
    begin
      ChildCount := 0;
      Exit;
    end;
    L := TPropEditList.Create;
    try
      GetProperties(L.AddProp);
      ChildCount := 0;
      Sender.BeginUpdate;
      try
        for I := 0 to L.Count - 1 do
          if L[I] <> nil then
          begin
            Sender.AddChild(Node, Pointer(L[I] as IProperty));
            L[I]._AddRef;
            Inc(ChildCount);
          end;
      finally
        Sender.EndUpdate;
      end;
    finally
      L.Free;
    end;
  end;
end;

function TfrmIDEObjects.GetCustomForm: TCustomForm;
begin
  Result := nil;
end;

function TfrmIDEObjects.GetIsControl: Boolean;
begin
  Result := False;
end;

function TfrmIDEObjects.GetRoot: TComponent;
begin
  Result := nil;
end;

function TfrmIDEObjects.IsDesignMsg(Sender: TControl;
  var Message: TMessage): Boolean;
begin
  Result := False;
end;

procedure TfrmIDEObjects.PaintGrid;
begin
end;

procedure TfrmIDEObjects.SetCustomForm(Value: TCustomForm);
begin
end;

procedure TfrmIDEObjects.SetIsControl(Value: Boolean);
begin
end;

function TfrmIDEObjects.UniqueName(const BaseName: string): string;
begin
  Result := '';
end;

procedure TfrmIDEObjects.AddToInterface(InvKind: Integer;
  const Name: string; VT: Word; const TypeInfo: string);
begin

end;

function TfrmIDEObjects.CanPaste: Boolean;
begin
  Result := False;
end;

procedure TfrmIDEObjects.ChainCall(const MethodName, InstanceName,
  InstanceMethod: string; TypeData: PTypeData);
begin

end;

procedure TfrmIDEObjects.ClearSelection;
begin

end;

procedure TfrmIDEObjects.CopySelection;
begin

end;

function TfrmIDEObjects.CreateComponent(ComponentClass: TComponentClass;
  Parent: TComponent; Left, Top, Width, Height: Integer): TComponent;
begin
  Result := nil;
  ns('CreateComponent');
end;

function TfrmIDEObjects.CreateMethod(const Name: string;
  TypeData: PTypeData): TMethod;
begin
  ns('CreateMethod');
end;

procedure TfrmIDEObjects.CutSelection;
begin
  ns('CutSelection');
end;

procedure TfrmIDEObjects.Edit(const Component: TComponent);
begin
  ns('Edit');
end;

function TfrmIDEObjects.GetAncestorDesigner: IDesigner;
begin
  ns('GetAncestorDesigner');
end;

function TfrmIDEObjects.GetComponent(const Name: string): TComponent;
var
  I: Integer;
begin
  for I := 0 to FObjects.Count - 1 do
    if (FObjects[I] is TComponent) and SameText(TComponent(FObjects[I]).Name, Name) then
    begin
      Result := TComponent(FObjects[I]);
      Exit;
    end;
  Result := nil;
end;

function TfrmIDEObjects.GetComponentName(Component: TComponent): string;
begin
  if Assigned(Component) then
    Result := Component.Name
  else
    Result := '';
end;

procedure TfrmIDEObjects.GetComponentNames(TypeData: PTypeData;
  Proc: TGetStrProc);

  procedure DoComponent(Comp: TComponent);
  var
    I: Integer;
  begin
    if (Comp.Name <> '') and Comp.InheritsFrom(TypeData.ClassType) then
      Proc(Comp.Name);
    for I := 0 to Comp.ComponentCount - 1 do
      DoComponent(Comp.Components[I]);
  end;

var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    DoComponent(Screen.Forms[I]);
  for I := 0 to Screen.DataModuleCount - 1 do
    DoComponent(Screen.DataModules[I]);
end;

function TfrmIDEObjects.GetContainerWindow: TWinControl;
begin
  Result := nil;
end;

function TfrmIDEObjects.GetIsDormant: Boolean;
begin
  Result := False;
end;

function TfrmIDEObjects.GetMethodName(const Method: TMethod): string;
var
  Inst: TObject;
begin
  if TMethod(Method).Code <> nil then
  begin
    Inst := TMethod(Method).Data;
    Result := Inst.MethodName(TMethod(Method).Code);
    if Result = '' then Result := IntToHex(Integer(TMethod(Method).Code), 8)
  end
  else
    Result := '';
end;

procedure TfrmIDEObjects.GetMethods(TypeData: PTypeData;
  Proc: TGetStrProc);
begin
end;

function TfrmIDEObjects.GetObject(const Name: string): TPersistent;
var
  I: Integer;
begin
  for I := 0 to FObjects.Count - 1 do
    if (FObjects[I] is TPersistent) and SameText(TPersistent(FObjects[I]).GetNamePath, Name) then
    begin
      Result := TPersistent(FObjects[I]);
      Exit;
    end;
  Result := nil;
end;

function TfrmIDEObjects.GetObjectName(Instance: TPersistent): string;
begin
  Result := Instance.GetNamePath;
end;

procedure TfrmIDEObjects.GetObjectNames(TypeData: PTypeData;
  Proc: TGetStrProc);
begin
  // TODO:
end;

function TfrmIDEObjects.GetPrivateDirectory: string;
begin
  Result := '';
end;

procedure TfrmIDEObjects.GetProjectModules(Proc: TGetModuleProc);
begin
  ns('GetProjectModules');
end;

function TfrmIDEObjects.GetRootClassName: string;
begin
  ns('GetRootClassName');
end;

function TfrmIDEObjects.GetScrollRanges(
  const ScrollPosition: TPoint): TPoint;
begin
  ns('GetScrollRanges');
end;

procedure TfrmIDEObjects.GetSelections(const List: IDesignerSelections);
begin
  // TODO:
end;

function TfrmIDEObjects.HasInterface: Boolean;
begin
  Result := False;
end;

function TfrmIDEObjects.HasInterfaceMember(const Name: string): Boolean;
begin
  Result := False;
end;

function TfrmIDEObjects.IsComponentLinkable(
  Component: TComponent): Boolean;
begin
  Result := False;
end;

function TfrmIDEObjects.IsSourceReadOnly: Boolean;
begin
  Result := True;
end;

procedure TfrmIDEObjects.MakeComponentLinkable(Component: TComponent);
begin
  ns('MakeComponentLinkable');
end;

function TfrmIDEObjects.MethodExists(const Name: string): Boolean;
begin
  Result := False;
end;

function TfrmIDEObjects.MethodFromAncestor(const Method: TMethod): Boolean;
begin
  Result := False;
end;

procedure TfrmIDEObjects.ModuleFileNames(var ImplFileName, IntfFileName,
  FormFileName: string);
begin
  ns('ModuleFileNames');
end;

procedure TfrmIDEObjects.NoSelection;
begin
end;

procedure TfrmIDEObjects.PasteSelection;
begin
  ns('PasteSelection');
end;

procedure TfrmIDEObjects.RenameMethod(const CurName, NewName: string);
begin
  ns('RenameMethod');
end;

procedure TfrmIDEObjects.Revert(Instance: TPersistent;
  PropInfo: PPropInfo);
begin
  ns('Revert');
end;

procedure TfrmIDEObjects.SelectComponent(Instance: TPersistent);
begin
end;

procedure TfrmIDEObjects.SetContainerWindow(
  const NewContainer: TWinControl);
begin
  ns('SetContainerWindow');
end;

procedure TfrmIDEObjects.SetSelections(const List: IDesignerSelections);
begin
 // ns('SetSelections');
end;

procedure TfrmIDEObjects.ShowMethod(const Name: string);
begin

end;

procedure TfrmIDEObjects.ns(const Name: string = '');
begin
  raise Exception.Create('Not supported: ' + Name);
end;

constructor TfrmIDEObjects.Create(AOwner: TComponent);
begin
  inherited;
  FObjects := TObjectList.Create(False);
end;

destructor TfrmIDEObjects.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;

procedure TfrmIDEObjects.Modified;
begin
end;

procedure TfrmIDEObjects.DsgnerNotification(AnObject: TPersistent; Operation: TOperation);
begin
end;

procedure TfrmIDEObjects.vtPropsAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  ASelected: Boolean;
  D: ICustomPropertyDrawing;
begin
  if Column <> 1 then Exit;
  try
    ASelected := (Sender.FocusedColumn = Column) and (Sender.FocusedNode = Node);
    if ASelected then
      TargetCanvas.Brush.Color := vtProps.Colors.FocusedSelectionColor
    else
      TargetCanvas.Brush.Color := vtProps.Color;
    InflateRect(CellRect, -vtProps.Margin, -1);
    with IProperty(Sender.GetNodeData(Node)^) do
      if QueryInterface(ICustomPropertyDrawing, D) = S_OK then
        D.PropDrawValue(TargetCanvas, CellRect, ASelected)
      else
        Canvas.TextRect(CellRect, CellRect.Left + 1, CellRect.Top + 1, GetValue);
  except
  end;
end;

procedure TfrmIDEObjects.pumTreePopup(Sender: TObject);
var
  Comp: TComponent;
begin
  if Assigned(tvComps.Selected) then
  begin
    Comp := TComponent(tvComps.Selected.Data);
    FCurrentEditor := GetComponentEditor(Comp, Self);
//    TODO:
    FCurrentEditor := nil;
  end;

end;

procedure TfrmIDEObjects.tvCompsDblClick(Sender: TObject);
var
  Comp: TComponent;
begin
  if Assigned(tvComps.Selected) then
  begin
    Comp := TComponent(tvComps.Selected.Data);
    FCurrentEditor := GetComponentEditor(Comp, Self);
    FCurrentEditor.Edit;
    FCurrentEditor := nil;
  end;
end;

procedure TfrmIDEObjects.vtPropsCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  A: TPropertyAttributes;
  L: TStringList;
begin
  if Column <> 1 then Exit;
  with IProperty(Sender.GetNodeData(Node)^) do
  begin
    A := GetAttributes;
    if paValueList in A then
    begin
      L := TStringList.Create;
      try
        GetValues(L.Append);
        if paSortList in A then L.Sort;
        EditLink := TComboEditLink.Create(L, csDropDown);
      finally
        L.Free;
      end;
    end
    else EditLink := TEditEditLink.Create;
  end;
end;

procedure TfrmIDEObjects.vtPropsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  with IProperty(Sender.GetNodeData(Node)^) do
    Allowed := (Column = 1) and not (paReadOnly in GetAttributes);
end;

function TfrmIDEObjects.FindRootAncestor(const AClassName: string): TComponent;
begin
  Result := nil;
end;

function TfrmIDEObjects.GetActiveClassGroup: TPersistentClass;
begin
  Result := nil;
end;

function TfrmIDEObjects.GetBaseRegKey: string;
begin
  Result := '';
end;

function TfrmIDEObjects.GetPathAndBaseExeName: string;
begin
  Result := '';
end;

function TfrmIDEObjects.GetIDEOptions: TCustomIniFile;
begin
  Result := nil;
end;

function TfrmIDEObjects.CreateCurrentComponent(Parent: TComponent;
  const Rect: TRect): TComponent;
begin
  Result := nil;
  ns('CreateCurrentComponent');
end;

function TfrmIDEObjects.IsComponentHidden(Component: TComponent): Boolean;
begin
  Result := False;
end;

procedure TfrmIDEObjects.DeleteSelection(ADoAll: Boolean);
begin
  ns('DeleteSelection');
end;

function TfrmIDEObjects.GetShiftState: TShiftState;
begin
  Result := [];
end;

procedure TfrmIDEObjects.ModalEdit(EditKey: Char;
  const ReturnWindow: IActivatable);
begin
  ns('ModalEdit');
end;

procedure TfrmIDEObjects.Resurrect;
begin
  ns('Resurrect');
end;

procedure TfrmIDEObjects.SelectItemName(const PropertyName: string);
begin
  ns('SelectItemName');
end;

procedure TfrmIDEObjects.tcPropsChange(Sender: TObject);
begin
  tvCompsChange(tvComps, tvComps.Selected);
end;

procedure TfrmIDEObjects.vtPropsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  N: TTreeNode;
  C: TComponent;
  P: PPropInfo;
begin
  if Column <> 0 then Exit;
  N := tvComps.Selected;
  if (tvComps.SelectionCount <> 1) or not Assigned(N) then Exit;
  C := TComponent(N.Data);
  with IProperty(Sender.GetNodeData(Node)^) do
  begin
    P := GetPropInfo;
    if not IsPublishedProp(C.ClassType.ClassParent, P.Name) then
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end;
end;

procedure TfrmIDEObjects.vtPropsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
begin
  if Column <> 1 then Exit;
    with IProperty(Sender.GetNodeData(Node)^) do
      if not (paReadonly in GetAttributes) then
        SetValue(NewText);
end;

procedure TfrmIDEObjects.actFreeObjectUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(tvComps.Selected) and Assigned(tvComps.Selected.Data);
end;

procedure TfrmIDEObjects.actFreeObjectExecute(Sender: TObject);
var
  Obj: TObject;
begin
  if Assigned(tvComps.Selected) and Assigned(tvComps.Selected.Data) then
  begin
    Obj := TObject(tvComps.Selected.Data);
    tvComps.Selected.Delete;
    Obj.Free;
  end;
end;

end.

