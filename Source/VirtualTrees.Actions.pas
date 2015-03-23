unit VirtualTrees.Actions;

interface

uses
  System.Classes,
  System.Actions,
  Vcl.ActnList,
  VirtualTrees;

type
  TVirtualTreeAction = class(TCustomAction)
  strict private
    fTree: TBaseVirtualTree;
    fTreeAutoDetect: Boolean; // True if a potential Virtual TreeView should be detected automatically, false if a specific Tree was assigned to the property "Tree"
    fOnAfterExecute: TNotifyEvent;
  strict protected
    procedure SetTree(Value: TBaseVirtualTree);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoAfterExecute;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    constructor Create(AOwner: TComponent); override;
    property Control: TBaseVirtualTree read fTree write SetTree;
    property OnAfterExecute: TNotifyEvent read fOnAfterExecute write fOnAfterExecute; // Executed after the action was performed
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
  end;

  TVirtualTreePerItemAction = class(TVirtualTreeAction)
  strict private
    fOnBeforeExecute: TNotifyEvent;
    function GetSelectedOnly: Boolean;
    procedure SetSelectedOnly(const Value: Boolean);
  strict protected
    fToExecute: TVTGetNodeProc; // method which is executed per item to perform this action
    fFilter: TVirtualNodeStates; // Apply only of nodes which match these states
    procedure DoBeforeExecute;
    property SelectedOnly: Boolean read GetSelectedOnly write SetSelectedOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property OnBeforeExecute: TNotifyEvent read fOnBeforeExecute write fOnBeforeExecute;
  end;

  TVirtualStringTreeCheckAll = class(TVirtualTreePerItemAction)
  protected
    fDesiredCheckState: TCheckState;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SelectedOnly;
  end;

  TVirtualStringTreeUncheckAll = class(TVirtualStringTreeCheckAll)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVirtualStringSelectAll = class(TVirtualTreeAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterActions('VirtualTree', [TVirtualStringTreeCheckAll,TVirtualStringTreeUncheckAll], nil);
end;

{ TVirtualStringTreeAction }

constructor TVirtualTreeAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTree := nil;
  fOnAfterExecute := nil;
  fTreeAutoDetect := True;
end;

procedure TVirtualTreeAction.DoAfterExecute;
begin
  if Assigned(fOnAfterExecute) then
    fOnAfterExecute(Self);
end;

function TVirtualTreeAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target is TVirtualStringTree);
end;

procedure TVirtualTreeAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Self.Control) and not fTreeAutoDetect then begin
    Enabled := (Self.Control.ChildCount[nil]>0)
  end//if
  else begin
    Enabled := (Target is TBaseVirtualTree) and ((Target as TBaseVirtualTree).ChildCount[nil]>0);
    if (Target is TBaseVirtualTree) then
      fTree := (Target as TVirtualStringTree);
  end;//else
end;

procedure TVirtualTreeAction.ExecuteTarget(Target: TObject);
begin
  DoAfterExecute();
end;

procedure TVirtualTreeAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTree) then
    FTree := nil;
end;

procedure TVirtualTreeAction.SetTree(Value: TBaseVirtualTree);
begin
  if Value <> fTree then begin
    fTree := Value;
    if Assigned(fTree) then begin
      fTree.FreeNotification(Self);// register Self as a component that should be notified when fTree is about to be destroyed.
    end;//if
    // Do not update the target of this action if it wa set explicitely by the developer
    fTreeAutoDetect := not Assigned(fTree);
  end;//if
end;


{ TVirtualTreePerItemAction }

constructor TVirtualTreePerItemAction.Create(AOwner: TComponent);
begin
  inherited;
  fFilter := [];
  fToExecute := nil;
  fOnBeforeExecute := nil;
end;

function TVirtualTreePerItemAction.GetSelectedOnly: Boolean;
begin
  exit(TVirtualNodeState.vsSelected in fFilter);
end;

procedure TVirtualTreePerItemAction.SetSelectedOnly(const Value: Boolean);
begin
  if Value then
    Include(fFilter, TVirtualNodeState.vsSelected)
  else
    Exclude(fFilter, TVirtualNodeState.vsSelected);
end;

procedure TVirtualTreePerItemAction.DoBeforeExecute;
begin
  if Assigned(fOnBeforeExecute) then
    fOnBeforeExecute(Self);
end;

procedure TVirtualTreePerItemAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(Self.Control) then
    Target := Self.Control;
  DoBeforeExecute();
  Control.IterateSubtree(nil, Self.fToExecute, nil, fFilter);
  Inherited ExecuteTarget(Target);
end;

{ TVirtualStringTreeCheckAll }

constructor TVirtualStringTreeCheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Check all items in the list';
  Caption := 'Check &All';
  fDesiredCheckState := csCheckedNormal;
  fToExecute := procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
                begin
                  Control.CheckState[Node] := fDesiredCheckState;
                end;
end;


{ TVirtualStringTreeUncheckAll }

constructor TVirtualStringTreeUncheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Uncheck all items in the list';
  Caption := '&Uncheck All';
  fDesiredCheckState := csUncheckedNormal;
end;


{ TVirtualStringSelectAll }

procedure TVirtualStringSelectAll.ExecuteTarget(Target: TObject);
begin
  Control.SelectAll(False);
  inherited;
end;



end.
