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

  TVirtualTreeCheckAll = class(TVirtualTreePerItemAction)
  protected
    fDesiredCheckState: TCheckState;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SelectedOnly;
  end;

  TVirtualTreeUncheckAll = class(TVirtualTreeCheckAll)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVirtualTreeSelectAll = class(TVirtualTreeAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TVirtualTreeCopy = class(TVirtualTreeAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

procedure Register;


implementation

uses
  Controls, Forms;

procedure Register;
begin
  RegisterActions('VirtualTree', [TVirtualTreeCheckAll, TVirtualTreeUncheckAll, TVirtualTreeSelectAll, TVirtualTreeCopy], nil);
end;

{ TVirtualTreeAction }

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
  Result := (Target is TBaseVirtualTree);
end;

procedure TVirtualTreeAction.UpdateTarget(Target: TObject);
begin
  if fTreeAutoDetect and (Target is TBaseVirtualTree) then
    fTree := (Target as TBaseVirtualTree);
  Enabled := Assigned(Control) and not Control.IsEmpty;
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
var
  lOldCursor: TCursor;
begin
  if Assigned(Self.Control) then
    Target := Self.Control;
  DoBeforeExecute();
  lOldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Control.BeginUpdate();
  try
    Control.IterateSubtree(nil, Self.fToExecute, nil, fFilter);
  finally
    Control.EndUpdate;
    Screen.Cursor := lOldCursor;
  end;
  Inherited ExecuteTarget(Target);
end;

{ TVirtualTreeCheckAll }

constructor TVirtualTreeCheckAll.Create(AOwner: TComponent);
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


{ TVirtualTreeUncheckAll }

constructor TVirtualTreeUncheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Uncheck all items in the list';
  Caption := '&Uncheck All';
  fDesiredCheckState := csUncheckedNormal;
end;


{ TVirtualStringSelectAll }

procedure TVirtualTreeSelectAll.UpdateTarget(Target: TObject);
begin
  Inherited;
  //Enabled := Enabled and (toMultiSelect in Control.TreeOptions.SelectionOptions)  // TreeOptions is protected  :-(
end;


procedure TVirtualTreeSelectAll.ExecuteTarget(Target: TObject);
begin
  Control.SelectAll(False);
  inherited;
end;

{ TVirtualTreeCopy }

procedure TVirtualTreeCopy.UpdateTarget(Target: TObject);
begin
  Inherited;
  Enabled := Enabled and (Control.VisibleCount > 0);
end;

procedure TVirtualTreeCopy.ExecuteTarget(Target: TObject);
begin
  Control.CopyToClipboard();
  Inherited;
end;

end.
