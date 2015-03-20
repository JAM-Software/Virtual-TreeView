unit VirtualTrees.Actions;

interface

uses
  System.Classes,
  System.Actions,
  Vcl.ActnList,
  VirtualTrees;

type
  TVirtualTreeAction = class(TCustomAction)
  private
    fTree: TBaseVirtualTree;
    fTreeAutoDetect: Boolean;
    fOnBeforeExecute: TNotifyEvent;
    fOnAfterExecute: TNotifyEvent;
  protected
    procedure SetTree(Value: TBaseVirtualTree);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoBeforeExecute;
    procedure DoAfterExecute;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    constructor Create(AOwner: TComponent); override;
    property Tree: TBaseVirtualTree read fTree write SetTree;
    property OnBeforeExecute: TNotifyEvent read fOnBeforeExecute write fOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read fOnAfterExecute write fOnAfterExecute;
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

  TVirtualStringTreeCheckAll = class(TVirtualTreeAction)
  protected
    fDesiredCheckState: TCheckState;
    fSelectedOnly: Boolean;
    procedure SetCheckState(VT: TBaseVirtualTree; aCheckState: TCheckState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property SelectedOnly: Boolean read fSelectedOnly write fSelectedOnly default False;
  end;

  TVirtualStringTreeUncheckAll = class(TVirtualStringTreeCheckAll)
  public
    constructor Create(AOwner: TComponent); override;
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
  fOnBeforeExecute := nil;
  fOnAfterExecute := nil;
  fTreeAutoDetect := True;
  Hint := 'Copy the content to the Clipboard';
end;

procedure TVirtualTreeAction.DoAfterExecute;
begin
  if Assigned(fOnAfterExecute) then
    fOnAfterExecute(Self);
end;

procedure TVirtualTreeAction.DoBeforeExecute;
begin
  if Assigned(fOnBeforeExecute) then
    fOnBeforeExecute(Self);
end;

function TVirtualTreeAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target is TVirtualStringTree);
end;

procedure TVirtualTreeAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Self.Tree) and not fTreeAutoDetect then begin
    Enabled := (Self.Tree.ChildCount[nil]>0)
  end//if
  else begin
    Enabled := (Target is TVirtualStringTree) and ((Target as TVirtualStringTree).ChildCount[nil]>0);
    if (Target is TVirtualStringTree) then
      fTree := (Target as TVirtualStringTree);
  end;//else
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

{ TVirtualStringTreeCheckAll }

constructor TVirtualStringTreeCheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Check all items in the list';
  Caption := 'Check &All';
  fSelectedOnly := False;
  fDesiredCheckState := csCheckedNormal;
end;

procedure TVirtualStringTreeCheckAll.ExecuteTarget(Target: TObject);
begin
  if Assigned(Self.Tree) then Target := Self.Tree;
  DoBeforeExecute();
  Inherited ExecuteTarget(Target);
  SetCheckState((Target as TVirtualStringTree), fDesiredCheckState);
  DoAfterExecute();
end;

procedure TVirtualStringTreeCheckAll.SetCheckState(VT: TBaseVirtualTree; aCheckState: TCheckState);
begin
  Inherited;
  VT.SetCheckStateForAll(aCheckState, Self.SelectedOnly);
end;

{ TVirtualStringTreeUncheckAll }

constructor TVirtualStringTreeUncheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Uncheck all items in the list';
  Caption := '&Uncheck All';
  fDesiredCheckState := csUncheckedNormal;
end;



end.
