unit dIDESpy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolsApi, ActnList;

type
  TIDESpyM = class(TDataModule)
    Actions: TActionList;
    actShowIDEObjects: TAction;
    procedure actShowIDEObjectsExecute(Sender: TObject);
  private
    FTopItem: TMenuItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses fIDEObj;

{$R *.DFM}

var
  Expert: TIDESpyM;

procedure Register;
begin
  Expert := TIDESpyM.Create(nil);
end;

{ TIDESpyM }

constructor TIDESpyM.Create(AOwner: TComponent);
var
  IDEActions: TCustomActionList;
  Menu: TMainMenu;
  Item: TMenuItem;
  I: Integer;
begin
  inherited;
  Expert := Self;

  IDEActions := (BorlandIDEServices as INTAServices).GetActionList;

  for I := Actions.ActionCount - 1 downto 0 do
    Actions[I].ActionList := IDEActions;

  Menu := (BorlandIDEServices as INTAServices).GetMainMenu;

  FTopItem := TMenuItem.Create(Self);
  FTopItem.Caption := 'IDE';
  Menu.Items.Add(FTopItem);

  Item := TMenuItem.Create(Self);
  Item.Action := actShowIDEObjects;
  FTopItem.Add(Item);
end;

destructor TIDESpyM.Destroy;
begin
  FreeAndNil(FTopItem);
  inherited;
end;

procedure TIDESpyM.actShowIDEObjectsExecute(Sender: TObject);
begin
  TfrmIDEObjects.Run;
end;

initialization

finalization

  FreeAndNil(Expert);
  
end.

