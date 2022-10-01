unit VirtualTrees.BaseAncestorFMX;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski                                                                         }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface
uses VirtualTrees.BaseTree;

type
  TVTAncestorFMX = class abstract(TBaseVirtualTree)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
  end;

implementation

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); //wymaga BaseTree
Var MM: TWMMouse;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P.X:= X;
  P.Y:= Y;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  FillTWMMouse(MM, Button, Shift, P.X, P.Y, isNC, false);
  if FHeader.HandleMessage(TMessage(MM)) then
    exit;//!!!

  FillTWMMouse(MM, Button, Shift, X, Y, isNC, false);
  // get information about the hit
  GetHitTestInfoAt(X, Y, True, hInfo);

  HandleMouseDown(MM, hInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); //wymaga BaseTree
Var MM: TWMMouse;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P.X:= X;
  P.Y:= Y;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  FillTWMMouse(MM, Button, Shift, P.X, P.Y, isNC, true);
  if FHeader.HandleMessage(TMessage(MM)) then
    exit;//!!!

  FillTWMMouse(MM, Button, Shift, X, Y, isNC, true);
  // get information about the hit
  GetHitTestInfoAt(X, Y, True, hInfo);
  HandleMouseUp(MM, hInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //wymaga BaseTree
Var M: TCMMouseWheel;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P:= Screen.MousePos;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  M.Msg:= CM_MOUSEWHEEL;
  M.ShiftState:= Shift;
  M.WheelDelta:= WheelDelta;
  M.XPos:= P.X;
  M.YPos:= P.Y;
  M.Result:= 0;
  CMMouseWheel(M);
  Handled:= M.Result<>0;
end;

end.

