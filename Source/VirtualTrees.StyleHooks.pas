unit VirtualTrees.StyleHooks;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is VirtualTrees.pas, released September 30, 2000.
//
// The initial developer of the original code is digital publishing AG (Munich, Germany, www.digitalpublishing.de),
// written by Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by digital publishing AG are Copyright
// (C) 1999-2001 digital publishing AG. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------


interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

//{$DEFINE VT_FMX}
{$IFNDEF VT_FMX}
  {$DEFINE VT_VCL}
{$ENDIF}


uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,

  System.Classes,
  Vcl.Themes,
  Vcl.Forms,
  Vcl.Controls;

const
  CM_UPDATE_VCLSTYLE_SCROLLBARS = CM_BASE + 2050;

type
  // XE2+ VCL Style
  TVclStyleScrollBarsHook = class(TScrollingStyleHook)
  strict private type
  {$REGION 'TVclStyleScrollBarWindow'}
      TVclStyleScrollBarWindow = class(TWinControl)strict private FScrollBarWindowOwner: TVclStyleScrollBarsHook;
    FScrollBarVertical: Boolean;
    FScrollBarVisible: Boolean;
    FScrollBarEnabled: Boolean;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  strict protected
    procedure CreateParams(var Params: TCreateParams);
    override;
  public
    constructor Create(AOwner: TComponent);
    override;
    property ScrollBarWindowOwner: TVclStyleScrollBarsHook read FScrollBarWindowOwner write FScrollBarWindowOwner;
    property ScrollBarVertical: Boolean read FScrollBarVertical write FScrollBarVertical;
    property ScrollBarVisible: Boolean read FScrollBarVisible write FScrollBarVisible;
    property ScrollBarEnabled: Boolean read FScrollBarEnabled write FScrollBarEnabled;
    end;
  {$ENDREGION}
  private
    FHorzScrollBarWindow: TVclStyleScrollBarWindow;
    FLeftMouseButtonDown: Boolean;
    FVertScrollBarWindow: TVclStyleScrollBarWindow;

    procedure CMUpdateVclStyleScrollbars(var Message: TMessage); message CM_UPDATE_VCLSTYLE_SCROLLBARS;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMLButtonDown(var Msg: TWMMouse);  message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Msg: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseMove(var Msg: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonUp(var Msg: TWMMouse); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMNCLButtonDblClk(var Msg: TWMMouse); message WM_NCLBUTTONDBLCLK;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMPosChanged(var Msg: TMessage); message WM_WINDOWPOSCHANGED;
  protected
    procedure CalcScrollBarsRect; virtual;
    procedure DrawHorzScrollBar(DC: HDC); virtual;
    procedure DrawVertScrollBar(DC: HDC); virtual;
    procedure MouseLeave; override;
    procedure PaintScroll; override;
    function PointInTreeHeader(const P: TPoint): Boolean;
    procedure UpdateScrollBarWindow;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
    property HorzScrollRect;
    property VertScrollRect;
  end;


implementation

uses
  System.SysUtils,
  System.Math,
  System.Types,
  Vcl.Graphics,
  VirtualTrees;

type
  TBaseVirtualTreeCracker = class(TBaseVirtualTree)
  end;


// XE2+ VCL Style
{ TVclStyleScrollBarsHook }

procedure TVclStyleScrollBarsHook.CalcScrollBarsRect;
var
  BorderValue: TSize;
  BarInfo: TScrollBarInfo;

  procedure CalcVerticalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    FVertScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FVertScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
  end;

  procedure CalcHorizontalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    FHorzScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FHorzScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
  end;

begin
  BorderValue.cx := 0;
  BorderValue.cy := 0;
  if HasBorder then
    if HasClientEdge then
    begin
      BorderValue.cx := GetSystemMetrics(SM_CXEDGE);
      BorderValue.cy := GetSystemMetrics(SM_CYEDGE);
    end;
  CalcVerticalRects;
  CalcHorizontalRects;

end;

constructor TVclStyleScrollBarsHook.Create(AControl: TWinControl);
begin
  inherited;
  FVertScrollBarWindow := TVclStyleScrollBarWindow.CreateParented
    (GetParent(Control.Handle));
  FVertScrollBarWindow.ScrollBarWindowOwner := Self;
  FVertScrollBarWindow.ScrollBarVertical := True;

  FHorzScrollBarWindow := TVclStyleScrollBarWindow.CreateParented
    (GetParent(Control.Handle));
  FHorzScrollBarWindow.ScrollBarWindowOwner := Self;

  VertSliderState := tsThumbBtnVertNormal;
  VertUpState := tsArrowBtnUpNormal;
  VertDownState := tsArrowBtnDownNormal;
  HorzSliderState := tsThumbBtnHorzNormal;
  HorzUpState := tsArrowBtnLeftNormal;
  HorzDownState := tsArrowBtnRightNormal;
end;

destructor TVclStyleScrollBarsHook.Destroy;
begin
  FVertScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FVertScrollBarWindow);
  FHorzScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FHorzScrollBarWindow);
  inherited;
end;

procedure TVclStyleScrollBarsHook.DrawHorzScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FHorzScrollBarWindow.Visible and StyleServices.Available and (seBorder in TBaseVirtualTree(Control).StyleElements) then
  begin
    B := TBitmap.Create;
    try
      B.Width := HorzScrollRect.Width;
      B.Height := HorzScrollRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -HorzScrollRect.Left,
        -HorzScrollRect.Top);
      R := HorzScrollRect;
      R.Left := HorzUpButtonRect.Right;
      R.Right := HorzDownButtonRect.Left;

      Details := StyleServices.GetElementDetails(tsUpperTrackHorzNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(HorzSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details, HorzSliderRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(HorzUpState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, HorzUpButtonRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails
          (HorzDownState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, HorzDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, HorzScrollRect.Left,
        HorzScrollRect.Top);
      with HorzScrollRect do
        BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0,
          SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

procedure TVclStyleScrollBarsHook.DrawVertScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FVertScrollBarWindow.Visible and StyleServices.Available and
    (seBorder in TBaseVirtualTree(Control).StyleElements) then
  begin
    B := TBitmap.Create;
    try
      B.Width := VertScrollRect.Width;
      B.Height := FVertScrollBarWindow.Height;
      MoveWindowOrg(B.Canvas.Handle, -VertScrollRect.Left,
        -VertScrollRect.Top);
      R := VertScrollRect;
      R.Bottom := B.Height + VertScrollRect.Top;
      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);
      R.Top := VertUpButtonRect.Bottom;
      R.Bottom := VertDownButtonRect.Top;

      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(VertSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        VertSliderRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(VertUpState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        VertUpButtonRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails
          (VertDownState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        VertDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, VertScrollRect.Left,
        VertScrollRect.Top);
      with VertScrollRect do
        BitBlt(DC, Left, Top, B.Width, B.Height - TBaseVirtualTreeCracker(Control).BorderWidth, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

procedure TVclStyleScrollBarsHook.MouseLeave;
begin
  inherited;
  if VertSliderState = tsThumbBtnVertHot then
    VertSliderState := tsThumbBtnVertNormal;

  if HorzSliderState = tsThumbBtnHorzHot then
    HorzSliderState := tsThumbBtnHorzNormal;

  if VertUpState = tsArrowBtnUpHot then
    VertUpState := tsArrowBtnUpNormal;

  if VertDownState = tsArrowBtnDownHot then
    VertDownState := tsArrowBtnDownNormal;

  if HorzUpState = tsArrowBtnLeftHot then
    HorzUpState := tsArrowBtnLeftNormal;

  if HorzDownState = tsArrowBtnRightHot then
    HorzDownState := tsArrowBtnRightNormal;

  PaintScroll;
end;

procedure TVclStyleScrollBarsHook.PaintScroll();
begin
  if FVertScrollBarWindow.HandleAllocated then begin
    FVertScrollBarWindow.Repaint;
    RedrawWindow(FVertScrollBarWindow.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE); // Fixes issue #698
  end;
  if FHorzScrollBarWindow.HandleAllocated then begin
    FHorzScrollBarWindow.Repaint;
    RedrawWindow(FHorzScrollBarWindow.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE); // Fixes issue #698
  end;
end;

function TVclStyleScrollBarsHook.PointInTreeHeader(const P: TPoint): Boolean;
begin
  Result := TBaseVirtualTreeCracker(Control).Header.InHeader(P);
end;

procedure TVclStyleScrollBarsHook.UpdateScrollBarWindow;
var
  R: TRect;
  Owner: TBaseVirtualTree;
  HeaderHeight: Integer;
  BorderWidth: Integer;
begin
  Owner := TBaseVirtualTree(Control);
  if (hoVisible in TBaseVirtualTreeCracker(Owner).Header.Options) then
    HeaderHeight := TBaseVirtualTreeCracker(Owner).Header.Height
  else
    HeaderHeight := 0;
  BorderWidth := 0;
  // VertScrollBarWindow

  if FVertScrollBarWindow.Visible and (seBorder in Control.StyleElements)
  then
  begin
    R := VertScrollRect;
    if Control.BiDiMode = bdRightToLeft then
    begin
      OffsetRect(R, -R.Left, 0);
      if HasBorder then
        OffsetRect(R, GetSystemMetrics(SM_CXEDGE), 0);
    end;
    if HasBorder then
      BorderWidth := GetSystemMetrics(SM_CYEDGE) * 2;
    ShowWindow(FVertScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(FVertScrollBarWindow.Handle, HWND_TOP, Control.Left + R.Left +
      TBaseVirtualTreeCracker(Control).BorderWidth, Control.Top + R.Top + HeaderHeight
      + TBaseVirtualTreeCracker(Control).BorderWidth, R.Right - R.Left,
      Control.Height - HeaderHeight - BorderWidth - TBaseVirtualTreeCracker(Control)
      .BorderWidth, SWP_SHOWWINDOW);
  end
  else
    ShowWindow(FVertScrollBarWindow.Handle, SW_HIDE);

  // HorzScrollBarWindow
  if FHorzScrollBarWindow.Visible and (seBorder in TBaseVirtualTree(Control).StyleElements)
  then
  begin
    R := HorzScrollRect;
    if Control.BiDiMode = bdRightToLeft then
      OffsetRect(R, VertScrollRect.Width, 0);
    ShowWindow(FHorzScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(FHorzScrollBarWindow.Handle, HWND_TOP, Control.Left + R.Left +
      TBaseVirtualTreeCracker(Control).BorderWidth, Control.Top + R.Top +
      TBaseVirtualTreeCracker(Control).BorderWidth + HeaderHeight, R.Right - R.Left,
      R.Bottom - R.Top, SWP_SHOWWINDOW);
  end
  else
    ShowWindow(FHorzScrollBarWindow.Handle, SW_HIDE);
end;

procedure TVclStyleScrollBarsHook.WMCaptureChanged(var Msg: TMessage);
begin
  if FVertScrollBarWindow.Visible and FVertScrollBarWindow.Enabled then
  begin
    if VertUpState = tsArrowBtnUpPressed then
    begin
      VertUpState := tsArrowBtnUpNormal;
      PaintScroll;
    end;

    if VertDownState = tsArrowBtnDownPressed then
    begin
      VertDownState := tsArrowBtnDownNormal;
      PaintScroll;
    end;
  end;

  if FHorzScrollBarWindow.Visible and FHorzScrollBarWindow.Enabled then
  begin
    if HorzUpState = tsArrowBtnLeftPressed then
    begin
      HorzUpState := tsArrowBtnLeftNormal;
      PaintScroll;
    end;

    if HorzDownState = tsArrowBtnRightPressed then
    begin
      HorzDownState := tsArrowBtnRightNormal;
      PaintScroll;
    end;
  end;

  CallDefaultProc(TMessage(Msg));
  Handled := True;
end;


  procedure TVclStyleScrollBarsHook.WMEraseBkgnd(var Message: TWMEraseBkgnd);
  begin

     Handled := True;
  end;


procedure TVclStyleScrollBarsHook.WMHScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.CMUpdateVclStyleScrollbars
  (var Message: TMessage);
begin
  CalcScrollBarsRect;
  PaintScroll;
end;

procedure TVclStyleScrollBarsHook.WMKeyDown(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMKeyUp(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMLButtonDown(var Msg: TWMMouse);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);
  if not PointInTreeHeader(P) then
  begin
    if FVertScrollBarWindow.Visible then
    begin
      if VertSliderState = tsThumbBtnVertPressed then
      begin
        PostMessage(Handle, WM_VSCROLL,
          Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
        FLeftMouseButtonDown := False;
        VertSliderState := tsThumbBtnVertNormal;
        PaintScroll;
        Handled := True;
        ReleaseCapture;
        Exit;
      end;

      if VertUpState = tsArrowBtnUpPressed then
        VertUpState := tsArrowBtnUpNormal;

      if VertDownState = tsArrowBtnDownPressed then
        VertDownState := tsArrowBtnDownNormal;
    end;

    if FHorzScrollBarWindow.Visible then
    begin
      if HorzSliderState = tsThumbBtnHorzPressed then
      begin
        PostMessage(Handle, WM_HSCROLL,
          Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
        FLeftMouseButtonDown := False;
        HorzSliderState := tsThumbBtnHorzNormal;
        PaintScroll;
        Handled := True;
        ReleaseCapture;
        Exit;
      end;

      if HorzUpState = tsArrowBtnLeftPressed then
        HorzUpState := tsArrowBtnLeftNormal;

      if HorzDownState = tsArrowBtnRightPressed then
        HorzDownState := tsArrowBtnRightNormal;
    end;
    PaintScroll;
  end;
  FLeftMouseButtonDown := False;
end;

procedure TVclStyleScrollBarsHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
begin
  inherited;
  if VertSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    if SF.nPos <> Round(ScrollPos) then
      ScrollPos := SF.nPos;

    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.Y - PrevScrollPos) /
      VertSliderRect.Height);
    if ScrollPos < SF.nMin then
      ScrollPos := SF.nMin;
    if ScrollPos > SF.nMax then
      ScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(ScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        ScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    PrevScrollPos := Mouse.CursorPos.Y;
    SF.nPos := Round(ScrollPos);

    SetScrollInfo(Handle, SB_VERT, SF, False);
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION,
      Min(Round(ScrollPos), High(SmallInt)))), 0);
    // Min() prevents range check error

    PaintScroll;
    Handled := True;
    Exit;
  end;

  if HorzSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    if SF.nPos <> Round(ScrollPos) then
      ScrollPos := SF.nPos;

    ScrollPos := ScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.X - PrevScrollPos) /
      HorzSliderRect.Width);
    if ScrollPos < SF.nMin then
      ScrollPos := SF.nMin;
    if ScrollPos > SF.nMax then
      ScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(ScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        ScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    PrevScrollPos := Mouse.CursorPos.X;
    SF.nPos := Round(ScrollPos);

    SetScrollInfo(Handle, SB_HORZ, SF, False);
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBPOSITION,
      Round(ScrollPos))), 0);

    PaintScroll;
    Handled := True;
    Exit;
  end;

  if HorzSliderState = tsThumbBtnHorzHot then
  begin
    HorzSliderState := tsThumbBtnHorzNormal;
    PaintScroll;
  end
  else if VertSliderState = tsThumbBtnVertHot then
  begin
    VertSliderState := tsThumbBtnVertNormal;
    PaintScroll;
  end
  else if HorzUpState = tsArrowBtnLeftHot then
  begin
    HorzUpState := tsArrowBtnLeftNormal;
    PaintScroll;
  end
  else if HorzDownState = tsArrowBtnRightHot then
  begin
    HorzDownState := tsArrowBtnRightNormal;
    PaintScroll;
  end
  else if VertUpState = tsArrowBtnUpHot then
  begin
    VertUpState := tsArrowBtnUpNormal;
    PaintScroll;
  end
  else if VertDownState = tsArrowBtnDownHot then
  begin
    VertDownState := tsArrowBtnDownNormal;
    PaintScroll;
  end;

  CallDefaultProc(TMessage(Msg));
  if FLeftMouseButtonDown then
    PaintScroll;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMMouseWheel(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMNCLButtonDblClk(var Msg: TWMMouse);
begin
  WMNCLButtonDown(Msg);
end;

procedure TVclStyleScrollBarsHook.WMNCLButtonDown(var Msg: TWMMouse);
var
  P: TPoint;
  SF: TScrollInfo;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if not PointInTreeHeader(P) then
  begin
    if FVertScrollBarWindow.Visible then
    begin
      if PtInRect(VertSliderRect, P) then
      begin
        FLeftMouseButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_VERT, SF);
        // FListPos := SF.nPos;
        ScrollPos := SF.nPos;
        PrevScrollPos := Mouse.CursorPos.Y;
        VertSliderState := tsThumbBtnVertPressed;
        PaintScroll;
        SetCapture(Handle);
        Handled := True;
        Exit;
      end;

      if FVertScrollBarWindow.Enabled then
      begin
        if PtInRect(VertDownButtonRect, P) then
          VertDownState := tsArrowBtnDownPressed;
        if PtInRect(VertUpButtonRect, P) then
          VertUpState := tsArrowBtnUpPressed;
      end;
    end;

    if FHorzScrollBarWindow.Visible then
    begin
      if PtInRect(HorzSliderRect, P) then
      begin
        FLeftMouseButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_HORZ, SF);
        // FListPos := SF.nPos;
        ScrollPos := SF.nPos;
        PrevScrollPos := Mouse.CursorPos.X;
        HorzSliderState := tsThumbBtnHorzPressed;
        PaintScroll;
        SetCapture(Handle);
        Handled := True;
        Exit;
      end;

      if FHorzScrollBarWindow.Enabled then
      begin
        if PtInRect(HorzDownButtonRect, P) then
          HorzDownState := tsArrowBtnRightPressed;
        if PtInRect(HorzUpButtonRect, P) then
          HorzUpState := tsArrowBtnLeftPressed;
      end;
    end;
    FLeftMouseButtonDown := True;
    PaintScroll;
  end;
end;

procedure TVclStyleScrollBarsHook.WMNCLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
  B: Boolean;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  B := PointInTreeHeader(P);

  if not B then
  begin
    if FVertScrollBarWindow.Visible then
      if FVertScrollBarWindow.Enabled then
      begin
        if VertSliderState = tsThumbBtnVertPressed then
        begin
          FLeftMouseButtonDown := False;
          VertSliderState := tsThumbBtnVertNormal;
          PaintScroll;
          Handled := True;
          Exit;
        end;

        if PtInRect(VertDownButtonRect, P) then
          VertDownState := tsArrowBtnDownHot
        else
          VertDownState := tsArrowBtnDownNormal;

        if PtInRect(VertUpButtonRect, P) then
          VertUpState := tsArrowBtnUpHot
        else
          VertUpState := tsArrowBtnUpNormal;
      end;

    if FHorzScrollBarWindow.Visible then
      if FHorzScrollBarWindow.Enabled then
      begin
        if HorzSliderState = tsThumbBtnHorzPressed then
        begin
          FLeftMouseButtonDown := False;
          HorzSliderState := tsThumbBtnHorzNormal;
          PaintScroll;
          Handled := True;
          Exit;
        end;

        if PtInRect(HorzDownButtonRect, P) then
          HorzDownState := tsArrowBtnRightHot
        else
          HorzDownState := tsArrowBtnRightNormal;

        if PtInRect(HorzUpButtonRect, P) then
          HorzUpState := tsArrowBtnLeftHot
        else
          HorzUpState := tsArrowBtnLeftNormal;
      end;
    CallDefaultProc(TMessage(Msg));
  end;

  if not B and (FHorzScrollBarWindow.Visible) or (FVertScrollBarWindow.Visible)
  then
    PaintScroll;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMNCMouseMove(var Msg: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
  B: Boolean;
begin
  inherited;
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if PointInTreeHeader(P) then
  begin
    CallDefaultProc(TMessage(Msg));
    PaintScroll;
    Handled := True;
    Exit;
  end;

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  MustUpdateScroll := False;
  if FVertScrollBarWindow.Enabled then
  begin
    B := PtInRect(VertSliderRect, P);
    if B and (VertSliderState = tsThumbBtnVertNormal) then
    begin
      VertSliderState := tsThumbBtnVertHot;
      MustUpdateScroll := True;
    end
    else if not B and (VertSliderState = tsThumbBtnVertHot) then
    begin
      VertSliderState := tsThumbBtnVertNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(VertDownButtonRect, P);
    if B and (VertDownState = tsArrowBtnDownNormal) then
    begin
      VertDownState := tsArrowBtnDownHot;
      MustUpdateScroll := True;
    end
    else if not B and (VertDownState = tsArrowBtnDownHot) then
    begin
      VertDownState := tsArrowBtnDownNormal;
      MustUpdateScroll := True;
    end;
    B := PtInRect(VertUpButtonRect, P);
    if B and (VertUpState = tsArrowBtnUpNormal) then
    begin
      VertUpState := tsArrowBtnUpHot;
      MustUpdateScroll := True;
    end
    else if not B and (VertUpState = tsArrowBtnUpHot) then
    begin
      VertUpState := tsArrowBtnUpNormal;
      MustUpdateScroll := True;
    end;
  end;

  if FHorzScrollBarWindow.Enabled then
  begin
    B := PtInRect(HorzSliderRect, P);
    if B and (HorzSliderState = tsThumbBtnHorzNormal) then
    begin
      HorzSliderState := tsThumbBtnHorzHot;
      MustUpdateScroll := True;
    end
    else if not B and (HorzSliderState = tsThumbBtnHorzHot) then
    begin
      HorzSliderState := tsThumbBtnHorzNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(HorzDownButtonRect, P);
    if B and (HorzDownState = tsArrowBtnRightNormal) then
    begin
      HorzDownState := tsArrowBtnRightHot;
      MustUpdateScroll := True;
    end
    else if not B and (HorzDownState = tsArrowBtnRightHot) then
    begin
      HorzDownState := tsArrowBtnRightNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(HorzUpButtonRect, P);
    if B and (HorzUpState = tsArrowBtnLeftNormal) then
    begin
      HorzUpState := tsArrowBtnLeftHot;
      MustUpdateScroll := True;
    end
    else if not B and (HorzUpState = tsArrowBtnLeftHot) then
    begin
      HorzUpState := tsArrowBtnLeftNormal;
      MustUpdateScroll := True;
    end;
  end;

  if MustUpdateScroll then
    PaintScroll;
end;

procedure TVclStyleScrollBarsHook.WMNCPaint(var Msg: TMessage);
begin
 if (tsWindowCreating in TBaseVirtualTree(Control).TreeStates) then
  begin
    CalcScrollBarsRect;
    UpdateScrollBarWindow;
 end;
end;

procedure TVclStyleScrollBarsHook.WMSize(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
  PaintScroll;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMMove(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  if not(tsWindowCreating in TBaseVirtualTree(Control).TreeStates) then
  begin
    CalcScrollBarsRect;
    UpdateScrollBarWindow;
    PaintScroll;
  end;
  Handled := True;
end;

procedure TVclStyleScrollBarsHook.WMPosChanged(var Msg: TMessage);
begin
  WMMove(Msg);
end;

procedure TVclStyleScrollBarsHook.WMVScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScroll
  ;
  Handled := True;
end;

{ TVclStyleScrollBarsHook.TVclStyleScrollBarWindow }

constructor TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.Create
  (AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FScrollBarWindowOwner := nil;
  FScrollBarVertical := False;
  FScrollBarVisible := False;
  FScrollBarEnabled := False;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.CreateParams
  (var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or
    WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMEraseBkgnd
  (var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMNCHitTest
  (var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
end;

procedure TVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMPaint
  (var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  BeginPaint(Handle, PS);
  try
    if FScrollBarWindowOwner <> nil then
    begin
      DC := GetWindowDC(Handle);
      try
        if FScrollBarVertical then
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.VertScrollRect.Left,
            -FScrollBarWindowOwner.VertScrollRect.Top);
          FScrollBarWindowOwner.DrawVertScrollBar(DC);
        end
        else
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.HorzScrollRect.Left,
            -FScrollBarWindowOwner.HorzScrollRect.Top);
          FScrollBarWindowOwner.DrawHorzScrollBar(DC);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

initialization
  TCustomStyleEngine.RegisterStyleHook(TVirtualStringTree, TVclStyleScrollBarsHook);
  TCustomStyleEngine.RegisterStyleHook(TVirtualDrawTree, TVclStyleScrollBarsHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TVirtualStringTree, TVclStyleScrollBarsHook);
  TCustomStyleEngine.UnRegisterStyleHook(TVirtualDrawTree, TVclStyleScrollBarsHook);

end.

