unit VirtualTrees.BaseAncestorVCL;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski, look at VirtualTrees.pas as some code moved from there                 }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface
uses
  Winapi.Windows,
  Winapi.oleacc,
  Winapi.ActiveX,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.StdCtrls,
  VirtualTrees.Types;

type
  TVTBaseAncestorVcl = class abstract(TCustomControl)
  private
    // MSAA support
    FAccessible: IAccessible;                    // The IAccessible interface to the window itself.
    FAccessibleItem: IAccessible;                // The IAccessible to the item that currently has focus.
    FAccessibleName: string;                     // The name the window is given for screen readers.
    FDottedBrushTreeLines: TBrush;               // used to paint dotted lines without special pens

    function GetDottedBrushGridLines: TBrush;
  protected // methods
    function DoRenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HRESULT; virtual; abstract;
    function RenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HResult; virtual; abstract;
    procedure NotifyAccessibilityCollapsed(); virtual; abstract;
    function PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush; virtual;
  protected //properties
    property DottedBrushTreeLines: TBrush read FDottedBrushTreeLines write FDottedBrushTreeLines;
    property DottedBrushGridLines: TBrush read GetDottedBrushGridLines;
  public // methods
    procedure CopyToClipboard; virtual; abstract;
    procedure CutToClipboard; virtual; abstract;
    function PasteFromClipboard: Boolean; virtual; abstract;

    function InvalidateRect(lpRect: PRect; bErase: BOOL): BOOL; inline;
    function UpdateWindow(): BOOL; inline;

    procedure ShowScrollBar(Bar: Integer; AShow: Boolean);
    function SetScrollInfo(Bar: Integer; const ScrollInfo: TScrollInfo; Redraw: Boolean): TDimension;
    function GetScrollInfo(Bar: Integer; var ScrollInfo: TScrollInfo): Boolean;
    function GetScrollPos(Bar: Integer): TDimension;
  public //properties
    property Accessible: IAccessible read FAccessible write FAccessible;
    property AccessibleItem: IAccessible read FAccessibleItem write FAccessibleItem;
    property AccessibleName: string read FAccessibleName write FAccessibleName;
  end;

implementation

//----------------------------------------------------------------------------------------------------------------------
function TVTBaseAncestorVcl.PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush;
begin
  if Assigned(CurrentDottedBrush) then
    begin
      Result := CurrentDottedBrush;
    end else
    begin
      Result := TBrush.Create;
      Result.Bitmap := TBitmap.Create;
    end;

  Result.Bitmap.Handle := CreateBitmap(8, 8, 1, 1, Bits);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.InvalidateRect(lpRect: PRect; bErase: BOOL): BOOL;
begin
  Result:= WinApi.Windows.InvalidateRect(Handle, lpRect, bErase);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.UpdateWindow(): BOOL;
begin
  Result:= WinApi.Windows.UpdateWindow(Handle);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTBaseAncestorVcl.ShowScrollBar(Bar: Integer; AShow: Boolean);
begin
  WinApi.Windows.ShowScrollBar(Handle, Bar, AShow);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.SetScrollInfo(Bar: Integer; const ScrollInfo: TScrollInfo; Redraw: Boolean): TDimension;
begin
  Result:= WinApi.Windows.SetScrollInfo(Handle, Bar, ScrollInfo, Redraw);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.GetDottedBrushGridLines: TBrush;
begin
  Result:= FDottedBrushTreeLines;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.GetScrollInfo(Bar: Integer; var ScrollInfo: TScrollInfo): Boolean;
begin
  Result:= WinApi.Windows.GetScrollInfo(Handle, Bar, ScrollInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTBaseAncestorVcl.GetScrollPos(Bar: Integer): TDimension;
begin
  Result:= WinApi.Windows.GetScrollPos(Handle, Bar);
end;

end.