unit VirtualTrees.Types;

interface

uses
  WinApi.ActiveX,
  System.Types,
  System.SysUtils;

type
{$IFDEF VT_FMX}
  TDimension = Single;
{$ELSE}
  TDimension = Integer; // For Firemonkey support, see #841
{$ENDIF}
  TColumnIndex = type Integer;
  TColumnPosition = type Cardinal;
  PCardinal = ^Cardinal;

  // The exception used by the trees.
  EVirtualTreeError = class(Exception);

  // Limits the speed interval which can be used for auto scrolling (milliseconds).
  TAutoScrollInterval = 1 .. 1000;

  TVTScrollIncrement = 1 .. 10000;

  // OLE drag'n drop support
  TFormatEtcArray = array of TFormatEtc;
  TFormatArray = array of Word;

  // protection against TRect record method that cause problems with with-statements
  TWithSafeRect = record
    case Integer of
      0 :
        (Left, Top, Right, Bottom : Integer);
      1 :
        (TopLeft, BottomRight : TPoint);
  end;

  TAddPopupItemType = (apNormal, apDisabled, apHidden);

implementation

end.
