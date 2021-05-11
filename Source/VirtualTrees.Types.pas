unit VirtualTrees.Types;

interface

uses
  WinApi.ActiveX,
  System.Types,
  System.SysUtils,
  System.UITypes,
  Vcl.Controls;

const
  VTVersion              = '7.5.0' deprecated 'This const is going to be removed in a future version';

const
  VTTreeStreamVersion    = 3;
  VTHeaderStreamVersion  = 6;    // The header needs an own stream version to indicate changes only relevant to the header.

  CacheThreshold         = 2000; // Number of nodes a tree must at least have to start caching and at the same
                                // time the maximum number of nodes between two cache entries.
  FadeAnimationStepCount = 255;  // Number of animation steps for hint fading (0..255).
  ShadowSize             = 5;    // Size in pixels of the hint shadow. This value has no influence on Win2K and XP systems
                                // as those OSes have native shadow support.

  // Special identifiers for columns.
  NoColumn                 = - 1;
  InvalidColumn            = - 2;

  // Indices for check state images used for checking.
  ckEmpty                  = 0; // an empty image used as place holder
  // radio buttons
  ckRadioUncheckedNormal   = 1;
  ckRadioUncheckedHot      = 2;
  ckRadioUncheckedPressed  = 3;
  ckRadioUncheckedDisabled = 4;
  ckRadioCheckedNormal     = 5;
  ckRadioCheckedHot        = 6;
  ckRadioCheckedPressed    = 7;
  ckRadioCheckedDisabled   = 8;
  // check boxes
  ckCheckUncheckedNormal   = 9;
  ckCheckUncheckedHot      = 10;
  ckCheckUncheckedPressed  = 11;
  ckCheckUncheckedDisabled = 12;
  ckCheckCheckedNormal     = 13;
  ckCheckCheckedHot        = 14;
  ckCheckCheckedPressed    = 15;
  ckCheckCheckedDisabled   = 16;
  ckCheckMixedNormal       = 17;
  ckCheckMixedHot          = 18;
  ckCheckMixedPressed      = 19;
  ckCheckMixedDisabled     = 20;
  // simple button
  ckButtonNormal           = 21;
  ckButtonHot              = 22;
  ckButtonPressed          = 23;
  ckButtonDisabled         = 24;

  // Instead using a TTimer class for each of the various events I use Windows timers with messages
  // as this is more economical.
  ExpandTimer            = 1;
  EditTimer              = 2;
  HeaderTimer            = 3;
  ScrollTimer            = 4;
  ChangeTimer            = 5;
  StructureChangeTimer   = 6;
  SearchTimer            = 7;
  ThemeChangedTimer      = 8;

  ThemeChangedTimerDelay = 500;

  // Virtual Treeview does not need to be subclassed by an eventual Theme Manager instance as it handles
  // Windows XP theme painting itself. Hence the special message is used to prevent subclassing.
  CM_DENYSUBCLASSING            = CM_BASE + 2000;

  // Decoupling message for auto-adjusting the internal edit window.
  CM_AUTOADJUST                 = CM_BASE + 2005;

  // Drag image helpers for Windows 2000 and up.
  IID_IDropTargetHelper : TGUID = (D1 : $4657278B; D2 : $411B; D3 : $11D2; D4 : ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDragSourceHelper : TGUID = (D1 : $DE5BF786; D2 : $477A; D3 : $11D2; D4 : ($83, $9D, $00, $C0, $4F, $D9, $18, $D0));
  IID_IDropTarget : TGUID       = (D1 : $00000122; D2 : $0000; D3 : $0000; D4 : ($C0, $00, $00, $00, $00, $00, $00, $46));

  // VT's own clipboard formats,
  // Note: The reference format is used internally to allow to link to a tree reference
  //       to implement optimized moves and other back references.
  CFSTR_VIRTUALTREE      = 'Virtual Tree Data';
  CFSTR_VTREFERENCE      = 'Virtual Tree Reference';
  CFSTR_HTML             = 'HTML Format';
  CFSTR_RTF              = 'Rich Text Format';
  CFSTR_RTFNOOBJS        = 'Rich Text Format Without Objects';
  CFSTR_CSV              = 'CSV';

  // Help identifiers for exceptions. Application developers are responsible to link them with actual help topics.
  hcTFEditLinkIsNil      = 2000;
  hcTFWrongMoveError     = 2001;
  hcTFWrongStreamFormat  = 2002;
  hcTFWrongStreamVersion = 2003;
  hcTFStreamTooSmall     = 2004;
  hcTFCorruptStream1     = 2005;
  hcTFCorruptStream2     = 2006;
  hcTFClipboardFailed    = 2007;
  hcTFCannotSetUserData  = 2008;

  // Header standard split cursor.
  crHeaderSplit          = TCursor(63);

  // Height changing cursor.
  crVertSplit            = TCursor(62);


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

  TCheckType = (
    ctNone,
    ctTriStateCheckBox,
    ctCheckBox,
    ctRadioButton,
    ctButton
    );

  // The check states include both, transient and fluent (temporary) states. The only temporary state defined so
  // far is the pressed state.
  TCheckState = (
    csUncheckedNormal,   // unchecked and not pressed
    csUncheckedPressed,  // unchecked and pressed
    csCheckedNormal,     // checked and not pressed
    csCheckedPressed,    // checked and pressed
    csMixedNormal,       // 3-state check box and not pressed
    csMixedPressed,      // 3-state check box and pressed
    csUncheckedDisabled, // disabled checkbox, not checkable
    csCheckedDisabled,   // disabled checkbox, not uncheckable
    csMixedDisabled      // disabled 3-state checkbox
    );

  /// Adds some convenience methods to type TCheckState
  TCheckStateHelper = record helper for TCheckState
  strict private
  const
    // Lookup to quickly convert a specific check state into its pressed counterpart and vice versa.
    cPressedState : array [TCheckState] of TCheckState   = (
      csUncheckedPressed, csUncheckedPressed, csCheckedPressed, csCheckedPressed, csMixedPressed, csMixedPressed, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
    cUnpressedState : array [TCheckState] of TCheckState = (
      csUncheckedNormal, csUncheckedNormal, csCheckedNormal, csCheckedNormal, csMixedNormal, csMixedNormal, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
    cEnabledState : array [TCheckState] of TCheckState   = (
      csUncheckedNormal, csUncheckedPressed, csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed, csUncheckedNormal, csCheckedNormal, csMixedNormal);
    cToggledState : array [TCheckState] of TCheckState   = (
      csCheckedNormal, csCheckedPressed, csUncheckedNormal, csUncheckedPressed, csCheckedNormal, csCheckedPressed, csUncheckedDisabled, csCheckedDisabled, csMixedDisabled);
  public
    function GetPressed() : TCheckState; inline;
    function GetUnpressed() : TCheckState; inline;
    function GetEnabled() : TCheckState; inline;
    function GetToggled() : TCheckState; inline;
    function IsDisabled() : Boolean; inline;
    function IsChecked() : Boolean; inline;
    function IsUnChecked() : Boolean; inline;
    function IsMixed() : Boolean; inline;
  end;


implementation

{ TCheckStateHelper }

function TCheckStateHelper.IsDisabled : Boolean;
begin
  Result := Self >= TCheckState.csUncheckedDisabled;
end;

function TCheckStateHelper.IsChecked : Boolean;
begin
  Result := Self in [csCheckedNormal, csCheckedPressed, csCheckedDisabled];
end;

function TCheckStateHelper.IsUnChecked : Boolean;
begin
  Result := Self in [csUncheckedNormal, csUncheckedPressed, csUncheckedDisabled];
end;

function TCheckStateHelper.IsMixed : Boolean;
begin
  Result := Self in [csMixedNormal, csMixedPressed, csMixedDisabled];
end;

function TCheckStateHelper.GetEnabled : TCheckState;
begin
  Result := cEnabledState[Self];
end;

function TCheckStateHelper.GetPressed() : TCheckState;
begin
  Result := cPressedState[Self];
end;

function TCheckStateHelper.GetUnpressed() : TCheckState;
begin
  Result := cUnpressedState[Self];
end;

function TCheckStateHelper.GetToggled() : TCheckState;
begin
  Result := cToggledState[Self];
end;

end.
