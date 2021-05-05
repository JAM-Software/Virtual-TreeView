unit VirtualTrees.Constants;

interface

uses
  System.UITypes,
  Vcl.Themes;

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

implementation

end.
