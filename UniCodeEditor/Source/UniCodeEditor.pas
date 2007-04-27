unit UniCodeEditor;

// Version 2.2.18
//
// UniCodeEditor, a Unicode Source Code Editor for Delphi.
//
// UniCodeEditor is released under the MIT license:
// Copyright (c) 1999-2005 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
// OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// You are asked to give the author(s) the due credit. This means that you acknowledge the work of the author(s)
// in the product documentation, about box, help or wherever a prominent place is.
//
//----------------------------------------------------------------------------------------------------------------------
//
// April 2007
//   - Bug fix: pressing Delete on the last line with the cursor at the end of the line removed the entire line.
//   - Bug fix: SetBlockBegin assumed a char index instead column value for computation of the max X coordinate.
// March 2007
//   - Improvement: removal of line styles now removes all occurences of this style.
// January 2007
//   - Bug fix: clipboard operations triggered by messages (WM_PASTE etc.) are not handled by standard code preventing
//              so proper work of the undo stack etc.
//   - Change: eoAutoExtendWorkWidth removed, it is problematic with existing apps and does not provide much advantage.
//   - Improvement: better group undo
//   - Bug fix: line breaks insertion undo was wrong
//   - Bug fix: default work width being the client width is suboptimal, start with 0
// December 2006
//   - Bug fix: target position for replace undo action was wrong.
//   - Bug fix: TUniCodeEditorContent.GetText does not add a line break for the last line.
//   - Bug fix: IME input does not set caret reversing so the correct input order
//   - Bug fix: Control char colors
//   - Improvement: control char painting using Wingdings 3 font instead relying on the current font.
//   - Bug fix: tab handling when unindenting
//   - Bug fix: undo action for auto line end cleanup
// November 2006
//   - Bug fix: handling of <return> key was not correct (on forms with a default button it cause the default button to trigger)
//   - Improvement: search code optimized
//   - Bug fix: wrong invalidation on char width change
//   - Improvement: implicit reset of work area width on clear and full text replacement
//   - Bug fix: horizontal page-wise scrolling was broken.
// September 2006
//   - Change: eoAutoExtendWorkWidth introduced
// August 2006
//   - Change: OnFocusChanged event
//   - Change: Extended text collection (from block or position)
// January 2006
//   - Bug fix: line highlighting with custom styles was wrong.
// November 2005
//   - Bug fix: line validation during change.
//   - Improvement: CaretOffset, for giving a x/y pair and a linear offset for caret placing.
//   - Bug fix: line deletion.
//   - Bug fix: cut text.
// September 2005
//   - Improvement: Added undo handling for Clear and Text set actions.
//   - Bug fix: Undo/Redo handling reworked.
//   - Improvement: TUCELine.ClearStyles and TUCELine.ClearMarkers
//   - Improvement: More line specific routines moved to the line class (e.g. column <-> char index conversion etc.).
// April 2005
//   - Bug fix: Internal line index of a line not updated when a new line is inserted.
// February 2005
//   - Bug fix: key handled flag removed, as it does not work with multiple keypresses.
// January 2005
//   - Change: UCE is now released under the MIT license.
//   - Bug fix: Cursor was not visible when moved to the right border.
//   - Bug fix: Thumb scrolling does not work correctly for large scroll ranges.
//   - Improvement: Introduction of local index in lines for quick lookup.
// December 2004
//   - Improvement: new option eoLineNumbersZeroBased to display line numbers in the gutter starting with 0 or 1.
// November 2004
//   - Bug fix: Correct insertion point for CutToClipboard.
//   - Bug fix: Wrong selection state when cutting/deleting text.
//   - Bug fix: Selection setting
//   - Improvement: Added bookmark change event.
//   - Bug fix: Selection end must be set too when start is set.
//   - Improvement: ClearSelection
//   Version 2.1, 2003-08-17, Mike Zinner
//     Added LineHeight property, ControlStyle csNeedsBorderPaint for XP border
//     Added properties SelStart, SelEnd, method SetCaretToEditorBottom
//     Added support for WMCopy, WMCut, WMPaste support
//     Bugfixed SetSelStart
//   Version 2.0, 2003-08-17, Mike Zinner
//     Repackaged for D7
//     Renamed to UniCodeEditor
//   Version 1.2, 2000-03-01, Mike Lischke
//     search and replace, bug fixes
//   Version 1.2, 1999-12-01, Mike Lischke
//     switched entirely to Unicode (some code already was using Unicode, e.g. output)
//     basic IME handling
//     mouse wheel support, undo/redo, bookmarks, painting optimization, scroll hint window, bug fixes
//   Version 1.1, 1999-07-01, Mike Lischke
//     line numbers, structural rework, clean up, bug fixes
//   Version 1.0, 1999-03-10, Mike Lischke
//     First implementation from a freeware lib (mwEdit), significant changes due to speed and needed functionality
//     (removed lots of redundancies in string handling, total rework of output, undo / redo, tab support, control char
//     output, first Unicode support etc.)
//
// Credits for their valuable assistance and code donations go to:
//   Mike Zinner, Tom-Vidar Nilsen
//
// Open issues:
//   - Because the the control depends on Unicode.pas, which is still beta, also the syntax editor must be considered
//     as being beta. In particular not all search (and replace) functionality is fully implemented and tested.
//   - Although the edit works with wide strings, the highlighter do not yet, because new tools must to be created
//     to build new highlighter classes which fully support wide strings (see also my homepage for DCG, the
//     Delphi Compiler Generator, which will soon be extended to create Unicode highlighters). The impact on the current
//     implementation can be seen where the edit needs to act on the text like looking for word boundaries etc.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$Booleval off} // Use fastest possible boolean evaluation.

{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Math,
  ExtCtrls, Forms, StdCtrls, Clipbrd, Unicode, Contnrs,
  UCEEditorKeyCommands, UCEHighlighter, UCEShared;

const
  UCEVersion = '2.2.18';

  // Self defined cursors for drag'n'drop.
  crDragMove = 2910;
  crDragCopy = 3110;

type
  // eoSmartTabs and eoUseTabs are mutual exclusive (avoid activating both, as this confuses the input).
  // To use any of the tab options you must also have eoWantTabs enabled, otherwise the TAB char
  // will never even reach the edit.
  TUniCodeEditOption = (
    eoAutoIndent,                 // Do auto indentation when inserting a new line.
    eoAutoUnindent,               // Do auto undindentation when deleting a character which
                                  // is only preceded by whitespace characters.
    eoCursorThroughTabs,          // Move cursor as if tabulators were (TabSize) spaces.
    eoGroupUndo,                  // While executing undo/redo handle all continous changes of
                                  // the same kind in one call instead undoing/redoing each command separately.
    eoHideSelection,              // Hide selection when unfocused.
    eoInserting,                  // Denotes whether the control is in insert or in overwrite mode
                                  // (this option in superseeded by eoReadOnly).
    eoKeepTrailingBlanks,         // Don't automatically remove white spaces at the end of a line.
    eoLineNumbers,                // Show line numbers in gutter.
    eoLineNumbersZeroBased,       // Line numbers in the gutter start with 0. Only meaningfull with eoLineNumbers.
    eoOptimalFill,                // Not yet implemented.
    eoReadOnly,                   // Prevent the content of the control from being changed.
    eoReadOnlySelection,          // Show selection if in read-only mode (useful for simulation of static text).
    eoScrollPastEOL,              // Allow the cursor to go past the end of the line.
    eoShowControlChars,           // Show tabulators, spaces and line breaks as glyphs.
    eoShowCursorWhileReadOnly,    // Don't hide the cursor if control is in read-only mode.
    eoShowScrollHint,             // Show a hint window with the current top line while scrolling with the thumb.
    eoSmartTabs,                  // Automatically put in enough spaces when TAB has been pressed to move
                                  // the cursor to the next non-white space character of the previous line
                                  // (actually only the cursor is moved, the spaces are inserted later).
    eoTripleClicks,               // Allow selecting an entire line with a triple click.
    eoUndoAfterSave,              // Don't clear the undo/redo stack after the control has been saved.
    eoUseUndoRedo,                // If set then undo and redo functionality is enabled.
    eoUseTabs,                    // Don't covert TABs to spaces but use TAB with fixed distances (see TabSize).
    eoUseSyntaxHighlighting,      // Switch on syntax highlighting (ignored if no highlighter is assigned).
    eoWantTabs                    // Use TABs for input rather than moving the focus to the next control.
  );
  TUniCodeEditOptions = set of TUniCodeEditOption;

const
  // These glyphs are taken from Wingdings 3.
  DefaultTabGlyph       = WideChar($34);
  DefaultLineBreakGlyph = WideChar($38);
  DefaultSpaceGlyph     = WideChar($56);

  DefaultOptions = [eoAutoIndent, eoAutoUnindent, eoCursorThroughTabs, eoGroupUndo, eoHideSelection, eoInserting,
    eoScrollPastEOL, eoSmartTabs, eoTripleClicks, eoUseSyntaxHighlighting, eoWantTabs];

type
  TReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TCustomUnicodeEdit = class;

  TBookmark = record
    Visible: Boolean;
    X, Y: Integer;
  end;

  TPaintEvent = procedure(Sender: TCustomUnicodeEdit; ACanvas: TCanvas) of object;
  TCaretEvent = procedure(Sender: TCustomUnicodeEdit; X, Y: Integer) of object;
  TReplaceTextEvent = procedure(Sender: TCustomUnicodeEdit; const Search, Replace: WideString; Line, Start, Stop: Integer;
    var Action: TReplaceAction) of object;
  TProcessCommandEvent = procedure(Sender: TCustomUnicodeEdit; var Command: TEditorCommand; var AChar: WideChar;
    Data: Pointer) of object;
  TGutterMouseDownEvent = procedure(Sender: TCustomUnicodeEdit; Button: TMouseButton; Shift: TShiftState; X, Y,
    Line: Integer) of object;
  TBookmarkChangeEvent = procedure(Sender: TCustomUniCodeEdit; Index, X, Y: Integer; var Allowed: Boolean) of object;
  TFocusChangedEvent = procedure(Sender: TObject; HasFocus: Boolean) of object;

  TCaretType = (
    ctVerticalLine,
    ctHorizontalLine,
    ctHalfBlock,
    ctBlock
  );

  // Undo / redo reasons.
  TChangeReason = (
    ecrNone,
    ecrInsert,
    ecrDelete,
    ecrReplace,
    ecrDragMove,
    ecrDragCopy,
    ecrState,
    ecrIndentation,
    ecrUnindentation
  );

  // Due to group undo it is necessary to determine which reason are similar enough to be undone in one go.
  TChangeReasonGroup = (
    crgNone,         // ecrNone
    crgInsert,       // ecrInsert, ecrReplace
    crgDelete,       // ecrDelete
    crgDrag,         // ecrDragMove, ecrDragCopy
    crgState,        // ecrState
    crgIndentation   // ecrIndentation, ecrUnindentation
  );

  // Used to store certain values for undo/redo actions.
  TEditState = record
    Caret: TPoint;      // The caret position when this state info was collected.
    BlockBegin: TPoint; // The start of a block (e.g. selection or new text position etc.).
    BlockEnd: TPoint;   // The end of that block.
  end;

  // Describes the actual action that was performed and can be undone and redone.
  TChangeAction = record
    Reason: TChangeReason;
    Group: TChangeReasonGroup;
    SourceStart: TPoint;
    SourceEnd: TPoint;
    OldText: WideString;
    TargetStart: TPoint;
    TargetEnd: TPoint;
    NewText: WideString;
  end;

  // This record keeps the details of a change in the editor.
  PChange = ^TChange;
  TChange = record
    Action: TChangeAction;
    OldState: TEditState;
    NewState: TEditState;
  end;

  TUniCodeEditorContent = class;

  // Undo and redo in one list.
  TUndoList = class
  private
    FList: TList;
    FCurrent: Integer;
    FMaxUndo: Integer;
    FOwner: TCustomUniCodeEdit;
    FPendingChange: PChange;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCurrentRedoReason: TChangeReason;
    function GetCurrentRedoReasonGroup: TChangeReasonGroup;
    function GetCurrentUndoReason: TChangeReason;
    function GetCurrentUndoReasonGroup: TChangeReasonGroup;
    procedure SetMaxUndo(Value: Integer);
  protected
    function CanAcceptNewChange: Boolean;
    procedure FinishPendingChange; overload;
    procedure FinishPendingChange(TargetEnd: TPoint); overload;
    function PrepareDeleteChange(SourceStart, SourceEnd: TPoint): Boolean;
    function PrepareDragCopyChange(SourceStart, SourceEnd, TargetStart: TPoint): Boolean;
    function PrepareDragMoveChange(SourceStart, SourceEnd, TargetStart: TPoint): Boolean;
    function PrepareIndentationChange(TargetStart, TargetEnd: TPoint): Boolean;
    function PrepareInsertChange(TargetStart: TPoint; const Text: WideString): Boolean;
    function PrepareReplaceChange(SourceStart, SourceEnd, TargetStart: TPoint; const Text: WideString): Boolean;
    function PrepareStateChange: Boolean;
    function PrepareUnindentationChange(TargetStart, TargetEnd: TPoint): Boolean;
    procedure RemoveChange(Index: Integer);
  public
    constructor Create(AOwner: TCustomUniCodeEdit);
    destructor Destroy; override;

    function GetRedoChange(var Change: PChange): TChangeReason;
    function GetUndoChange(var Change: PChange): TChangeReason;
    procedure ClearList;

    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CurrentRedoReason: TChangeReason read GetCurrentRedoReason;
    property CurrentRedoReasonGroup: TChangeReasonGroup read GetCurrentRedoReasonGroup;
    property CurrentUndoReason: TChangeReason read GetCurrentUndoReason;
    property CurrentUndoReasonGroup: TChangeReasonGroup read GetCurrentUndoReasonGroup;
    property MaxUndo: Integer read FMaxUndo write SetMaxUndo;
  end;

  // States that describe the look of a line.
  TUCELineStates = set of (
    lsFolded,                // The line is folded. Use the line info record to know what to display and
                             // how large the folded area is.
    lsSelected,              // Set when the whole line is selected.
    lsValidated,             // If not set then line has not been officially validated (for users of the edit control).
    lsValidatedInternally,   // If not set then line has not initialized its internal structures.
    lsWrapped                // Line is wrapped at the the right margin.
  );

  // Default implementation of the custom style interface. This is mainly used for certain fixed properties
  // that must be set at design time (e.g. selection style, scroll hint).
  TUCELineStyle = class(TInterfacedPersistent, IUCELineStyle)
  private
    FForeground: TColor;
    FBackground: TColor;
    FForceFontStyles: Boolean;
    FFontStyles: TFontStyles;
  protected
    function GetBackground: TColor; virtual;
    function GetFontStyles: TFontStyles; virtual;
    function GetForceFontStyles: Boolean; virtual;
    function GetForeground: TColor; virtual;
    procedure SetBackground(const Color: TColor); virtual;
    procedure SetFontStyles(const Styles: TFontStyles); virtual;
    procedure SetForceFontStyles(Force: Boolean); virtual;
    procedure SetForeground(const Color: TColor); virtual;
  published
    property Background: TColor read GetBackground write SetBackground;
    property Foreground: TColor read GetForeground write SetForeground;
    property FontStyles: TFontStyles read GetFontStyles write SetFontStyles;
    property ForceFontStyles: Boolean read GetForceFontStyles write SetForceFontStyles;
  end;

  // The actual data of one line.
  TStyleStack = class(TStack)
  end;

  TDistanceArray = array of Integer;

  TUCELine = class
  private
    FOwner: TUniCodeEditorContent;
    FLexerState: Integer;                        // Lexer state to be used when this line is lexed. This is the state
                                                 // that the lexer was in when it finished the previous line.
    FText: WideString;                           // The text of the line.
    FIndex: Integer;                             // The index in the parent list.
    FStates: TUCELineStates;
    FBounds: TRect;
    FMarkers: TInterfaceList;                    // A list of markers to show in the gutter of the edit.
    FStyles: TStyleStack;                        // A stack of custom draw styles for the line. Only the top style is actually applied.
    FData: TObject;                              // Application defined data.
    FCharWidthArray: TDistanceArray;             // Distances between two consecutive characters. Needed for ExtTextOut.
    procedure SetText(const Value: WideString);
  protected
    procedure Changed; virtual;
    function CharIndexToColumn(Index: Integer): Integer;
    function ColumnToCharIndex(Column: Integer): Integer;
    procedure ComputeCharacterWidths(TabSize, DefaultWidth: Integer);
    procedure DrawMarkers(Index: Integer; Canvas: TCanvas; X, Y: Integer); virtual;
    function GetLineEnd(IgnoreWhiteSpace: Boolean): Integer;
    procedure InternalInvalidate; virtual;
    procedure InternalValidate; virtual;
    function NextCharPos(ThisPosition: Integer; ForceNonDefault: Boolean = False): Integer;
    function PreviousCharPos(ThisPosition: Integer): Integer; overload;
  public
    constructor Create(Owner: TUniCodeEditorContent); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TUCELine);
    function AddMarker(Marker: IUCELineMarker): Integer;
    procedure ClearMarkers;
    procedure ClearStyles;
    function HasMarker(Marker: IUCELineMarker): Boolean;
    procedure Invalidate; virtual;
    procedure RemoveMarker(Marker: IUCELineMarker);
    procedure ReplaceMarker(OldMarker, NewMarker: IUCELineMarker);
    function PeekStyle: IUCELineStyle;
    procedure PopStyle;
    procedure PushStyle(Style: IUCELineStyle);
    procedure RemoveStyle(Style: IUCELineStyle);
    procedure Validate; virtual;

    property Bounds: TRect read FBounds;
    property CharacterDistances: TDistanceArray read FCharWidthArray;
    property Data: TObject read FData write FData;
    property Index: Integer read FIndex;
    property LexerState: Integer read FLexerState write FLexerState;
    property States: TUCELineStates read FStates;
    property Text: WideString read FText write SetText;
  end;

  // The format of the text to load or write.
  TTextFormat = (
    tfANSI,        // Plain ANSI text. A language must be specified to allow conversion.
    tfUTF8,        // Unicode encoded as UTF 8.
    tfUTF16,       // UTF 16 with the system default byte order (on Windows this is LSB first, little-endian).
    tfUTF16LE,     // UTF 16 (little-endian) with LSB first.
    tfUTF16BE      // UTF 16 (big-endian) with MSB first.
  );

  TLineEvent = procedure(Sender: TObject; Line: TUCELine) of object;
  
  // This is an internal class for the editor and should not be used outside. It contains the actual text along
  // with additional info for each line.
  TUniCodeEditorContent = class
  private
    FOwner: TCustomUniCodeEdit;
    FLines: array of TUCELine;                   // The line storage.
    FCount: Integer;                             // The actual number of used lines. Might differ from the length of FLines.
    FModified: Boolean;

    FOnChangeLine,
    FOnValidateLine,
    FOnDeleteLine: TLineEvent; 
    function GetLine(Index: Integer): TUCELine;
    function GetLineNoInit(Index: Integer): TUCELine;
    function GetText: WideString;
    procedure Grow;
    procedure SetModified(const Value: Boolean);
    procedure SetText(const Value: WideString);
  protected
    procedure AutoAdjustWorkWidth(Index: Integer);
    function ConvertTabs(const S: WideString): WideString;
    procedure DoChangeLine(Line: TUCELine); virtual;
    procedure DoDeleteLine(Line: TUCELine); virtual;
    procedure DoValidateLine(Line: TUCELine); virtual;
    function EndPoint: TPoint;
    procedure InternalClear;
    function InternalCollectText(Start, Stop: TPoint): WideString;
    procedure InternalInvalidateAll;
    procedure SetCapacity(NewCapacity: Integer);
  public
    constructor Create(AOwner: TCustomUniCodeEdit); virtual;
    destructor Destroy; override;

    function AddLine(const Text: WideString): TUCELine; overload;
    function AddLine(const Line: TUCELine): TUCELine; overload;
    procedure AddStrings(const Strings: TStrings); overload;
    procedure AddStrings(const Strings: TWideStrings); overload;
    procedure AssignTo(Destination: TObject);
    procedure Clear;
    function CollectTextFromBlock(Start, Stop: TPoint): WideString;
    function CollectTextFromPosition(Start, Stop: TPoint): WideString;
    procedure DeleteLine(Index: Integer);
    procedure Error(const Msg: string; Data: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function InsertLine(Index: Integer; const Text: WideString): TUCELine;
    procedure LoadFromStream(Stream: TStream; Format: TTextFormat; Language: LCID = 0);
    procedure SaveToStream(Stream: TStream; Format: TTextFormat; Language: LCID = 0; WithBOM: Boolean = True);

    property Modified: Boolean read FModified write SetModified;
    property Count: Integer read FCount;
    property Line[Index: Integer]: TUCELine read GetLine; default;
    property LineNoInit[Index: Integer]: TUCELine read GetLineNoInit;
    property Text: WideString read GetText write SetText;

    property OnChangeLine: TLineEvent read FOnChangeLine write FOnChangeLine;
    property OnDeleteLine: TLineEvent read FOnDeleteLine write FOnDeleteLine;
    property OnValidateLine: TLineEvent read FOnValidateLine write FOnValidateLine;
  end;

  TBookmarkOptions = class(TPersistent)
  private
    FEnableKeys: Boolean;
    FGlyphsVisible: Boolean;
    FLeftMargin: Integer;
    FOwner: TCustomUniCodeEdit;
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
  public
    constructor Create(AOwner: TCustomUniCodeEdit);
  published
    property EnableKeys: Boolean read FEnableKeys write FEnableKeys default True;
    property GlyphsVisible: Boolean read FGlyphsVisible write SetGlyphsVisible default True;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 2;
  end;

  TScrollHintWindow = class(THintWindow)
  protected
    procedure Paint; override;
  end;

  TSearchOption = (
    soBackwards,             // Search backwards instead of forward.
    soEntireScope,           // Search in entire text, not only in selected text.
    soIgnoreNonSpacing,      // Ignore non-spacing characters in search.
    soMatchCase,             // Case sensitive search.
    soPrompt,                // Ask user for each replace action.
    soRegularExpression,     // Search using regular expressions.
    soReplace,               // Replace, not simple search.
    soReplaceAll,            // Replace all occurences
    soSelectedOnly,          // Search in selected text only.
    soSpaceCompress,         // Handle several consecutive white spaces as one white space,
                             // so "ab   cd" will match "ab cd" and "ab        cd".
    soWholeWord              // Match entire words only.
  );
  TSearchOptions = set of TSearchOption;

  // Miscellanous events.
  // Note: the scroll event uses pixel values for both horizontal and vertical movements.
  TUCEScrollEvent = procedure(Sender: TCustomUniCodeEdit; DeltaX, DeltaY: Integer) of object;

  TCustomUniCodeEdit = class(TCustomControl)
  private
    FSelectionBlockBegin: TPoint;      // Start and end point of the current selection area.
    FSelectionBlockEnd: TPoint;
    FSortedBlockBegin: TPoint;         // Also contains the selection area but points are sorted so that
    FSortedBlockEnd: TPoint;           // the start point is before the end point.

    FBookmarkOptions: TBookmarkOptions;
    FBookMarks: array[0..9] of TBookMark;
    FBorderStyle: TBorderStyle;
    FCaretOffset: TPoint;
    FCaretVisible,
    FDragWasStarted: Boolean;
    FCaretX: Integer;
    FCaretY: Integer;
    FCharsInWindow: Integer;
    FCharWidth: Integer;
    FWorkWidth: Integer;
    FContent: TUniCodeEditorContent;
    FData: TObject;                    // This is a placeholder for application defined data.
                                       // the selection block end or just the cursor position.
    FDoubleClickTime: Cardinal;
    FExtraLineSpacing: Integer;
    FControlCharFont: TFont;
    FLineNumberFont: TFont;
    FGutterColor: TColor;
    FGutterRect: TRect;
    FGutterWidth: Integer;
    FHighLighter: TUCEHighlighter;
    FIndentSize: Integer;
    FInsertCaret: TCaretType;
    FInternalBMList: TImageList;
    FDropTarget: Boolean;              // Needed to know in the scroll timer event whether to change
    FKeyStrokes: TKeyStrokes;
    FLastCaret: TPoint;
    FLastDblClick: UINT;
    FLastValidLine: Integer;
    FLinesInWindow: Integer;
    FMarginColor: TColor;
    FModified: Boolean;
    FMultiClicked: Boolean;
    FOffsetX,
    FOffsetY: Integer;
    FKeypressHandled: Boolean;         // Used to prevent double keyboard input processing (e.g. Ctrl+shift+I
                                       // which automatically gets converted to tabulator).
    FDefaultStyle: IUCELineStyle;      // Used to dynamically switch the style (colors, font styles) of all lines.

    FOnBookmarkChange: TBookmarkChangeEvent;
    FOnCaretChange: TCaretEvent;
    FOnFocusChanged: TFocusChangedEvent;
    FOnGutterMouseDown: TGutterMouseDownEvent;
    FOnPaint: TPaintEvent;
    FOnProcessCommand: TProcessCommandEvent;
    FOnProcessUserCommand: TProcessCommandEvent;
    FOnReplaceText: TReplaceTextEvent;

    // Miscellanous events
    FOnScroll: TUCEScrollEvent;                   // Called when one or both window offsets changed.
    FOnSettingChange: TNotifyEvent;
    FOptions: TUniCodeEditOptions;
    FOverwriteCaret: TCaretType;
    FRightMargin: Integer;
    FScrollBars: TScrollStyle;
    FScrollHint: TScrollHintWindow;
    FScrollTimer: TTimer;
    FSelectedColor,
    FScrollHintColor: TUCELineStyle;
    FTabSize: Integer;
    FTextHeight: Integer;
    FUndoList: TUndoList;
    FUpdateCount: Integer;

    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TPoint;
    function GetFont: TFont;
    function GetLineText: WideString;
    function GetMaxUndo: Integer;
    function GetSelectedText: WideString;
    function GetSelectionAvailable: Boolean;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetText: WideString;
    function GetTopLine: Integer;

    procedure SetBlockBegin(Value: TPoint);
    procedure SetBlockEnd(Value: TPoint);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretXY(const Value: TPoint);
    procedure SetCaretY(Value: Integer);
    procedure SetCharWidth(const Value: Integer);
    procedure SetDefaultStyle(const Value: IUCELineStyle);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetGutterColor(Value: TColor);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHighlighter(const Value: TUCEHighlighter);
    procedure SetIndentSize(Value: integer);
    procedure SetInsertCaret(const Value: TCaretType);
    procedure SetKeystrokes(const Value: TKeyStrokes);
    procedure SetLineNumberFont(const Value: TFont);
    procedure SetLineText(Value: WideString);
    procedure SetMarginColor(const Value: TColor);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(const Value: Boolean);
    procedure SetOffsetX(Value: Integer);
    procedure SetOffsetY(Value: Integer);
    procedure SetOptions(const Value: TUniCodeEditOptions);
    procedure SetOverwriteCaret(const Value: TCaretType);
    procedure SetRightMargin(Value: Integer);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetScrollHintColor(const Value: TUCELineStyle);
    procedure SetSelectedColor(const Value: TUCELineStyle);
    procedure SetSelEnd(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelText(const Value: WideString);
    procedure SetSelTextExternal(const Value: WideString);
    procedure SetTabSize(Value: Integer);
    procedure SetText(const Value: WideString);
    procedure SetTopLine(Value: Integer);
    procedure SetUpdateState(Updating: Boolean);
    procedure SetWordBlock(Value: TPoint);
    procedure SetWorkWidth(const Value: Integer);

    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMCopy(var Message: TWMCopy); message WM_COPY;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMScroll); message WM_HSCROLL;
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Message: TMessage); message WM_IME_NOTIFY;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
  protected
    function CaretXPix: Integer;
    function CaretYPix: Integer;
    procedure ComputeCaret(X, Y: Integer);
    function CopyOnDrop(Source: TObject): Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DeleteLastChar;
    procedure DeleteSelection(NeedRescan: Boolean);
    procedure DoCaretChange; virtual;
    procedure DoGutterMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y, Line: Integer); virtual;
    procedure DoScroll(DeltaX, DeltaY: Integer); virtual;
    procedure DoSettingChanged; virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure FontChanged(Sender: TObject);
    procedure HideCaret;
    procedure InitializeCaret;
    procedure InsertLineBreak(MoveCaret: Boolean);
    procedure InsertText(Value: WideString);
    procedure InternalBlockIndent(Start, Stop: TPoint);
    procedure InternalBlockUnindent(Start, Stop: TPoint);
    procedure InsertCharacter(NewChar: WideChar);
    function IsIdentChar(const AChar: WideChar; IdChars: TIdentChars): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function LeftSpaces(const S: WideString): Cardinal;
    procedure LineNumberFontChanged(Sender: TObject);
    procedure LinesChanged;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NextSmartTab(X, Y: Integer; SkipNonWhite: Boolean): Integer;
    function NextWordPos: TPoint; virtual;
    procedure OnScrollTimer(Sender: TObject);
    procedure Paint; override;
    procedure PaintGutter(TextCanvas: TCanvas); virtual;
    procedure PaintHighlighted(TextCanvas: TCanvas);
    procedure PaintText(TextCanvas: TCanvas); virtual;
    function PosInSelection(Pos: TPoint): Boolean;
    procedure ProcessCommand(var Command: TEditorCommand; var AChar: WideChar; Data: Pointer); virtual;
    function RecordState: TEditState;
    procedure ReplaceLeftTabs(var S: WideString);
    procedure RescanLine(Index: Integer);
    procedure ResetCaret; virtual;
    procedure ShowCaret;
    function TextFromClipboard: WideString; virtual;
    procedure TextToClipboard(Text: WideString); virtual;
    function TranslateKeyCode(Code: Word; Shift: TShiftState; var Data: Pointer): TEditorCommand;
    procedure TripleClick;
    function Unindent(X, Y: Cardinal): Cardinal;
    procedure UpdateCaret;
    procedure UpdateScrollBars;
    function WorkWidthToCharIndex(const LineIndex: Integer): Integer;

    property BookMarkOptions: TBookmarkOptions read FBookmarkOptions write FBookmarkOptions;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    property ExtraLineSpacing: Integer read FExtraLineSpacing write SetExtraLineSpacing default 0;
    property Font: TFont read GetFont write SetFont;
    property GutterColor: TColor read FGutterColor write SetGutterColor;
    property GutterWidth: Integer read FGutterWidth write SetGutterWidth;
    property HighLighter: TUCEHighlighter read FHighLighter write SetHighlighter;
    property IndentSize: Integer read FIndentSize write SetIndentSize default 2;
    property InsertCaret: TCaretType read FInsertCaret write SetInsertCaret default ctVerticalLine;
    property Keystrokes: TKeyStrokes read FKeyStrokes write SetKeystrokes;
    property LineNumberFont: TFont read FLineNumberFont write SetLineNumberFont;
    property MarginColor: TColor read FMarginColor write SetMarginColor default clSilver;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo;
    property Options: TUniCodeEditOptions read FOptions write SetOptions default DefaultOptions;
    property OverwriteCaret: TCaretType read FOverwriteCaret write SetOverwriteCaret default ctBlock;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 80;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property ScrollHintColor: TUCELineStyle read FScrollHintColor write SetScrollHintColor;
    property SelectedColor: TUCELineStyle read FSelectedColor write SetSelectedColor;
    property TabSize: Integer read FTabSize write SetTabSize default 8;
    property TabStop default True;
    property UpdateCount: Integer read FUpdateCount;
    property WorkWidth: Integer read FWorkWidth write SetWorkWidth;

    property OnBookmarkChange: TBookmarkChangeEvent read FOnBookmarkChange write FOnBookmarkChange;
    property OnCaretChange: TCaretEvent read FOnCaretChange write FOnCaretChange;
    property OnGutterMouseDown: TGutterMouseDownEvent read FOnGutterMouseDown write FOnGutterMouseDown;
    property OnFocusChanged: TFocusChangedEvent read FOnFocusChanged write FOnFocusChanged;
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnProcessCommand: TProcessCommandEvent read FOnProcessCommand write FOnProcessCommand;
    property OnProcessUserCommand: TProcessCommandEvent read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read FOnReplaceText write FOnReplaceText;
    property OnScroll: TUCEScrollEvent read FOnScroll write FOnScroll;
    property OnSettingChange: TNotifyEvent read FOnSettingChange write FOnSettingChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure BlockIndent;
    procedure BlockUnindent;
//    function ActivateKeyboard(const C: WideChar): Boolean;
    procedure CaretOffset(Position: TPoint; Offset: Integer);
    function CharPositionToRowColumn(Position: Cardinal): TPoint;
    procedure ClearAll(KeepUndoList: Boolean);
    procedure ClearSelection;
    procedure ClearUndo;
    procedure CommandProcessor(Command: TEditorCommand; AChar: WideChar; Data: Pointer); virtual;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
    function GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;
    function GetWordAndBounds(Pos: TPoint; var BB, BE: TPoint): WideString;
    procedure GotoBookMark(BookMark: Integer);
    function IsBookmark(BookMark: Integer): Boolean;
    function LastWordPos: TPoint; virtual;
    function LineFromPos(Pos: TPoint): Integer;
    procedure LoadFromFile(const FileName: WideString; TextFormat: TTextFormat);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PasteFromClipboard;
    function PositionFromPoint(X, Y: Integer): TPoint;
    procedure Redo;
    procedure RefreshLine(Index: Integer);
    procedure RefreshLines(Start, Stop: Integer);
    procedure RefreshToBottom(Index: Integer);
    procedure RemoveBookmark(BookMark: Integer);
    function RowColumnToCharPosition(P: TPoint): Cardinal;
    procedure SaveToFile(const FileName: WideString; TextFormat: TTextFormat);
    function SearchReplace(const SearchText, ReplaceText: WideString; Options: TSearchOptions): Integer;
    procedure SelectAll;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetCaretToEditorBottom;
    procedure SetDefaultKeystrokes; virtual;
    procedure Undo;
    procedure WndProc(var Msg: TMessage); override;
    function WordAtPos(X, Y: Integer): WideString;

    property BlockBegin: TPoint read FSortedBlockBegin write SetBlockBegin;
    property BlockEnd: TPoint read FSortedBlockEnd write SetBlockEnd;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read FCaretX write SetCaretX;
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;
    property CaretY: Integer read FCaretY write SetCaretY;
    property Content: TUniCodeEditorContent read FContent;
    property Data: TObject read FData write FData;
    property DefaultStyle: IUCELineStyle read FDefaultStyle write SetDefaultStyle;
    property LineHeight: integer read FTextHeight;
    property LinesInWindow: Integer read FLinesInWindow;
    property LineText: WideString read GetLineText write SetLineText;
    property Modified: Boolean read FModified write SetModified;
    property OffsetX: Integer read FOffsetX write SetOffsetX;
    property OffsetY: Integer read FOffsetY write SetOffsetY;
    property SelectedText: WideString read GetSelectedText write SetSelTextExternal;
    property SelectionAvailable: Boolean read GetSelectionAvailable;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property Text: WideString read GetText write SetText;
    property TopLine: Integer read GetTopLine write SetTopLine;
    property UndoList: TUndoList read FUndoList;
  end;

  TUniCodeEdit = class(TCustomUniCodeEdit)
  published
    property Align;
    property Anchors;
    property Constraints;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property BookmarkOptions;
    property CharWidth;
    property Color;
    property Ctl3D;
    property Enabled;
    property ExtraLineSpacing;
    property Font;
    property GutterColor;
    property GutterWidth;
    property Height;
    property HighLighter;
    property IndentSize;
    property InsertCaret;
    property Keystrokes;
    property LineNumberFont;
    property MarginColor;
    property MaxUndo;
    property Name;
    property Options;
    property OverwriteCaret;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightMargin;
    property ShowHint;
    property ScrollBars;
    property ScrollHintColor;
    property SelectedColor;
    property TabOrder;
    property TabSize;
    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
    property WorkWidth;

    property OnBookmarkChange;
    property OnCaretChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFocusChanged;
    property OnGutterMouseDown;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnScroll;
    property OnSettingChange;
    property OnStartDrag;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  {$ifdef COMPILER_6_UP}
    RtlConsts,
  {$else}
    Consts,
  {$endif COMPILER_6_UP}
  CommCtrl, ImgList, Dialogs, IMM, StringContainers;

{$R UniCodeEditor.res}

resourcestring
  SGeneralIMEError = 'General IME error encountered. No specific error info is available, though.';

const
  EmptyState: TEditState = ();
  WideSpaces = WideChar(#32) + WideChar(#9);
var
  PlatformIsUnicode: Boolean;

//----------------------------------------------------------------------------------------------------------------------

procedure AssignError(ClassName: string; Source: TObject);

var
  SourceName: string;
  
begin
  if Assigned(Source) then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';
  raise EConvertError.CreateResFmt(@SAssignError, [SourceName, ClassName]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ConvertAndAddImages(IL: TImageList);

// Loads the internal bookmark image list.

var
  OneImage: TBitmap;
  I: Integer;
  MaskColor: TColor;
  Name: string;

begin
  // Since we want our bookmark images looking in the correct system colors,
  // we have to remap their colors. I really would like to use
  // IL.GetInstRes(HInstance, rtBitmap, Resname, 16, [lrMap3DColors], clWhite) for this task,
  // but could not get it working under NT. It seems that image lists are not very well
  // supported under WinNT 4.
  OneImage := TBitmap.Create;
  try
    // it is assumed that the image height determines also the width of
    // one entry in the image list
    IL.Clear;
    IL.Height := 16;
    IL.Width := 16;
    for I := 0 to 9 do
    begin
      Name := 'MARK_' + IntToStr(I);
      OneImage.Handle := CreateGrayMappedRes(HInstance, PChar(Name));
      MaskColor := OneImage.Canvas.Pixels[0, 0];
      IL.AddMasked(OneImage, MaskColor);
    end;
  finally
    OneImage.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function Min(X, Y: Integer): Integer;

begin
  if X < Y then
    Result := X
  else
    Result := Y;
end;

//----------------------------------------------------------------------------------------------------------------------

function MaxPoint(P1, P2: TPoint): TPoint;

begin
  if P1.Y > P2.Y then
    Result := P1
  else
    if P1.Y < P2.Y then
      Result := P2
    else
      if P1.X > P2.X then
        Result := P1
      else
        Result := P2;
end;

//----------------------------------------------------------------------------------------------------------------------

function MaxPointAsReference(var P1, P2: TPoint): PPoint;

begin
  if P1.Y > P2.Y then
    Result := @P1
  else
    if P1.Y < P2.Y then
      Result := @P2
    else
      if P1.X > P2.X then
        Result := @P1
      else
        Result := @P2;
end;

//----------------------------------------------------------------------------------------------------------------------

function MinPoint(P1, P2: TPoint): TPoint;

begin
  if P1.Y < P2.Y then
    Result := P1
  else
    if P1.Y > P2.Y then
      Result := P2
    else
      if P1.X < P2.X then
        Result := P1
      else
        Result := P2;
end;

//----------------------------------------------------------------------------------------------------------------------

function MinPointAsReference(var P1, P2: TPoint): PPoint;

begin
  if P1.Y < P2.Y then
    Result := @P1
  else
    if P1.Y > P2.Y then
      Result := @P2
    else
      if P1.X < P2.X then
        Result := @P1
      else
        Result := @P2;
end;

//----------------------------------------------------------------------------------------------------------------------

function PointsAreEqual(P1, P2: TPoint): Boolean;

begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

//----------------- TUndoList ------------------------------------------------------------------------------------------

constructor TUndoList.Create(AOwner: TCustomUniCodeEdit);

begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
  FMaxUndo := 10;
  FCurrent := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TUndoList.Destroy;

begin
  if Assigned(FPendingChange) then
    FreeMem(FPendingChange);

  ClearList;
  FList.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetCanRedo: Boolean;

begin
  Result := FCurrent < FList.Count - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetCanUndo: Boolean;

begin
  Result := FCurrent > -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetCurrentRedoReason: TChangeReason;

begin
  if FCurrent = FList.Count - 1 then
    Result := ecrNone
  else
    Result := PChange(FList.Items[FCurrent + 1]).Action.Reason;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetCurrentRedoReasonGroup: TChangeReasonGroup;

begin
  if FCurrent = FList.Count - 1 then
    Result := crgNone
  else
    Result := PChange(FList.Items[FCurrent + 1]).Action.Group;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetCurrentUndoReason: TChangeReason;

begin
  if FCurrent = -1 then
    Result := ecrNone
  else
    Result := PChange(FList.Items[FCurrent]).Action.Reason;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetCurrentUndoReasonGroup: TChangeReasonGroup;

begin
  if FCurrent = -1 then
    Result := crgNone
  else
    Result := PChange(FList.Items[FCurrent]).Action.Group;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUndoList.SetMaxUndo(Value: Integer);

begin
  if FMaxUndo <> Value then
  begin
    FMaxUndo := Value;
    // FCurrent ist automatically updated.
    while FMaxUndo < FList.Count - 1 do
      RemoveChange(0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.CanAcceptNewChange: Boolean;

// Returns an indicator telling if a new change can be accepted in this list.

begin
  Result := (FPendingChange = nil) and (FMaxUndo > 0) and (FOwner.UpdateCount = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUndoList.FinishPendingChange;

// If there is currently a change entry pending for addition then that operation is finished.
// The method takes care to not add more than allowed undo records.

begin
  if Assigned(FPendingChange) then
  begin
    // A new undo entry deletes all redo entries if there are any.
    while FCurrent < FList.Count - 1 do
      RemoveChange(FList.Count - 1);

    while FList.Count >= FMaxUndo do
      RemoveChange(0);
    FPendingChange.NewState := FOwner.RecordState;
    case FPendingChange.Action.Reason of
      ecrNone:
        FPendingChange.Action.Group := crgNone;
      ecrInsert, ecrReplace:
        FPendingChange.Action.Group := crgInsert;
      ecrDelete:
        FPendingChange.Action.Group := crgDelete;
      ecrDragMove, ecrDragCopy:
        FPendingChange.Action.Group := crgDrag;
      ecrState:
        FPendingChange.Action.Group := crgState;
      ecrIndentation, ecrUnindentation:
        FPendingChange.Action.Group := crgIndentation;
    end;

    FList.Add(FPendingChange);
    Inc(FCurrent);
    FPendingChange := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUndoList.FinishPendingChange(TargetEnd: TPoint);

// Overloaded method which does the same as the other one but allows to set a final target end position.

begin
  if Assigned(FPendingChange) then
  begin
    // A new undo entry deletes all redo entries if there are any.
    while FCurrent < FList.Count - 1 do
      RemoveChange(FList.Count - 1);

    while FList.Count >= FMaxUndo do
      RemoveChange(0);

    FPendingChange.Action.TargetEnd := TargetEnd;
    FPendingChange.NewState := FOwner.RecordState;
    case FPendingChange.Action.Reason of
      ecrNone:
        FPendingChange.Action.Group := crgNone;
      ecrInsert, ecrReplace:
        FPendingChange.Action.Group := crgInsert;
      ecrDelete:
        FPendingChange.Action.Group := crgDelete;
      ecrDragMove, ecrDragCopy:
        FPendingChange.Action.Group := crgDrag;
      ecrState:
        FPendingChange.Action.Group := crgState;
      ecrIndentation, ecrUnindentation:
        FPendingChange.Action.Group := crgIndentation;
    end;

    FList.Add(FPendingChange);
    Inc(FCurrent);
    FPendingChange := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareDeleteChange(SourceStart, SourceEnd: TPoint): Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrDelete;
    FPendingChange.OldState := FOwner.RecordState;
    FPendingChange.Action.SourceStart := SourceStart;
    FPendingChange.Action.SourceEnd := SourceEnd;
    FPendingChange.Action.OldText := FOwner.FContent.CollectTextFromBlock(SourceStart, SourceEnd);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareDragCopyChange(SourceStart, SourceEnd, TargetStart: TPoint): Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrDragCopy;
    FPendingChange.OldState := FOwner.RecordState;
    FPendingChange.Action.SourceStart := SourceStart;
    FPendingChange.Action.SourceEnd := SourceEnd;
    FPendingChange.Action.TargetStart := TargetStart;

    // Compute end point of the target operation.
    if SourceStart.Y = SourceEnd.Y then
    begin
      FPendingChange.Action.TargetEnd.Y := TargetStart.Y;
      FPendingChange.Action.TargetEnd.X := TargetStart.X + SourceEnd.X - SourceStart.X;
    end
    else
    begin
      FPendingChange.Action.TargetEnd.Y := TargetStart.Y + SourceEnd.Y - SourceStart.Y;
      FPendingChange.Action.TargetEnd.X := SourceEnd.X;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareDragMoveChange(SourceStart, SourceEnd, TargetStart: TPoint): Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrDragMove;
    FPendingChange.OldState := FOwner.RecordState;
    FPendingChange.Action.SourceStart := SourceStart;
    FPendingChange.Action.SourceEnd := SourceEnd;
    FPendingChange.Action.TargetStart := TargetStart;

    // Compute end point of the target operation.
    if SourceStart.Y = SourceEnd.Y then
    begin
      FPendingChange.Action.TargetEnd.Y := TargetStart.Y;
      FPendingChange.Action.TargetEnd.X := TargetStart.X + SourceEnd.X - SourceStart.X;
    end
    else
    begin
      FPendingChange.Action.TargetEnd.Y := TargetStart.Y + SourceEnd.Y - SourceStart.Y;
      FPendingChange.Action.TargetEnd.X := SourceEnd.X;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareIndentationChange(TargetStart, TargetEnd: TPoint): Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrIndentation;
    FPendingChange.OldState := FOwner.RecordState;
    FPendingChange.Action.TargetStart := TargetStart;
    FPendingChange.Action.TargetEnd := TargetEnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareInsertChange(TargetStart: TPoint; const Text: WideString): Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrInsert;
    FPendingChange.OldState := FOwner.RecordState;
    FPendingChange.Action.TargetStart := TargetStart;
    FPendingChange.Action.NewText := Text;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareReplaceChange(SourceStart, SourceEnd, TargetStart: TPoint; const Text: WideString): Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.
// Note: SourceStart and SourceEnd are the block bounds of the text to be replaced.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrReplace;
    FPendingChange.OldState := FOwner.RecordState;
    FPendingChange.Action.SourceStart := SourceStart;
    FPendingChange.Action.SourceEnd := SourceEnd;
    FPendingChange.Action.OldText := FOwner.FContent.CollectTextFromBlock(SourceStart, SourceEnd);
    FPendingChange.Action.TargetStart := TargetStart;
    FPendingChange.Action.NewText := Text;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareStateChange: Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrState;
    FPendingChange.OldState := FOwner.RecordState;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.PrepareUnindentationChange(TargetStart, TargetEnd: TPoint): Boolean;

// Prepares a new change entry for the undo/redo queue. If already a change entry is pending
// or no undo is allowed then nothing happens.

begin
  Result := CanAcceptNewChange;
  if Result then
  begin
    FPendingChange := AllocMem(SizeOf(TChange));
    FPendingChange.Action.Reason := ecrUnindentation;
    FPendingChange.OldState := FOwner.RecordState;
    FPendingChange.Action.TargetStart := TargetStart;
    FPendingChange.Action.TargetEnd := TargetEnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUndoList.RemoveChange(Index: Integer);

var
  AChange: PChange;

begin
  if (Index > -1) and (Index < FList.Count) then
  begin
    AChange := FList.Items[Index];
    Dispose(AChange);
    FList.Delete(Index);
  end;
  if FCurrent > FList.Count - 1 then
    FCurrent := FList.Count - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetRedoChange(var Change: PChange): TChangeReason;

begin
  if FCurrent = FList.Count - 1 then
    Result := ecrNone
  else
  begin
    Inc(FCurrent);
    Change := FList.Items[FCurrent];
    Result := Change.Action.Reason;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUndoList.GetUndoChange(var Change: PChange): TChangeReason;

begin
  if FCurrent = -1 then
    Result := ecrNone
  else
  begin
    Change := FList.Items[FCurrent];
    Result := Change.Action.Reason;
    Dec(FCurrent);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUndoList.ClearList;

var
  I: Integer;

begin
  for I := FList.Count - 1 downto 0 do
    RemoveChange(I);
end;

//----------------- TBookmarkOptions -----------------------------------------------------------------------------------

constructor TBookmarkOptions.Create(AOwner: TCustomUniCodeEdit);

begin
  inherited Create;
  FOwner := AOwner;
  FEnableKeys := True;
  FGlyphsVisible := True;
  FLeftMargin := 2;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBookmarkOptions.SetGlyphsVisible(Value: Boolean);

begin
  if FGlyphsVisible <> Value then
  begin
    FGlyphsVisible := Value;
    FOwner.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBookmarkOptions.SetLeftMargin(Value: Integer);

begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Value;
    FOwner.Invalidate;
  end;
end;

//----------------- TUCELineStyle --------------------------------------------------------------------------------------

function TUCELineStyle.GetBackground: TColor;

begin
  Result := FBackground;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELineStyle.GetFontStyles: TFontStyles;

begin
  Result := FFontStyles;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELineStyle.GetForceFontStyles: Boolean;

begin
  Result := FForceFontStyles;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELineStyle.GetForeground: TColor;

begin
  Result := FForeground;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELineStyle.SetBackground(const Color: TColor);

begin
  FBackground := Color;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELineStyle.SetFontStyles(const Styles: TFontStyles);

begin
  FFontStyles := Styles;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELineStyle.SetForceFontStyles(Force: Boolean);

begin
  FForceFontStyles := Force;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELineStyle.SetForeground(const Color: TColor);

begin
  FForeground := Color;
end;

//----------------- TUCELine -------------------------------------------------------------------------------------------

constructor TUCELine.Create(Owner: TUniCodeEditorContent);

begin
  FOwner := Owner;
  
  // Markers and styles member are created on demand.
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TUCELine.Destroy;

begin
  FMarkers.Free;
  FStyles.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.SetText(const Value: WideString);

begin
  FText := Value;
  FOwner.AutoAdjustWorkWidth(FIndex);
  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.Changed;

begin
  InternalInvalidate;
  FOwner.DoChangeLine(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.CharIndexToColumn(Index: Integer): Integer;

// Computes the corresponding column from the given character position.

var
  I: Integer;
  CharWidth: Integer;

begin
  InternalValidate;

  I := 0;
  Result := 0;
  CharWidth := FOwner.FOwner.CharWidth;
  while I < Index do
  begin
    if I < Length(FCharWidthArray) then
      Inc(Result, FCharWidthArray[I])
    else
      Inc(Result, CharWidth);
    Inc(I);
  end;
  Result := Result div CharWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.ColumnToCharIndex(Column: Integer): Integer;

// Calculates the character index into the line given by Y. Column is usually a
// caret position. Because of non-default width characters (like tabulators) the
// CaretX position is merely a column value and does not directly correspond to a
// particular character. The result can be used to address a character in the line
// given by Y or is a generic char index if X is beyond any char in Y.

var
  Run: Integer;
  CharWidth: Integer;

begin
  InternalValidate;

  CharWidth := FOwner.FOwner.CharWidth;
  Column := Column * CharWidth;
  Run := 0;
  Result := 0;
  while Result < Length(FCharWidthArray) do
  begin
    Inc(Run, FCharWidthArray[Result]);
    if Run > Column then
      Break;
    Inc(Result);
  end;
  // If the run is still smaller than the given column then add "virtual" chars
  // to be able to add a character (far) beyond the current end of the line.
  if Run < Column then
    Inc(Result, (Column - Run) div CharWidth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.ComputeCharacterWidths(TabSize, DefaultWidth: Integer);

// Creates an array of integer values describing the width of each character
// of a given line in pixels.

var
  I: Integer;
  P: PWideChar;
  TabPixelSize: Integer;

begin
  P := PWideChar(FText);
  SetLength(FCharWidthArray, Length(FText));
  TabPixelSize := TabSize * DefaultWidth;

  I := 0;
  while P^ <> WideNull do
  begin
    if P^ = WideTabulator then
      FCharWidthArray[I] := TabPixelSize
    else
      FCharWidthArray[I] := DefaultWidth;

    Inc(I);
    Inc(P);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.DrawMarkers(Index: Integer; Canvas: TCanvas; X, Y: Integer);

var
  I: Integer;
  Marker: IUCELineMarker;
  Size: TSize;

begin
  if Assigned(FMarkers) then
    for I := 0 to FMarkers.Count - 1 do
    begin
      Marker := IUCELineMarker(FMarkers[I]);
      Marker.Draw(Index, Canvas, X, Y);
      Size := Marker.GetSize(Index);
      Inc(X, Size.cx);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.GetLineEnd(IgnoreWhiteSpace: Boolean): Integer;

// Calculates the first unused column of the line given by Index.
// If IgnoreWhitespace is true then trailing white spaces are threated as if they wouldn't exist,
// hence the returned column is the first one after the last non-white space character.

begin
  Result := CharIndexToColumn(Length(FText));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.InternalInvalidate;

// Invalidates the line's internal structures and its external state as seen by the application.

begin
  if lsValidatedInternally in FStates then
  begin
    Exclude(FStates, lsValidatedInternally);
    FCharwidthArray := nil;
  end;

  // Changes made to the line that invalidates it also very likely renders the external state invalid.
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.InternalValidate;

// Validates the line's internal structures.

begin
  if not (lsValidatedInternally in FStates) then
  begin
    Include(FStates, lsValidatedInternally);
    ComputeCharacterWidths(FOwner.FOwner.TabSize, FOwner.FOwner.CharWidth);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.NextCharPos(ThisPosition: Integer; ForceNonDefault: Boolean): Integer;

// Determines the next character position for the given line as column value depending on the
// column given by ThisPosition. The returned index can directly be used to set the caret.

var
  PixelPos, Run: Integer;
  I: Cardinal;

begin
  InternalValidate;

  with FOwner.FOwner do
  begin
    // Turn column into pixel position.
    PixelPos := ThisPosition * CharWidth;

    if ((eoCursorThroughTabs in Options) and not ForceNonDefault) or
      (ThisPosition >= GetLineEnd(False)) then
      Result := ThisPosition + 1
    else
    begin
      // The task is to find the first column which corresponds directly to a
      // character (which is not always the case, eg. tabs) and is larger than
      // the given position.
      Run := 0;
      // sum up character widths until we find a pixel position larger than the
      // given one
      for I := 0 to High(FCharWidthArray) do
      begin
        // values in the char width array always correspond to an actual character index
        if PixelPos < Run then
          Break
        else
          Inc(Run, FCharWidthArray[I]);
      end;
      Result := Run div CharWidth;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.PreviousCharPos(ThisPosition: Integer): Integer;

// Determines the previous character position for the given line depending on the
// column given by ThisPosition. The returned index can directly be used to set the caret.

var
  PixelPos: Integer;
  Run: Integer;
  I: Integer;

begin
  InternalValidate;

  with FOwner.FOwner do
  begin
    // turn column into pixel position:
    PixelPos := ThisPosition * FCharWidth;
    if (eoCursorThroughTabs in FOptions) or (ThisPosition > GetLineEnd(False)) then
      Result := ThisPosition - 1
    else
    begin
      // The task is to find the last column which corresponds directly to a
      // character (which is not always the case, eg. tabs) and is smaller than
      // the given position.
      Run := 0;
      // sum up character widths until we find the largest pixel position smaller
      // than the given one
      for I := 0 to High(FCharWidthArray) do
      begin
        // values in the char width array always correspond to an actual character index
        if PixelPos <= (Run + FCharWidthArray[I]) then
          Break
        else
          Inc(Run, FCharWidthArray[I]);
      end;
      Result := Run div CharWidth;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.Assign(Source: TUCELine);

var
  I: Integer;
  Index: Integer;
  
begin
  FLexerState := Source.FLexerState;
  FText := Source.FText;
  FOwner.AutoAdjustWorkWidth(FIndex);
  FStates := Source.FStates;
  FBounds := Source.FBounds;
  for I := 0 to Source.FMarkers.Count - 1 do
    FMarkers.Add(Source.FMarkers[I]);
  for I := 0 to Source.FStyles.Count - 1 do
  begin
    Index := FStyles.List.Add(Source.FStyles.List[I]);
    IUCELineStyle(FStyles.List[Index])._AddRef;
  end;
  FData := Source.FData;

  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.AddMarker(Marker: IUCELineMarker): Integer;

// Adds a new marker at the end of the list of markers for this line.

begin
  if Assigned(Marker) then
  begin
    if FMarkers = nil then
      FMarkers := TInterfaceList.Create;

    // If the given marker already is in the list then remove it first.
    // This way you can simply move it to the end of the list by re-adding it.
    if FMarkers.IndexOf(Marker) > -1 then
      FMarkers.Remove(Marker)
    else
      Marker._AddRef;
    Result := FMarkers.Add(Marker);
  end
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.ClearMarkers;

// Removes all markers for that line.

begin
  if Assigned(FMarkers) then
  begin
    FMarkers.Clear;
    FreeAndNil(FMarkers);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.ClearStyles;

// Removes all styles of that line.

var
  I: Integer;

begin
  if Assigned(FStyles) then
  begin
    for I := 0 to FStyles.Count - 1 do
      IUCELineStyle(FStyles.List[I])._Release;
    FreeAndNil(FStyles);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.HasMarker(Marker: IUCELineMarker): Boolean;

begin
  Result := Assigned(FMarkers) and (FMarkers.IndexOf(Marker) > -1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.Invalidate;

// Invalidates the line's external validation state as seen by the application.

begin
  if lsValidated in FStates then
    Exclude(FStates, lsValidated);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.RemoveMarker(Marker: IUCELineMarker);

// Removes the given marker from the list of markers for this line.

var
  Index: Integer;

begin
  if Assigned(FMarkers) then
  begin
    Index := FMarkers.IndexOf(Marker);
    if Index > -1 then
    begin
      FMarkers.Remove(Marker);

      if FMarkers.Count = 0 then
        FreeAndNil(FMarkers);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.ReplaceMarker(OldMarker, NewMarker: IUCELineMarker);

// Removes the given marker from the list of markers for this line.

var
  Index: Integer;

begin
  if Assigned(FMarkers) then
  begin
    Index := FMarkers.IndexOf(OldMarker);
    if Index > -1 then
      FMarkers[Index] := NewMarker;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCELine.PeekStyle: IUCELineStyle;

begin
  if Assigned(FStyles) then
    Result := IUCELineStyle(FStyles.Peek)
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.PopStyle;

// Removes the top entry of the style stack.

var
  Style: IUCELineStyle;

begin
  if Assigned(FStyles) then
  begin
    // When leaving the method this inteface is released once.
    Pointer(Style) := FStyles.Pop;

    if FStyles.Count = 0 then
      FreeAndNil(FStyles);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.PushStyle(Style: IUCELineStyle);

// Adds a new style to the style stack for this line. This new style becomes the primary style until it is removed.

begin
  if FStyles = nil then
    FStyles := TStyleStack.Create;

  Style._AddRef; // Need to addref explicitely as we now only handle the interface as pointer.
  FStyles.Push(Pointer(Style));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.RemoveStyle(Style: IUCELineStyle);

var
  Index: Integer;

begin
  if Assigned(FStyles) then
  begin
    repeat
      Index := FStyles.List.IndexOf(Pointer(Style));
      if Index < 0 then
        Break;

      FStyles.List.Remove(Pointer(Style));
      Style._Release;
      if FStyles.Count = 0 then
      begin
        FreeAndNil(FStyles);
        Break;
      end;
    until False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCELine.Validate;

// Validates the line's external validation state as seen by the application.

begin
  if not (lsValidated in FStates) then
  begin
    Include(FStates, lsValidated);
    FOwner.DoValidateLine(Self);
  end;
end;

//----------------- TUniCodeEditorContent ------------------------------------------------------------------------------

constructor TUniCodeEditorContent.Create(AOwner: TCustomUniCodeEdit);

begin
  inherited Create;

  FOwner := AOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TUniCodeEditorContent.Destroy;

begin
  Clear;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.GetLine(Index: Integer): TUCELine;

begin
  Result := FLines[Index];
  if Assigned(Result) then
  begin
    Result.InternalValidate;
    Result.Validate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.GetLineNoInit(Index: Integer): TUCELine;

begin
  Result := FLines[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.GetText: WideString;

var
  I: Integer;
  Buffer: TBufferedString;

begin
  Buffer := TBufferedString.Create;
  try
    for I := 0 to FCount - 1 do
    begin
      Buffer.Add(FLines[I].FText);
      Buffer.AddNewLine;
    end;

    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.Grow;

var
  Delta,
  Len: Integer;

begin
  Len := Length(FLines);
  if Len > 64 then
    Delta := Len div 4
  else
  begin
    if Len > 8 then
      Delta := 16
    else
      Delta := 4;
  end;
  SetCapacity(Len + Delta);
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.AddLine(const Text: WideString): TUCELine;

begin
  Result := InsertLine(FCount, Text);
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.AddLine(const Line: TUCELine): TUCELine;

begin
  Result := InsertLine(FCount, Text);
  Result.Assign(Line);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.AddStrings(const Strings: TStrings);

var
  I: Integer;

begin
  for I := 0 to Strings.Count - 1 do
    AddLine(Strings[I]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.AddStrings(const Strings: TWideStrings);

var
  I: Integer;

begin
  for I := 0 to Strings.Count - 1 do
    AddLine(Strings[I]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.AssignTo(Destination: TObject);

var
  I: Integer;
  WStrings: TUniCodeEditorContent;
  AStrings: TStrings;

begin
  if Destination is TUniCodeEditorContent then
  begin
    WStrings := Destination as TUniCodeEditorContent;
    WStrings.Clear;
    for I := 0 to Count - 1 do
      WStrings.AddLine(FLines[I]);
  end
  else
    if Destination is TStrings then
    begin
      AStrings := Destination as TStrings;
      AStrings.Clear;
      for I := 0 to Count - 1 do
        AStrings.Add(FLines[I].FText);
    end
    else
      AssignError(ClassName, Destination);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.Clear;

begin
  if not (csDestroying in FOwner.ComponentState) then
    FOwner.FUndoList.PrepareDeleteChange(Point(0, 0), EndPoint);

  InternalClear;

  if not (csDestroying in FOwner.ComponentState) then
  begin
    FOwner.FUndoList.FinishPendingChange;
    FOwner.ResetCaret;
    FOwner.Invalidate;
    DoChangeLine(nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.CollectTextFromBlock(Start, Stop: TPoint): WideString;

// Collects text from a range of lines given by Start and Stop. The character at Stop position is not copied.
// This method is stable against out-of-range values and returns as much as possible text that can be retrieved.
// The given coordinates must be row/column adresses and are all 0-based.

var
  Temp1: TPoint;
  Temp2: TPoint;

begin
  if FCount > 0 then
  begin
    // First do a couple of sanity checks.
    Temp1 := MinPoint(Start, Stop);
    Temp2 := MaxPoint(Start, Stop);

    if Temp1.Y < 0 then
      Temp1.Y := 0;
    if Temp1.Y > FCount - 1 then
      Temp1.Y := FCount - 1;
    if Temp2.Y < 0 then
      Temp2.Y := 0;
    if Temp2.Y > FCount - 1 then
      Temp2.Y := FCount - 1;

    if Temp1.X < 0 then
      Temp1.X := 0;
    if Temp2.X < 0 then
      Temp2.X := 0;

    Start := Point(FLines[Temp1.Y].ColumnToCharIndex(Temp1.X), Temp1.Y);
    Stop := Point(FLines[Temp2.Y].ColumnToCharIndex(Temp2.X), Temp2.Y);
    Result := InternalCollectText(Start, Stop);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.CollectTextFromPosition(Start, Stop: TPoint): WideString;

// Same as CollectTextFromBlock but this version works with line/character indices.

begin
  if FCount > 0 then
    Result := InternalCollectText(Start, Stop);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.DeleteLine(Index: Integer);

begin
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);

  DoDeleteLine(FLines[Index]);
  FLines[Index].Free;
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FLines[Index + 1], FLines[Index], (FCount - Index) * SizeOf(TUCELine));
    FLines[FCount] := nil;
  end;

  // Reindex all following lines.
  while Index  < FCount do
  begin
    Dec(FLines[Index].FIndex);
    Inc(Index);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.Error(const Msg: string; Data: Integer);

  //--------------- local function --------------------------------------------

  function ReturnAddr: Pointer;

  asm
          MOV EAX, [EBP + 4]
  end;

  //--------------- end local function ----------------------------------------

begin
  raise EStringListError.CreateFmt(Msg, [Data])at ReturnAddr;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.Exchange(Index1, Index2: Integer);

var
  Temp: TUCELine;

begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SListIndexError, Index2);

  Temp := FLines[Index1];
  FLines[Index1] := FLines[Index2];
  FLines[Index2] := Temp;

  FLines[Index1].FIndex := Index1;
  FLines[Index2].FIndex := Index2;

  DoChangeLine(FLines[Index1]);
  DoChangeLine(FLines[Index2]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.InsertLine(Index: Integer; const Text: WideString): TUCELine;

begin
  if FCount = Length(FLines) then
    Grow;
  if Index < FCount then
    System.Move(FLines[Index], FLines[Index + 1], (FCount - Index) * SizeOf(TUCELine));

  Inc(FCount);
  FLines[Index] := TUCELine.Create(Self);
  FLines[Index].FText := Text;
  FLines[Index].FIndex := Index;
  AutoAdjustWorkWidth(Index);

  Result := FLines[Index];

  // Reindex all following lines.
  Inc(Index);
  while Index  < FCount do
  begin
    Inc(FLines[Index].FIndex);
    Inc(Index);
  end;

  DoChangeLine(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.DoChangeLine(Line: TUCELine);

begin
  Modified := True;
  if Assigned(Line) and (Line.Index < FOwner.FLastValidLine) then
    FOwner.RescanLine(Line.Index);
  if Assigned(FOnChangeLine) then
    FOnChangeLine(Self, Line);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.AutoAdjustWorkWidth(Index: Integer);

// This procedure extends the work width of the owner control if the line length of the given line
// is larger than the current work with.
// The line is internally validated if queried for its total length.

var
  NewWidth: Integer;

begin
  NewWidth := FLines[Index].GetLineEnd(True) * FOwner.FCharWidth;
  if FOwner.FWorkWidth < NewWidth then
    FOwner.WorkWidth := NewWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.ConvertTabs(const S: WideString): WideString;

// Converts all (horizontal) tabulator characters in S to spaces if the owner control has not the eoUseTabs option set.
// Otherwise the string is simply used as result.

var
  Run: PWideChar;
  TabPos: PWideChar;
  StringEnd: PWideChar;
  Buffer: TBufferedString;

begin
  if not (eoUseTabs in FOwner.Options) then
  begin
    Buffer := TBufferedString.Create;
    try
      Run := PWideChar(S);
      StringEnd := Run + Length(S);

      repeat
        TabPos := StrPosW(Run, WideTabulator);
        if TabPos = nil then
        begin
          // No (more) tabs in the string. Add the remaining chars to the buffer
          // and stop the loop.
          Buffer.Add(Run, StringEnd - Run);
          Break;
        end;

        Buffer.Add(Run, TabPos - Run);
        
        // Replace tabulators by spaces by adding the right amount to the buffer.
        Run := TabPos;
        while TabPos^ = WideTabulator do
          Inc(TabPos);
        Buffer.Add(WideStringOfChar(' ', FOwner.TabSize * (TabPos - Run)));

        Run := TabPos;
      until Run^ = WideNull;
      Result := Buffer.AsString;
    finally
      Buffer.Free;
    end;
  end
  else
    Result := S;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.DoDeleteLine(Line: TUCELine);

begin
  if Assigned(FOnDeleteLine) then
    FOnDeleteLine(Self, Line);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.DoValidateLine(Line: TUCELine);

begin
  if Assigned(FOnValidateLine) then
    FOnValidateLine(Self, Line);
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.EndPoint: TPoint;

// Returns the position after the very last letter in the text as row/column pair.

begin
  if FCount = 0 then
    Result := Point(0, 0)
  else
  begin
    Result.Y := FCount - 1;

    // Don't trigger the external validation event for that line. To determine its end
    // it is enough to internally validate the line.
    Result.X := FLines[FCount - 1].GetLineEnd(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.InternalClear;

var
  I: Integer;

begin
  for I := 0 to FCount - 1 do
  begin
    DoDeleteLine(FLines[I]);
    FLines[I].Free;
  end;
  SetCapacity(0);
  FOwner.WorkWidth := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorContent.InternalCollectText(Start, Stop: TPoint): WideString;

// Collects text from a range of lines given by Start and Stop. The character at Stop position is not copied.
// This method is stable against out-of-range values and returns as much as possible text that can be retrieved.
// The given coordinates must be line/char index pairs (zero based).

var
  Buffer: TBufferedString;
  Temp1: TPoint;
  Temp2: TPoint;

begin
  // First do a couple of sanity checks.
  Temp1 := MinPoint(Start, Stop);
  Temp2 := MaxPoint(Start, Stop);

  if Temp1.Y < 0 then
    Temp1.Y := 0;
  if Temp1.Y > FCount - 1 then
    Temp1.Y := FCount - 1;
  if Temp2.Y < 0 then
    Temp2.Y := 0;
  if Temp2.Y > FCount - 1 then
    Temp2.Y := FCount - 1;

  if Temp1.X < 0 then
    Temp1.X := 0;
  if Temp2.X < 0 then
    Temp2.X := 0;

  Start := Temp1;
  Stop := Temp2;

  Buffer := TBufferedString.Create;
  try
    if Start.Y = Stop.Y then
    begin
      // Start and stop on the same line.
      if Stop.X - Start.X > 0 then
        Buffer.Add(Copy(FLines[Start.Y].Text, Start.X + 1, Stop.X - Start.X));
    end
    else
    begin
      Buffer.Add(Copy(FLines[Start.Y].Text, Start.X + 1, Length(FLines[Start.Y].Text)));
      Buffer.AddNewLine;
      Inc(Start.Y);
      while Start.Y < Stop.Y do
      begin
        Buffer.Add(FLines[Start.Y].Text);
        Buffer.AddNewLine;
        Inc(Start.Y);
      end;
      if Stop.X > 0 then
        Buffer.Add(Copy(FLines[Stop.Y].Text, 1, Stop.X));
    end;

    Result := Buffer.AsString;
  finally
    Buffer.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.InternalInvalidateAll;

// Invalidates all lines (internal validation state).

var
  I: Integer;

begin
  for I := 0 to FCount - 1 do
    FLines[I].InternalInvalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.SetCapacity(NewCapacity: Integer);

begin
  SetLength(FLines, NewCapacity);
  if NewCapacity < FCount then
    FCount := NewCapacity;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.LoadFromStream(Stream: TStream; Format: TTextFormat; Language: LCID = 0);

// Replaces the current content with the text from the given stream.
// Format tells what is in the stream. If this is tfANSI you also must set Language to a value that
// describes the content of the file.

var
  SW: WideString;
  SA: string;
  Size: Integer;
  CodePage: Integer;

begin
  case Format of
    tfANSI,
    tfUTF8:
      begin
        Size := Stream.Size - Stream.Position;
        SetLength(SA, Size);
        Stream.ReadBuffer(PChar(SA)^, Size);
        if Format = tfANSI then
        begin
          // Check if we actually have UTF 8 here, despite the user said if would be ANSI.
          if CompareMem(@BOM_UTF8, PChar(SA), SizeOf(BOM_UTF8)) then
            CodePage := CP_UTF8
          else
            if Language = 0 then
              CodePage := CP_ACP
            else
              CodePage := CodePageFromLocale(Language);
        end
        else
          CodePage := CP_UTF8;

        SW := StringToWideStringEx(SA, CodePage);
        SetText(SW);
      end;
    tfUTF16:
      begin
        Size := Stream.Size - Stream.Position;
        SetLength(SW, Size);
        Stream.ReadBuffer(PWideChar(SW)^, 2 * Size);
        SetText(SW);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.SaveToStream(Stream: TStream; Format: TTextFormat; Language: LCID = 0;
  WithBOM: Boolean = True);

// Saves the currently loaded text into the given stream. WithBOM determines whether to write a
// byte order mark or not.
// Note: when saved as ANSI text there will never be a BOM.

const
  ANSINewline: AnsiString = #13#10;
  
var
  BOM: WideString;
  SA: string;
  I: Integer;
  CodePage: Integer;

begin
  case Format of
    tfANSI:
      begin
        // ANSI format does not have a BOM, so none is written out.
        if Language = 0 then
          CodePage := CP_ACP
        else
          CodePage := CodePageFromLocale(Language);
        for I := 0 to FCount - 1 do
        begin
          SA := WideStringToStringEx(FLines[I].Text, CodePage);
          Stream.WriteBuffer(PChar(SA)^, Length(SA));
          if I < FCount - 1 then
            Stream.WriteBuffer(PChar(ANSINewline)^, Length(ANSINewline));
        end;
      end;
    tfUTF8:
      begin
        if WithBOM then
        begin
          BOM := BOM_LSB_FIRST;
          SA := WideStringToStringEx(BOM, CP_UTF8);
          Stream.WriteBuffer(PChar(SA)^, Length(SA));
        end;
        for I := 0 to FCount - 1 do
        begin
          SA := WideStringToStringEx(FLines[I].Text, CP_UTF8);
          Stream.WriteBuffer(PChar(SA)^, Length(SA));
          if I < FCount - 1 then
            Stream.WriteBuffer(PChar(ANSINewline)^, Length(ANSINewline));
        end;
      end;
    tfUTF16:
      begin
        if WithBOM then
        begin
          BOM := BOM_LSB_FIRST;
          Stream.WriteBuffer(PWideChar(BOM)^, Length(BOM));
        end;
        for I := 0 to FCount - 1 do
        begin
          Stream.WriteBuffer(PWideChar(FLines[I].FText)^, 2 * Length(FLines[I].FText));
          if I < FCount - 1 then
            Stream.WriteBuffer(PWideChar(WideCRLF)^, 2 * Length(WideCRLF));
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.SetModified(const Value: Boolean);

begin
  if FModified <> Value then
  begin
    FModified := Value;
    FOwner.Modified := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorContent.SetText(const Value: WideString);

// Replaces the current content with the text given by Value. Eventual byte order marks are correctly handled.

var
  Head,
  Tail: PWideChar;
  S: WideString;

begin
  // Special case: empty input and empty content.
  if (Value <> '') or (FCount > 0) then
  begin
    FOwner.FUndoList.PrepareReplaceChange(Point(0, 0), EndPoint, Point(0, 0), Value);

    FOwner.BeginUpdate;
    try
      InternalClear;
      Head := PWideChar(Value);
      if (Head^ = BOM_LSB_FIRST) or (Head^ = BOM_MSB_FIRST) then
      begin
        // Consider byte order mark.
        if Head^ = BOM_MSB_FIRST then
          StrSwapByteOrder(Head);
        Inc(Head);
      end;
      while Head^ <> WideNull do
      begin
        Tail := Head;
        while not (Tail^ in [WideNull, WideLineFeed, WideCarriageReturn, WideVerticalTab, WideFormFeed]) and
          (Tail^ <> WideLineSeparator) and (Tail^ <> WideParagraphSeparator) do
          Inc(Tail);
        SetString(S, Head, Tail - Head);
        AddLine(S);
        Head := Tail;
        if Head^ <> WideNull then
        begin
          Inc(Head);
          if (Tail^ = WideCarriageReturn) and (Head^ = WideLineFeed) then
            Inc(Head);
        end;
      end;

      FOwner.ResetCaret;
      DoChangeLine(nil);
    finally
      FOwner.EndUpdate;
    end;
    FOwner.FUndoList.FinishPendingChange(EndPoint);
  end;
end;

//----------------- TScrollHintWindow ----------------------------------------------------------------------------------

procedure TScrollHintWindow.Paint;

var
  R: TRect;

begin
  R := ClientRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end;

//----------------- TCustomUniCodeEdit ----------------------------------------------------------------------------------

constructor TCustomUniCodeEdit.Create(AOwner: TComponent);

var
  I: Integer;

begin
  inherited Create(AOwner);
  DoubleBuffered := False;
  FOptions := DefaultOptions;
  FContent := TUniCodeEditorContent.Create(Self);

  FControlCharFont := TFont.Create;
  FControlCharFont.Name := 'Wingdings 3';
  FControlCharFont.Size := 10;
  FControlCharFont.Charset := SYMBOL_CHARSET;
  
  FLineNumberFont := TFont.Create;
  FLineNumberFont.Name := 'Terminal';
  FLineNumberFont.Size := 6;
  FLineNumberFont.Color := clWindowText;
  FLineNumberFont.Style := [];
  FLineNumberFont.OnChange := LineNumberFontChanged;

  FUndoList := TUndoList.Create(Self);
  FSelectedColor := TUCELineStyle.Create;
  FScrollHintColor := TUCELineStyle.Create;
  FScrollHintColor.FBackground := clAppWorkSpace;
  FScrollHintColor.FForeground := clInfoText;
  FBookmarkOptions := TBookmarkOptions.Create(Self);

  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 20;
  FScrollTimer.OnTimer := OnScrollTimer;

  FWorkWidth := 0; 
  FCharsInWindow := 1;

  // FRightMargin and FCharWidth have to be set before FontChanged.
  // is called for the first time.
  FRightMargin := 80;

  // This char width value is only a dummy and will be updated later.
  FCharWidth := 8;
  FMarginColor := clSilver;
  FGutterWidth := 30;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption, csReflector, csNeedsBorderPaint];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clWindow;
  FCaretX := 0;
  FCaretY := 0;
  FCaretOffset := Point(0, 0);
  FGutterColor := clBtnFace;
  Font.OnChange := FontChanged;
  Font.Name := 'Courier New';
  Font.Size := 10;
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  with FGutterRect do
  begin
    Left := 0;
    Top := 0;
    Right := FGutterWidth - 1;
    Bottom := Height;
  end;
  FTabSize := 8;
  FUpdateCount := 0;
  FIndentSize := 2;
  FScrollBars := ssBoth;
  FSelectionBlockBegin := Point(0, 0);
  FSelectionBlockEnd := Point(0, 0);
  FInternalBMList := TImageList.Create(Self);
  ConvertAndAddImages(FInternalBMList);
  for I := 0 to 9 do
    FBookmarks[I].Y := -1;
  FBorderStyle := bsSingle;
  FInsertCaret := ctVerticalLine;
  FOverwriteCaret := ctBlock;
  FKeyStrokes := TKeyStrokes.Create(Self);

  // Retrive double click time to be used in triple clicks.
  // Make this time smaller to avoid triple clicks in cases of double clicks and drag'n drop start.
  FDoubleClickTime := GetDoubleClickTime;
  SetDefaultKeystrokes;

  FScrollHint := TScrollHintWindow.Create(Self);
  FScrollHint.Name := 'Scroll';
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TCustomUniCodeEdit.Destroy;

begin
  FKeyStrokes.Free;
  FLineNumberFont.Free;
  FControlCharFont.Free;
  FContent.Free;
  FSelectedColor.Free;
  FScrollHintColor.Free;
  FBookmarkOptions.Free;
  FScrollTimer.Free;
  FUndoList.Free;

  // The scroll hint window is automatically destroyed.
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetCanRedo: Boolean;

begin
  Result := (eoUseUndoRedo in FOptions) and FUndoList.CanRedo and not (eoReadOnly in FOptions);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetCanUndo: Boolean;

begin
  Result := (eoUseUndoRedo in FOptions) and FUndoList.CanUndo and not (eoReadOnly in FOptions);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetCaretXY: TPoint;

begin
  Result := Point(FCaretX, FCaretY);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetFont: TFont;

begin
  Result := inherited Font;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetLineText: WideString;

// Returns the text of the current line.
// If there is no line currently then one is created implicitely.

var
  WasModified: Boolean;

begin
  if FContent.Count = 0 then
  begin
    WasModified := Modified;
    FContent.AddLine('');
    // It's a dummy line we just have inserted hence we should restore the
    // previous modification state.
    Modified := WasModified;
  end;
  Result := FContent[FCaretY].Text;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetMaxUndo: Integer;

begin
  Result := FUndoList.MaxUndo;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetSelectedText: WideString;

// Returns the currently selected text.

begin
  Result := FContent.CollectTextFromBlock(BlockBegin, BlockEnd);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetSelectionAvailable: Boolean;

begin
  Result := ((BlockBegin.X <> BlockEnd.X) or (BlockBegin.Y <> BlockEnd.Y)) and (FContent.Count > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetSelEnd: Integer;

var
  Loop: Integer;
  X, Y: Integer;

begin
  X := BlockEnd.X;
  Y := BlockEnd.Y;

  Result := 0;
  Loop := 0;
  while Loop < (Y - 1) do
  begin
    Inc(Result, Length(FContent[Loop].Text) + 2);
    Inc(Loop);
  end;
  Result := Result + X;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetSelStart: Integer;

var
  Loop: Integer;
  X, Y: Integer;

begin
  Result := 0;

  if SelectionAvailable then
  begin
    X := BlockBegin.X;
    Y := BlockBegin.Y;
    Loop := 0;

    while Loop < Y do
    begin
      Inc(Result, Length(FContent[Loop].Text) + 2);
      Inc(Loop);
    end;

    Result := Result + Min(X, Length(FContent[Loop].Text) + 2);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetText: WideString;

begin
  Result := FContent.Text;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetTopLine: Integer;

begin
  Result := -FOffsetY + 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetBlockBegin(Value: TPoint);

// Values are logical coordinates (x -> column).

var
  Index: Integer;

begin
  if Value.Y > FContent.Count - 1 then
    Value.Y := FContent.Count - 1;
  if Value.Y < 0 then
    Value.Y := 0;

  if Value.X < 0 then
    Value.X := 0
  else
  begin
    Index := WorkWidth div CharWidth;
    if Value.X > Index then
      Value.X := Index;
  end;

  if (FUpdateCount = 0) and SelectionAvailable then
    RefreshLines(FSortedBlockBegin.Y, FSortedBlockEnd.Y);

  FSelectionBlockBegin := Value;
  FSelectionBlockEnd := Value;
  FSortedBlockBegin := Value;
  FSortedBlockEnd := Value;

  if (FUpdateCount = 0) and SelectionAvailable then
    RefreshLines(FSortedBlockBegin.Y, FSortedBlockEnd.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetBlockEnd(Value: TPoint);

var
  Index: Integer;
  
begin
  if Value.Y > FContent.Count - 1 then
    Value.Y := FContent.Count - 1;
  if Value.Y < 0 then
    Value.Y := 0;

  if Value.X < 0 then
    Value.X := 0
  else
  begin
    Index := WorkWidthToCharIndex(Value.Y);
    if Value.X > Index then
      Value.X := INdex;
  end;

  if not PointsEqual(Value, FSelectionBlockEnd) then
  begin
    if FUpdateCount = 0 then
    begin
      if Value.Y <> FSelectionBlockEnd.Y then
        RefreshLines(Value.Y, FSelectionBlockEnd.Y)
      else
        RefreshLine(FSelectionBlockEnd.Y);
    end;

    FSelectionBlockEnd := Value;
    FSortedBlockBegin := MinPoint(FSelectionBlockBegin, FSelectionBlockEnd);
    FSortedBlockEnd := MaxPoint(FSelectionBlockBegin, FSelectionBlockEnd);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetBorderStyle(Value: TBorderStyle);

begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetCaretX(Value: Integer);

var
  I: Integer;

begin
  if Value < 0 then
    Value := 0;

  if FCaretX <> Value then
  begin
    if (CaretY < FContent.Count) then
      I := FContent[FCaretY].GetLineEnd(False)
    else
      I := 0;

    if eoScrollPastEOL in FOptions then
    begin
      I := WorkWidthToCharIndex(FCaretY);
      if Value > I then
        Value := I;
    end
    else
    begin
      if Value > I then
        Value := I;
    end;
    FCaretX := Value;
    EnsureCursorPosVisible;
    if FUpdateCount = 0 then
    begin
      UpdateCaret;
      DoCaretChange;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetCaretXY(const Value: TPoint);

var
  FinishUndo: Boolean;

begin
  // Set Y first because ScrollPastEOL can effect the X pos, prevent SetCaretX/Y from sending OnChange events.
  FinishUndo := FUndoList.PrepareStateChange;
  Inc(FUpdateCount);
  CaretY := Value.Y;
  CaretX := Value.X;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    UpdateCaret;
    UpdateScrollBars;
    DoCaretChange;
  end;

  if FinishUndo then
    FUndoList.FinishPendingChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetCaretY(Value: Integer);

var
  Len: Integer;
  S: WideString;
  FinishUndo: Boolean;

begin
  if (Value < 0) or (FContent.Count = 0) then
    Value := 0;
  if (FContent.Count > 0) and (Value > FContent.Count - 1) then
    Value := FContent.Count - 1;
  if FCaretY <> Value then
  begin
    // Remove trailing blanks in the line we just leaving
    if (FCaretY > -1) and (FCaretY < FContent.Count) and not (eoKeepTrailingBlanks in FOptions) then
    begin
      S := WideTrimRight(FContent[FCaretY].Text);
      if S <> FContent[FCaretY].Text then
      begin
        // Do not trigger a change event here, but add an undo action.
        FinishUndo := FUndoList.PrepareDeleteChange(
          Point(FContent[FCaretY].CharIndexToColumn(Length(S)), FCaretY),
          Point(FContent[FCaretY].GetLineEnd(False), FCaretY));
        FContent[FCaretY].FText := WideTrimRight(S);
        if FinishUndo then
          FUndoList.FinishPendingChange;
      end;
    end;
    FCaretY := Value;

    if not (eoScrollPastEOL in FOptions) then
    begin
      Len := FContent[FCaretY].GetLineEnd(False);
      if FCaretX > Len then
      begin
        CaretX := Len;
        if FCaretX < -FOffsetX * FCharWidth then
          OffsetX := FCaretX * FCharWidth;
        Exit;
      end;
    end;
    EnsureCursorPosVisible;
    if FUpdateCount = 0 then
    begin
      UpdateCaret;
      DoCaretChange;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetCharWidth(const Value: Integer);

begin
  if FCharWidth <> Value then
  begin
    FCharWidth := Value;
    
    // Invalidate all lines. They have to recompute their distance arrays.
    FContent.InternalInvalidateAll;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetDefaultStyle(const Value: IUCELineStyle);

begin
  if FDefaultStyle <> Value then
  begin
    FDefaultStyle := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetExtraLineSpacing(const Value: Integer);

begin
  FExtraLineSpacing := Value;
  FontChanged(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetFont(const Value: TFont);

var
  SavePitch: TFontPitch;

begin
  SavePitch := Value.Pitch;
  Value.Pitch := fpFixed;
  inherited Font := Value;
  Value.Pitch := SavePitch;
  FControlCharFont.Size := Value.Size;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetGutterColor(Value: TColor);

begin
  if FGutterColor <> Value then
  begin
    FGutterColor := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetGutterWidth(Value: Integer);

var
  OldValue: Integer;

begin
  OldValue := GutterWidth;
  if OldValue <> Value then
  begin
    if Value < 0 then
      Value := 0;
    FGutterWidth := Value;
    FGutterRect.Right := Value - 1;
    if HandleAllocated then
    begin
      FCharsInWindow := (ClientWidth - FGutterRect.Right + 1) div FCharWidth;
      UpdateCaret;
      UpdateScrollBars;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetHighlighter(const Value: TUCEHighlighter);

begin
  if Value <> FHighLighter then
  begin
    if Assigned(FHighLighter) then
      FHighLighter.WindowList.RemoveInstance(Self);
    FHighLighter := Value;
    if Assigned(FHighLighter) then
    begin
      Value.WindowList.AddInstance(Self);
      Value.FreeNotification(Self);
      RescanLine(0);
    end
    else
      Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetIndentSize(Value: Integer);

begin
  if Value < 1 then
    Value := 1;
  if FIndentSize <> Value then
  begin
    FIndentSize := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetInsertCaret(const Value: TCaretType);

begin
  if FInsertCaret <> Value then
  begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetKeystrokes(const Value: TKeyStrokes);

begin
  if Value = nil then
    FKeyStrokes.Clear
  else
    FKeyStrokes.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetLineNumberFont(const Value: TFont);

begin
  FLineNumberFont.Assign(Value);
  LineNumberFontChanged(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetLineText(Value: WideString);

begin
  if FContent.Count = 0 then
    FContent.AddLine('');
  if FCaretY < FContent.Count then
  begin
    FContent[FCaretY].Text := Value;
    RefreshLine(FCaretY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetMarginColor(const Value: TColor);

begin
  if FMarginColor <> Value then
  begin
    FMarginColor := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetMaxUndo(const Value: Integer);

begin
  if Value > -1 then
  begin
    FUndoList.MaxUndo := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetModified(const Value: Boolean);

begin
  if FModified <> Value then
  begin
    FModified := Value;
    FContent.Modified := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetOffsetX(Value: Integer);

var
  dX: Integer;
  R: TRect;

begin
  if Value < ClientWidth - FGutterRect.Right - WorkWidth - FCharWidth then
    Value := ClientWidth - FGutterRect.Right - WorkWidth - FCharWidth;
  if Value > 0 then
    Value := 0;
  if FOffsetX <> Value then
  begin
    dX := Value - FOffsetX;
    FOffsetX := Value;
    R := Rect(FGutterRect.Right + 1, 0, ClientWidth, ClientHeight);
    ScrollWindow(Handle, dX, 0, nil, @R);
    if (CaretXPix + FCaretOffset.X) < (FGutterRect.Right + 1) then
      HideCaret
    else
      ShowCaret;
    if FUpdateCount = 0 then
      UpdateScrollbars;
    DoScroll(dX, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetOffsetY(Value: Integer);

// This offset is measured in lines not pixels.

var
  dY: Integer;

begin
  // Range check, order is important here.
  if Value < (FLinesInWindow - FContent.Count) then
    Value := FLinesInWindow - FContent.Count;
  if Value > 0 then
    Value := 0;
  if FOffsetY <> Value then
  begin
    dY := FTextHeight * (Value - FOffsetY);
    FOffsetY := Value;
    ScrollWindow(Handle, 0, dY, nil, nil);
    if FUpdateCount = 0 then
      UpdateScrollbars;
    DoScroll(0, dY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetOptions(const Value: TUniCodeEditOptions);

var
  ToBeSet,
  ToBeCleared,
  ToChange: TUniCodeEditOptions;
  Index: Integer;

begin
  if FOptions <> Value then
  begin
    ToBeSet := Value - FOptions;
    ToBeCleared := FOptions - Value;
    FOptions := Value;
    ToChange := ToBeSet + ToBeCleared;

    if eoReadOnly in FOptions then
      if eoShowCursorWhileReadOnly in ToBeSet then
        InitializeCaret
      else
      begin
        HideCaret;
        DestroyCaret;
      end;

    if HandleAllocated and ((eoHideSelection in ToChange) or
      (eoShowControlChars in ToChange) or
      (eoUseSyntaxHighlighting in ToChange)) then
      Invalidate;

    if eoLineNumbers in ToChange then
    begin
      FGutterRect.Right := FGutterWidth - 1;
      if HandleAllocated then
      begin
        FCharsInWindow := (ClientWidth - FGutterRect.Right + 1) div FCharWidth;
        Invalidate;
      end;
    end;

    // Reset position in case it's past EOL currently.
    Index := WorkWidthToCharIndex(FCaretY);
    if (eoScrollPastEOL in ToBeCleared) and (FCaretX > Index) then
      CaretX := Index;
    if HandleAllocated and (eoLineNumbers in ToChange) and Focused then
      UpdateCaret;
    DoSettingChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetOverwriteCaret(const Value: TCaretType);

begin
  if FOverwriteCaret <> Value then
  begin
    FOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetRightMargin(Value: Integer);

begin
  if (FRightMargin <> Value) then
  begin
    FRightMargin := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetScrollBars(const Value: TScrollStyle);

begin
  if (FScrollBars <> Value) then
  begin
    FScrollBars := Value;
    RecreateWnd;
    UpdateScrollBars;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetScrollHintColor(const Value: TUCELineStyle);

begin
  FScrollHintColor.FBackground := Value.FBackground;
  FScrollHintColor.FForeground := Value.FForeground;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetSelectedColor(const Value: TUCELineStyle);

begin
  FSelectedColor.FBackground := Value.FBackground;
  FSelectedColor.FForeground := Value.FForeground;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetSelEnd(const Value: Integer);

var
  P: TPoint;
  Loop: Integer;
  Count: Integer;

begin
  Loop := 0;
  Count := 0;
  while (Loop < FContent.Count - 1) and ((Count + Length(FContent[Loop].Text) + 2) < Value) do
  begin
    Inc(Count, Length(FContent[Loop].Text) + 2);
    Inc(loop);
  end;
  P.Y := Loop;
  P.X := Value - Count;
  If P.X > Length(FContent[Loop].Text) then
    P.X := Length(FContent[Loop].Text);
  Blockend := P;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetSelStart(const Value: Integer);

var
  Loop: Integer;
  Count: Integer;
  P: TPoint;

begin
  if FContent.Count = 0 then
    P := Point(0, 0)
  else
  begin
    Loop := 0;
    Count := 0;
    while (Loop < FContent.Count) and ((Count + Length(FContent[Loop].Text) + 2) < Value) do
    begin
      Inc(Count, Length(FContent[Loop].Text) + 2);
      Inc(loop);
    end;
    P.X := Value - Count;
    if Loop = FContent.Count then
      Dec(Loop);
    P.Y := Loop;
    if P.X > Length(FContent[Loop].Text) then
      P.X := Length(FContent[Loop].Text);
  end;
  BlockBegin := P;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetSelText(const Value: WideString);

// Replaces the current selection by the given string. If there is no selection then the new text is
// inserted at the current block start position.
// On return the new text is selected (BlockBegin and BlockEnd point to start and end of the new text).

begin
  if SelectionAvailable then
    DeleteSelection(Value = '');
  if Value <> '' then
    InsertText(Value);
  RefreshToBottom(BlockBegin.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetSelTextExternal(const Value: WideString);


begin
  FUndoList.PrepareReplaceChange(BlockBegin, BlockEnd, BlockBegin, Value);
  SetSelText(Value);
  CaretXY := BlockEnd;

  // Remove selection indicator.
  BlockBegin := CaretXY;
  FUndoList.FinishPendingChange(CaretXY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetTabSize(Value: Integer);

begin
  if Value < 1 then
    Value := 1;
  if FTabSize <> Value then
  begin
    FTabSize := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetText(const Value: WideString);

begin
  FContent.Text := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetTopLine(Value: Integer);

begin
  if Value < 1 then
    Value := 1;
  if TopLine <> Value then
  begin
    if Value > FContent.Count then
      Value := FContent.Count;
    OffsetY := 1 - Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetUpdateState(Updating: Boolean);

begin
  SendMessage(Handle, WM_SETREDRAW, Ord(not Updating), 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetWordBlock(Value: TPoint);

var
  BB, BE: TPoint;

begin
  GetWordAndBounds(Value, BB, BE);
  BlockBegin := BB;
  BlockEnd := BE;
  CaretXY := BE;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetWorkWidth(const Value: Integer);

begin
  if (FWorkWidth <> Value) and HandleAllocated then
  begin
    if Value < 0 then
      FWorkWidth := 0
    else
      FWorkWidth := Value;
    Invalidate;
    UpdateScrollbars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.CMMouseWheel(var Message: TCMMouseWheel);

var
  ScrollCount: Integer;

begin
  inherited;
  if Message.Result = 0 then
    with Message do
    begin
      Result := 1;
      if ssCtrl in ShiftState then
        ScrollCount := FLinesInWindow * (WheelDelta div WHEEL_DELTA)
      else
        ScrollCount := Mouse.WheelScrollLines * (WheelDelta div WHEEL_DELTA);
      TopLine := TopLine - ScrollCount;
      Update;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.CMSysColorChange(var Message: TMessage);

begin
  inherited;
  ConvertAndAddImages(FInternalBMList);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMCopy(var Message: TWMCopy);

begin
  CopyToClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMCut(var Message: TWMCut);

begin
  CutToClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMEraseBkgnd(var Message: TMessage);

begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);

begin
  Msg.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if eoWantTabs in FOptions then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMHScroll(var Message: TWMScroll);

  //--------------- local functions -------------------------------------------

  function GetRealScrollPosition: Integer;

  var
    SI: TScrollInfo;
    Code: Integer;

  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    GetScrollInfo(Handle, Code, SI);
    Result := SI.nTrackPos;
  end;

  //--------------- end local functions ---------------------------------------

begin
  case Message.ScrollCode of
    SB_BOTTOM:
      OffsetX := -WorkWidth;
    SB_ENDSCROLL:
      ;
    SB_LINELEFT:
      if FOffsetX < 0 then
        OffsetX := FOffsetX + FCharWidth;
    SB_LINERIGHT:
      OffsetX := FOffsetX - FCharWidth;
    SB_PAGELEFT:
      OffsetX := FOffsetX + ClientWidth - FGutterRect.Right + 1;
    SB_PAGERIGHT:
      OffsetX := FOffsetX - ClientWidth + FGutterRect.Right + 1;
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      OffsetX := -GetRealScrollPosition;
    SB_TOP:
      OffsetX := 0;
  end;
  Message.Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMImeComposition(var Message: TMessage);

var
  IMEContext: HIMC;
  P: Pointer;
  Size: Integer;

begin
  if (Message.LParam and GCS_RESULTSTR) <> 0 then
  begin
    IMEContext := ImmGetContext(Handle);
    try
      // Note: The docs say the return value would always be in bytes, even for Unicode input.
      //       This is obviously not true! The return value is number of characters (without null terminator).
      Size := ImmGetCompositionString(IMEContext, GCS_RESULTSTR, nil, 0);
      if Size < 0 then
      begin
        // A problem occured. IMM_ERROR_NODATA can be ignored but IMM_ERROR_GENERAL cannot.
        if Size = IMM_ERROR_GENERAL then
          raise Exception.Create(SGeneralIMEError);
      end
      else
      begin
        Inc(Size); // Add place for the null terminator.
        P := nil;
        try
          if PlatformIsUnicode then
          begin
            P := AllocMem(2 * Size);
            ImmGetCompositionStringW(IMEContext, GCS_RESULTSTR, P, 2 * Size);
            PWideChar(P)[Size - 1] := #0;
            InsertText(PWideChar(P));
          end
          else
          begin
            // On non-Unicode platforms we get only back meaningful values if the system
            // local corresponds to the locale of the input method editor.
            // In this case we get ANSI strings that must be converted with the current system locale.
            P := AllocMem(Size);
            ImmGetCompositionString(IMEContext, GCS_RESULTSTR, P, Size);
            PChar(P)[Size - 1] := #0;
            InsertText(PChar(P));
          end;

          // The text is selected when inserted via InsertText so remove selection and put the caret
          // at the end of the new text.
          BlockBegin := BlockEnd;
          CaretXY := BlockEnd;
        finally
          FreeMem(P);
        end;
      end;
    finally
      ImmReleaseContext(Handle, IMEContext);
    end;
    Message.Result := 0;
  end
  else
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMImeNotify(var Message: TMessage);

var
  IMC: HIMC;
  LogFont: TLogFont;

begin
  with Message do
  begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          IMC := ImmGetContext(Handle);
          if IMC <> 0 then
          begin
            // need to tell the composition window what font we are using currently
            GetObject(Font.Handle, SizeOf(TLogFont), @LogFont);
            ImmSetCompositionFont(IMC, @LogFont);
            ImmReleaseContext(Handle, IMC);
          end;
          Message.Result := 0;
        end;
    else
      inherited;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMKillFocus(var Message: TWMKillFocus);

begin
  inherited;
  HideCaret;
  DestroyCaret;
  if (eoHideSelection in FOptions) and SelectionAvailable then
    Invalidate;

  if Assigned(FOnFocusChanged) then
    FOnFocusChanged(Self, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMPaste(var Message: TWMPaste);

begin
  PasteFromClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMSetCursor(var Message: TWMSetCursor);

var
  P: TPoint;

begin
  if (csDesigning in ComponentState) or (Screen.Cursor <> crDefault) then
    inherited
  else
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if (Message.HitTest <> HTCLIENT) or
      PtInRect(FGutterRect, P) then
      Windows.SetCursor(Screen.Cursors[crArrow])
    else
    begin
      P := PositionFromPoint(P.X, P.Y);
      if PosInSelection(P) then
        Windows.SetCursor(Screen.Cursors[crArrow])
      else
        Windows.SetCursor(Screen.Cursors[Cursor]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMSetFocus(var Message: TWMSetFocus);

begin
  inherited;
  InitializeCaret;
  UpdateScrollBars;
  if (eoHideSelection in FOptions) and SelectionAvailable then
    Invalidate;

  if Assigned(FOnFocusChanged) then
    FOnFocusChanged(Self, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMSize(var Message: TWMSize);

begin
  inherited;
  
  if HandleAllocated then
  begin
    FCharsInWindow := (ClientWidth - FGutterRect.Right + 1) div FCharWidth;
    FLinesInWindow := ClientHeight div FTextHeight;
    FGutterRect.Bottom := ClientHeight;
    if (FContent.Count - TopLine + 1) < FLinesInWindow then
      TopLine := FContent.Count - FLinesInWindow + 1;

    OffsetX := FOffsetX; // Used to update the offset if it at the end of the work area.
    UpdateScrollBars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WMVScroll(var Message: TWMScroll);

  //--------------- local functions -------------------------------------------

  function GetRealScrollPosition: Integer;

  var
    SI: TScrollInfo;
    Code: Integer;

  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_VERT;
    GetScrollInfo(Handle, Code, SI);
    Result := SI.nTrackPos;
  end;

  //--------------- end local functions ---------------------------------------

var
  LineString: WideString;
  HintRect: TRect;
  P: TPoint;
  ButtonH: Integer;

begin
  case Message.ScrollCode of
    SB_BOTTOM:
      OffsetY := -FContent.Count;
    SB_ENDSCROLL:
      FScrollHint.ReleaseHandle;
    SB_LINEUP:
      OffsetY := FOffsetY + 1;
    SB_LINEDOWN:
      OffsetY := FOffsetY - 1;
    SB_PAGELEFT:
      OffsetY := FOffsetY + FLinesInWindow;
    SB_PAGERIGHT:
      OffsetY := FOffsetY - FLinesInWindow;
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        OffsetY := -GetRealScrollPosition;
        if eoShowScrollHint in FOptions then
        begin
          LineString := IntToStr(-FOffsetY + 1);
          // Calculate optimal size for scroll hint window.
          HintRect := FScrollHint.CalcHintRect(2000, LineString, nil);
          // Place it parallel to the thumb.
          P := ClientToScreen(Point(0, 0));
          ButtonH := GetSystemMetrics(SM_CYVSCROLL) + 8;
          OffsetRect(HintRect,
            P.X + ClientWidth - HintRect.Right - 2,
            ButtonH div 2 + P.Y + Round((ClientHeight - HintRect.Bottom - ButtonH) * -FOffsetY / FContent.Count));

          with FScrollHint do
          begin
            Font.Color := FScrollHintColor.FForeground;
            Color := FScrollHintColor.FBackground;
            ActivateHint(HintRect, LineString);
            UpdateWindow(Handle);
          end;
        end;
      end;
    SB_TOP:
      OffsetY := 0;
  end;
  Message.Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.CaretXPix: Integer;

begin
  Result := FCaretX * FCharWidth + FGutterRect.Right + 1 + FOffsetX;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.CaretYPix: Integer;

begin
  Result := (FCaretY + FOffsetY) * FTextHeight;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ComputeCaret(X, Y: Integer);

// Sets the caret to the line/column indicated by the given pixel position.

begin
  FUndoList.PrepareStateChange;
  CaretXY := PositionFromPoint(X, Y);
  FUndoList.FinishPendingChange;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.CopyOnDrop(Source: TObject): Boolean;

// determines wether a drag'n drop operation is a move or a copy

var
  Shift: TShiftState;

begin
  // determine move or copy operation
  Shift := KeyDataToShiftState(0);
  // w/o modifier keys the operation is determined source and target edit being the same or not
  if Shift = [] then
    Result := Source <> Self
  else
    Result := (ssCtrl in Shift) or (Source <> Self) and not (ssShift in Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.CreateParams(var Params: TCreateParams);

const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);

  CS_OFF = CS_HREDRAW or CS_VREDRAW;

begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    WindowClass.Style := WindowClass.Style and not CS_OFF;
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUnicodeEdit.DeleteLastChar;

var
  Temp: WideString;
  Len: Integer;
  I: Integer;
  J: Integer;
  CX: Integer;
  TargetPoint: TPoint;

begin
  if not (eoReadOnly in FOptions) then
  begin
    if SelectionAvailable then
    begin
      FUndoList.PrepareDeleteChange(BlockBegin, BlockEnd);
      SetSelText('');
      BlockBegin := CaretXY;
      FUndoList.FinishPendingChange;
    end
    else
    begin
      Temp := LineText;
      Len := Length(Temp);

      // Get char index from column.
      CX := FContent[FCaretY].ColumnToCharIndex(FCaretX);
      if CX <= Len then
      begin
        if CX > 0 then
        begin
          // Current index is somewhere on the line. First check whether we
          // can do auto unindentation.
          I := CX - 1;
          while (I > -1) and UnicodeIsWhiteSpace(Word(Temp[I + 1])) do
            Dec(I);
          if (eoAutoUnindent in FOptions) and (I = -1) then
          begin
            // Fine, all conditions are met to do auto unindentation.
            I := Unindent(FCaretX, FCaretY);
            FUndoList.PrepareDeleteChange(Point(I, FCaretY), CaretXY);

            // Read the current line text again. Unindent might have changed tabs to spaces.
            Temp := LineText;
            J := FContent[FCaretY].ColumnToCharIndex(I);
            CX := FContent[FCaretY].ColumnToCharIndex(FCaretX);
            
            Delete(Temp, J + 1, CX - J);
            LineText := Temp;
            CaretX := I;
            FUndoList.FinishPendingChange;
          end
          else
          begin
            // No unindentation, just delete last character;
            // Need to test for non-default width character though.
            FUndoList.PrepareDeleteChange(Point(FCaretX - 1, FCaretY), CaretXY);
            if FContent[FCaretY].CharIndexToColumn(CX) = FCaretX then
            begin
              Delete(Temp, CX, 1);
              CaretX := FContent[FCaretY].PreviousCharPos(FCaretX);
            end
            else
            begin
              Delete(Temp, CX + 1, 1);
              CaretX := FContent[FCaretY].CharIndexToColumn(CX);
            end;
            LineText := Temp;
            FUndoList.FinishPendingChange;
          end;
        end
        else
        begin
          // Character index is at line start, but the first char might be a non-default
          // width char and the current column is somewhere inbetween -> check this.
          if FCaretX > 0 then
          begin
            // Yes, the first char is of non-default width, so delete this char
            // instead of moving the line one step up.
            FUndoList.PrepareDeleteChange(Point(FCaretX - 1, FCaretY), CaretXY);
            Delete(Temp, 1, 1);
            LineText := Temp;
            CaretX := 0;
            FUndoList.FinishPendingChange;
          end
          else // No, first char is just an ordinary one, so move this line one step up.
            if FCaretY > 0 then
            begin
              TargetPoint := Point(FContent[FCaretY - 1].GetLineEnd(False), FCaretY - 1);
              FUndoList.PrepareDeleteChange(TargetPoint, CaretXY);
              BlockBegin := TargetPoint;
              BlockEnd := CaretXY;
              SetSelText('');
              CaretXY := TargetPoint;
              BlockBegin := TargetPoint;
              FUndoList.FinishPendingChange;
              RefreshToBottom(FCaretY);
            end;
        end;
      end
      else
      begin
        // Caret is beyond the end of the current line -> just do a cursor move.
        FUndoList.PrepareStateChange;
        CaretX := FContent[FCaretY].CharIndexToColumn(CX - 1);
        FUndoList.FinishPendingChange;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DeleteSelection(NeedRescan: Boolean);

// Deletes all characters in the current selection and causes the highlighter
// to rescan modified lines if NeedRescan is True. Set this to False if you are going
// to insert new text at the current cursor position afterwards and start the rescan then.

var
  I,
  Count,
  Start: Integer;
  TempString,
  TempString2: WideString;
  BeginX,
  EndX: Integer;

begin
  BeginX := FContent[BlockBegin.Y].ColumnToCharIndex(BlockBegin.X);
  EndX := FContent[BlockEnd.Y].ColumnToCharIndex(BlockEnd.X);

  if BlockBegin.Y = BlockEnd.Y then
  begin
    TempString := FContent[BlockBegin.Y].Text;
    Delete(TempString, BeginX + 1, EndX - BeginX);
    FContent[BlockBegin.Y].Text := TempString;
  end
  else
  begin
    TempString := FContent[BlockBegin.Y].Text;
    if Length(TempString) < BeginX then
      TempString := TempString + WideStringOfChar(' ', BeginX - Length(TempString))
    else
      Delete(TempString, BeginX + 1, Length(TempString));
    if BlockEnd.Y < FContent.Count then
    begin
      TempString2 := FContent[BlockENd.Y].Text;
      Delete(TempString2, 1, EndX);
      TempString := TempString + TempString2;
    end;
    Start := BlockBegin.Y;
    Count := BlockEnd.Y - BlockBegin.Y;
    for I := 0 to 9 do
      if FBookmarks[I].Y >= BlockEnd.Y then
        SetBookmark(I, FBookmarks[I].X, FBookmarks[I].Y - Count)
      else
        if FBookmarks[I].Y > BlockBegin.Y then
          SetBookmark(I, FBookmarks[I].X, BlockBegin.Y);

    try
      for I := Count + Start - 1 downto Start do
        FContent.DeleteLine(I);
    finally
      // Don't rescan if strings are empty now or something is to be inserted afterwards.
      if (Start > 0) and NeedRescan then
        RescanLine(Start - 1);
    end;
    // If all lines have been removed then TempString can only be an empty WideString.
    if Start < FContent.Count then
      FContent[Start].Text := TempString;

    CaretY := BlockBegin.Y;
  end;

  CaretX := BlockBegin.X;
  BlockBegin := CaretXY;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DoCaretChange;

begin
  {// try setting the correct keyboard layout
  if not (csLButtonDown in ControlState) and
     (FCaretY < FContent.Count) and
     (FCaretX > 0) and
     (FCaretX < Length(FContent[FCaretY])) then
    ActivateKeyboard(FContent[FCaretY][FCaretX]);}
  if Assigned(FOnCaretChange) then
    FOnCaretChange(Self, FCaretX, FCaretY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DoGutterMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y, Line: Integer);

begin
  if Assigned(FOnGutterMouseDown) then
    FOnGutterMouseDown(Self, Button, Shift, X, Y, Line);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DoScroll(DeltaX, DeltaY: Integer);

begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, DeltaX, DeltaY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DoSettingChanged;

begin
  if Assigned(FOnSettingChange) then
    FOnSettingChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DoStartDrag(var DragObject: TDragObject);

begin
  FDragWasStarted := False;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DragCanceled;

begin
  FScrollTimer.Enabled := False;

  if FDragWasStarted then
    // If dragging was started but then cancelled then restore previous caret position.
    CaretXY := FLastCaret
  else
    // A simple click with a drag operation moves the caret and removes the selection mark.
    BlockBegin := FLastCaret;
    
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

// drag operations are also allowed between different edits

begin
  FDragWasStarted := True;
  inherited;
  if Accept and (Source is TCustomUniCodeEdit) then
  begin
    case State of
      dsDragLeave:
        begin
          FDropTarget := False;
          FScrollTimer.Enabled := False;
          // give the timer time to process its pending message
          Application.ProcessMessages;
        end;
      dsDragEnter:
        begin
          // remember that we are drag target currently as the originator of the
          // drag operation might be another syntax edit instance
          FDropTarget := True;
          SetFocus;
          FScrollTimer.Enabled := True;
        end;
    end;

    // If source and target are identical then we cannot drop on selected text.
    // If this edit is not the source then we accept the operation and will
    // replace the selected text by the incoming one.
    if Source = Self then
      Accept := not PosInSelection(PositionFromPoint(X, Y))
    else
      Accept := True;

    if Accept then
    begin
      // if Ctrl is pressed or Source is not this control then change
      // cursor to indicate copy instead of move
      if CopyOnDrop(Source) then
        TCustomUniCodeEdit(Source).DragCursor := crDragCopy
      else
        TCustomUniCodeEdit(Source).DragCursor := crDragMove;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.FontChanged(Sender: TObject);

var
  DC: HDC;
  Save: THandle;
  Metrics: TTextMetric;
  TMFont: HFONT;
  TMLogFont: TLogFont;

begin
  GetObject(Font.Handle, SizeOf(TLogFont), @TMLogFont);

  // We need to get the font with bold and italics set so we have enough room
  // for the widest character it will draw.  This is because fixed pitch fonts
  // always have the same character width, but ONLY when the same attributes
  // are being used.  That is, a bold character can be wider than a non-bold
  // character in a fixed pitch font.
  TMLogFont.lfWeight := FW_BOLD;
  TMLogFont.lfItalic := 1;
  TMFont := CreateFontIndirect(TMLogFont);
  try
    DC := GetDC(0);
    try
      Save := SelectObject(DC, TMFont);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, Save);
    finally
      ReleaseDC(0, DC);
    end;
  finally
    DeleteObject(TMFont);
  end;

  with Metrics do
  begin
    // Note:  Through trial-and-error I found that tmAveCharWidth should be used
    // instead of tmMaxCharWidth as you would think. I'm basing this behavior
    // on the Delphi IDE editor behavior. If tmMaxCharWidth is used, we end up
    // with chars being much farther apart than the same font in the IDE.
    CharWidth := tmAveCharWidth;

    FTextHeight := tmHeight + tmExternalLeading + FExtraLineSpacing;
    if Assigned(Parent) then
    begin
      FLinesInWindow := ClientHeight div FTextHeight;
      FCharsInWindow := (ClientWidth - FGutterRect.Right + 1) div FCharWidth;
    end;
    if HandleAllocated then
    begin
      if Focused then
      begin
        HideCaret;
        InitializeCaret;
      end;
      UpdateScrollBars;
    end;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.HideCaret;

begin
  if FCaretVisible then
    FCaretVisible := not Windows.HideCaret(Handle);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.InitializeCaret;

var
  ct: TCaretType;
  cw, ch: Integer;

begin
  if not (eoReadOnly in FOptions) or
    (eoShowCursorWhileReadOnly in FOptions) then
  begin
    // CreateCaret automatically destroys the previous one, so we don't have to
    // worry about cleaning up the old one here with DestroyCaret.
    // Ideally, we will have properties that control what these two carets look like.
    HideCaret;
    if eoInserting in FOptions then
      ct := FInsertCaret
    else
      ct := FOverwriteCaret;
    case ct of
      ctHorizontalLine:
        begin
          cw := FCharWidth;
          ch := 2;
          FCaretOffset := Point(0, FTextHeight - 2);
        end;
      ctHalfBlock:
        begin
          cw := FCharWidth;
          ch := (FTextHeight - 2) div 2;
          FCaretOffset := Point(0, ch);
        end;
      ctBlock:
        begin
          cw := FCharWidth;
          ch := FTextHeight - 2;
          FCaretOffset := Point(0, 0);
        end;
    else // ctVerticalLine
      cw := 2;
      ch := FTextHeight - 2;
      FCaretOffset := Point(0, 0);
    end;
    CreateCaret(Handle, 0, cw, ch);
    UpdateCaret;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.InsertLineBreak(MoveCaret: Boolean);

// Inserts a new line at the current cursor position. If MoveCaret is True then the cursor is moved to the start
// of the new line.

var
  FinishUndo: Boolean;
  
begin
  if not (eoReadOnly in FOptions) then
  begin
    if SelectionAvailable then
      FinishUndo := FUndoList.PrepareReplaceChange(BlockBegin, BlockEnd, BlockBegin, WideCRLF)
    else
    begin
      FinishUndo := FUndoList.PrepareInsertChange(CaretXY, WideCRLF);
      BlockBegin := CaretXY;
    end;
    SetSelText(WideCRLF);
    // Remove selection marker.
    BlockBegin := BlockEnd;

    if MoveCaret then
    begin
      if eoAutoIndent in FOptions then
      begin
        CaretXY := Point(NextSmartTab(0, FCaretY + 1, False), FCaretY + 1);
        LineText := WideStringOfChar(' ', FCaretX) +  LineText;
      end
      else
        CaretXY := Point(0, FCaretY + 1);
      if FinishUndo then
        FUndoList.FinishPendingChange(BlockEnd);
      RefreshToBottom(FCaretY - 1);
    end
    else
    begin
      if FinishUndo then
        FUndoList.FinishPendingChange(Point(0, FCaretY + 1));
      RefreshToBottom(FCaretY);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.InsertText(Value: WideString);

// Inserts the given text at the current block start position and updates BlockEnd to point to the end
// of the new text in the editor.
// On return the new text is selected (BlockBegin and BlockEnd point to start and end of the new text).
// The caret is not moved.

var
  I,
  Len: Integer;
  TempString,
  TempString2: WideString;
  Helper: WideString;
  CX: Integer;
  TempY: Integer;
  Temp: TWideStringList;

begin
  if Value <> '' then
  begin
    Temp := TWideStringList.Create;
    try
      Temp.Text := Value + WideCRLF;
      if FContent.Count = 0 then
      begin
        CX := BlockBegin.X;
        TempString := '';
      end
      else
      begin
        CX := FContent[BlockBegin.Y].ColumnToCharIndex(BlockBegin.X);
        TempString := FContent[BlockBegin.Y].Text;
      end;
      Len := CX - Length(TempString);

      if Temp.Count = 1 then
      begin
        if Len > 0 then
          TempString := TempString + WideStringOfChar(' ', Len);
        Insert(Temp[0], TempString, CX + 1);
        if FContent.Count = 0 then
          FContent.AddLine(TempString)
        else
          FContent[BlockBegin.Y].Text := TempString;
        TempY := BlockBegin.Y;
        TempString := Value;
      end
      else
      begin
        if Len > 0 then
        begin
          Helper := WideStringOfChar(' ', Len);
          TempString := TempString + Helper;
        end;
        if CX = 0 then
          TempString := Temp[0]
        else
          TempString := Copy(TempString, 1, CX) + Temp[0];

        if FContent.Count = 0 then
        begin
          TempString2 := '';
          FContent.AddLine(TempString);
        end
        else
        begin
          TempString2 := Copy(FContent[BlockBegin.Y].Text, CX + 1, Length(FContent[BlockBegin.Y].Text));
          FContent[FCaretY].Text := TempString;
        end;
        TempY := FCaretY + 1;
        for I := 1 to Temp.Count - 2 do
        begin
          FContent.InsertLine(TempY, Temp[I]);
          Inc(TempY);
        end;

        // Relocate bookmarks.
        for I := 0 to 9 do
          if FBookmarks[I].Y >= CaretY then
            SetBookmark(I, FBookmarks[I].X, FBookmarks[I].Y + Temp.Count - 1);

        TempString := Temp[Temp.Count - 1];
        FContent.InsertLine(TempY, TempString + TempString2);
        RescanLine(FCaretY);
        CX := 0;
      end;
      BlockEnd := Point(FContent.LineNoInit[TempY].CharIndexToColumn(CX + Length(TempString)), TempY);
    finally
      Temp.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.InternalBlockIndent(Start, Stop: TPoint);

// Inserts FIndentSize space characters in front of every line in the range given by the parameters.
// Note: A designed side effect is that all leading tab characters in the concerned
//       lines will be replaced by the proper number of spaces!

var
  I, Last: Integer;
  Line: WideString;

begin
  // Exclude last selection line if it does not contain any selected chars.
  if Stop.X > 0 then
    Last := Stop.Y
  else
    Last := Stop.Y - 1;

  for I := Start.Y to Last do
  begin
    Line := FContent[I].Text;
    ReplaceLeftTabs(Line);
    Insert(WideStringOfChar(' ', FIndentSize), Line, 1);
    FContent[I].Text := Line;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.InternalBlockUnindent(Start, Stop: TPoint);

// Removes first FIndentSize space characters of every line in the current selection
// or all leading white spaces if their count is less than FIndentSize.
// Note: A designed side effect is that all leading tab characters in the concerned
//       lines will be replaced by the proper number of spaces!

var
  I, J,
  Last: Integer;
  Line: WideString;

begin
  // Exclude last selection line if it does not contain any selected chars.
  if Stop.X > 0 then
    Last := Stop.Y
  else
    Last := Stop.Y - 1;

  for I := Start.Y to Last do
  begin
    Line := FContent[I].Text;
    ReplaceLeftTabs(Line);
    J := LeftSpaces(Line);
    if J >= FIndentSize then
      Delete(Line, 1, FIndentSize)
    else
      Delete(Line, 1, J);
    FContent[I].Text := Line;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.InsertCharacter(NewChar: WideChar);

// Special input handler for single character input. This method is triggered from the command handler
// if a single character is to be inserted.

var
  CX, I: Integer;
  NewText, Temp: WideString;
  TargetEnd: TPoint;

begin
  // #127 is Ctrl + Backspace
  if ((NewChar = WideTabulator) or (NewChar >= WideSpace)) and (NewChar <> #127) and not (eoReadOnly in FOptions) then
  begin
    // Special case tabulator: If the editor is using smart tabs currently then the selection is not deleted,
    // instead it is expanded to the next smart tab position.
    // If smart tabs are not used and tab chars are generally disabled then the input is converted to space chars.
    if NewChar = WideTabulator then
    begin
      CX := NextSmartTab(FCaretX, FCaretY, True);
      if not (eoInserting in FOptions) then
      begin
        // In overwrite mode only the cursor position is changed.
        CaretXY := Point(CX, FCaretY);
      end
      else
      begin
        if eoSmartTabs in FOptions then
        begin
          I := FContent[FCaretY].ColumnToCharIndex(CaretX);
          Temp := LineText;
          NewText := WideStringOfChar(' ', CX - FCaretX);
          FUndoList.PrepareInsertChange(CaretXY, NewText);
          Insert(NewText, Temp, I + 1);
          LineText := Temp;

          TargetEnd := Point(CX, FCaretY);

          // Adjust selection if there is one.
          if SelectionAvailable then
          begin
            if (FCaretY = BlockBegin.Y) and (FCaretX <= BlockBegin.X) then
              BlockBegin := Point(FSelectionBlockBegin.X + CX - FCaretX, FCaretY);
            if FCaretY = BlockEnd.Y then
              BlockEnd := Point(FSelectionBlockEnd.X + CX - FCaretX, FCaretY);
          end;
          CaretX := CX;
          FUndoList.FinishPendingChange(TargetEnd);
          NewText := '';
        end
        else
        begin
          if eoUseTabs in FOptions then
            NewText := NewChar
          else
            NewText := WideStringOfChar(' ', FTabSize);
        end;
      end;
    end
    else
      NewText := NewChar;

    if NewText <> '' then
    begin
      if SelectionAvailable then
      begin
        FUndoList.PrepareReplaceChange(BlockBegin, BlockEnd, BlockBegin, NewText);
        SetSelText(NewText);
      end
      else
      begin
        BlockBegin := CaretXY;
        if eoInserting in FOptions then
        begin
          FUndoList.PrepareInsertChange(CaretXY, NewText);
          SetSelText(NewText);
        end
        else
        begin
          FUndoList.PrepareReplaceChange(BlockBegin, BlockEnd, BlockBegin, NewText);
          BlockEnd := Point(BlockBegin.X + FContent[FCaretY].NextCharPos(BlockBegin.X), FCaretY);
          SetSelText(NewText);
        end;
      end;
      BlockBegin := BlockEnd;
      CaretXY := BlockEnd;
      FUndoList.FinishPendingChange(BlockEnd);
    end;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.IsIdentChar(const AChar: WideChar; IdChars: TIdentChars): Boolean;

// Compatibility routine to check whether a given character is in the provided (SBCS) characters list.
// Works only if AChar is in the ANSI code page (value is < $100).
// Consider this function as being temporary until highlighters are ready which can deal with Unicode.

begin
  Result := Char(AChar) in IdChars;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.KeyDown(var Key: Word; Shift: TShiftState);

var
  Data: Pointer;
  Cmd: TEditorCommand;

begin
  inherited;

  Data := nil;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then
    begin
      Key := 0;
      CommandProcessor(Cmd, WideNull, Data);
      FKeypressHandled := True;
    end
    else
      FKeypressHandled := False;
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.KeyPress(var Key: Char);

var
  CW: WideChar;

begin
  inherited;

  if not FKeypressHandled then
  begin
    CW := KeyUnicode(Key);
    CommandProcessor(ecChar, CW, nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.LeftSpaces(const S: WideString): Cardinal;

// counts the number of white spaces at the beginning of the WideString;
// a tab is considered as one space here

var
  Run: PWideChar;

begin
  Run := PWideChar(S);
  while UnicodeIsWhiteSpace(Word(Run^)) do
    Inc(Run);
  Result := Run - PWideChar(S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.LineNumberFontChanged(Sender: TObject);

begin
  if HandleAllocated then
    InvalidateRect(Handle, @FGutterRect, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.LinesChanged;

begin
  if HandleAllocated then
  begin
    UpdateScrollBars;
    RescanLine(FCaretY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.Loaded;

begin
  inherited Loaded;
  UpdateScrollBars;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  Position: TPoint; // Postion in line/column format.

begin
  inherited;

  if Button = mbLeft then
  begin
    if not Focused then
    begin
      // Focus change. Don't use the SetFocus method as this does not work for MDI windows or embedded OLE
      // controls.
      if CanFocus then
        Windows.SetFocus(Handle);
      // Don't delete selection if the edit wasn't focused previously.
      if SelectionAvailable then
        Exit;
    end;

    Position := PositionFromPoint(X, Y);
    if PtInRect(FGutterRect, Point(X, Y)) then
      DoGutterMouseDown(Button, Shift, X, Y, Position.Y)
    else
    begin
      // MouseDown will also be called when the mouse has been clicked twice (double click),
      // hence we can handle the triple click stuff herein too
      if ssDouble in Shift then
      begin
        FLastDblClick := GetTickCount;
        if FContent.Count > 0 then
          SetWordBlock(CaretXY);
        FMultiClicked := True;
      end
      else
      begin
        if (eoTripleClicks in FOptions) and (Shift = [ssLeft]) and (FLastDblClick > 0) then
        begin
          if (GetTickCount - FLastDblClick) < FDoubleClickTime then
          begin
            TripleClick;
            Exit;
          end;
          FLastDblClick := 0;
        end;

        SetCapture(Handle);
        // keep last cursor position to restore it in case of cancelled drag operation
        FLastCaret := CaretXY;
        ComputeCaret(X, Y);
        if ((Shift - [ssCtrl]) = [ssLeft]) and PosInSelection(Position) then
          BeginDrag(False)
        else
        begin
          if ssShift in Shift then
            BlockEnd := CaretXY
          else
            BlockBegin := CaretXY;
        end;
      end;
    end;
  end
  else
    FScrollTimer.enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.MouseMove(Shift: TShiftState; X, Y: Integer);

var
  P: TPoint;

begin
  inherited;
  if X > FGutterWidth then
  begin
    P := PositionFromPoint(X, Y);
    // don't use ssLeft in Shift as this parameter is not always reliable (mouse wheel!)
    if (csLButtonDown in ControlState) and not FMultiClicked then
    begin
      if not FScrollTimer.Enabled
        and ((FLastCaret.X - P.X <> 0) or (FLastCaret.Y - P.Y <> 0)) then
        FScrollTimer.Enabled := True;
    end
    else
      FScrollTimer.enabled := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  FMultiClicked := False;
  FScrollTimer.enabled := False;
  ReleaseCapture;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.NextSmartTab(X, Y: Integer; SkipNonWhite: Boolean): Integer;

// Calculates the indent position starting with X and depending on the first
// line above Y which contains a white space char before X.
// X and Result are column values!

var
  I: Integer;
  Len: Integer;
  Line: WideString;

begin
  // cursor at line 0?
  if Y = 0 then
    Result := X // no next tab position available
  else
  begin
    // Find a line above the current line which size is greater than X.
    repeat
      Dec(Y);
      I := FContent[Y].GetLineEnd(True);
      if I > X then
        Break;
    until Y = 0;

    // found a line?
    if I > X then
    begin
      Line := FContent[Y].Text;
      Len := Length(Line);
      I := FContent[Y].ColumnToCharIndex(X);
      // skip non-white spaces if required
      if SkipNonWhite then
        while (I < Len) and not UnicodeIsWhiteSpace(Word(Line[I + 1])) do
          Inc(I);

      // Now skip any white space character.
      while (I < Len) and UnicodeIsWhiteSpace(Word(Line[I + 1])) do
        Inc(I);
      Result := FContent[Y].CharIndexToColumn(I);
    end
    else
      Result := X;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.NextWordPos: TPoint;

// Calculates the caret position of the next word start or the line end if there's
// no further word on the current line and the caret is not already on the line's end.

var
  CX, CY: Integer;
  X: Integer;
  FoundWhiteSpace: Boolean;
  Len: Integer;
  Temp: WideString;
  IdChars: TIdentChars;

begin
  Result := CaretXY;
  // ensure at least an empty line is there
  Temp := LineText;
  CX := FContent[FCaretY].ColumnToCharIndex(FCaretX) + 1;
  if CX < Length(FContent[FCaretY].Text) then
    CY := FCaretY
  else
  begin
    CY := FCaretY + 1;
    if CY > FContent.Count - 1 then
      Exit;
    if WideTrim(FContent[CY].Text) = '' then
    begin
      Result := Point(0, CY);
      Exit;
    end;
    CX := 1;
  end;

  if FHighLighter <> nil then
    IdChars := FHighLighter.IdentChars
  else
    IDchars := [#33..#255];

  Temp := FContent[CY].Text;
  Len := Length(Temp);
  FoundWhiteSpace := (not IsIdentChar(Temp[CX], IdChars)) or (CY <> FCaretY);
  if not FoundWhiteSpace then
    for X := CX to Len do
      if not IsIdentChar(Temp[X], IdChars) then
      begin
        CX := X;
        FoundWhiteSpace := True;
        Break;
      end;

  if FoundWhiteSpace then
  begin
    FoundWhiteSpace := False;
    for X := CX to Len do
      if IsIdentChar(Temp[X], IdChars) then
      begin
        Result := Point(X - 1, CY);
        FoundWhiteSpace := True;
        Break;
      end;
  end;

  if not FoundWhiteSpace then
    Result := Point(Len, CY);
  Result.X := FContent[CY].CharIndexToColumn(Result.X);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.OnScrollTimer(Sender: TObject);

// do autoscrolling while mouse down and set selection appropriately

var
  SP,
    CP: TPoint;
  NewCaret: TPoint;
  R: TRect;

begin
  Inc(FUpdateCount);
  // get current caret postion
  GetCursorPos(SP);
  GetWindowRect(Handle, R);
  InflateRect(R, -FTextHeight, -FTextHeight);
  CP := ScreenToClient(SP);
  if not PtInRect(R, SP) then
  begin
    // scroll only if mouse is outside the specified rectangle (which is a bit smaller
    // than the client area in order to allow to stop scrolling if the mouse is moved
    // over an other edit to drop text to)
    if SP.X < R.Left then
      OffsetX := FOffsetX + (R.Left - SP.X);
    if SP.X > R.Right then
      OffsetX := FOffsetX + (R.Right - SP.X);
    if SP.Y < R.Top then
      OffsetY := FOffsetY + (R.Top - SP.Y);
    if SP.Y > R.Bottom then
      OffsetY := FOffsetY + (R.Bottom - SP.Y);
  end;
  NewCaret := PositionFromPoint(CP.X, CP.Y);
  if (not (eoScrollPastEOL in FOptions)) and
    (NewCaret.Y > FContent.Count - 1) then // added by MikeZ
    NewCaret.Y := FContent.Count - 1; // added by MikeZ

  Dec(FUpdateCount);
  if not PointsAreEqual(CaretXY, NewCaret) then
  begin
    if not FDropTarget then
      BlockEnd := NewCaret;
    CaretXY := NewCaret;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.Paint;

var
  WasVisible: Boolean;
  SaveIndex: Integer;

begin
  WasVisible := FCaretVisible;
  if WasVisible then
    HideCaret;

  Canvas.Lock;
  try
    SaveIndex := SaveDC(Canvas.Handle);
    ExcludeClipRect(Canvas.Handle, 0, 0, FGutterRect.Right, FGutterRect.Bottom);
    Canvas.Font := Font;
    if FContent.Count > 0 then
    begin
      if Assigned(FHighLighter) and
        (eoUseSyntaxHighlighting in FOptions) then
        PaintHighlighted(Canvas)
      else
        PaintText(Canvas);
    end
    else
      with Canvas do
      begin
        Brush.Color := Self.Color;
        FillRect(ClipRect);

        // draw the right margin marker
        Pen.Color := FMarginColor;
        Pen.Width := 1;
        MoveTo(FRightMargin * FCharWidth + FGutterRect.Right + 1 + FOffsetX, 0);
        LineTo(FRightMargin * FCharWidth + FGutterRect.Right + 1 + FOffsetX, ClientHeight);
      end;

    RestoreDC(Canvas.Handle, SaveIndex);
    
    // Draw gutter after the content as it needs already validated lines.
    PaintGutter(Canvas);
    if Assigned(FOnPaint) then
      FOnPaint(Self, Canvas);
    if WasVisible then
      ShowCaret;
  finally
    Canvas.Unlock;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.PaintGutter(TextCanvas: TCanvas);

var
  I: Integer;
  UpdateRect, R: TRect;
  StartLine,
  EndLine,
  LineIndex,
  YOffset: Integer;
  Number: string;
  LineBase: Integer;

begin
  // Draw only if (part of) gutter rect is invalid.
  if IntersectRect(UpdateRect, TextCanvas.ClipRect, FGutterRect) then
  begin
    with TextCanvas do
    begin
      Brush.Color := FGutterColor;
      UpdateRect.Left := FGutterRect.Right - 5;
      FillRect(UpdateRect);

      UpdateRect.Right := FGutterRect.Right - 3;
      Pen.Color := clBtnHighlight;
      Pen.Style := psSolid;
      Pen.Width := 1;
      MoveTo(UpdateRect.Right, 0);
      LineTo(UpdateRect.Right, UpdateRect.Bottom);

      Inc(UpdateRect.Right);
      Pen.Color := clBtnShadow;
      MoveTo(UpdateRect.Right, 0);
      LineTo(UpdateRect.Right, UpdateRect.Bottom);

      Inc(UpdateRect.Right, 2);
      Pen.Color := Self.Color;
      Pen.Width := 2;
      MoveTo(UpdateRect.Right, 0);
      LineTo(UpdateRect.Right, UpdateRect.Bottom);

      StartLine := (UpdateRect.Top div FTextHeight) - FOffsetY;
      EndLine := (UpdateRect.Bottom div FTextHeight) - FOffsetY + 1;
      if EndLine > FContent.Count - 1 then
        EndLine := FContent.Count - 1;
      YOffset := (UpdateRect.Top div FTextHeight) * FTextHeight;
      R := Rect(0, YOffset, FGutterRect.Right - 5, YOffset + FTextHeight);

      if eoLineNumbers in FOptions then
      begin
        Font.Assign(FLineNumberFont);

        // Make the line numbers showing one or zero based, depending on options.
        LineBase := Ord(not (eoLineNumbersZeroBased in FOptions));
        for LineIndex := StartLine + LineBase to EndLine + LineBase do
        begin
          Number := IntToStr(LineIndex);
          FillRect(R);
          DrawText(Handle, PChar(Number), Length(Number), R, DT_VCENTER or DT_RIGHT or DT_SINGLELINE);
          OffsetRect(R, 0, FTextHeight);
        end;
      end;

      // Let the lines draw their images.
      for LineIndex := StartLine to EndLine do
      begin
        FContent[LineIndex].DrawMarkers(LineIndex, TextCanvas, 0, YOffset);
        Inc(YOffset, FTextHeight);
      end;

      // Fill rest of gutter rect if necessary.
      Brush.Color := FGutterColor;
      if R.Top < UpdateRect.Bottom then
      begin
        R.Bottom := UpdateRect.Bottom;
        FillRect(R);
      end;

      if BookmarkOptions.GlyphsVisible then
      begin
        for I := 0 to 9 do
          with FBookMarks[I] do
            if Visible and (Y >= StartLine) and (Y <= EndLine) then
            begin
              FInternalBMList.Draw(TextCanvas, FBookmarkOptions.LeftMargin, (Y + FOffsetY) * FTextHeight, I);
            end;
      end;

      ExcludeClipRect(Handle, 0, 0, FGutterRect.Right, FGutterRect.Bottom);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.PaintHighlighted(TextCanvas: TCanvas);

// main drawing routine to paint the control if syntax highlighting is used

var
  YOffset,
  LineIndex,
  PaintOffset,
  OldOffset,
  StartIndex,
  StopIndex,
  TempIndex: Integer;
  TokenData: TTokenData;

  I,
  StartLine,
  EndLine: Integer;
  UpdateRect,
  Dummy,
  R: TRect;
  SelStart,
  SelEnd: Integer;
  CurrentStyle: Cardinal; // to optimize drawing selected/unselected text
  ShowSelection: Boolean;
  LineBase: PWideChar;

  CustomStyle: IUCELineStyle;
  
  //--------------- local functions -------------------------------------------

  procedure PaintToken(Start, Len: Cardinal);

  // paints the given string at current paint offset, no control chars are painted,
  // text attributes must be set up already on call;
  // side effects: current paint offset will be updated

  var
    Index,
    Counter: Cardinal;
    P: PWideChar;

  begin
    // replace non-printable characters,
    // calculate update rect for this token (-part) btw.
    P := LineBase + Start;
    OldOffset := PaintOffset;
    Index := Start;
    Counter := Len;
    while Counter > 0 do
    begin
      if P^ < ' ' then
        P^ := ' ';
      Inc(P);
      Inc(PaintOffset, FContent[LineIndex].CharacterDistances[Index]);
      if PaintOffset > UpdateRect.Right then
      begin
        // The token is longer than what we need to draw. Stop looping here
        // and draw only the necessary part.
        Len := Index - Start + 1;
        Break;
      end;
      Inc(Index);
      Dec(Counter);
    end;

    R := Rect(OldOffset, YOffset, PaintOffset, YOffset + FTextHeight);
    // finally paint text
    ExtTextOutW(TextCanvas.Handle, OldOffset, YOffset, ETO_OPAQUE, @R,
      LineBase + Start, Len, @FContent[LineIndex].CharacterDistances[Start]);
  end;

  //---------------------------------------------------------------------------

  procedure PaintTokenWithControlChars(Start, Len: Cardinal);

  // paints the given WideString at current paint offset, considers control characters,
  // text attributes must be set up already on call;
  // side effects: current paint offset will be updated

  var
    I: Integer;
    Counter: Integer;
    Run: PWideChar;
    LastFont: HFONT;

  begin
    Run := LineBase + Start;
    Counter := Len;

    // TODO: Draw only as many characters as fit in the update rect.
    with TextCanvas do
      while Counter > 0 do
      begin
        // Find Tab or space character.
        I := StrNScanW(Run, WideSpaces);

        // Something to draw before tab/space?
        if I > 0 then
        begin
          if I > Counter then
            I := Counter;
          OldOffset := PaintOffset;
          Inc(PaintOffset, I * FCharWidth);
          R := Rect(OldOffset, YOffset, PaintOffset, YOffset + FTextHeight);
          ExtTextOutW(Handle, OldOffset, YOffset, ETO_OPAQUE, @R, Run, I, @FContent[LineIndex].CharacterDistances[Start]);
          Inc(Start, I);
          Dec(Counter, I);
          Inc(Run, I);
        end;

        // Tabulator or space to draw?
        if Counter > 0 then
        begin
          SetTextAlign(Handle, TA_CENTER);
          LastFont := SelectObject(Handle, FControlCharFont.Handle);

          while Counter > 0 do
          begin
            OldOffset := PaintOffset;
            Inc(PaintOffset, FContent[LineIndex].CharacterDistances[Start]);
            R := Rect(OldOffset, YOffset, PaintOffset, YOffset + FTextHeight);

            case Run^ of
              WideTabulator:
                ExtTextOutW(Handle, (PaintOffset + OldOffset) div 2, YOffset, ETO_OPAQUE, @R, DefaultTabGlyph, 1, nil);
              WideSpace:
                // The space glyph does not need to be in the middle but since it is only one char wide it doesn't matter.
                ExtTextOutW(Handle, (PaintOffset + OldOffset) div 2, YOffset, ETO_OPAQUE, @R, DefaultSpaceGlyph, 1, nil);
            else
              Break;
            end;
            Inc(Start);
            Dec(Counter);
            Inc(Run);
          end;

          SetTextAlign(Handle, TA_LEFT);
          SelectObject(Handle, LastFont);
        end;
      end;
  end;

  //----------------------------------------------------------------------------

  procedure PickColors(Foreground, Background: TColor; ForSelection: Boolean);

  // Selects the given colors into the target canvas. If CustomStyle is Assigned
  // then use this style instead of the given colors.
  // If ForSelection is true then fore- and background colors of the custom style
  // (if assigned) are reversed. If no custom style is given then the the passed in
  // colors are directly used. In this case the default style has no influence.
  // If there is a custom style assigned, which has its font style forcing flag set
  // then also font styles from this custom style are applied.

  var
    Style: IUCELineStyle;

  begin
    if ForSelection then
    begin
      if Assigned(CustomStyle) then
      begin
        if CustomStyle.Foreground <> clDefault then
          Background := CustomStyle.Foreground;
        if CustomStyle.Background <> clDefault then
          Foreground := CustomStyle.Background;
      end;
    end
    else
    begin
      Style := FDefaultStyle;
      if Assigned(CustomStyle) then
        Style := CustomStyle;

      if Assigned(Style) then
      begin
        if Style.Foreground <> clDefault then
          Foreground := Style.Foreground;
        if Style.Background <> clDefault then
          Background := Style.Background;
      end;
    end;

    with TextCanvas do
    begin
      if Foreground = clDefault then
        Font.Color := Self.Font.Color
      else
        Font.Color := Foreground;
      if Background = clDefault then
        Brush.Color := Self.Color
      else
        Brush.Color := Background;

      if Assigned(Style) and Style.ForceFontStyles then
        Font.Style := Style.FontStyles;
    end;
  end;

  //----------------------------------------------------------------------------

  procedure PaintLineBreak;

  var
    LastState: Integer;
    LastFont: HFONT;
    
  begin
    // Ask highlighter which color to use for line break.
    // Keep and restore last state. It is used for the next line later.
    LastState := FHighlighter.GetRange;
    FHighlighter.SetLine(#13#10);
    FHighLighter.Next;
    TokenData := FHighLighter.GetTokenInfo;
    with TokenData do
    begin
      Font.Style := Style;
      PickColors(Foreground, Background, False);
    end;

    R := Rect(PaintOffset, YOffset, PaintOffset + Abs(FControlCharFont.Height), YOffset + FTextHeight);
    if IntersectRect(Dummy, R, UpdateRect) then
    begin
      LastFont := SelectObject(TextCanvas.Handle, FControlCharFont.Handle);
      ExtTextOutW(TextCanvas.Handle, PaintOffset, YOffset, ETO_OPAQUE, @R, DefaultLineBreakGlyph, 1, nil);
      SelectObject(TextCanvas.Handle, LastFont);
    end;
    Inc(PaintOffset, Abs(FControlCharFont.Height));

    FHighlighter.Next;
    FHighlighter.SetRange(LastState);
  end;

  //--------------- end local functions ----------------------------------------

var
  S: WideString;
  
begin
  with TextCanvas do
  begin
    UpdateRect := ClipRect;

    // Optimize drawing by using the update area.
    StartLine := (UpdateRect.Top div FTextHeight) - FOffsetY;
    EndLine := (UpdateRect.Bottom div FTextHeight) - FOffsetY + 1;
    if EndLine > FContent.Count - 1 then
      EndLine := FContent.Count - 1;

    // The vertical position we will paint at. Start at top and increment by
    // FTextHeight for each iteration of the loop (see bottom of for loop).
    YOffset := (UpdateRect.Top div FTextHeight) * FTextHeight;

    // Tell highlighter which state it should use from now on.
    if StartLine < FContent.Count then
    begin
      // Scan all lines until the start line so we have a reliable lexer state.
      // If the start line is the very first line in the control then there is no
      // need to scan it. The highlighter will use the default start state in that case. 
      if (StartLine > FLastValidLine) and (StartLine > 0) then
      begin
        // Not yet scanned, so do it now.
        I := FLastValidLine;
        // Scan all lines up to the start line.
        repeat
          // TODO: The highlighters still use ANSI!
          // SetLine will reset the current lexer state so we have to set it afterwards again.
          FHighLighter.SetLine(FContent[I].Text);
          if I > 0 then
            FHighLighter.SetRange(FContent[I].LexerState);
          repeat
            FHighLighter.Next;
          until not FHighLighter.HasMoreTokens;
          Inc(I);
          // Keep the start state for this line.
          FContent[I].LexerState := FHighLighter.GetRange;
        until I = StartLine;
      end;
    end;
    
    // Determine in advance whether to show selection or not.
    // Note: We have four concurrent options determining when to show selection (apart
    //       from SelectionAvailable). These are: focus state, eoReadOnly, eoReadOnlySelection and
    //       eoHideSelection. Given these states one can derive a logic table with 16 results
    //       and from this table get a logical equation. Optimizing this eq. leads to
    //       the four conditions written and described below. If we name the mentioned
    //       states (in the same order as above) with A, B, C and D, respectively, then
    //       the resulting eq. is: Sel = A¬B or AC or ¬B¬D or C¬D.
    // show selection if...
    ShowSelection := // selection is available and
      SelectionAvailable and (
        // edit is focused and not read only (then both selection opts have no influence) or
        (Focused and not (eoReadOnly in FOptions)) or
        // edit is focused and selection in r/o mode is allowed (then read only and
        // hide selection when unfocused don't matter) or
        (Focused and (eoReadOnlySelection in FOptions)) or
        // edit is not read only and selection will not be hidden when unfocused (then
        // being focused doesn't matter and r/o selection has no influence) or
        ((FOptions * [eoReadOnly, eoHideSelection]) = []) or
        // r/o selection is enabled and selection will not be hidden when unfocused
        // (then being focused or in r/o mode don't matter).
        ((eoReadOnlySelection in FOptions) and not (eoHideSelection in FOptions))
      );

    // Loop through the lines which need repainting.
    for LineIndex := StartLine to EndLine do
    begin
      // Tell highlighter what line we are working on. This will get the first token set up.
      S := FContent[LineIndex].Text;

      FHighLighter.SetLine(S);
      if LineIndex > 0 then
        FHighLighter.SetRange(FContent[LineIndex].LexerState);
      FHighLighter.Next;

      LineBase := PWideChar(S);
      PaintOffset := FOffsetX + FGutterRect.Right + 1;

      // Get a potential custom style.
      CustomStyle := FContent[LineIndex].PeekStyle;

      // If no selection is to draw then go straight ahead.
      if (LineIndex < BlockBegin.Y) or (LineIndex > BlockEnd.Y) or not ShowSelection then
      begin
        while FHighLighter.HasMoreTokens and (PaintOffset < UpdateRect.Right) do
        begin
          TokenData := FHighLighter.GetTokenInfo;
          with TokenData do
          begin
            Font.Style := Style;
            PickColors(Foreground, Background, False);

            if eoShowControlChars in FOptions then
              PaintTokenWithControlChars(Position, Length)
            else
              PaintToken(Position, Length);
          end;
          FHighLighter.Next;
        end;

        if (PaintOffset < UpdateRect.Right) and (eoShowControlChars in FOptions) then
          PaintLineBreak;

        // Prepare end of line drawing.
        PickColors(Self.Font.Color, Self.Color, False);
      end
      else
      begin
        // Selection parts are to be considered, so determine start and end position of
        // the selection on this particular line.
        if BlockBegin.Y < LineIndex then
          SelStart := 0
        else
          SelStart := FContent[LineIndex].ColumnToCharIndex(BlockBegin.X);
        if BlockEnd.Y > LineIndex then
          SelEnd := MaxInt // any large number makes it here
        else
          SelEnd := FContent[LineIndex].ColumnToCharIndex(BlockEnd.X);

        CurrentStyle := 0;
        StopIndex := 0;
        while FHighLighter.HasMoreTokens do
        begin
          // Get the current token (string) to be painted.
          TokenData := FHighLighter.GetTokenInfo;
          // Start with the beginning of the token.
          StartIndex := 0;
          // Stop at the end of the token.
          StopIndex := TokenData.Length;
          Font.Style := TokenData.Style;

          // Paint up to selection start. That length will be the StopIndex
          // or the start of the selection mark, whichever is less.
          TempIndex := Min(StopIndex + TokenData.Position, SelStart) - StartIndex - TokenData.Position;
          if TempIndex > 0 then
          begin
            // set needed styles
            with TokenData do
            begin
              Font.Style := Style;
              PickColors(Foreground, Background, False);
              CurrentStyle := 1;
              if eoShowControlChars in FOptions then
                PaintTokenWithControlChars(Position, TempIndex)
              else
                PaintToken(Position, TempIndex);
            end;
            // update the start index into the line text to skip past the
            // text we just painted.
            Inc(StartIndex, TempIndex);
          end;

          // Paint the selection text. That length will be the StopIndex or the
          // end of the selection mark, whichever is less.
          TempIndex := Min(StopIndex + TokenData.Position, SelEnd) - StartIndex - TokenData.Position;
          if TempIndex > 0 then // Have anything to paint?
          begin
            if CurrentStyle <> 2 then // other than selected style?
            begin
              Font.Style := TokenData.Style;
              // Set the selection highlight colors.
              with FSelectedColor do
                PickColors(Foreground, Background, True);
              CurrentStyle := 2;
            end;
            // Paint the selection text
            with TokenData do
              if eoShowControlChars in FOptions then
                PaintTokenWithControlChars(Position + StartIndex, TempIndex)
              else
                PaintToken(Position + StartIndex, TempIndex);
            // Update the start index into the line text to skip past the
            // text we just painted.
            Inc(StartIndex, TempIndex);
          end;

          // Paint the post-selection text, the length is whatever is left over.
          Font.Style := TokenData.Style;
          TempIndex := StopIndex - StartIndex;
          if TempIndex > 0 then
          begin
            if CurrentStyle <> 1 then // other than unselected style?
            begin
              Font.Style := TokenData.Style;
              // set needed styles
              with TokenData do
                PickColors(Foreground, Background, False);
              CurrentStyle := 1;
            end;
            with TokenData do
              if eoShowControlChars in FOptions then
                PaintTokenWithControlChars(Position + StartIndex, TempIndex)
              else
                PaintToken(Position + StartIndex, TempIndex);
          end;

          if CurrentStyle <> 2 then
            CurrentStyle := 0;
          FHighLighter.Next;
        end;

        // Prepare drawing of the rest of the line.
        if LineIndex < BlockEnd.Y then
        begin
          with FSelectedColor do
            PickColors(Foreground, Background, True);
        end
        else
          PickColors(clDefault, clDefault, False);
        if eoShowControlChars in FOptions then
        begin
          if StopIndex < SelEnd then
          begin
            if Assigned(CustomStyle) then
              Font.Color := CustomStyle.Background
            else
              Font.Color := FSelectedColor.Foreground;
          end
          else
          begin
            if Assigned(CustomStyle) then
              Font.Color := CustomStyle.Foreground
            else
              Font.Color := Self.Font.Color;
          end;
          PaintLineBreak;
        end;
      end;

      // Clear background of the rest of the line.
      FillRect(Rect(PaintOffset, YOffset, UpdateRect.Right, YOffset + FTextHeight));

      // Draw the right margin marker.
      Pen.Color := FMarginColor;
      Pen.Width := 1;
      MoveTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, YOffset);
      LineTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, YOffset + FTextHeight);

      // This line has been painted, increment the vertical offset and loop back
      // for the next line of text.
      Inc(YOffset, FTextHeight);

      // Store state value to avoid later rescans.
      if (LineIndex + 1) < FContent.Count then
      begin
        while FHighLighter.HasMoreTokens do
          FHighLighter.Next;
        FContent[LineIndex + 1].LexerState := FHighLighter.GetRange;
      end;
    end;

    // Finally erase all the space not covered by lines.
    if YOffset < UpdateRect.Bottom then
    begin
      Brush.Color := Self.Color;
      FillRect(Rect(UpdateRect.Left, YOffset, UpdateRect.Right, UpdateRect.Bottom));

      // Draw the right margin marker.
      Pen.Color := FMarginColor;
      Pen.Width := 1;
      MoveTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, YOffset);
      LineTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, UpdateRect.Bottom);
    end;

    if EndLine > FLastValidLine then
      FLastValidLine := EndLine;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.PaintText(TextCanvas: TCanvas);

// main drawing routine to paint the control if no syntax highlighting is used

var
  YOffset,
  LineIndex,
  PaintOffset,
  OldOffset,
  StartIndex,
  StopIndex,
  TempIndex: Integer;
  TokenLen: Integer;

  StartLine,
  EndLine: Integer;
  UpdateRect,
  Dummy,
  R: TRect;
  SelStart,
  SelEnd: Integer;
  ShowSelection: Boolean;
  LineBase: PWideChar;

  CustomStyle: IUCELineStyle;
  
  //--------------- local functions -------------------------------------------

  procedure PaintToken(Start, Len: Cardinal);

  // paints the given WideString at current paint offset, no control chars are painted,
  // text attributes must be set up already on call;
  // side effects: current paint offset will be updated

  var
    Index,
      Counter: Cardinal;
    P: PWideChar;

  begin
    // replace non-printable characters,
    // calculate update rect for this token (-part) btw.
    P := LineBase + Start;
    OldOffset := PaintOffset;
    Index := Start;
    Counter := Len;
    while Counter > 0 do
    begin
      if P^ < ' ' then
        P^ := ' ';
      Inc(P);
      Inc(PaintOffset, FContent[LineIndex].CharacterDistances[Index]);
      if PaintOffset > UpdateRect.Right then
      begin
        // The token is longer than what we need to draw. Stop looping here
        // and draw only the necessary part.
        Len := Index - Start + 1;
        Break;
      end;
      Inc(Index);
      Dec(Counter);
    end;

    R := Rect(OldOffset, YOffset, PaintOffset, YOffset + FTextHeight);
    // finally paint text
    ExtTextOutW(TextCanvas.Handle, OldOffset, YOffset, ETO_OPAQUE, @R, LineBase + Start, Len,
      @FContent[LineIndex].CharacterDistances[Start]);
  end;

  //---------------------------------------------------------------------------

  procedure PaintTokenWithControlChars(Start, Len: Cardinal);

  // paints the given string at current paint offset, considers control characters,
  // text attributes must be set up already on call;
  // side effects: current paint offset will be updated

  var
    I: Cardinal;
    Counter: Cardinal;
    Run: PWideChar;
    TabPos: PWideChar;

  begin
    // replace space characters
    Run := LineBase + Start;
    Counter := Len;
    while Counter > 0 do
    begin
      if Run^ = ' ' then
        Run^ := DefaultSpaceGlyph;
      Inc(Run);
      Dec(Counter);
    end;

    Run := LineBase + Start;
    Counter := Len;
    with TextCanvas do
      while Counter > 0 do
      begin
        // find TAB character
        TabPos := StrScanW(Run, WideTabulator);
        // something to draw before tab?
        if Assigned(TabPos) then
          I := Min(Counter, TabPos - Run)
        else
          I := Counter;
        if I > 0 then
        begin
          OldOffset := PaintOffset;
          Inc(PaintOffset, I * Cardinal(FCharWidth));
          R := Rect(OldOffset, YOffset, PaintOffset, YOffset + FTextHeight);
          ExtTextOutW(Handle, OldOffset, YOffset, ETO_OPAQUE, @R, Run, I, @FContent[LineIndex].CharacterDistances[Start]);
          Inc(Start, I);
          Dec(Counter, I);
          Inc(Run, I);
        end;

        // tab to draw?
        if Assigned(TabPos) then
        begin
          while (TabPos^ = WideTabulator) and (Counter > 0) do
          begin
            OldOffset := PaintOffset;
            Inc(PaintOffset, FContent[LineIndex].CharacterDistances[Start]);
            R := Rect(OldOffset, YOffset, PaintOffset, YOffset + FTextHeight);

            SetTextAlign(Handle, TA_CENTER);
            ExtTextOutW(Handle, (PaintOffset + OldOffset) div 2, YOffset, ETO_OPAQUE, @R, DefaultTabGlyph, 1, nil);
            SetTextAlign(Handle, TA_LEFT);
            Inc(Start);
            Dec(Counter);
            Inc(TabPos);
          end;
          Run := TabPos;
        end;
      end;
  end;

  //----------------------------------------------------------------------------

  procedure PickColors(Foreground, Background: TColor; ForSelection: Boolean);

  // Selects the given colors into the target canvas. If CustomStyle is Assigned
  // then use this style instead of the given colors.
  // If ForSelection is true then fore- and background colors of the custom style
  // (if assigned) are reversed. If no custom style is given then the the passed in
  // colors are directly used. In this case the default style has no influence.
  // If there is a custom style assigned, which has its font style forcing flag set
  // then also font styles from this custom style are applied.

  var
    Style: IUCELineStyle;

  begin
    if ForSelection then
    begin
      if Assigned(CustomStyle) then
      begin
        if CustomStyle.Foreground <> clDefault then
          Background := CustomStyle.Foreground;
        if CustomStyle.Background <> clDefault then
          Foreground := CustomStyle.Background;
      end;
    end
    else
    begin
      Style := FDefaultStyle;
      if Assigned(CustomStyle) then
        Style := CustomStyle;

      if Assigned(Style) then
      begin
        if Style.Foreground <> clDefault then
          Foreground := Style.Foreground;
        if Style.Background <> clDefault then
          Background := Style.Background;
      end;
    end;

    with TextCanvas do
    begin
      if Foreground = clDefault then
        Font.Color := Self.Font.Color
      else
        Font.Color := Foreground;
      if Background = clDefault then
        Brush.Color := Self.Color
      else
        Brush.Color := Background;

      if Assigned(Style) and Style.ForceFontStyles then
        Font.Style := Style.FontStyles;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure PaintLineBreak;

  begin
    R := Rect(PaintOffset, YOffset, PaintOffset + FCharWidth, YOffset + FTextHeight);
    if IntersectRect(Dummy, R, UpdateRect) then
      ExtTextOutW(TextCanvas.Handle, PaintOffset, YOffset, ETO_OPAQUE, @R, DefaultLineBreakGlyph, 1, nil);
    Inc(PaintOffset, FCharWidth);
  end;

  //--------------- end local functions ----------------------------------------

begin
  with TextCanvas do
  begin
    Font := Self.Font;
    UpdateRect := ClipRect;
    // optimize drawing by using the update area
    StartLine := (UpdateRect.Top div FTextHeight) - FOffsetY;
    EndLine := (UpdateRect.Bottom div FTextHeight) - FOffsetY + 1;
    if EndLine > FContent.Count - 1 then
      EndLine := FContent.Count - 1;

    // The vertical position we will paint at. Start at top and increment by
    // FTextHeight for each iteration of the loop (see bottom of for loop).
    YOffset := (UpdateRect.Top div FTextHeight) * FTextHeight;

    // Determine in advance whether to show selection or not.
    // Note: We have four concurrent options determining when to show selection (apart
    //       from SelectionAvailable). These are: focus state, eoReadOnly, eoReadOnlySelection and
    //       eoHideSelection. Given these states one can derive a logic table with 16 results
    //       and from this table get a logical equation. Optimizing this eq. leads to
    //       the four conditions written and described below. If we name the mentioned
    //       states (in the same order as above) with A, B, C and D, respectively, then
    //       the resulting eq. is: Sel = A¬B or AC or ¬B¬D or C¬D.
    // show selection if...
    ShowSelection := // selection is available and
      SelectionAvailable and (
         // edit is focused and not read only (then both selection opts have no influence) or
        (Focused and not (eoReadOnly in FOptions)) or
         // edit is focused and selection in r/o mode is allowed (then read only and
         // hide selection when unfocused don't matter) or
        (Focused and (eoReadOnlySelection in FOptions)) or
         // edit is not read only and selection will not be hidden when unfocused (then
         // being focused doesn't matter and r/o selection has no influence) or
        ((FOptions * [eoReadOnly, eoHideSelection]) = []) or
         // r/o selection is enabled and selection will not be hidden when unfocused
         // (then being focused or in r/o mode don't matter)
        ((eoReadOnlySelection in FOptions) and not (eoHideSelection in FOptions))
      );

    // Loop from the top line in the window to the last line in the window.
    for LineIndex := StartLine to EndLine do
    begin
      PaintOffset := FOffsetX + FGutterRect.Right + 1;
      LineBase := PWideChar(FContent[LineIndex].Text);
      TokenLen := Length(FContent[LineIndex].Text);

      // Get a potential custom style.
      CustomStyle := FContent[LineIndex].PeekStyle;

      if TokenLen > 0 then
      begin
        // If no selection is to draw then go straight ahead.
        if (LineIndex < BlockBegin.Y) or (LineIndex > BlockEnd.Y) or not ShowSelection then
        begin
          Font.Style := Self.Font.Style;
          PickColors(Self.Font.Color, Self.Color, False);

          if eoShowControlChars in FOptions then
            PaintTokenWithControlChars(0, TokenLen)
          else
            PaintToken(0, TokenLen);

          if eoShowControlChars in FOptions then
            PaintLineBreak;
        end
        else
        begin
          // Selection parts are to be considered, so determine start and end position of
          // the selection on this particular line.
          if BlockBegin.Y < LineIndex then
            SelStart := 0
          else
            SelStart := FContent[LineIndex].ColumnToCharIndex(BlockBegin.X);
          if BlockEnd.Y > LineIndex then
            SelEnd := MaxInt
          else
            SelEnd := FContent[LineIndex].ColumnToCharIndex(BlockEnd.X);

          // Start with the beginning of the token.
          StartIndex := 0;
          StopIndex := TokenLen;

          // Paint up to selection start. That length will be the StopIndex
          // or the start of the selection mark, whichever is less.
          TempIndex := Min(TokenLen, SelStart);
          if TempIndex > 0 then
          begin
            // Set needed colors and styles.
            PickColors(Self.Font.Color, Self.Color, False);

            if eoShowControlChars in FOptions then
              PaintTokenWithControlChars(0, TempIndex)
            else
              PaintToken(0, TempIndex);
            // update the start index into the line text to skip past the
            // text we just painted.
            Inc(StartIndex, TempIndex);
          end;

          // Paint the selection text. That length will be the StopIndex or the
          // end of the selection mark, whichever is less.
          TempIndex := Min(StopIndex, SelEnd) - StartIndex;
          if TempIndex > 0 then // Have anything to paint?
          begin
            // set the selection highlight colors
            with FSelectedColor do
            begin
              // Set needed colors and styles.
              PickColors(Foreground, Background, True);
            end;
            // paint the selection text
            if eoShowControlChars in FOptions then
              PaintTokenWithControlChars(StartIndex, TempIndex)
            else
              PaintToken(StartIndex, TempIndex);
            // Update the start index into the line text to skip past the
            // text we just painted.
            Inc(StartIndex, TempIndex);
          end;

          // Paint the post-selection text, the length is whatever is left over.
          Font.Style := Self.Font.Style;
          TempIndex := StopIndex - StartIndex;
          if TempIndex > 0 then
          begin
            // Set needed colors and styles.
            PickColors(Self.Font.Color, Self.Color, False);
            if eoShowControlChars in FOptions then
              PaintTokenWithControlChars(StartIndex, TempIndex)
            else
              PaintToken(StartIndex, TempIndex);
          end;

          // Prepare drawing of the rest of the line.
          if ShowSelection and (LineIndex < BlockEnd.Y) then
            PickColors(FSelectedColor.Foreground, FSelectedColor.Background, True)
          else
            PickColors(Self.Font.Color, Self.Color, False);
          if eoShowControlChars in FOptions then
          begin
            if ShowSelection and (StopIndex < SelEnd) then
              PickColors(FSelectedColor.Foreground, FSelectedColor.Background, True)
            else
              PickColors(Self.Font.Color, Self.Color, False);
            PaintLineBreak;
          end;
        end;
      end
      else
        PickColors(Self.Font.Color, Self.Color, False);

      // Clear background of the rest of the line.
      FillRect(Rect(PaintOffset, YOffset, UpdateRect.Right, YOffset + FTextHeight));

      // draw the right margin marker
      Pen.Color := FMarginColor;
      Pen.Width := 1;
      MoveTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, YOffset);
      LineTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, YOffset + FTextHeight);

      // This line has been painted, increment the vertical offset and loop back
      // for the next line of text.
      Inc(YOffset, FTextHeight);
    end;

    // finally erase all the space not covered by lines
    if YOffset < UpdateRect.Bottom then
    begin
      Brush.Color := Self.Color;
      FillRect(Rect(UpdateRect.Left, YOffset, UpdateRect.Right, UpdateRect.Bottom));

      // draw the right margin marker
      Pen.Color := FMarginColor;
      Pen.Width := 1;
      MoveTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, YOffset);
      LineTo(FRightMargin * FCharWidth + FOffsetX + FGutterRect.Right + 1, UpdateRect.Bottom);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.PosInSelection(Pos: TPoint): Boolean;

// Determines whether the given position (line/column reference) is in the currrent
// selection block.

begin
  Result := False;
  if SelectionAvailable then
  begin
    if FSortedBlockBegin.Y = FSortedBlockEnd.Y then
      Result := (Pos.Y = FSortedBlockBegin.Y) and (Pos.X >= FSortedBlockBegin.X) and (Pos.X <= FSortedBlockEnd.X)
    else
      Result := ((Pos.Y > FSortedBlockBegin.Y) and (Pos.Y < FSortedBlockEnd.Y)) or
        ((Pos.Y = FSortedBlockBegin.Y) and (Pos.X >= FSortedBlockBegin.X)) or
        ((Pos.Y = FSortedBlockEnd.Y) and (Pos.X <= FSortedBlockEnd.X));

    // So far so good, let's also take into account when
    // the block end is beyond a line end.
    if Result and (Pos.Y = BlockEnd.Y) then
      Result := Pos.X < FContent[Pos.Y].GetLineEnd(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ProcessCommand(var Command: TEditorCommand; var AChar: WideChar; Data: Pointer);

begin
  if Command < ecUserFirst then
  begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end
  else
  begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.RecordState: TEditState;

// Prepare the result structure so it can be used for undo-regstration.

begin
  with Result do
  begin
    Caret := CaretXY;
    BlockBegin := FSortedBlockBegin;
    BlockEnd := FSortedBlockEnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ReplaceLeftTabs(var S: WideString);

// replaces all leading tab characters in the given WideString by a number of spaces given
// by the current tab size

var
  Count: Integer;

begin
  Count := LeftSpaces(S);
  while Count > 0 do
  begin
    if S[Count] = WideTabulator then
    begin
      Delete(S, Count, 1);
      Insert(WideStringOfChar(' ', FTabSize), S, Count);
    end;
    Dec(Count);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.RescanLine(Index: Integer);

// If there is a syntax highlighter set then this method checks the line given by index for a change of the
// lexer end state. If this change involves something that concerns more than one line (e.g. multi line comment)
// then all following lines are invalidated and must later be rescanned.

var
  NewEndState: Integer;

begin
  // If the given line isn't validated yet or there is no highlighter set then we don't need to do anything.
  if Assigned(FHighlighter) and (Index > -1) and (Index < FLastValidLine) then
  begin
    // If the given line is the last one in the editor then we simply set it invalid and let it alone.
    // It will be validated next time it is drawn.
    if Index = FContent.Count - 1 then
      FLastValidLine := Index
    else
    begin
      FHighlighter.SetLine(FContent[Index].Text);
      if Index > 0 then
        FHighLighter.SetRange(FContent[Index].LexerState);
      repeat
        FHighLighter.Next;
      until not FHighLighter.HasMoreTokens;

      NewEndState := FHighlighter.GetRange;
      if FContent.LineNoInit[Index + 1].LexerState <> NewEndState then
      begin
        // End states differ. Everything following must be rescanned.
        FContent.LineNoInit[Index + 1].LexerState := NewEndState;
        FLastValidLine := Index;
        RefreshToBottom(Index);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ResetCaret;

begin
  if (FCaretX <> 0) or (FCaretY <> 0) then
  begin
    FCaretX := 0;
    FCaretY := 0;
    EnsureCursorPosVisible;
    if FUpdateCount = 0 then
    begin
      UpdateCaret;
      DoCaretChange;
    end;
  end;

  // Reset also the selection range.
  BlockBegin := CaretXY;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ShowCaret;

begin
  if not FCaretVisible then
    FCaretVisible := Windows.ShowCaret(Handle);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.TextFromClipboard: WideString;

var
  Data: THandle;

begin
  // ANSI text is automatically converted to Unicode if no Unicode version of this text is available.
  if Clipboard.HasFormat(CF_UNICODETEXT) then
  begin
    // Delphi's TClipboard does not support Unicode (D5 and lower) hence we
    // need to handle the stuff manually.
    Data := Clipboard.GetAsHandle(CF_UNICODETEXT);
    try
      if Data <> 0 then
        Result := PWideChar(GlobalLock(Data));
    finally
      if Data <> 0 then
        GlobalUnlock(Data);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.TextToClipboard(Text: WideString);

var
  Data: THandle;
  DataPtr: Pointer;
  Size: Cardinal;

begin
  // Delphi's TClipboard does not support Unicode (D5 and lower) hence we
  // need to handle the stuff manually.
  Size := Length(Text);
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2 * Size + 2);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(PWideChar(Text)^, DataPtr^, 2 * Size + 2);
      Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    GlobalFree(Data);
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.TranslateKeyCode(Code: word; Shift: TShiftState; var Data: Pointer): TEditorCommand;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call. The client must call FreeMem on Data if it is not nil.

var
  I: Integer;

begin
  I := Keystrokes.FindKeycode(Code, Shift);
  if I >= 0 then
    Result := Keystrokes[I].Command
  else
    Result := ecNone;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.TripleClick;

// mark entire line if the user clicked three times within the double click interval

begin
  FMultiClicked := True;
  BlockBegin := Point(0, CaretY);
  BlockEnd := Point(0, CaretY + 1);
  FLastDblClick := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.Unindent(X, Y: Cardinal): Cardinal;

// Calculates the next unindent position starting with X and depending on the
// first line above Y which contains a white space char before X.
// X must be a column value!

  //--------------- local functions -------------------------------------------

  function FindNonWhiteSpaceColumn(X, Y: Cardinal; var NewX: Cardinal): Boolean;

  var
    Index: Integer;
    Line: WideString;

  begin
    NewX := X;
    Result := False;
    Line := FContent[Y].Text;
    while NewX > 0 do
    begin
      Dec(NewX);
      // turn column into its corresponding character index...
      Index := FContent[Y].ColumnToCharIndex(NewX);
      // ... and turn it back into the start column of this char
      NewX := FContent[Y].CharIndexToColumn(Index);
      // fork out if we found a non-white space char
      if (Index < Length(Line)) and not UnicodeIsWhiteSpace(Word(Line[Index + 1])) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  //---------------------------------------------------------------------------

  function FindWhiteSpaceColumn(X, Y: Cardinal; var NewX: Cardinal): Boolean;

  var
    Index: Integer;
    Line: WideString;

  begin
    NewX := X;
    Result := False;
    Line := FContent[Y].Text;
    while NewX > 0 do
    begin
      Dec(NewX);
      // turn column into its corresponding character index...
      Index := FContent[Y].ColumnToCharIndex(NewX);
      // ... and turn it back into the start column of this char
      NewX := FContent[Y].CharIndexToColumn(Index);
      // fork out if we found a white space
      if (Index < Length(Line)) and UnicodeIsWhiteSpace(Word(Line[Index + 1])) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  Line: WideString;
  I: Integer;
  Buffer: TBufferedString;
  NeedUndo: Boolean;

begin
  // cursor at line 0?
  if (X = 0) or (Y = 0) then
    Result := 0 // no previous tab position available
  else
  begin
    // Start by converting all tab chars into space chars.
    Line := FContent[Y].Text;
    Buffer := TBufferedString.Create;
    try
      NeedUndo := False;
      for I := 1 to Length(Line) do
        if Line[I] = WideTabulator then
        begin
          Buffer.Add(StringOfChar(' ', FTabSize));
          NeedUndo := True;
        end
        else
          Buffer.Add(Line[I]);
      Line := Buffer.AsString;
    finally
      Buffer.Free;
    end;

    // Add an undo action if we change the line. It should be reversable.
    // Since we replace tabs with the exact amount of spaces they represent
    // it is not necessary to change the involved text block boundaries.
    if NeedUndo then
    begin
      FUndoList.PrepareReplaceChange(Point(0, Y), Point(FContent[Y].GetLineEnd(False), Y), Point(0, Y), Line);
      FContent[Y].Text := Line;
      FUndoList.FinishPendingChange(Point(FContent[Y].GetLineEnd(False), Y));
    end;

    // Find a line above the current line, which is not empty and whose first
    // non-white character is before X.
    repeat
      // Go one line up (picturally speaking, actually we use the previous line).
      Dec(Y);

      // Find last non-white space before current column.
      if FindNonWhiteSpaceColumn(X, Y, Result) then
      begin
        // We found a non-white space. Now search the first white
        // space column before this column.
        if FindWhiteSpaceColumn(Result, Y, Result) then
        begin
          // If there's such a column then advance to next column (which is then
          // a non-white space column).
          Result := FContent[Y].NextCharPos(Result);
        end;
        // Leave loop, since we either found what we were looking for or
        // the line in question has no white chars at the beginning (which means
        // unindentation target column is 0).
        Break;
      end;
      // Turn around if there was no unindent column by going up one line.
    until Y = 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.UpdateCaret;

var
  CX, CY: Integer;

begin
  CX := CaretXPix + FCaretOffset.X;
  CY := CaretYPix + FCaretOffset.Y;
  SetCaretPos(CX - 1, CY);
  if (CX < FGutterRect.Right + 1) or (CX > ClientWidth) or
    (CY < 0) or (CY > ClientHeight - FTextHeight) then
    HideCaret
  else
    ShowCaret;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.UpdateScrollBars;

var
  ScrollInfo: TScrollInfo;
  Page: Integer;

begin
  if (FUpdateCount = 0) and (ClientWidth > 0) then
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nTrackPos := 0;

    // Vertical scrollbar.
    if FScrollBars in [ssBoth, ssVertical] then
    begin
      if (FLinesInWindow > 0) and (FContent.Count > FLinesInWindow) then
      begin
        ScrollInfo.nPage := FLinesInWindow + 1;
        ScrollInfo.nMax := FContent.Count;
        ScrollInfo.nPos := -FOffsetY;
      end;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    end;

    // Horizontal scrollbar.
    if FScrollBars in [ssBoth, ssHorizontal] then
    begin
      ScrollInfo.nMax := WorkWidth + FGutterRect.Right + FCharWidth; // Add one empty cell to the width.
      Page := ClientWidth;
      if Page >= 0 then
        ScrollInfo.nPage := Page
      else
        ScrollInfo.nPage := 0;
      ScrollInfo.nPos := -FOffsetX;
      SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.WorkWidthToCharIndex(const LineIndex: Integer): Integer;

// Converts the current content width to a character index in the given line.

begin
  if LineIndex < FContent.Count then
    Result := FContent.LineNoInit[LineIndex].ColumnToCharIndex(WorkWidth div FCharWidth)
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.BeginUpdate;

begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.BlockIndent;

// Inserts FIndentSize space characters in front of every line in the current selection.
// Note: A designed side effect is that all leading tab characters in the concerned
//       lines will be replaced by the proper number of spaces!

var
  I: Integer;
  Line: WideString;
  BB, BE: PPoint;

begin
  if SelectionAvailable then
  begin
    FUndoList.PrepareIndentationChange(BlockBegin, BlockEnd);
    BeginUpdate;
    try
      InternalBlockIndent(BlockBegin, BlockEnd);
      BB := MinPointAsReference(FSelectionBlockBegin, FSelectionBlockEnd);
      BE := MaxPointAsReference(FSelectionBlockBegin, FSelectionBlockEnd);

      // Adjust selection properly (the Delphi IDE moves the selection start and
      // end to the first non-whitespace if there's no non-whitespace before the
      // particular X position, hence I'll do it too).
      Inc(BB.X, FIndentSize);
      Line := FContent[BB.Y].Text;
      I := LeftSpaces(Line);
      if I >= BB.X then
        BB.X := I;
      I := FContent[BB.Y].GetLineEnd(False);
      if I < BB.X then
        BB.X := I;

      if BE.X > 0 then
      begin
        Inc(BE.X, FIndentSize);
        Line := FContent[BE.Y].Text;
        I := LeftSpaces(Line);
        if I >= BE.X then
          BE.X := I;
        I := FContent[BE.Y].GetLineEnd(False);
        if I < BE.X then
          BE.X := I;
      end;

      // Finally update the sorted block fields and the caret.
      BlockEnd := FSelectionBlockEnd;
      CaretXY := BlockEnd;
    finally
      EndUpdate;
    end;
    FUndoList.FinishPendingChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.BlockUnindent;

// Removes first FIndentSize space characters of every line in the current selection
// or all leading white spaces if their count is less than FIndentSize.
// Note: A designed side effect is that all leading tab characters in the concerned
//       lines will be replaced by the proper number of spaces!

var
  I: Integer;
  Line: WideString;
  BB, BE: PPoint;

begin
  if SelectionAvailable then
  begin
    FUndoList.PrepareUnindentationChange(BlockBegin, BlockEnd);
    BeginUpdate;
    try
      InternalBlockUnindent(BlockBegin, BlockEnd);
      BB := MinPointAsReference(FSelectionBlockBegin, FSelectionBlockEnd);
      BE := MaxPointAsReference(FSelectionBlockBegin, FSelectionBlockEnd);

      // Adjust selection properly (the Delphi IDE moves the selection start and
      // end to the first non-whitespace if there's no non-whitespace before the
      // particular X position, hence I'll do it too).
      Dec(BB.X, FIndentSize);
      Line := FContent[BB.Y].Text;
      I := LeftSpaces(Line);
      if I >= BB.X then
        BB.X := I;
      I := FContent[BB.Y].GetLineEnd(False);
      if I < BB.X then
        BB.X := I;

      if BE.X > 0 then
      begin
        Dec(BE.X, FIndentSize);
        Line := FContent[BE.Y].Text;
        I := LeftSpaces(Line);
        if I >= BE.X then
          BE.X := I;
        I := FContent[BE.Y].GetLineEnd(False);
        if I < BE.X then
          BE.X := I;
      end;

      // Finally update the sorted block fields and the caret.
      BlockEnd := FSelectionBlockEnd;
      CaretXY := BlockEnd;
    finally
      EndUpdate;
    end;
    FUndoList.FinishPendingChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

(*
  TODO: Must be adjusted for the latest Unicode version.
const
  // associated languages to particular Unicode code block,
  CodeBlockToLanguage: array[TUnicodeBlock] of LCID =
  (
    LANG_NEUTRAL, // Basic Latin
    LANG_NEUTRAL, // Latin-1 Supplement
    LANG_NEUTRAL, // Latin Extended-A
    LANG_NEUTRAL, // Latin Extended-B
    LANG_NEUTRAL, // IPA Extensions
    LANG_NEUTRAL, // Spacing Modifier Letters
    LANG_NEUTRAL, // Combining Diacritical Marks
    LANG_GREEK, // Greek
    LANG_RUSSIAN, // Cyrillic
    LANG_NEUTRAL, // Armenian
    LANG_HEBREW, // Hebrew
    LANG_ARABIC, // Arabic
    LANG_NEUTRAL, // Devanagari
    LANG_NEUTRAL, // Bengali
    LANG_NEUTRAL, // Gurmukhi
    LANG_NEUTRAL, // Gujarati
    LANG_NEUTRAL, // Oriya
    LANG_NEUTRAL, // Tamil
    LANG_NEUTRAL, // Telugu
    LANG_NEUTRAL, // Kannada
    LANG_NEUTRAL, // Malayalam
    LANG_THAI, // Thai
    LANG_NEUTRAL, // Lao
    LANG_NEUTRAL, // Tibetan
    LANG_NEUTRAL, // Georgian
    LANG_NEUTRAL, // Hangul Jamo
    LANG_NEUTRAL, // Latin Extended Additional
    LANG_GREEK, // Greek Extended
    LANG_NEUTRAL, // General Punctuation
    LANG_NEUTRAL, // Superscripts and Subscripts
    LANG_NEUTRAL, // Currency Symbols
    LANG_NEUTRAL, // Combining Marks for Symbols
    LANG_NEUTRAL, // Letterlike Symbols
    LANG_NEUTRAL, // Number Forms
    LANG_NEUTRAL, // Arrows
    LANG_NEUTRAL, // Mathematical Operators
    LANG_NEUTRAL, // Miscellaneous Technical
    LANG_NEUTRAL, // Control Pictures
    LANG_NEUTRAL, // Optical Character Recognition
    LANG_NEUTRAL, // Enclosed Alphanumerics
    LANG_NEUTRAL, // Box Drawing
    LANG_NEUTRAL, // Block Elements
    LANG_NEUTRAL, // Geometric Shapes
    LANG_NEUTRAL, // Miscellaneous Symbols
    LANG_NEUTRAL, // Dingbats
    LANG_NEUTRAL, // CJK Symbols and Punctuation
    LANG_JAPANESE, // Hiragana
    LANG_JAPANESE, // Katakana
    LANG_NEUTRAL, // Bopomofo
    LANG_NEUTRAL, // Hangul Compatibility Jamo
    LANG_NEUTRAL, // Kanbun
    LANG_CHINESE, // Enclosed CJK Letters and Months
    LANG_CHINESE, // CJK Compatibility
    LANG_CHINESE, // CJK Unified Ideographs
    LANG_NEUTRAL, // Hangul Syllables
    LANG_NEUTRAL, // High Surrogates
    LANG_NEUTRAL, // High Private Use Surrogates
    LANG_NEUTRAL, // Low Surrogates
    LANG_NEUTRAL, // Private Use
    LANG_ARABIC, // CJK Compatibility Ideographs
    LANG_NEUTRAL, // Alphabetic Presentation Forms
    LANG_ARABIC, // Arabic Presentation Forms-A
    LANG_NEUTRAL, // Combining Half Marks
    LANG_CHINESE, // CJK Compatibility Forms
    LANG_NEUTRAL, // Small Form Variants
    LANG_ARABIC, // Arabic Presentation Forms-B
    LANG_NEUTRAL, // Halfwidth and Fullwidth Forms
    LANG_NEUTRAL // Specials
    );

function TCustomUniCodeEdit.ActivateKeyboard(const C: WideChar): Boolean;

// Tries to activate a keyboard layout based on the code block which contains C.
// If the layout can be activated (if needed then it is loaded) then Result is True
// otherwise False.
// Consider this function as being experimental as the character to language conversion
// does not work very well. Need to investigate this further...

var
  CodeBlock: TUnicodeBlock;
  KBID: string;

begin
  CodeBlock := CodeBlockFromChar(Word(C));
  Result := ActivateKeyboardLayout(CodeBlockToLanguage[CodeBlock], 0) <> 0;
  if not Result then
  begin
    KBID := Format('%x', [CodeBlockToLanguage[CodeBlock]]);
    Result := LoadKeyboardLayout(PChar(KBID), KLF_SUBSTITUTE_OK) <> 0;
  end;
end;
*)

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.CaretOffset(Position: TPoint; Offset: Integer);

// Places the caret at the given location and moves it then by the number of characters given in Offset (back or forth,
// depending on the sign of Offset). Advance is computed as if the entire content would be one long string.

var
  LineLength: Integer;
  
begin
  if Position.Y < 0 then
    Position.Y := 0;
  if Position.Y >= FContent.Count then
    Position.Y := FContent.Count - 1;

  if Offset <> 0 then
  begin
    if Offset < 0 then
    begin
      Offset := -Offset;
      repeat
        if Position.X < Offset then
        begin
          Dec(Offset, Position.X + 2); // +2 for the line break
          Position.X := Length(FContent[Position.Y].Text);
          Dec(Position.Y);
        end
        else
        begin
          Dec(Position.X, Offset);
          Break;
        end;
      until Position.Y < 0;
    end
    else
    begin
      repeat
        LineLength := Length(FContent[Position.Y].Text);
        if Position.X + Offset > LineLength then
        begin
          Dec(Offset, LineLength - Position.X + 2); // +2 for the line break
          Position.X := 0;
          Inc(Position.Y);
        end
        else
        begin
          Inc(Position.X, Offset);
          Break;
        end;
      until Position.Y >= FContent.Count;
    end;
  end;

  CaretXY := Position;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.CharPositionToRowColumn(Position: Cardinal): TPoint;

// Calculates the total position of the column/row reference point as would all lines be
// one continous WideString.

var
  I: Integer;
  Len: Cardinal;

begin
  Result.Y := 0;
  for I := 0 to FContent.Count - 1 do
  begin
    Len := Length(FContent[I].Text) + 2; // add 2 for WideCRLF
    if Position < Len then
      Break;
    Inc(Result.Y);
    Dec(Position, Len);
  end;
  Result.X := FContent[Result.Y].ColumnToCharIndex(Position);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ClearAll(KeepUndoList: Boolean);

// Deletes the entire text and (in case KeepUndoList is not True) clears also the undo list.

var
  I: Integer;

begin
  FWorkWidth := 0;
  
  for I := 0 to 9 do
    RemoveBookmark(I);

  if FContent.Count > 0 then
  begin
    BeginUpdate;
    try
      FContent.Clear;
      BlockBegin := Point(0, 0);
      CaretXY := Point(0, 0);
    finally
      EndUpdate;
    end;
  end;
  if not KeepUndoList then
    FUndoList.ClearList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ClearSelection;

// Removes the current selection mark if there is any.

begin
  BlockBegin := Point(0, 0);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.ClearUndo;

begin
  FUndoList.ClearList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.CommandProcessor(Command: TEditorCommand; AChar: WideChar; Data: Pointer);

// Executes the give editor command.

var
  CX, LineEnd: Integer;
  Helper: WideString;
  Action: Boolean;
  WP: TPoint;
  Caret: TPoint;
  LastCaretPos: TPoint;

  Start: TPoint;
  Stop: TPoint;
  
begin
  ProcessCommand(Command, AChar, Data);
  if (Command = ecNone) or (Command >= ecUserFirst) then
    Exit;

  LastCaretPos := CaretXY;
  case Command of
    ecLeft,
    ecSelLeft:
      begin
        FUndoList.PrepareStateChange;
        if FCaretY < FContent.Count then
          CX := FContent[FCaretY].PreviousCharPos(FCaretX)
        else
          CX := FCaretX - 1;

        if not (eoScrollPastEOL in FOptions) and (FCaretX = 0) then
        begin
          if FCaretY > 0 then
            CaretXY := Point(FContent[FCaretY - 1].GetLineEnd(False), FCaretY - 1);
        end
        else
          CaretX := CX;

        if Command = ecSelLeft then
        begin
          if not SelectionAvailable then
            BlockBegin:= LastCaretPos;
          BlockEnd := CaretXY;
        end
        else
          BlockBegin := CaretXY;

        FUndoList.FinishPendingChange;
      end;

    ecRight,
    ecSelRight:
      begin
        FUndoList.PrepareStateChange;
        if FCaretY < FContent.Count then
          CX := FContent[FCaretY].NextCharPos(FCaretX)
        else
          CX := FCaretX + 1;
        if not (eoScrollPastEOL in FOptions) and (FCaretY < FContent.Count) and (CX > FContent[FCaretY].GetLineEnd(False)) then
        begin
          if FCaretY < FContent.Count - 1 then
            CaretXY := Point(0, FCaretY + 1);
        end
        else
          CaretX := CX;

        if Command = ecSelRight then
        begin
          if not SelectionAvailable then
            BlockBegin:= LastCaretPos;
          BlockEnd := CaretXY;
        end
        else
          BlockBegin := CaretXY;

        FUndoList.FinishPendingChange;
      end;

    ecUp,
    ecSelUp:
      if FCaretY > 0 then
      begin
        FUndoList.PrepareStateChange;
        if Command = ecSelUp then
        begin
          if not SelectionAvailable then
            BlockBegin := CaretXY;
          CaretY := FCaretY - 1;
          BlockEnd := CaretXY;
        end
        else
        begin
          CaretY := FCaretY - 1;
          BlockBegin := CaretXY;
        end;

        FUndoList.FinishPendingChange;
      end
      else
        if (Command <> ecSelUp) and SelectionAvailable then
          ClearSelection;

    ecDown,
    ecSelDown:
      if FCaretY < FContent.Count - 1 then
      begin
        FUndoList.PrepareStateChange;
        if Command = ecSelDown then
        begin
          if not SelectionAvailable then
            BlockBegin := CaretXY;
          CaretY := FCaretY + 1;
          BlockEnd := CaretXY;
        end
        else
        begin
          CaretY := FCaretY + 1;
          BlockBegin := CaretXY;
        end;

        FUndoList.FinishPendingChange;
      end
      else
        if (Command <> ecSelDown) and SelectionAvailable then
          ClearSelection;

    ecWordLeft,
    ecSelWordLeft:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelWordLeft) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretXY := LastWordPos;
        if Command = ecSelWordLeft then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecWordRight,
    ecSelWordRight:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelWordRight) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretXY := NextWordPos;
        if Command = ecSelWordRight then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecLineStart,
    ecSelLineStart:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelLineStart) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretX := 0;
        if Command = ecSelLineStart then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecLineEnd,
    ecSelLineEnd:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelLineEnd) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        if eoKeepTrailingBlanks in FOptions then
          Helper := LineText
        else
        begin
          Helper := WideTrimRight(LineText);
          if Helper <> LineText then
          begin
            FUndoList.PrepareDeleteChange(
              Point(FContent[FCaretY].CharIndexToColumn(Length(Helper)), FCaretY),
              Point(FContent[FCaretY].GetLineEnd(False), FCaretY));
            LineText := Helper;
          end;
        end;
        CaretX := FContent[FCaretY].GetLineEnd(False);
        if Command = ecSelLineEnd then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecPageUp,
    ecSelPageUp:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelPageUp) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        Caret := Point(FCaretX, FCaretY - FLinesInWindow);
        if Command = ecSelPageUp then
          BlockEnd := Caret
        else
          BlockBegin := Caret;
        TopLine := TopLine - LinesInWindow;
        CaretXY := Caret;
        FUndoList.FinishPendingChange;
      end;

    ecPageDown,
    ecSelPageDown:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelPageDown) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        Caret := Point(FCaretX, FCaretY + FLinesInWindow);
        if (Command = ecSelPageDown) then
          BlockEnd := Caret
        else
          BlockBegin := Caret;
        TopLine := TopLine + LinesInWindow;
        CaretXY := Caret;
        FUndoList.FinishPendingChange;
      end;

    ecPageLeft,
    ecSelPageLeft:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelPageLeft) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        OffsetX := FOffsetX + FCharsInWindow * FCharWidth;
        CaretX := FCaretX - FCharsInWindow;
        if (Command = ecSelPageLeft) then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecPageRight,
    ecSelPageRight:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelPageRight) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        OffsetX := FOffsetX - FCharsInWindow * FCharWidth;
        CaretX := FCaretX + FCharsInWindow;
        if (Command = ecSelPageRight) then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecPageTop,
    ecSelPageTop:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelPageTop) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretY := TopLine - 1;
        if Command = ecSelPageTop then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecPageBottom,
    ecSelPageBottom:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelPageBottom) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretY := TopLine + LinesInWindow - 2;
        if (Command = ecSelPageBottom) then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecEditorTop,
    ecSelEditorTop:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelEditorTop) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretXY := Point(0, 0);
        if Command = ecSelEditorTop then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecEditorBottom,
    ecSelEditorBottom:
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelEditorBottom) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretXY := FContent.EndPoint;
        OffsetY := FLinesInWindow - FContent.Count;
        if Command = ecSelEditorBottom then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecGotoXY,
    ecSelGotoXY:
      if Data <> nil then
      begin
        FUndoList.PrepareStateChange;
        if (Command = ecSelGotoXY) and (not SelectionAvailable) then
          BlockBegin := CaretXY;
        CaretXY := PPoint(Data)^;
        if Command = ecSelGotoXY then
          BlockEnd := CaretXY
        else
          BlockBegin := CaretXY;
        FUndoList.FinishPendingChange;
      end;

    ecSelectAll:
      SelectAll;

    ecDeleteLastChar:
      DeleteLastChar;

    ecDeleteChar:
      if not (eoReadOnly in FOptions) and (FCaretY < FContent.Count) then
      begin
        if SelectionAvailable then
        begin
          FUndoList.PrepareDeleteChange(BlockBegin, BlockEnd);
          SetSelText('');
          BlockBegin := CaretXY;
          FUndoList.FinishPendingChange;
        end
        else
        begin
          CX := FContent[FCaretY].NextCharPos(FCaretX);
          LineEnd := FContent[FCaretY].GetLineEnd(False);
          if CX > LineEnd then
          begin
            // Beyond line end means we insert spaces up the caret and delete the line break.
            // But if the caret is on the last line (or beyond that) do nothing.
            if FCaretY < FContent.Count - 1 then
            begin
              Helper := WideStringOfChar(' ', CX - LineEnd - 1);
              WP := Point(LineEnd, FCaretY);
              FUndoList.PrepareReplaceChange(WP, Point(0, FCaretY + 1), WP, Helper);
              BlockBegin := WP;
              BlockEnd := Point(0, FCaretY + 1);
              RefreshToBottom(FCaretY);
            end;
          end
          else
          begin
            Helper := '';
            FUndoList.PrepareDeleteChange(CaretXY, Point(FCaretX + 1, FCaretY));
            BlockBegin := CaretXY;
            BlockEnd := Point(CX, FCaretY);
          end;
          SetSelText(Helper);
          BlockBegin := LastCaretPos;
          CaretXY := LastCaretPos;
          FUndoList.FinishPendingChange(CaretXY);
        end;
      end;

    ecDeleteWord:
      if not (eoReadOnly in FOptions) then
      begin
        WP := NextWordPos;
        if (FCaretX <> WP.X) or (FCaretY <> WP.Y) then
        begin
          BlockBegin := CaretXY;
          BlockEnd := WP;
          FUndoList.PrepareDeleteChange(BlockBegin, BlockEnd);
          SetSelText('');
          FUndoList.FinishPendingChange;
        end;
      end;

    ecDeleteLastWord:
      if not (eoReadOnly in FOptions) then
      begin
        WP := LastWordPos;
        if (FCaretX <> WP.X) or (CaretY <> WP.Y) then
        begin
          BlockBegin := CaretXY;
          BlockEnd := WP;
          FUndoList.PrepareDeleteChange(BlockBegin, BlockEnd);
          SetSelText('');
          FUndoList.FinishPendingChange;
        end;
      end;

    ecDeleteBOL:
      if not (eoReadOnly in FOptions) then
      begin
        FUndoList.PrepareDeleteChange(BlockBegin, BlockEnd);
        BlockBegin := Point(0, FCaretY);
        BlockEnd := CaretXY;
        SetSelText('');
        CaretXY := Point(0, FCaretY);
        FUndoList.FinishPendingChange;
      end;

    ecDeleteEOL:
      if not (eoReadOnly in FOptions) then
      begin
        BlockBegin := CaretXY;
        BlockEnd := Point(Length(LineText), FCaretY);
        FUndoList.PrepareDeleteChange(BlockBegin, BlockEnd);
        SetSelText('');
        FUndoList.FinishPendingChange;
      end;

    ecDeleteLine:
      if not (eoReadOnly in FOptions) and (FCaretY < FContent.Count) then
      begin
        Start := Point(0, FCaretY);
        Stop := Point(0, FCaretY + 1);
        if Stop.Y = FContent.Count then
        begin
          // Last line is about to be deleted.
          Dec(Stop.Y);
          Stop.X := FContent[FCaretY].CharIndexToColumn(Length(FContent[FCaretY].Text));
          if Start.Y > 0 then
          begin
            Dec(Start.Y);
            Start.X := FContent[FCaretY].CharIndexToColumn(Length(FContent[Start.Y].Text));
          end;
        end;
        FUndoList.PrepareDeleteChange(Start, Stop);
        FContent.DeleteLine(FCaretY);
        CaretXY := Point(0, FCaretY);
        BlockBegin := CaretXY;
        RefreshToBottom(FCaretY);
        FUndoList.FinishPendingChange;
      end;

    ecClearAll:
      if not (eoReadOnly in FOptions) then
        ClearAll(True);

    ecInsertLine:
      InsertLineBreak(False);
    ecLineBreak:
      InsertLineBreak(True);

    ecChar:
      InsertCharacter(AChar);

    ecUndo:
      Undo;

    ecRedo:
      Redo;

    ecGotoMarker0..ecGotoMarker9:
      if BookmarkOptions.EnableKeys then
        GotoBookMark(Command - ecGotoMarker0);

    ecToggleMarker0..ecToggleMarker9:
      begin
        if BookmarkOptions.EnableKeys then
        begin
          CX := Command - ecToggleMarker0;
          Action := FBookMarks[CX].Y <> CaretY;
          RemoveBookmark(CX);
          if Action then
            SetBookMark(CX, FCaretX, CaretY);
        end;
      end;

    ecCut:
      if not (eoReadOnly in FOptions) and SelectionAvailable then
        CutToClipboard;

    ecCopy:
      CopyToClipboard;

    ecPaste:
      if not (eoReadOnly in FOptions) then
        PasteFromClipboard;

    ecScrollUp:
      begin
        FUndoList.PrepareStateChange;
        TopLine := TopLine - 1;
        if CaretY > TopLine + FLinesInWindow - 2 then
          CaretY := TopLine + LinesInWindow - 2;
        FUndoList.FinishPendingChange;
      end;

    ecScrollDown:
      begin
        FUndoList.PrepareStateChange;
        TopLine := TopLine + 1;
        if CaretY < TopLine - 1 then
          CaretY := TopLine - 1;
        FUndoList.FinishPendingChange;
      end;

    ecScrollLeft:
      begin
        FUndoList.PrepareStateChange;
        Dec(FOffsetX, FCharWidth);
        if FCaretX > -FOffsetX * FCharWidth + FCharsInWindow then
          CaretX := -FOffsetX * FCharWidth + FCharsInWindow;
        FUndoList.FinishPendingChange;
      end;

    ecScrollRight:
      begin
        FUndoList.PrepareStateChange;
        Inc(FOffsetX, FCharWidth);
        if FCaretX < -FOffsetX * FCharWidth then
          CaretX := -FOffsetX * FCharWidth;
        FUndoList.FinishPendingChange;
      end;

    ecInsertMode:
      begin
        Include(FOptions, eoInserting);
        InitializeCaret;
        DoSettingChanged;
      end;

    ecOverwriteMode:
      begin
        Exclude(FOptions, eoInserting);
        InitializeCaret;
        DoSettingChanged;
      end;

    ecToggleMode:
      begin
        if eoInserting in FOptions then
          Exclude(FOptions, eoInserting)
        else
          Include(FOptions, eoInserting);
        InitializeCaret;
        DoSettingChanged;
      end;

    ecBlockIndent:
      if not (eoReadOnly in FOptions) then
        BlockIndent;

    ecBlockUnindent:
      if not (eoReadOnly in FOptions) then
        BlockUnindent;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.CopyToClipboard;

begin
  // Delphi's TClipboard does not support Unicode (D5 and lower) hence we
  // need to handle the stuff "manually"
  if SelectionAvailable then
    TextToClipboard(SelectedText);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.CutToClipboard;

begin
  if SelectionAvailable then
  begin
    FUndoList.PrepareDeleteChange(BlockBegin, BlockEnd);
    CopyToClipboard;
    SetSelText('');
    FUndoList.FinishPendingChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.DragDrop(Source: TObject; X, Y: Integer);

var
  DoMove,
  DoReplace: Boolean;
  Text: WideString;

begin
  if not (eoReadOnly in FOptions) then
  begin
    inherited;

    // Determine move or copy operation.
    DoMove := not CopyOnDrop(Source);
    if Source = Self then
    begin
      // Ignore request if target position is in the selection.
      if not PosInSelection(CaretXY) then
      begin
        // Keep text to move or copy.
        Text := SelectedText;
        if DoMove then
        begin
          FUndoList.PrepareDragMoveChange(BlockBegin, BlockEnd, CaretXY);
          SetSelText('');
        end
        else
          FUndoList.PrepareDragCopyChange(BlockBegin, BlockEnd, CaretXY);

        // Insert the text at new position.
        BlockBegin := CaretXY;
        SetSelText(Text);

        // Mark it again as selected.
        BlockEnd := Point(CaretX + BlockEnd.X - BlockBegin.X,
          FCaretY + BlockEnd.Y - BlockBegin.Y);

        FUndoList.FinishPendingChange(BlockEnd);
      end;
    end
    else
      if Source is TCustomUniCodeEdit then
      begin
        // Drag'n drop operation between different edits.
        Text := TCustomUniCodeEdit(Source).GetSelectedText;
        DoReplace := PosInSelection(PositionFromPoint(X, Y));

        // Replace selected text here with selected text from other edit if the drag cursor
        // points to selected text, otherwise insert the new text.
        if DoReplace then
          FUndoList.PrepareReplaceChange(BlockBegin, BlockEnd, BlockBegin, Text)
        else
        begin
          BlockBegin := CaretXY; // Make sure nothing is selected anymore.
          FUndoList.PrepareInsertChange(CaretXY, Text);
        end;
        SetSelText(Text);

        // If the user wants to move text then additionally delete the selected text in the other edit.
        if DoMove then
          TCustomUniCodeEdit(Source).SelectedText := '';

        FUndoList.FinishPendingChange(BlockEnd);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.EndUpdate;

begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    SetUpdateState(False);
    UpdateScrollBars;
    UpdateCaret;
    DoCaretChange;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.EnsureCursorPosVisible;

begin
  // make sure X is visible
  if FCaretX < (-FOffsetX div FCharWidth) then
    OffsetX := -FCaretX * FCharWidth
  else
    if FCaretX >= (FCharsInWindow - FOffsetX div FCharWidth) then
      OffsetX := -(FCaretX - FCharsInWindow + 1) * FCharWidth;

  // make sure Y is visible
  if FCaretY < (TopLine - 1) then
    TopLine := FCaretY + 1
  else
    if FCaretY > (TopLine + (LinesInWindow - 2)) then
      TopLine := FCaretY - (LinesInWindow - 2);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;

begin
  Result := False;
  if Bookmark in [0..9] then
  begin
    X := FBookmarks[Bookmark].X;
    Y := FBookmarks[Bookmark].Y;
    Result := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.GetWordAndBounds(Pos: TPoint; var BB, BE: TPoint): WideString;

// Returns the word and its boundaries at position Pos. All positions are given
// as column/row values.
// Note: Compare Pos with BB and BE to determine whether the word is really at the
//       given position or has been automatically choosen because there was no word
//       at this position.

var
  CX: Integer;
  Line: WideString;
  IdChars: TIdentChars;
  Len: Integer;

begin
  // make sure the given position is valid
  if Pos.Y < 0 then
    Pos.Y := 0;
  if Pos.Y >= FContent.Count then
    Pos.Y := FContent.Count - 1;
  if Pos.X < 0 then
    Pos.X := 0;

  // determine WideString to be searched through
  if Pos.Y > -1 then
    Line := FContent[Pos.Y].Text
  else
    Line := '';
  Len := Length(Line);
  BB.Y := Pos.Y;
  BE.Y := Pos.Y;

  if Len > 0 then
  begin
    // setup initial states
    CX := FContent[Pos.Y].ColumnToCharIndex(Pos.X);
    if CX > Len - 1 then
      CX := Len - 1;
    if FHighLighter <> nil then
      IdChars := FHighLighter.IdentChars
    else
      IDchars := [#33..#255];

    // four cases are to be considered:
    // 1. IdChar at current position
    if IsIdentChar(Line[CX + 1], IdChars) then
    begin
      // find start of word
      BB.X := CX;
      while (BB.X > 0) and IsIdentChar(Line[BB.X], IdChars) do
        Dec(BB.X);
      // find end of word
      BE.X := CX + 1;
      while (BE.X < Len) and IsIdentChar(Line[BE.X + 1], IdChars) do
        Inc(BE.X);
      // copy word
      Result := Copy(Line, BB.X + 1, BE.X - BB.X);
    end
    else
    begin
      // 2. no ID char at current position, so search to the left
      BE.X := CX;
      while (BE.X > 0) and not IsIdentChar(Line[BE.X], IdChars) do
        Dec(BE.X);
      if BE.X > 0 then
      begin
        CX := BE.X;
        // find start of word
        while (CX > 0) and IsIdentChar(Line[CX], IdChars) do
          Dec(CX);
        BB.X := CX;
        Result := Copy(Line, BB.X + 1, BE.X - BB.X);
      end
      else
      begin
        // 3. no ID char found to the left, so search to the right
        BB.X := CX + 1;
        while (BB.X < Len) and not IsIdentChar(Line[BB.X + 1], IdChars) do
          Inc(BB.X);
        if BB.X < Len then
        begin
          // find end of word
          BE.X := BB.X + 1;
          while (BE.X < Len) and IsIdentChar(Line[BE.X + 1], IdChars) do
            Inc(BE.X);
          Result := Copy(Line, BB.X + 1, BE.X - BB.X);
        end
        else
        begin
          // 4. nothing found, return all we have
          BB.X := 0;
          BE.X := Len;
          Result := Line;
        end;
      end;
    end;

    // finally turn char indices into columns
    BB.X := FContent[BB.Y].CharIndexToColumn(BB.X);
    BE.X := FContent[BE.Y].CharIndexToColumn(BE.X);
  end
  else
  begin
    Result := '';
    BB.X := 0;
    BE.X := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.GotoBookMark(BookMark: Integer);

begin
  if BookMark in [0..9] then
  begin
    CaretXY := Point(FBookMarks[BookMark].X, FBookMarks[BookMark].Y);
    EnsureCursorPosVisible;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.IsBookmark(BookMark: Integer): Boolean;

var
  X, Y: Integer;

begin
  Result := GetBookMark(BookMark, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.LastWordPos: TPoint;

var
  CX, CY: Integer;
  X: Integer;
  FoundNonWhiteSpace: Boolean;
  Len: Integer;
  Temp: WideString;
  IdChars: TIdentChars;

begin
  Result := CaretXY;
  Len := Length(LineText);
  CX := FContent[FCaretY].ColumnToCharIndex(FCaretX);
  if CX > Len then
    CX := Len;
  if CX > 0 then
    CY := FCaretY
  else
  begin
    if FCaretY < 1 then
    begin
      Result := Point(0, 0);
      Exit;
    end
    else
    begin
      Result := Point(FContent[FCaretY - 1].GetLineEnd(False), FCaretY - 1);
      Exit;
    end;
  end;

  if FHighLighter <> nil then
    IdChars := FHighLighter.IdentChars
  else
    IDchars := [#33..#255];

  Temp := FContent[CY].Text;
  FoundNonWhiteSpace := IsIdentChar(Temp[CX], IdChars);
  if not FoundNonWhiteSpace then
    for X := CX downto 2 do
      if IsIdentChar(Temp[X - 1], IdChars) then
      begin
        CX := X - 1;
        FoundNonWhiteSpace := True;
        Break;
      end;

  if FoundNonWhiteSpace then
  begin
    FoundNonWhiteSpace := False;
    for X := CX downto 2 do
      if not IsIdentChar(Temp[X - 1], IdChars) then
      begin
        Result := Point(X - 1, CY);
        FoundNonWhiteSpace := True;
        Break;
      end;

    if not FoundNonWhiteSpace then
      Result := Point(0, CY);
  end
  else
  begin
    Dec(CY);
    if CY = -1 then
      Result := Point(0, 0)
    else
      Result := Point(length(FContent[CY].Text), CY);
  end;
  Result.X := FContent[Result.Y].CharIndexToColumn(Result.X);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.LineFromPos(Pos: TPoint): Integer;

// returns the index of the line at position Pos (which must be given in client coordinates)

begin
  Result := -FOffsetY + (Pos.Y div FTextHeight) + 1;
  if Result < 0 then
    Result := 0;
  if Result > FContent.Count - 1 then
    Result := FContent.Count - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.LoadFromFile(const FileName: WideString; TextFormat: TTextFormat);

var
  Stream: TFileStream;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    FContent.LoadFromStream(Stream, TextFormat);
    CaretX := 0;
    CaretY := 0;
    UpdateScrollbars;
    Invalidate;
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FHighLighter) then
  begin
    FHighLighter := nil;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.PasteFromClipboard;

var
  NewText: WideString;

begin
  // ANSI text is automatically converted to Unicode if no Unicode version of this text is available.
  if Clipboard.HasFormat(CF_UNICODETEXT) then
  begin
    NewText := TextFromClipboard;
    if SelectionAvailable then
      FUndoList.PrepareReplaceChange(BlockBegin, BlockEnd, BlockBegin, NewText)
    else
    begin
      FUndoList.PrepareInsertChange(CaretXY, NewText);
      BlockBegin := CaretXY;
    end;
    SetSelText(NewText);
    // Remove selection marker and move cursor to the end of the new text.
    BlockBegin := BlockEnd;
    CaretXY := BlockEnd;
    FUndoList.FinishPendingChange(BlockEnd);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.PositionFromPoint(X, Y: Integer): TPoint;

// calculates the column/line position from the given pixel coordinates (which must
// be in client coordinates)

begin
  // note: for the column value a real division with round is used (instead of
  // an integer div) to automatically switch to the proper column depending on
  // whether the position is before the half width of the column or after
  Result := Point(Round((X - FOffsetX - FGutterRect.Right + 1) / FCharWidth), Y div FTextHeight - FOffsetY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.Redo;

var
  Change: PChange;
  LastReasonGroup: TChangeReasonGroup;

begin
  if CanRedo then
  begin
    BeginUpdate;
    repeat
      // Pop last entry from undo stack and get its values.
      FUndoList.GetRedoChange(Change);

      with Change^ do
      begin
        LastReasonGroup := Action.Group;
        case Action.Reason of
          ecrInsert:
            begin
              // Make sure nothing is selected.
              BlockBegin := Action.TargetStart;
              SetSelText(Action.NewText);
            end;
          ecrDelete:
            begin
              // Select text to delete...
              BlockBegin := Action.SourceStart;
              BlockEnd := Action.SourceEnd;
              // ... and delete it.
              SetSelText('');
            end;
          ecrState:
            begin
              // Nothing to do.
            end;
          ecrReplace:
            begin
              BlockBegin := Action.SourceStart;
              BlockEnd := Action.SourceEnd;
              SetSelText(Action.NewText);
            end;
          ecrDragCopy,
          ecrDragMove:
            begin
              BlockBegin := Action.TargetStart;
              SetSelText(FContent.CollectTextFromBlock(Action.SourceStart, Action.SourceEnd));
              if Action.Reason = ecrDragMove then
              begin
                BlockBegin := Action.SourceStart;
                BlockEnd := Action.SourceEnd;
                SetSelText('');
              end;
            end;
          ecrIndentation:
            begin
              BlockBegin := Action.TargetStart;
              BlockEnd := Action.TargetEnd;
              BlockIndent;
            end;
          ecrUnindentation:
            begin
              BlockBegin := Action.TargetStart;
              BlockEnd := Action.TargetEnd;
              BlockUnindent;
            end;
        end;
        BlockBegin := NewState.BlockBegin;
        BlockEnd := NewState.BlockEnd;
        CaretXY := NewState.Caret;
      end;

    until not (eoGroupUndo in FOptions) or not CanRedo or (LastReasonGroup <> FUndoList.CurrentRedoReasonGroup);
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.RefreshLine(Index: Integer);

var
  R: TRect;

begin
  R := Rect(0, (Index + FOffsetY) * FTextHeight, ClientWidth, (Index + FOffsetY + 1) * FTextHeight);
  InvalidateRect(Handle, @R, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.RefreshLines(Start, Stop: Integer);

var
  R: TRect;

begin
  if Start <= Stop then
  begin
    Inc(Start, FOffsetY);
    if Start < 0 then
      Start := 0;
    Inc(Stop, FOffsetY + 1);
    if Stop > FLinesInWindow then
      Stop := FLinesInWindow;
    // Consider partially visible line.
    if (Stop = FLinesInWindow) and ((ClientHeight mod FLinesInWindow) <> 0) then
      Inc(Stop);
    R := Rect(0, Start * FTextHeight, ClientWidth, Stop * FTextHeight);
  end
  else
  begin
    Stop := Stop + FOffsetY;
    if Stop < 0 then
      Stop := 0;
    Start := Start + FOffsetY + 1;
    if Start > FLinesInWindow then
      Start := FLinesInWindow;
    // consider partially visible line
    if (ClientHeight mod FLinesInWindow) <> 0 then
      Inc(Start);
    R := Rect(0, Stop * FTextHeight, ClientWidth, Start * FTextHeight);
  end;
  InvalidateRect(Handle, @R, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.RefreshToBottom(Index: Integer);

var
  R: TRect;

begin
  R := Rect(0, (Index + FOffsetY) * FTextHeight, ClientWidth, ClientHeight);
  InvalidateRect(Handle, @R, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.RemoveBookmark(BookMark: Integer);

var
  Allowed: Boolean;

begin
  if (BookMark in [0..9]) and FBookMarks[BookMark].Visible then
    with FBookMarks[BookMark] do
    begin
      Allowed := True;
      if Assigned(FOnBookmarkChange) then
        FOnBookmarkChange(Self, Bookmark, X, Y, Allowed);
      Visible := not Allowed;
      if Allowed then
      begin
        RefreshLine(Y);
        Y := -1;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.RowColumnToCharPosition(P: TPoint): Cardinal;

// Calculates the total position of the column/row reference point as would all lines be
// one continous WideString.

var
  I: Integer;

begin
  Result := 0;
  P.X := FContent[P.Y].ColumnToCharIndex(P.X);
  for I := 0 to Min(P.Y, FContent.Count) - 1 do
    Inc(Result, Length(FContent[I].Text) + 2); // add 2 for WideCRLF
  if P.Y < FContent.Count then
    Inc(Result, P.X);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SaveToFile(const FileName: WideString; TextFormat: TTextFormat);

var
  Stream: TFileStream;

begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    FContent.SaveToStream(Stream, TextFormat, 0, True);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.SearchReplace(const SearchText, ReplaceText: WideString; Options: TSearchOptions): Integer;

// Searchs for the given text and optionally replaces it with new text.
// Result is the number of matches found.

var
  StartPoint: TPoint;
  EndPoint: TPoint; // search range
  Current: TPoint; // current search position
  DoBackward: Boolean;
  DoFromCursor: Boolean;
  DoPrompt: Boolean;
  DoReplace: Boolean;
  DoReplaceAll: Boolean;
  Changed: Boolean;
  Action: TReplaceAction;

  //--------------- local functions -------------------------------------------

  function InValidSearchRange(First, Last: Integer): Boolean;

  begin
    Result := not (((Current.Y = StartPoint.Y) and (First < StartPoint.X)) or
      ((Current.Y = EndPoint.Y) and (Last > EndPoint.X)));
  end;

  //---------------------------------------------------------------------------

  procedure DoSearchReplace(Searcher: TSearchEngine);

  var
    SearchLen,
    ReplaceLen,
    N: Integer;
    Start,
    Stop,
    ColStart,
    ColStop: Integer;
    Found,
    DoStop: Boolean;
    SearchOptions: TSearchFlags;
    Offset: Integer;

  begin
    // TODO: Make search engine accept other input.
    //Searcher := TUTBMSearch.Create(FContent);
    Searcher := TUTBMSearch.Create(nil);
    try
      // initialize the search engine
      SearchOptions := [];
      if soMatchCase in Options then
        Include(SearchOptions, sfCaseSensitive);
      if soIgnoreNonSpacing in Options then
        Include(SearchOptions, sfIgnoreNonSpacing);
      if soSpaceCompress in Options then
        Include(SearchOptions, sfSpaceCompress);
      if soWholeWord in Options then
        Include(SearchOptions, sfWholeWordOnly);
      //soWholeWord in Options;
      Searcher.FindPrepare(SearchText, SearchOptions);
      // search while the current search position is inside of the search range
      SearchLen := Length(SearchText);
      ReplaceLen := Length(ReplaceText);
      DoStop := False;

      while not DoStop and (Current.Y >= StartPoint.Y) and (Current.Y <= EndPoint.Y) do
      begin
        // need a running offset because further search results are determined with regard
        // to the unchanged string but when replacing the string changes all the time
        Offset := 0;
        Found := Searcher.FindAll(FContent[Current.Y].Text);

        if DoBackward then
          N := Pred(Searcher.Count)
        else
          N := 0;
        // Operate on all results in this line.
        while not DoStop and Found do
        begin
          if (N < 0) or (N = Searcher.Count) then
            Break;
          Searcher.GetResult(N, Start, Stop);
          Inc(Start, Offset);
          Inc(Stop, Offset);
          
          // Convert character positions to column values,
          // Need to invalidate the char width array in every run because there might be non-single characters
          // like tabulators in text or replace string.
          ColStart := FContent[Current.Y].CharIndexToColumn(Start);
          ColStop := FContent[Current.Y].CharIndexToColumn(Stop);
          if DoBackward then
            Dec(N)
          else
            Inc(N);
          // Is the search result entirely in the search range?
          if not InValidSearchRange(Start, Stop) then
            Continue;

          // Increase search count.
          Inc(Result);
          
          // Select the text, so the user can see it in the OnReplaceText event
          // handler or as the search result.
          Current.X := ColStart;
          BlockBegin := Current;
          if DoBackward then
            CaretXY := Current;
          Current.X := ColStop;
          BlockEnd := Current;
          if not DoBackward then
            CaretXY := Current;

          // If it's a search only we can leave the procedure now.
          if not (DoReplace or DoReplaceAll) then
          begin
            DoStop := True;
            Break;
          end;

          // Prompt and replace or replace all. If user chooses to replace
          // all after prompting, turn off prompting.
          if DoPrompt then
          begin
            Action := raCancel;
            Application.ProcessMessages;
            FOnReplaceText(Self, SearchText, ReplaceText, Current.Y, ColStart, ColStop, Action);
            if Action = raCancel then
            begin
              DoStop := True;
              Break;
            end;
          end
          else
            Action := raReplace;
          if Action <> raSkip then
          begin
            // user has been prompted and has requested to silently replace all
            // so turn off prompting
            if Action = raReplaceAll then
            begin
              if not DoReplaceAll then
              begin
                DoReplaceAll := True;
                BeginUpdate;
              end;
              DoPrompt := False;
            end;
            SetSelTextExternal(ReplaceText);
            Changed := True;
          end;
          // calculate position offset (this offset is character index not column based)
          if not DoBackward then
            Inc(Offset, ReplaceLen - SearchLen);
          if not DoReplaceAll then
            DoStop := True;
        end;
        // search next / previous line
        if DoBackward then
          Dec(Current.Y)
        else
          Inc(Current.Y);
      end;
    finally
      Searcher.Free;
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  Engine: TSearchEngine;
  
begin
  Result := 0;
  // can't search for or replace an empty WideString
  if (Length(SearchText) > 0) and (FContent.Count > 0) then
  begin
    Changed := False;
    // get the text range to search in, ignore search in selection if nothing is selected
    DoBackward := soBackwards in Options;
    DoPrompt := (soPrompt in Options) and Assigned(FOnReplaceText);
    DoReplace := soReplace in Options;
    DoReplaceAll := soReplaceAll in Options;
    DoFromCursor := not (soEntireScope in Options);
    if SelectionAvailable and (soSelectedOnly in Options) then
    begin
      StartPoint := BlockBegin;
      EndPoint := BlockEnd;
      // Ignore the cursor position when searching in the selection.
      if DoBackward and not (soRegularExpression in Options) then
        Current := EndPoint
      else
        Current := StartPoint;
    end
    else
    begin
      StartPoint := Point(0, 0);
      EndPoint.Y := FContent.Count - 1;
      EndPoint.X := Length(FContent[EndPoint.Y].Text) + 1;
      if DoFromCursor then
        if DoBackward then
          EndPoint := CaretXY
        else
          StartPoint := CaretXY;
      if DoBackward then
        Current := EndPoint
      else
        Current := StartPoint;
    end;
    if DoReplaceAll and not DoPrompt then
      BeginUpdate;

    if soRegularExpression in Options then
      Engine := TURESearch.Create(nil)
    else
      Engine := TUTBMSearch.Create(nil);
    try
      DoSearchReplace(Engine);
    finally
      Engine.Free;
      if DoReplaceAll and not DoPrompt then
        EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SelectAll;

begin
  if FContent.Count > 0 then
  begin
    FUndoList.PrepareStateChange;
    BlockBegin := Point(0, 0);
    BlockEnd := FContent.EndPoint;
    CaretXY := BlockEnd;
    FUndoList.FinishPendingChange;

    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);

var
  Allowed: Boolean;

begin
  if (BookMark in [0..9]) and (Y <= FContent.Count) then
  begin
    Allowed := True;
    if Assigned(FOnBookmarkChange) then
      FOnBookmarkChange(Self, Bookmark, X, Y, Allowed);
    if Allowed then
    begin
      FBookMarks[BookMark].X := X;
      FBookMarks[BookMark].Y := Y;
      FBookMarks[BookMark].Visible := True;
      RefreshLine(Y);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetCaretToEditorBottom;

begin
  CaretXY := FContent.EndPoint;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.SetDefaultKeystrokes;

begin
  FKeyStrokes.ResetDefaults;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.Undo;

var
  Change: PChange;
  LastReasonGroup: TChangeReasonGroup;

begin
  if CanUndo then
  begin
    BeginUpdate;                 
    repeat
      // Pop last entry from undo stack and get its values.
      FUndoList.GetUndoChange(Change);

      with Change^ do
      begin
        LastReasonGroup := Action.Group;
        case Action.Reason of
          ecrInsert:
            begin
              BlockBegin := Action.TargetStart;
              BlockEnd := Action.TargetEnd;
              SetSelText('');
            end;
          ecrDelete:
            begin
              BlockBegin := Action.SourceStart;
              SetSelText(Action.OldText);
            end;
          ecrState:
            begin
              // Nothing to do.
            end;
          ecrReplace:
            begin
              BlockBegin := Action.TargetStart;
              BlockEnd := Action.TargetEnd;
              SetSelText(Action.OldText);
            end;
          ecrDragCopy,
          ecrDragMove:
            begin
              BlockBegin := Action.SourceStart;
              if Action.Reason = ecrDragMove then
                SetSelText(FContent.CollectTextFromBlock(Action.TargetStart, Action.TargetEnd));
              BlockBegin := Action.TargetStart;
              BlockEnd := Action.TargetEnd;
              SetSelText('');
            end;
          ecrIndentation:
            begin
              BlockBegin := Action.TargetStart;
              BlockEnd := Action.TargetEnd;
              BlockUnindent;
            end;
          ecrUnindentation:
            begin
              BlockBegin := Action.TargetStart;
              BlockEnd := Action.TargetEnd;
              BlockIndent;
            end;
        end;
        BlockBegin := OldState.BlockBegin;
        BlockEnd := OldState.BlockEnd;
        CaretXY := OldState.Caret;
      end;
    until not (eoGroupUndo in FOptions) or not CanUndo or (LastReasonGroup <> FUndoList.CurrentUndoReasonGroup);
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeEdit.WndProc(var Msg: TMessage);

// prevent Alt-Backspace from beeping

begin
  if (Msg.Msg = WM_SYSCHAR) and
    (Msg.wParam = VK_BACK) and
    ((Msg.lParam and $20000000) <> 0) then
  begin
    Msg.Msg := 0;
  end
  else
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeEdit.WordAtPos(X, Y: Integer): WideString;

// returns the word at the given position, X and Y must be given in client coordinates,

var
  BB, BE: TPoint;

begin
  Result := GetWordAndBounds(PositionFromPoint(X, Y), BB, BE);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  Screen.Cursors[crDragMove] := LoadCursor(HInstance, 'DRAGMOVE');
  Screen.Cursors[crDragCopy] := LoadCursor(HInstance, 'DRAGCOPY');
  PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);
finalization
end.

