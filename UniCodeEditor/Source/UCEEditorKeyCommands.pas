unit UCEEditorKeyCommands;

//----------------------------------------------------------------------------------------------------------------------
//
// UniCodeEditor, a Unicode Source Code Editor for Delphi.
//
// UniCodeEditor is released under the MIT license:
// Copyright (c) 1999-2004 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
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
// Description
//   Support functions and declarations for TSyntaxEdit.
//   This unit is a slightly changed version of the same named unit in mwEdit
//
// Changes:
//   Version 2.0, 2003-08-17, Mike Zinner
//     Repackaged for D7
//   Version 1.0, 1999-03-10, Mike Lischke
//     Initial Version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, Classes, Menus, SysUtils;

const
  //--------------------------------------------------------------------------------
  // NOTE:  If you add an editor command, you must also update the
  //        EditorCommandStrs constant array in implementation section below, or the
  //        command will not show up in the IDE.
  //--------------------------------------------------------------------------------

  // "Editor Commands".  Key strokes are translated from a table into these.
  // I used constants instead of a set so that additional commands could be
  // added in descendants (you can't extend a set)
  ecNone = 0; // Nothing.  Useful for user event to handle command

  ecLeft = 1; // Move cursor left one char
  ecRight = 2; // Move cursor right one char
  ecUp = 3; // Move cursor up one line
  ecDown = 4; // Move cursor down one line
  ecWordLeft = 5; // Move cursor left one Word
  ecWordRight = 6; // Move cursor right one Word
  ecLineStart = 7; // Move cursor to beginning of line
  ecLineEnd = 8; // Move cursor to end of line
  ecPageUp = 9; // Move cursor up one page
  ecPageDown = 10; // Move cursor down one page
  ecPageLeft = 11; // Move cursor right one page
  ecPageRight = 12; // Move cursor left one page
  ecPageTop = 13; // Move cursor to top of page
  ecPageBottom = 14; // Move cursor to bottom of page
  ecEditorTop = 15; // Move cursor to absolute beginning
  ecEditorBottom = 16; // Move cursor to absolute end
  ecGotoXY = 17; // Move cursor to specific coordinates, Data = PPoint

  ecSelection = 100; // Add this to ecXXX command to get equivalent
                            // command, but with selection enabled. This is not
                            // a command itself.
  // Same as commands above, except they affect selection, too
  ecSelLeft = ecLeft + ecSelection;
  ecSelRight = ecRight + ecSelection;
  ecSelUp = ecUp + ecSelection;
  ecSelDown = ecDown + ecSelection;
  ecSelWordLeft = ecWordLeft + ecSelection;
  ecSelWordRight = ecWordRight + ecSelection;
  ecSelLineStart = ecLineStart + ecSelection;
  ecSelLineEnd = ecLineEnd + ecSelection;
  ecSelPageUp = ecPageUp + ecSelection;
  ecSelPageDown = ecPageDown + ecSelection;
  ecSelPageLeft = ecPageLeft + ecSelection;
  ecSelPageRight = ecPageRight + ecSelection;
  ecSelPageTop = ecPageTop + ecSelection;
  ecSelPageBottom = ecPageBottom + ecSelection;
  ecSelEditorTop = ecEditorTop + ecSelection;
  ecSelEditorBottom = ecEditorBottom + ecSelection;
  ecSelGotoXY = ecGotoXY + ecSelection; // Data = PPoint

  ecSelectAll = 199; // Select entire contents of editor, cursor to end

  ecDeleteLastChar = 401; // Delete last char (i.e. backspace key)
  ecDeleteChar = 402; // Delete char at cursor (i.e. delete key)
  ecDeleteWord = 403; // Delete from cursor to end of Word
  ecDeleteLastWord = 404; // Delete from cursor to start of Word
  ecDeleteBOL = 405; // Delete from cursor to beginning of line
  ecDeleteEOL = 406; // Delete from cursor to end of line
  ecDeleteLine = 407; // Delete current line
  ecClearAll = 408; // Delete everything
  ecLineBreak = 409; // Break line at current position, move caret to new line
  ecInsertLine = 410; // Break line at current position, leave caret
  ecChar = 411; // Insert a character at current position

  ecUndo = 601; // Perform undo if available
  ecRedo = 602; // Perform redo if available
  ecCut = 603; // Cut selection to clipboard
  ecCopy = 604; // Copy selection to clipboard
  ecPaste = 605; // Paste clipboard to current position
  ecScrollUp = 606; // Scroll up one line leaving cursor position unchanged.
  ecScrollDown = 607; // Scroll down one line leaving cursor position unchanged.
  ecScrollLeft = 608; // Scroll left one char leaving cursor position unchanged.
  ecScrollRight = 609; // Scroll right one char leaving cursor position unchanged.
  ecInsertMode = 610; // Set insert mode
  ecOverwriteMode = 611; // Set overwrite mode
  ecToggleMode = 612; // Toggle ins/ovr mode
  ecBlockIndent = 613; // Indent selection
  ecBlockUnindent = 614; // Unindent selection
  ecDelete = 615; // like ecCut but without replacing contents of clipboard

  ecGotoMarker0 = 701; // Goto marker
  ecGotoMarker1 = 702; // Goto marker
  ecGotoMarker2 = 703; // Goto marker
  ecGotoMarker3 = 704; // Goto marker
  ecGotoMarker4 = 705; // Goto marker
  ecGotoMarker5 = 706; // Goto marker
  ecGotoMarker6 = 707; // Goto marker
  ecGotoMarker7 = 708; // Goto marker
  ecGotoMarker8 = 709; // Goto marker
  ecGotoMarker9 = 710; // Goto marker
  ecToggleMarker0 = 751; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker1 = 752; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker2 = 753; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker3 = 754; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker4 = 755; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker5 = 756; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker6 = 757; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker7 = 758; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker8 = 759; // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker9 = 760; // Set marker, Data = PPoint - X, Y Pos

  ecUserFirst = 1001; // Start of user-defined commands

type
  EKeyException = class(Exception);

  TEditorCommand = type Word;

  TKeyStroke = class(TCollectionItem)
  private
    FKey: Word; // virtual keycode
    FShift: TShiftState;
    FCommand: TEditorCommand;
    procedure SetKey(const Value: Word);
    procedure SetShift(const Value: TShiftState);
    function GetShortCut: TShortCut;
    procedure SetShortCut(const Value: TShortCut);
    procedure SetCommand(const Value: TEditorCommand);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;

    // No duplicate checking is done if assignment made via these properties!
    property Key: Word read FKey write SetKey;
    property Shift: TShiftState read FShift write SetShift;
  published
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
    property Command: TEditorCommand read FCommand write SetCommand;
  end;

  TKeyStrokes = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TKeyStroke;
    procedure SetItem(Index: Integer; Value: TKeyStroke);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);

    function Add: TKeyStroke;
    procedure Assign(Source: TPersistent); override;
    function FindCommand(Cmd: TEditorCommand): Integer;
    function FindShortcut(SC: TShortcut): Integer;
    function FindKeycode(Code: Word; SS: TShiftState): Integer;
    procedure ResetDefaults;

    property Items[Index: Integer]: TKeyStroke read GetItem write SetItem; default;
  end;

// These are mainly for the TEditorCommand property editor, but could be useful elsewhere.
function EditorCommandToDescrString(Cmd: TEditorCommand): string;
function EditorCommandToCodeString(Cmd: TEditorCommand): string;
procedure GetEditorCommandValues(Proc: TGetStrProc);
function IdentToEditorCommand(const Ident: string; var Cmd: Longint): Boolean;
function EditorCommandToIdent(Cmd: Longint; var Ident: string): Boolean;

//----------------------------------------------------------------------------------------------------------------------

implementation

const
  EditorCommandStrs: array[0..82] of TIdentMapEntry = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGoto'),
    (Value: ecSelLeft; Name: 'ecSelectLeft'),
    (Value: ecSelRight; Name: 'ecSelectRight'),
    (Value: ecSelUp; Name: 'ecSelectUp'),
    (Value: ecSelDown; Name: 'ecSelectDown'),
    (Value: ecSelWordLeft; Name: 'ecSelectWordLeft'),
    (Value: ecSelWordRight; Name: 'ecSelectWordRight'),
    (Value: ecSelLineStart; Name: 'ecSelectLineStart'),
    (Value: ecSelLineEnd; Name: 'ecSelectLineEnd'),
    (Value: ecSelPageUp; Name: 'ecSelectPageUp'),
    (Value: ecSelPageDown; Name: 'ecSelectPageDown'),
    (Value: ecSelPageLeft; Name: 'ecSelectPageLeft'),
    (Value: ecSelPageRight; Name: 'ecSelectPageRight'),
    (Value: ecSelPageTop; Name: 'ecSelectPageTop'),
    (Value: ecSelPageBottom; Name: 'ecSelectPageBottom'),
    (Value: ecSelEditorTop; Name: 'ecSelectEditorTop'),
    (Value: ecSelEditorBottom; Name: 'ecSelectEditorBottom'),
    (Value: ecSelGotoXY; Name: 'ecSelectGoto'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBOL; Name: 'ecDeleteBOL'),
    (Value: ecDeleteEOL; Name: 'ecDeleteEOL'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClearAll; Name: 'ecClearAll'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecDelete; Name: 'ecDelete'),
    (Value: ecUserFirst; Name: 'ecUserFirst'),
    (Value: ecGotoMarker0; Name: 'ecGotoMarker0'),
    (Value: ecGotoMarker1; Name: 'ecGotoMarker1'),
    (Value: ecGotoMarker2; Name: 'ecGotoMarker2'),
    (Value: ecGotoMarker3; Name: 'ecGotoMarker3'),
    (Value: ecGotoMarker4; Name: 'ecGotoMarker4'),
    (Value: ecGotoMarker5; Name: 'ecGotoMarker5'),
    (Value: ecGotoMarker6; Name: 'ecGotoMarker6'),
    (Value: ecGotoMarker7; Name: 'ecGotoMarker7'),
    (Value: ecGotoMarker8; Name: 'ecGotoMarker8'),
    (Value: ecGotoMarker9; Name: 'ecGotoMarker9'),
    (Value: ecToggleMarker0; Name: 'ecToggleMarker0'),
    (Value: ecToggleMarker1; Name: 'ecToggleMarker1'),
    (Value: ecToggleMarker2; Name: 'ecToggleMarker2'),
    (Value: ecToggleMarker3; Name: 'ecToggleMarker3'),
    (Value: ecToggleMarker4; Name: 'ecToggleMarker4'),
    (Value: ecToggleMarker5; Name: 'ecToggleMarker5'),
    (Value: ecToggleMarker6; Name: 'ecToggleMarker6'),
    (Value: ecToggleMarker7; Name: 'ecToggleMarker7'),
    (Value: ecToggleMarker8; Name: 'ecToggleMarker8'),
    (Value: ecToggleMarker9; Name: 'ecToggleMarker9'));

//----------------------------------------------------------------------------------------------------------------------

procedure GetEditorCommandValues(Proc: TGetStrProc);

var
  I: Integer;

begin
  for I := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    Proc(EditorCommandStrs[I].Name);
end;

//----------------------------------------------------------------------------------------------------------------------

function IdentToEditorCommand(const Ident: string; var Cmd: Longint): Boolean;

begin
  Result := IdentToInt(Ident, Cmd, EditorCommandStrs);
end;

//----------------------------------------------------------------------------------------------------------------------

function EditorCommandToIdent(Cmd: Longint; var Ident: string): Boolean;

begin
  Result := IntToIdent(Cmd, Ident, EditorCommandStrs);
end;

//----------------------------------------------------------------------------------------------------------------------

function EditorCommandToDescrString(Cmd: TEditorCommand): string;

// converts the ecXXXX description into a more appealing phrase,
// The approach used here is to split the string at those positions where
// a capital letter (or number) appears followed by a small letter.

var
  Temp, Part: string;
  Head, Tail: PChar;

begin
  Result := '';
  if EditorCommandToIdent(Cmd, Temp) then
  begin
    // remove 'ec'
    Delete(Temp, 1, 2);
    Head := PChar(Temp);
    while Head^ <> #0 do
    begin
      Tail := Head;
      // skip leading capital letters or numbers if there are some
      while Tail^ in ['A'..'Z', '0'..'9'] do
        Inc(Tail);
      // walk through the string until any other than a small letter has been found
      // (this automatically checks for the strings end)
      while Tail^ in ['a'..'z'] do
        Inc(Tail);
      SetString(Part, Head, Tail - Head);
      if Result <> '' then
        Result := Result + ' ' + Part
      else
        Result := Result + Part;
      Head := Tail;
    end;
  end
  else
    Result := IntToStr(Cmd);
end;

//----------------------------------------------------------------------------------------------------------------------

function EditorCommandToCodeString(Cmd: TEditorCommand): string;

begin
  if not EditorCommandToIdent(Cmd, Result) then
    Result := IntToStr(Cmd);
end;

//----------------- TKeyStroke -----------------------------------------------------------------------------------------

procedure TKeyStroke.Assign(Source: TPersistent);

begin
  if Source is TKeyStroke then
  begin
    Key := TKeyStroke(Source).Key;
    Shift := TKeyStroke(Source).Shift;
    Command := TKeyStroke(Source).Command;
  end
  else
    inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStroke.GetDisplayName: string;

begin
  Result := EditorCommandToCodeString(Command) + ' - ' + ShortCutToText(ShortCut);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStroke.GetShortCut: TShortCut;

begin
  Result := Menus.ShortCut(Key, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TKeyStroke.SetCommand(const Value: TEditorCommand);

begin
  if Value <> FCommand then
    FCommand := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TKeyStroke.SetKey(const Value: Word);

begin
  if Value <> FKey then
    FKey := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TKeyStroke.SetShift(const Value: TShiftState);

begin
  if Value <> FShift then
    FShift := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TKeyStroke.SetShortCut(const Value: TShortCut);

var
  NewKey: Word;
  NewShift: TShiftState;
  Dup: Integer;

begin
  // Duplicate values of no shortcut are OK.
  if Value <> 0 then
  begin
    // Check for duplicate shortcut in the collection and disallow if there is.
    Dup := TKeyStrokes(Collection).FindShortcut(Value);
    if (Dup <> -1) and (Dup <> Self.Index) then
      raise EKeyException.Create('Shortcut already exists');
  end;

  Menus.ShortCutToKey(Value, NewKey, NewShift);
  if (NewKey <> Key) or (NewShift <> Shift) then
  begin
    Key := NewKey;
    Shift := NewShift;
  end;
end;

//----------------- TKeyStrokes ----------------------------------------------------------------------------------------

constructor TKeyStrokes.Create(AOwner: TPersistent);

begin
  inherited Create(TKeyStroke);
  FOwner := AOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStrokes.Add: TKeyStroke;

begin
  Result := TKeyStroke(inherited Add);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TKeyStrokes.Assign(Source: TPersistent);

var
  I: Integer;

begin
  if Source is TKeyStrokes then
  begin
    Clear;
    for I := 0 to TKeyStrokes(Source).Count - 1 do
    begin
      with Add do
        Assign(TKeyStrokes(Source)[I]);
    end;
  end
  else
    inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStrokes.FindCommand(Cmd: TEditorCommand): Integer;

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Command = Cmd then
    begin
      Result := I;
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStrokes.FindKeycode(Code: Word; SS: TShiftState): Integer;

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to Count - 1 do
    if (Items[I].Key = Code) and (Items[I].Shift = SS) then
    begin
      Result := I;
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStrokes.FindShortcut(SC: TShortcut): Integer;

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Shortcut = SC then
    begin
      Result := I;
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStrokes.GetItem(Index: Integer): TKeyStroke;

begin
  Result := TKeyStroke(inherited GetItem(Index));
end;

//----------------------------------------------------------------------------------------------------------------------

function TKeyStrokes.GetOwner: TPersistent;

begin
  Result := FOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TKeyStrokes.ResetDefaults;

  //--------------- local functions -------------------------------------------

  procedure AddKey(const ACmd: TEditorCommand; const AKey: Word; const AShift: TShiftState);

  begin
    with Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  Clear;

  AddKey(ecSelUp, VK_UP, [ssShift]);

  AddKey(ecUp, VK_UP, []);

  AddKey(ecScrollUp, VK_UP, [ssCtrl]);

  AddKey(ecDown, VK_DOWN, []);

  AddKey(ecSelDown, VK_DOWN, [ssShift]);

  AddKey(ecScrollDown, VK_DOWN, [ssCtrl]);

  AddKey(ecLeft, VK_LEFT, []);

  AddKey(ecSelLeft, VK_LEFT, [ssShift]);

  AddKey(ecWordLeft, VK_LEFT, [ssCtrl]);

  AddKey(ecSelWordLeft, VK_LEFT, [ssShift, ssCtrl]);

  AddKey(ecRight, VK_RIGHT, []);

  AddKey(ecSelRight, VK_RIGHT, [ssShift]);

  AddKey(ecWordRight, VK_RIGHT, [ssCtrl]);

  AddKey(ecSelWordRight, VK_RIGHT, [ssShift, ssCtrl]);

  AddKey(ecPageDown, VK_NEXT, []);

  AddKey(ecSelPageDown, VK_NEXT, [ssShift]);

  AddKey(ecPageBottom, VK_NEXT, [ssCtrl]);

  AddKey(ecSelPageBottom, VK_NEXT, [ssShift, ssCtrl]);

  AddKey(ecPageUp, VK_PRIOR, []);

  AddKey(ecSelPageUp, VK_PRIOR, [ssShift]);

  AddKey(ecPageTop, VK_PRIOR, [ssCtrl]);

  AddKey(ecSelPageTop, VK_PRIOR, [ssShift, ssCtrl]);

  AddKey(ecLineStart, VK_HOME, []);

  AddKey(ecSelLineStart, VK_HOME, [ssShift]);

  AddKey(ecEditorTop, VK_HOME, [ssCtrl]);

  AddKey(ecSelEditorTop, VK_HOME, [ssShift, ssCtrl]);

  AddKey(ecLineEnd, VK_END, []);

  AddKey(ecSelLineEnd, VK_END, [ssShift]);

  AddKey(ecEditorBottom, VK_END, [ssCtrl]);

  AddKey(ecSelEditorBottom, VK_END, [ssShift, ssCtrl]);

  AddKey(ecToggleMode, VK_INSERT, []);

  AddKey(ecPaste, Ord('V'), [ssCtrl]);
  AddKey(ecPaste, VK_INSERT, [ssShift]);

  AddKey(ecDeleteChar, VK_DELETE, []);

  AddKey(ecDeleteLastChar, VK_BACK, []);

  AddKey(ecDeleteLastWord, VK_BACK, [ssCtrl]);

  AddKey(ecUndo, Ord('Z'), [ssCtrl]);
  AddKey(ecUndo, VK_BACK, [ssAlt]);

  AddKey(ecRedo, Ord('Z'), [ssCtrl, ssShift]);
  AddKey(ecRedo, VK_BACK, [ssAlt, ssShift]);

  AddKey(ecLineBreak, VK_RETURN, []);
  AddKey(ecLineBreak, Ord('M'), [ssCtrl]);

  AddKey(ecSelectAll, Ord('A'), [ssCtrl]);

  AddKey(ecCopy, Ord('C'), [ssCtrl]);
  AddKey(ecCopy, VK_INSERT, [ssCtrl]);

  AddKey(ecCut, Ord('X'), [ssCtrl]);
  AddKey(ecCut, VK_DELETE, [ssShift]);

  AddKey(ecBlockIndent, Ord('I'), [ssCtrl, ssShift]);

  AddKey(ecInsertLine, Ord('N'), [ssCtrl]);

  AddKey(ecDeleteWord, Ord('T'), [ssCtrl]);
  AddKey(ecDeleteWord, VK_DELETE, [ssCtrl]);

  AddKey(ecBlockUnindent, Ord('U'), [ssCtrl, ssShift]);

  AddKey(ecDeleteLine, Ord('Y'), [ssCtrl]);

  AddKey(ecDeleteEOL, Ord('Y'), [ssCtrl, ssShift]);

  AddKey(ecGotoMarker0, Ord('0'), [ssCtrl]);

  AddKey(ecGotoMarker1, Ord('1'), [ssCtrl]);

  AddKey(ecGotoMarker2, Ord('2'), [ssCtrl]);

  AddKey(ecGotoMarker3, Ord('3'), [ssCtrl]);

  AddKey(ecGotoMarker4, Ord('4'), [ssCtrl]);

  AddKey(ecGotoMarker5, Ord('5'), [ssCtrl]);

  AddKey(ecGotoMarker6, Ord('6'), [ssCtrl]);

  AddKey(ecGotoMarker7, Ord('7'), [ssCtrl]);

  AddKey(ecGotoMarker8, Ord('8'), [ssCtrl]);

  AddKey(ecGotoMarker9, Ord('9'), [ssCtrl]);

  AddKey(ecToggleMarker0, Ord('0'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker1, Ord('1'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker2, Ord('2'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker3, Ord('3'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker4, Ord('4'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker5, Ord('5'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker6, Ord('6'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker7, Ord('7'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker8, Ord('8'), [ssCtrl, ssShift]);

  AddKey(ecToggleMarker9, Ord('9'), [ssCtrl, ssShift]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TKeyStrokes.SetItem(Index: Integer; Value: TKeyStroke);

begin
  inherited SetItem(Index, Value);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  RegisterIntegerConsts(TypeInfo(TEditorCommand), IdentToEditorCommand, EditorCommandToIdent);
end.

