unit UniCodeConsole;

// UniCodeEditor, a Unicode Source Code Editor for Delphi.
//
// UniCodeEditor is released under the MIT license:
// Copyright (c) 1999-2005 Mike Zinner, Mike Lischke (support@soft-gems.net, www.soft-gems.net).
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

interface

uses UniCodeEditor, Unicode, Classes, Contnrs, Types,
  Windows, Messages, SysUtils, Controls, Math;

type
  TExecuteCommandProcedure = procedure(Cmd: WideString; Key: Word; Shift: TShiftState) of object;
  TIsMultilineCommandFunction = function(Cmd: WideString; Key: Word; Shift: TShiftState): Boolean of object;

  TCommandHistoryObject = class
    constructor Create(Cmd: WideString);
  private
    FCmd: WideString;
  published
    property Cmd: WideString read FCmd write FCmd;
  end;

  TCustomUniCodeConsole = class(TCustomUniCodeEdit)
  private
    CommandHistory: TObjectList;
    CommandHistoryPos: integer;

    StoredCaretPos: TPoint;

    CurrentCommandStartLineNr: Integer;
    CurrentCommandEndLineNr: Integer;

    FOnExecuteCommand: TExecuteCommandProcedure;
    FOnIsMultilineCommand: TIsMultilineCommandFunction;

    FConsolePrompt: WideString;
    FConsolePromptLen: Integer;

    FConsoleDelimiter: WideString;

    FReadScriptInput: Boolean;

    FExecuting: Boolean;

    procedure SetConsolePrompt(ConsolePrompt: WideString);
    procedure SetConsoleCommandSelStart(value: integer);

    procedure SetConsoleCommand(Cmd: WideString);
    function GetConsoleCommand: WideString;

    procedure SetConsoleCommandExcludeDelimiter(Cmd: WideString);
    function GetConsoleCommandExcludeDelimiter: WideString;

    procedure SetConsoleHistory(History: AnsiString);
    function GetConsoleHistory: AnsiString;
  protected
    procedure LineChanged(Sender: TObject; Line: TUCELine);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    function GetConsolePromptMultiline: WideString;

    property ConsolePromptLen: Integer read FConsolePromptLen;
    property ConsolePrompt: WideString read FConsolePrompt write SetConsolePrompt;

    property ConsoleDelimiter: WideString read FConsoleDelimiter write FConsoleDelimiter;

    property OnExecuteCommand: TExecuteCommandProcedure read FOnExecuteCommand write FOnExecuteCommand;
    property OnIsMultilineCommand: TIsMultilineCommandFunction read FOnIsMultilineCommand write FOnIsMultilineCommand;

    property ReadScriptInput: Boolean read FReadScriptInput write FReadScriptInput;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddOutput(const Text: WideString);
    procedure PrepareNextConsoleCommand;
    procedure PrepareNextConsoleCommandMultiline;

    procedure ExecuteCommand;

    property ConsoleCommand: WideString read GetConsoleCommand write SetConsoleCommand;
    property ConsoleCommandExcludeDelim: WideString read GetConsoleCommandExcludeDelimiter write
      SetConsoleCommandExcludeDelimiter;
    property ConsoleCommandSelStart: Integer write SetConsoleCommandSelStart;

    property ConsoleHistory: AnsiString read GetConsoleHistory write SetConsoleHistory;
  end;

  TUniCodeConsole = class(TCustomUniCodeConsole)
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
    property BookMarkOptions;
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
    property MaxRightChar;
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

    property OnBookmarkChange;
    property OnCaretChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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
    property OnSettingChange;
    property OnStartDrag;

    property ConsolePromptLen;
    property ConsolePrompt;

    property ConsoleDelimiter;

    property OnExecuteCommand;
    property OnIsMultilineCommand;

    property ReadScriptInput;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  UCEEditorKeyCommands;
  
//----------------------------------------------------------------------------------------------------------------------

constructor TCommandHistoryObject.Create(Cmd: WideString);

begin
  inherited Create;

  FCmd := Cmd;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TCustomUniCodeConsole.Create(AOwner: TComponent);

begin
  inherited;

  CommandHistory := TObjectList.Create;

  FOnExecuteCommand := nil;
  FOnIsMultilineCommand := nil;

  ConsolePrompt := '> ';

  FConsoleDelimiter := ';';
  Content.OnChangeLine := LineChanged;

  FExecuting := False;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TCustomUniCodeConsole.Destroy;

begin
  CommandHistory.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.PrepareNextConsoleCommand;

begin
  Content.AddLine(ConsolePrompt);
  CaretY := Content.Count;
  CaretX := ConsolePromptLen;
  Invalidate;

  CurrentCommandStartLineNr := Content.Count - 1;
  CurrentCommandEndLineNr := CurrentCommandStartLineNr;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.LineChanged(Sender: TObject; Line: TUCELine);

begin
  if Assigned(Line) then
    InvalidateLine(Line.Index);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.ExecuteCommand;

var Key: Word;

begin
  Key := VK_Return;

  KeyDown(Key, []);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.KeyDown(var Key: Word; Shift: TShiftState);

var
  Cmd: WideString;
  CmdTrimmed: WideString;
  CopiedText: WideString;

begin
  // clear selection, but keep it for Ctrl+C
  if Not((Key = VK_Control) or (Key = VK_Shift) or
      ((Key = Ord('C')) and (Shift = [ssCtrl])) or
      ((Key = Ord('X')) and (Shift = [ssCtrl])) or
      ((Key = Ord('V')) and (Shift = [ssCtrl])) or
      ((Key = VK_Left) and (Shift = [ssShift])) or
      ((Key = VK_Right) and (Shift = [ssShift])) or
      ((Key = VK_HOME) and (Shift = [ssShift])) or
      ((Key = VK_END) and (Shift = [ssShift]))) then
    ClearSelection;

  if (Content.Count > 0) and (Length(Content[CaretY].Text) >= MaxRightChar) then
    MaxRightChar := MaxRightChar + 20;

  if (Key = VK_Return) and (Shift<>[ssShift]) then
  begin
    Cmd := GetConsoleCommand;
    CmdTrimmed := Trim(Cmd);

    if (CompareText(CmdTrimmed, 'cls') = 0) then
    begin
      Content.Clear;
      MaxRightChar := 80;

      PrepareNextConsoleCommand;
      Invalidate;
      Key := 0;
    end
    else
      if (CmdTrimmed = '') then
      begin
        PrepareNextConsoleCommand;
        Key := 0;
      end
      else
        if (CmdTrimmed <> '') then
        begin
          if (Copy(CmdTrimmed, Length(CmdTrimmed) -
            Length(FConsoleDelimiter) + 1, Length(FConsoleDelimiter)) = FConsoleDelimiter) then
          begin
            if (Assigned(OnExecuteCommand)) then
            begin
              FExecuting := True;
              try
                OnExecuteCommand(Cmd, Key, Shift)
              finally
                FExecuting := False;
              end;
            end
            else
              PrepareNextConsoleCommand;
          end
          else
          begin
            if (Assigned(OnIsMultilineCommand)) then
            begin
              if (not (OnIsMultilineCommand(Cmd, Key, Shift))) then
              begin
                if (Assigned(OnExecuteCommand)) then
                begin
                  FExecuting := True;
                  try
                    OnExecuteCommand(Cmd, Key, Shift)
                  finally
                    FExecuting := False;
                  end;
                end
                else
                  PrepareNextConsoleCommand;
              end
              else
                PrepareNextConsoleCommandMultiline;
            end
            else
              PrepareNextConsoleCommandMultiline;
          end;

          //Add command to history
          if (cmd <> '') then
          begin
            if (CommandHistory.Count = 0) or
               ((CommandHistory.Count>0) and (
                Trim(TCommandHistoryObject(
                  CommandHistory[CommandHistory.Count-1]).cmd) <>
                Trim(cmd))) then
            begin
              CommandHistory.Add(TCommandHistoryObject.Create(cmd));
              CommandHistoryPos := CommandHistory.Count;
            end;
          end;

          Key := 0;
        end;
  end
  else
    if (Key = VK_Return) and (Shift=[ssShift]) then
    begin
      ConsoleCommand := ConsoleCommand + #13#10;
    end
    else
      if (Key = VK_UP) and (Shift = []) then
      begin
        if (CaretY <= CurrentCommandStartLineNr) then
          Key := 0;
      end
      else
        if (Key = VK_DOWN) and (Shift = []) then
        begin
          if (CaretY >= CurrentCommandEndLineNr) then
            Key := 0;
        end

        else
          if ((Key = VK_PRIOR) and (Shift = [])) or
            ((Key = VK_UP) and (Shift = [ssCtrl])) then
          begin
            if (CommandHistoryPos > 0) then
            begin
              dec(CommandHistoryPos);
              ConsoleCommand := TCommandHistoryObject(CommandHistory[CommandHistoryPos]).Cmd;
            end;

            Key := 0;
          end
          else
            if ((Key = VK_NEXT) and (Shift = [])) or
              ((Key = VK_DOWN) and (Shift = [ssCtrl])) then
            begin
              if (CommandHistoryPos < CommandHistory.Count - 1) then
              begin
                inc(CommandHistoryPos);
                ConsoleCommand := TCommandHistoryObject(CommandHistory[CommandHistoryPos]).Cmd;
              end
              else
              begin
                ConsoleCommand := '';
        {Lines[CaretY]:=ConsolePrompt;
        CaretX:=ConsolePromptLen;}
                CommandHistoryPos := CommandHistory.Count;
              end;

              Key := 0;
            end
            else
              if (Key = VK_HOME) then
              begin
                CaretX := ConsolePromptLen;

                Key := 0;
              end
              else
                if (Key = VK_PRIOR) then
                begin
                  Key := 0;
                end
                else
                  if (Key = VK_NEXT) then
                  begin
                    Key := 0;
                  end
                  else
                    if (Key = VK_BACK) or (Key = VK_LEFT) then
                    begin
                      if (CaretX < Integer(ConsolePromptLen + 1)) then
                        Key := 0;
                    end
                    else
                      if (Key = VK_RIGHT) then
                      begin
                        if (CaretX >= Length(Content[CaretY].Text)) then
                          Key := 0;
                      end
                      else
                        if (Key = VK_ESCAPE) then
                        begin
                          ConsoleCommand := '';

                          Key := 0;
                        end
                        else
                          if (Key = Ord('V')) and (Shift = [ssCtrl]) then
                          begin
                            CopiedText := TextFromClipboard;

                            // if there is one linebreak,
                            // subsitute the complete console command
                            if (Pos(#10, CopiedText) > 0) then
                            begin
                              ConsoleCommand := CopiedText;
                              Key := 0;
                            end;
                          end;

  if (Key <> 0) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.AddOutput(const Text: WideString);

// Takes the given text and adds it to the console output. The text is scanned for certain control characters and the
// output is adjusted accordingly.

const
  ControlChars = [
    WideChar(#$00),  // Terminator
    WideChar(#$07),  // BELL
    WideChar(#$08),  // Back space
    WideChar(#$09),  // Horizontal tabulator
    WideChar(#$0A),  // Line feed (new line)
    WideChar(#$0D),  // Carriage return (back to start of line)
    WideChar(#$18),  // Cancel
    WideChar(#$1B)   // Escape
  ];

var
  Head, Tail: PWideChar;
  S: WideString;
  Index: Integer;
  Cmd, Prompt: WideString;

begin
  Index := Content.Count - 1;
  if Index < 0 then
  begin
    Content.AddLine('');
    Inc(Index);
  end;

  // if the console is not executing, remove the
  // current cmd and prompt to print the message
  if (Not(FExecuting)) then
  begin
    Cmd := ConsoleCommand;
    Prompt := ConsolePrompt;

    ConsoleCommand := '';
    ConsolePrompt := '';
  end;

  Head := PWideChar(Text);
  while Head^ <> #0 do
  begin
    Tail := Head;

    // Collect everything not containing any control character.
    while not (Tail^ in ControlChars) do
      Inc(Tail);
    if Head <> Tail then
    begin
      SetString(S, Head, Tail - Head);
      Content[Index].Text := Content[Index].Text + S;
    end;

    // Now handle control character.
    case Tail^ of
      WideChar(#$00):  // Terminator
        ; // Nothing to do.
      WideChar(#$07):  // BELL
        Inc(Tail); // Just swallow it.
      WideChar(#$08):  // Back space
        begin
          // Delete previous character if there is one.
          CommandProcessor(ecDeleteLastChar, Tail^, nil);
          Inc(Tail);
        end;
      WideChar(#$09):  // Horizontal tabulator
        begin
          // Add 8 space characters at current cursor position.
          InsertText('        ');
          Inc(Tail);
        end;
      WideChar(#$0A):  // Line feed (new line)
        begin
          Content.AddLine('');
          Inc(Index);
          CaretXY := Point(0, Index);
          Inc(Tail);
        end;
      WideChar(#$0D):  // Carriage return (back to start of line)
        begin
          CaretX := 0;
          Inc(Tail);
        end;
      WideChar(#$18),  // Cancel
      WideChar(#$1B):  // Escape
        begin
          // Clear line.
          Content[Index].Text := '';
          Inc(Tail);
        end;
    end;
    Head := Tail;
  end;

  // if the prompt and cmd have been removed, put them back again
  if (Not(FExecuting)) then
  begin
    ConsoleCommand := Cmd;
    ConsolePrompt := Prompt;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.PrepareNextConsoleCommandMultiline;

begin
  Content.AddLine(GetConsolePromptMultiline);
  CaretY := Content.Count;
  CaretX := ConsolePromptLen;
  Invalidate;

  Inc(CurrentCommandEndLineNr);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.SetConsoleCommand(Cmd: WideString);

var
  CmdLines: TWideStringList;
  i: integer;
begin
  if (Content.Count > 0) then
  begin
    //Clear current command
    for i := 0 to CurrentCommandEndLineNr - CurrentCommandStartLineNr do
      Content.DeleteLine(Content.Count - 1);
  end;

  CurrentCommandStartLineNr := Content.Count;

  //Add new command
  CmdLines := TWideStringList.Create;
  try
    CmdLines.Text := Cmd;

    if (CmdLines.Count > 0) then
    begin
      Content.AddLine(ConsolePrompt + CmdLines[0]);

      for i := 1 to CmdLines.Count - 1 do
        Content.AddLine(GetConsolePromptMultiline + CmdLines[i]);
    end
    else
      Content.AddLine(ConsolePrompt);
  finally
    CmdLines.Free;
  end;

  CurrentCommandEndLineNr := Content.Count - 1;

  SetCaretToEditorBottom;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeConsole.GetConsoleCommand: WideString;

var
  cmd: WideString;
  i: integer;

begin
  if (Content.Count > 0) then
  begin
    cmd := '';
    for i := CurrentCommandStartLineNr to CurrentCommandEndLineNr do
      if (CurrentCommandEndLineNr < Content.Count) then
        cmd := cmd +
          Copy(Content[i].Text, ConsolePromptLen + 1, Length(Content[i].Text)) + #13#10;

    Result := cmd;
  end
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.SetConsoleCommandExcludeDelimiter(Cmd: WideString);

begin
  Cmd := TrimRight(Cmd);

  if (Copy(Cmd, Length(Cmd) -
    Length(ConsoleDelimiter) + 1, Length(ConsoleDelimiter)) = ConsoleDelimiter) then
    ConsoleCommand := Cmd
  else
    ConsoleCommand := Cmd + ConsoleDelimiter;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeConsole.GetConsoleCommandExcludeDelimiter: WideString;

begin
  Result := TrimRight(ConsoleCommand);

  Result := Copy(Result, 1, Length(Result) -
    Length(ConsoleDelimiter));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  StoredCaretPos.X := CaretX;
  StoredCaretPos.Y := CaretY;

  if (StoredCaretPos.Y > Content.Count) then
    StoredCaretPos.Y := Content.Count;

  if (StoredCaretPos.X < Integer(ConsolePromptLen)) then
    StoredCaretPos.X := ConsolePromptLen;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (SelStart<>SelEnd) and
    (Trim(SelectedText) <> '') then
    CopyToClipboard;

  if (CaretY < CurrentCommandStartLineNr) or
    (CaretY > CurrentCommandEndLineNr) then
  begin
    CaretY := StoredCaretPos.Y;
  end;

  if (CaretX < ConsolePromptLen) then
    CaretX := StoredCaretPos.X;

  //ClearSelection;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.SetConsolePrompt(ConsolePrompt: WideString);

var
  OldConsolePrompt: WideString;
  OldConsolePromptLen: Integer;
  i: integer;

begin
  OldConsolePrompt := FConsolePrompt;
  OldConsolePromptLen := ConsolePromptLen;

  FConsolePrompt := ConsolePrompt;
  FConsolePromptLen := Length(FConsolePrompt);

  if (Content.Count > 0) and (Not(FReadScriptInput)) then
    if (CurrentCommandStartLineNr < Content.Count) then
      if (Copy(Content[CurrentCommandStartLineNr].Text, 1, OldConsolePromptLen) =
        OldConsolePrompt) then
      begin
        for i := CurrentCommandStartLineNr to CurrentCommandEndLineNr do
        begin
          if (i = CurrentCommandStartLineNr) then
            Content[i].Text := ConsolePrompt +
              Copy(Content[i].Text, OldConsolePromptLen + 1, Length(Content[i].Text))
          else
            Content[i].Text := GetConsolePromptMultiline +
              Copy(Content[i].Text, OldConsolePromptLen + 1, Length(Content[i].Text));
        end;
      end;

  SetCaretToEditorBottom;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.SetConsoleCommandSelStart(value: integer);

var
  Loop: Integer;
  Count: Integer;

begin
  Loop := CurrentCommandStartLineNr;
  Count := 0;
  while ((Count + Length(Content[Loop].Text) - ConsolePromptLen + 2) < value)
    and (Loop < Content.Count) do
  begin
    Count := Count + Length(Content[Loop].Text) - ConsolePromptLen + 2;
    Inc(loop);
  end;
  CaretY := Loop;
  CaretX := ConsolePromptLen + Value - Count;
end;

function TCustomUniCodeConsole.GetConsolePromptMultiline: WideString;
begin
  if (ConsolePromptLen > 3) then
    Result := StringOfChar(' ', ConsolePromptLen - 3) + '>> '
  else
    Result := '>>';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCustomUniCodeConsole.SetConsoleHistory(History: AnsiString);

var
  Lines: TStringList;
  I: Integer;
  cmd: AnsiString;

begin
  CommandHistory.Clear;
  cmd := '';

  Lines := TStringList.Create;
  try
    Lines.Text := History;

    for I := 2 to Lines.Count-2 do
    begin
      if (Lines[I] = '  </history-entry>') and
        (cmd <> '') then
      begin
        CommandHistory.Add(TCommandHistoryObject.Create(
          UTF8ToWideString(
            StringReplace(
              StringReplace(
                StringReplace(cmd, '&gt;', '>', [rfReplaceAll]),
                '&lt;', '<', [rfReplaceAll]),
              '&amp;', '&', [rfReplaceAll])
          )));

        cmd := '';
      end
      else
        if (Lines[I] <> '  <history-entry>') then
          cmd := cmd + Lines[I] + #13#10;
    end;

    CommandHistoryPos := CommandHistory.Count;
  finally
    Lines.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCustomUniCodeConsole.GetConsoleHistory: AnsiString;

var
  I: Integer;
  Start: Integer;

begin
  Result := '<?xml version="1.0"?>'#13#10'<consolehistory>'#13#10;

  Start := Max(0, CommandHistory.Count - 1000);

  for I := Start to CommandHistory.Count-1 do
    Result := Result + '  <history-entry>'#13#10+
      Trim(
        StringReplace(
          StringReplace(
            StringReplace(
              WideStringToUTF8(
                TCommandHistoryObject(CommandHistory[I]).Cmd
              ), '&', '&amp;', [rfReplaceAll]),
            '<', '&lt;', [rfReplaceAll]),
          '>', '&gt;', [rfReplaceAll])
        )+#13#10+
      '  </history-entry>'#13#10;

  Result := Result + '</consolehistory>';
end;

//----------------------------------------------------------------------------------------------------------------------


end.

