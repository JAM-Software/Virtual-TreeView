unit UCEShortcutEditor;

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
//   This unit contains a simple editor to edit short cuts with the key commands editor used
//   for the Unicode Syntax Edit (USE) package. The code here is entirely from mwEdit.
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, UCEEditorKeyCommands, Menus;

type
  TKeystrokeEditorForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    cmbCommand: TComboBox;
    hkKeystroke: THotKey;
    btnOK: TButton;
    btnCancel: TButton;
    bntClearKey: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure bntClearKeyClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure SetCommand(const Value: TEditorCommand);
    procedure SetKeystroke(const Value: TShortcut);
    procedure AddEditorCommand(const S: string);
    function GetCommand: TEditorCommand;
    function GetKeystroke: TShortcut;
  public
    property Command: TEditorCommand read GetCommand write SetCommand;
    property Keystroke: TShortcut read GetKeystroke write SetKeystroke;
  end;

var
  KeystrokeEditorForm: TKeystrokeEditorForm;

implementation

{$R *.DFM}

procedure TKeystrokeEditorForm.SetCommand(const Value: TEditorCommand);
begin
  cmbCommand.Text := EditorCommandToCodeString(Value);
end;

procedure TKeystrokeEditorForm.SetKeystroke(const Value: TShortcut);
begin
  hkKeystroke.Hotkey := Value;
end;

procedure TKeystrokeEditorForm.FormCreate(Sender: TObject);
begin
  GetEditorCommandValues(AddEditorCommand);
end;

procedure TKeystrokeEditorForm.AddEditorCommand(const S: string);
begin
  cmbCommand.Items.Add(S);
end;

function TKeystrokeEditorForm.GetCommand: TEditorCommand;
var
  NewCmd: longint;
begin
  if not IdentToEditorCommand(cmbCommand.Text, NewCmd) then
  begin
    try
      NewCmd := StrToInt(cmbCommand.Text);
    except
      NewCmd := ecNone;
    end;
  end;
  Result := NewCmd;
end;

function TKeystrokeEditorForm.GetKeystroke: TShortcut;
begin
  Result := hkKeystroke.HotKey;
end;

procedure TKeystrokeEditorForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  // THotKey does not allow some special keys to be entered by the user, hence we
  // need a work around here.
  if hkKeystroke.Focused then
    case Key of
      VK_BACK,
        VK_RETURN:
        begin
          hkKeystroke.HotKey := Menus.ShortCut(Key, Shift);
          Key := 0; // eat the key so THotKey doesn't get it
        end;
    end;
end;

procedure TKeystrokeEditorForm.bntClearKeyClick(Sender: TObject);
begin
  hkKeystroke.HotKey := 0;
end;

procedure TKeystrokeEditorForm.FormActivate(Sender: TObject);
begin
  ActiveControl := hkKeyStroke;
end;

end.

