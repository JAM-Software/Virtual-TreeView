unit UniCodeEditorReg;

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
//   Registration unit for the UniCodeEditor control.
//
// Changes:
//   Version 2.0, 2003-08-17, Mike Zinner
//     Repackaged for D7
//   Version 1.0, 1999-03-10, Mike Lischke
//     Initial Version
//
//----------------------------------------------------------------------------------------------------------------------

interface

procedure Register;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  DesignIntf, DesignWindows, DesignEditors, ComponentDesigner,
  VCLEditors,
  Dialogs, Forms, Graphics, Classes,
  UCEEditorKeyCommands,
  UCEKeyCommandsEditor,
  UCEDelphiHighlighter,
  UCECPPHighlighter,
  UCESQLHighlighter,
  UCEHTMLHighlighter,
  UCEDCGHighlighter,
  UniCodeEditor,
  UniCodeConsole;

type
  TUniCodeEditorFontProperty = class(TFontProperty)
  public
    procedure Edit; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorFontProperty.Edit;

const
  // context ids for the Font editor
  hcDFontEditor = 25000;

var
  FontDialog: TFontDialog;

begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    FontDialog.HelpContext := hcDFontEditor;
    FontDialog.Options := FontDialog.Options + [fdShowHelp, fdForceFontExist, fdFixedPitchOnly];
    if FontDialog.Execute then
      SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TUniCodeEditorCommandProperty = class(TIntegerProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

//----------------- TUniCodeEditorCommandProperty -----------------------------------------------------------------------

procedure TUniCodeEditorCommandProperty.Edit;

begin
  ShowMessage('I''m thinking that this will show a dialog that has a list'#13#10 +
    'of all editor commands and a description of them to choose from.');
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorCommandProperty.GetAttributes: TPropertyAttributes;

begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

//----------------------------------------------------------------------------------------------------------------------

function TUniCodeEditorCommandProperty.GetValue: string;

begin
  Result := EditorCommandToCodeString(TEditorCommand(GetOrdValue));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorCommandProperty.GetValues(Proc: TGetStrProc);

begin
  GetEditorCommandValues(Proc);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUniCodeEditorCommandProperty.SetValue(const Value: string);

var
  NewValue: Longint;

begin
  if IdentToEditorCommand(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterComponents('UniCodeEditor', [TUniCodeEdit, TUCEDelphiHighlighter, TUCECPPHighlighter, TUCEHTMLHighlighter,
      TUCESQLHighlighter, TUCEDCGHighlighter, TUniCodeConsole]);
  RegisterPropertyEditor(TypeInfo(TFont), TUniCodeEdit, 'Font', TUniCodeEditorFontProperty);
  RegisterPropertyEditor(TypeInfo(TEditorCommand), nil, 'Command', TUniCodeEditorCommandProperty);
  RegisterPropertyEditor(TypeInfo(TKeystrokes), nil, '', TKeyStrokesProperty);
end;

//----------------------------------------------------------------------------------------------------------------------

end.

