unit UCEKeyCommandsEditor;

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
//   Keyboard shortcuts editor for UniCodeEditor
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
  ComCtrls, UCEEditorKeyCommands, Menus, StdCtrls,
  DesignIntf, DesignWindows, DesignEditors;

type
  TKeystrokesEditorForm = class(TForm)
    KeyCmdList: TListView;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnReset: TButton;
    procedure FormResize(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FKeystrokes: TKeystrokes;
    procedure SetKeystrokes(const Value: TKeystrokes);
    procedure UpdateKeystrokesList;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Keystrokes: TKeystrokes read FKeystrokes write SetKeystrokes;
  end;

  TKeyStrokesProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

{$R *.DFM}

uses UCEShortcutEditor;

{ TKeyStrokesProperty }

procedure TKeyStrokesProperty.Edit;

var
  Dlg: TKeystrokesEditorForm;

begin
  Application.CreateForm(TKeystrokesEditorForm, Dlg);
  try
    Dlg.Caption := Self.GetName;
    Dlg.Keystrokes := TKeystrokes(GetOrdValue);
    if Dlg.ShowModal = mrOk then
    begin
      { SetOrdValue will operate on all selected propertiy values }
      SetOrdValue(Longint(Dlg.Keystrokes));
      Modified;
    end;
  finally
    Dlg.Free;
  end;
end;

function TKeyStrokesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TKeystrokesEditorForm }

constructor TKeystrokesEditorForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeystrokes := nil;
end;

destructor TKeystrokesEditorForm.Destroy;
begin
  FKeystrokes.Free;
  inherited Destroy;
end;

procedure TKeystrokesEditorForm.SetKeystrokes(const Value: TKeystrokes);
begin
  if FKeystrokes = nil then
    FKeystrokes := TKeystrokes.Create(Self);
  FKeystrokes.Assign(Value);
  UpdateKeystrokesList;
end;

procedure TKeystrokesEditorForm.UpdateKeystrokesList;
var
  x: integer;
begin
  KeyCmdList.Items.BeginUpdate;
  try
    KeyCmdList.Items.Clear;
    for x := 0 to FKeystrokes.Count - 1 do
    begin
      with KeyCmdList.Items.Add do
      begin
        Caption := EditorCommandToCodeString(FKeystrokes[x].Command);
        if FKeystrokes[x].ShortCut = 0 then
          SubItems.Add('<none>')
        else
          SubItems.Add(Menus.ShortCutToText(FKeystrokes[x].ShortCut));
      end;
    end;
  finally
    KeyCmdList.Items.EndUpdate;
  end;
end;

procedure TKeystrokesEditorForm.FormResize(Sender: TObject);
var
  x: integer;
begin
  for x := 0 to ControlCount - 1 do
    if Controls[x] is TButton then
    begin
      Controls[x].Left := ClientWidth - Controls[x].Width - 7;
      if Controls[x] = btnOK then
        Controls[x].Top := ClientHeight - (Controls[x].Height * 2) - 10;
      if Controls[x] = btnCancel then
        Controls[x].Top := ClientHeight - Controls[x].Height - 3;
    end
    else
      if Controls[x] is TListView then
      begin
        Controls[x].Width := ClientWidth - 96;
        Controls[x].Height := ClientHeight - 8;
      end;
end;

procedure TKeystrokesEditorForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  Msg.MinMaxInfo.ptMinTrackSize := Point(300, 225);
end;

procedure TKeystrokesEditorForm.btnAddClick(Sender: TObject);
var
  NewStroke: TKeystroke;
begin
  with TKeystrokeEditorForm.Create(Self) do
  try
    Command := ecNone;
    Keystroke := 0;
    if ShowModal = mrOK then
    begin
      NewStroke := FKeystrokes.Add;
      NewStroke.Command := Command;
      try
        NewStroke.ShortCut := Keystroke;
      except
        on EKeyException do
        begin
              // Shortcut already exists in the collection!
          MessageDlg('The keystroke "' + Menus.ShortCutToText(Keystroke) +
            '" is already assigned to another editor command.', mtError,
            [mbOK], 0);
          NewStroke.Free;
          exit;
        end;
          // Some other kind of exception, we don't deal with it...
      end;

      with KeyCmdList.Items.Add do
      begin
        Caption := EditorCommandToCodeString(NewStroke.Command);
        if NewStroke.ShortCut = 0 then
          SubItems.Add('<none>')
        else
          SubItems.Add(Menus.ShortCutToText(NewStroke.ShortCut));
      end;
    end;
  finally
    Free;
  end;
end;

procedure TKeystrokesEditorForm.btnEditClick(Sender: TObject);
var
  SelItem: TListItem;
  OldShortcut: TShortcut;
begin
  SelItem := KeyCmdList.Selected;
  if SelItem = nil then
  begin
    MessageBeep(1);
    exit;
  end;
  with TKeystrokeEditorForm.Create(Self) do
  try
    Command := FKeystrokes[SelItem.Index].Command;
    Keystroke := FKeystrokes[SelItem.Index].Shortcut;
    if ShowModal = mrOK then
    begin
      FKeystrokes[SelItem.Index].Command := Command;
      OldShortCut := FKeystrokes[SelItem.Index].ShortCut;
      try
        FKeystrokes[SelItem.Index].ShortCut := Keystroke;
      except
        on EKeyException do
        begin
              // Shortcut already exists in the collection!
          MessageDlg('The keystroke "' + Menus.ShortCutToText(Keystroke) +
            '" is already assigned to another editor command.'#13#10 +
            'The short cut for this item has not been changed.', mtError,
            [mbOK], 0);
          FKeystrokes[SelItem.Index].ShortCut := OldShortCut;
        end;
          // Some other kind of exception, we don't deal with it...
      end;

      KeyCmdList.Items.BeginUpdate;
      try
        with SelItem do
        begin
          Caption := EditorCommandToCodeString(FKeystrokes[Index].Command);
          if FKeystrokes[Index].ShortCut = 0 then
            SubItems[0] := '<none>'
          else
            SubItems[0] := Menus.ShortCutToText(FKeystrokes[Index].Shortcut);
        end;
      finally
        KeyCmdList.Items.EndUpdate;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TKeystrokesEditorForm.btnDeleteClick(Sender: TObject);
var
  SelItem: TListItem;
begin
  SelItem := KeyCmdList.Selected;
  if SelItem = nil then
  begin
    MessageBeep(1);
    exit;
  end;
  FKeystrokes[SelItem.Index].Free;
  KeyCmdList.Items.Delete(SelItem.Index);
end;

procedure TKeystrokesEditorForm.btnResetClick(Sender: TObject);

begin
  FKeystrokes.ResetDefaults;
  UpdateKeystrokesList;
end;

procedure TKeystrokesEditorForm.FormCreate(Sender: TObject);

begin
  KeyCmdList.RowSelect := True;
end;

end.

