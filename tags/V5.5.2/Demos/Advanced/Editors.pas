unit Editors;

// Utility unit for the advanced Virtual Treeview demo application which contains the implementation of edit link
// interfaces used in other samples of the demo.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VirtualTrees, ExtDlgs, ImgList, Buttons, ExtCtrls, ComCtrls,
  Mask;

type
  // Describes the type of value a property tree node stores in its data property.
  TValueType = (
    vtNone,
    vtString,
    vtPickString,
    vtNumber,
    vtPickNumber,
    vtMemo,
    vtDate
  );

//----------------------------------------------------------------------------------------------------------------------

type
  // Node data record for the the document properties treeview.
  PPropertyData = ^TPropertyData;
  TPropertyData = record
    ValueType: TValueType;
    Value: UnicodeString;      // This value can actually be a date or a number too.
    Changed: Boolean;
  end;

  // Our own edit link to implement several different node editors.
  TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TWinControl;        // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

//----------------------------------------------------------------------------------------------------------------------

type
  TPropertyTextKind = (
    ptkText,
    ptkHint
  );

// The following constants provide the property tree with default data.

const
  // Types of editors to use for a certain node in VST3.
  ValueTypes: array[0..1, 0..12] of TValueType = (
    (
      vtString,     // Title
      vtString,     // Theme
      vtPickString, // Category
      vtMemo,       // Keywords
      vtNone,       // Template
      vtNone,       // Page count
      vtNone,       // Word count
      vtNone,       // Character count
      vtNone,       // Lines
      vtNone,       // Paragraphs
      vtNone,       // Scaled
      vtNone,       // Links to update
      vtMemo),      // Comments
    (
      vtString,     // Author
      vtNone,       // Most recently saved by
      vtNumber,     // Revision number
      vtPickString, // Primary application
      vtString,     // Company name
      vtNone,       // Creation date
      vtDate,       // Most recently saved at
      vtNone,       // Last print
      vtNone,
      vtNone,
      vtNone,
      vtNone,
      vtNone)
  );

  // types of editors to use for a certain node in VST3
  DefaultValue: array[0..1, 0..12] of String = (
    (
      'Virtual Treeview',         // Title
      'native Delphi controls',   // Theme
      'Virtual Controls',         // Category
      'virtual, treeview, VCL',   // Keywords
      'no template used',         // Template
      '> 900',                    // Page count
      '?',                        // Word count
      '~ 1.000.000',              // Character count
      '~ 28.000',                 // Lines
      '',                         // Paragraphs
      'False',                    // Scaled
      'www.delphi-gems.com',    // Links to update
      'Virtual Treeview is much more than a simple treeview.'), // Comments
    (
      'Dipl. Ing. Mike Lischke',  // Author
      'Mike Lischke',             // Most recently saved by
      '3.0',                      // Revision number
      'Delphi',                   // Primary application
      '',                         // Company name
      'July 1999',                // Creation date
      'January 2002',             // Most recently saved at
      '',                         // Last print
      '',
      '',
      '',
      '',
      '')
  );

  // Fixed strings for property tree (VST3).
  PropertyTexts: array[0..1, 0..12, TPropertyTextKind] of string = (
    (// first (upper) subtree
     ('Title', 'Title of the file or document'),
     ('Theme', 'Theme of the file or document'),
     ('Category', 'Category of theme'),
     ('Keywords', 'List of keywords which describe the content of the file'),
     ('Template', 'Name of the template which was used to create the document'),
     ('Page count', 'Number of pages in the document'),
     ('Word count', 'Number of words in the document'),
     ('Character count', 'Number of characters in the document'),
     ('Lines', 'Number of lines in the document'),
     ('Paragraphs', 'Number of paragraphs in the document'),
     ('Scaled', 'Scaling of the document for output'),
     ('Links to update', 'Links which must be updated'),
     ('Comments', 'Description or comments for the file')
     ),
    (// second (lower) subtree
     ('Author', 'name of the author of the file or document'),
     ('Most recently saved by', 'Name of the person who has saved the document last'),
     ('Revision number', 'Revision number of the file or document'),
     ('Primary application', 'Name of the application which is primarily used to create this kind of file'),
     ('Company name', 'Name of the company or institution'),
     ('Creation date', 'Date when the file or document was created'),
     ('Most recently saved at', 'Date when the file or document was saved the last time'),
     ('Last print', 'Date when the file or document was printed the last time'),
     ('', ''),   // the remaining 5 entries are not used
     ('', ''),
     ('', ''),
     ('', ''),
     ('', '')
   )
  );

//----------------------------------------------------------------------------------------------------------------------

type
  PGridData = ^TGridData;
  TGridData = record
    ValueType: array[0..3] of TValueType; // one for each column
    Value: array[0..3] of Variant;
    Changed: Boolean;
  end;

  // Our own edit link to implement several different node editors.
  TGridEditLink = class(TPropertyEditLink, IVTEditLink)
  public
    function EndEdit: Boolean; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  PropertiesDemo, GridDemo;

//----------------- TPropertyEditLink ----------------------------------------------------------------------------------

// This implementation is used in VST3 to make a connection beween the tree
// and the actual edit window which might be a simple edit, a combobox
// or a memo etc.

destructor TPropertyEditLink.Destroy;

begin
  //FEdit.Free; casues issue #357. Fix:
  if FEdit.HandleAllocated then
    PostMessage(FEdit.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

var
  CanAdvance: Boolean;

begin
  CanAdvance := true;

  case Key of
    VK_ESCAPE:
      begin
        Key := 0;//ESC will be handled in EditKeyUp()
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;

    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        if FEdit is TComboBox then
          CanAdvance := CanAdvance and not TComboBox(FEdit).DroppedDown;
        if FEdit is TDateTimePicker then
          CanAdvance :=  CanAdvance and not TDateTimePicker(FEdit).DroppedDown;

        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TPropertyEditLink.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;//VK_ESCAPE
  end;//case
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.BeginEdit: Boolean;

begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.CancelEdit: Boolean;

begin
  Result := True;
  FEdit.Hide;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.EndEdit: Boolean;

var
  Data: PPropertyData;
  Buffer: array[0..1024] of Char;
  S: UnicodeString;

begin
  Result := True;

  Data := FTree.GetNodeData(FNode);
  if FEdit is TComboBox then
    S := TComboBox(FEdit).Text
  else
  begin
    GetWindowText(FEdit.Handle, Buffer, 1024);
    S := Buffer;
  end;

  if S <> Data.Value then
  begin
    Data.Value := S;
    Data.Changed := True;
    FTree.InvalidateNode(FNode);
  end;
  FEdit.Hide;
  FTree.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.GetBounds: TRect;

begin
  Result := FEdit.BoundsRect;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;

var
  Data: PPropertyData;

begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // determine what edit type actually is needed
  FEdit.Free;
  FEdit := nil;
  Data := FTree.GetNodeData(Node);
  case Data.ValueType of
    vtString:
      begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtPickString:
      begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value;
          Items.Add(Text);
          Items.Add('Standard');
          Items.Add('Additional');
          Items.Add('Win32');
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtNumber:
      begin
        FEdit := TMaskEdit.Create(nil);
        with FEdit as TMaskEdit do
        begin
          Visible := False;
          Parent := Tree;
          EditMask := '9999';
          Text := Data.Value;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtPickNumber:
      begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtMemo:
      begin
        FEdit := TComboBox.Create(nil);
        // In reality this should be a drop down memo but this requires
        // a special control.
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value;
          Items.Add(Data.Value);
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtDate:
      begin
        FEdit := TDateTimePicker.Create(nil);
        with FEdit as TDateTimePicker do
        begin
          Visible := False;
          Parent := Tree;
          CalColors.MonthBackColor := clWindow;
          CalColors.TextColor := clBlack;
          CalColors.TitleBackColor := clBtnShadow;
          CalColors.TitleTextColor := clBlack;
          CalColors.TrailingTextColor := clBtnFace;
          Date := StrToDate(Data.Value);
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
  else
    Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);

begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.SetBounds(R: TRect);

var
  Dummy: Integer;

begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEdit.BoundsRect := R;
end;

//---------------- TGridEditLink ---------------------------------------------------------------------------------------

function TGridEditLink.EndEdit: Boolean;

var
  Data: PGridData;
  Buffer: array[0..1024] of Char;
  S: UnicodeString;
  I: Integer;
  
begin
  Result := True;
  Data := FTree.GetNodeData(FNode);
  if FEdit is TComboBox then
  begin
    S := TComboBox(FEdit).Text;
    if S <> Data.Value[FColumn - 1] then
    begin
      Data.Value[FColumn - 1] := S;
      Data.Changed := True;
    end;
  end
  else
    if FEdit is TMaskEdit then
    begin
      I := StrToInt(Trim(TMaskEdit(FEdit).EditText));
      if I <> Data.Value[FColumn - 1] then
      begin
        Data.Value[FColumn - 1] := I;
        Data.Changed := True;
      end;
    end
    else
    begin
      GetWindowText(FEdit.Handle, Buffer, 1024);
      S := Buffer;
      if S <> Data.Value[FColumn - 1] then
      begin
        Data.Value[FColumn - 1] := S;
        Data.Changed := True;
      end;
    end;

  if Data.Changed then
    FTree.InvalidateNode(FNode);
  FEdit.Hide;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGridEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;

var
  Data: PGridData;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // Determine what edit type actually is needed.
  FEdit.Free;
  FEdit := nil;
  Data := FTree.GetNodeData(Node);
  case Data.ValueType[FColumn - 1] of
    vtString:
      begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value[FColumn - 1];
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtPickString:
      begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value[FColumn - 1];
          // Here you would usually do a lookup somewhere to get
          // values for the combobox. We only add some dummy values.
          case FColumn of
            2:
              begin
                Items.Add('John');
                Items.Add('Mike');
                Items.Add('Barney');
                Items.Add('Tim');
              end;
            3:
              begin
                Items.Add('Doe');
                Items.Add('Lischke');
                Items.Add('Miller');
                Items.Add('Smith');
              end;
          end;
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtNumber:
      begin
        FEdit := TMaskEdit.Create(nil);
        with FEdit as TMaskEdit do
        begin
          Visible := False;
          Parent := Tree;
          EditMask := '9999;0; ';
          Text := Data.Value[FColumn - 1];
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtPickNumber:
      begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value[FColumn - 1];
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtMemo:
      begin
        FEdit := TComboBox.Create(nil);
        // In reality this should be a drop down memo but this requires
        // a special control.
        with FEdit as TComboBox do
        begin
          Visible := False;
          Parent := Tree;
          Text := Data.Value[FColumn - 1];
          Items.Add(Data.Value[FColumn - 1]);
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
    vtDate:
      begin
        FEdit := TDateTimePicker.Create(nil);
        with FEdit as TDateTimePicker do
        begin
          Visible := False;
          Parent := Tree;
          CalColors.MonthBackColor := clWindow;
          CalColors.TextColor := clBlack;
          CalColors.TitleBackColor := clBtnShadow;
          CalColors.TitleTextColor := clBlack;
          CalColors.TrailingTextColor := clBtnFace;
          Date := StrToDate(Data.Value[FColumn - 1]);
          OnKeyDown := EditKeyDown;
          OnKeyUp := EditKeyUp;
        end;
      end;
  else
    Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
