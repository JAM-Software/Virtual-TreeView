unit VTCellSelectionTests;

// Virtual Treeview cell selection tests
// Written by CheeWee Chua.

interface

uses
  DUnitX.TestFramework, Vcl.Forms, VirtualTrees, System.Types,
  Winapi.Messages, Winapi.Windows, Vcl.ComCtrls;

type

  [TestFixture]
  TCellSelectionTests = class(TObject)
  strict private
  const MaxTries = 10;
  type
    TVTChangeEventProc = reference to procedure(Sender: TBaseVirtualTree; Node: PVirtualNode);
    TVTChangeCellEventProc = reference to procedure(Sender: TBaseVirtualTree; const Cells: TVTCellArray);
  var
    FTree: TVirtualStringTree;
    FForm: TForm;
    FNode1,
    FNode2,
    FNode3,
    FNode4,
    FNode5,
    FNode6,
    FNode7,
    FNode8: PVirtualNode;
    FRichEdit: TRichEdit;

    FChangeEventProc: TVTChangeEventProc;
    FChangeCellEventProc: TVTChangeCellEventProc;

    procedure AssignChange(const AChangeEventProc: TVTChangeEventProc); overload;
    procedure AssignChange(const AChangeCellEventProc: TVTChangeCellEventProc); overload;
    procedure DoChangeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoChangeCellEvent(Sender: TBaseVirtualTree; const Cells: TVTCellArray);
    procedure FreeNodeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FClipboardAllocated: LongBool;
    FClipboardWindow: HWND;
    procedure CompleteClipboardCopy;
    procedure OpenClipboard;
    procedure CloseClipboard;
    procedure MainWndProc(var Message: TMessage);
    procedure WndProc(var Message: TMessage);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestChangeCellEvent;

    [Test]
    procedure TestSelectSingleCell;

    [Test]
    procedure TestShiftClickMultipleCells;

    [Test]
    procedure TestClear;

    [Test]
    procedure TestClearCellSelection;

    [Test]
    procedure TestSelectMultipleCellsFailWithoutMultiSelect;

    [Test]
    procedure TestSelectMultipleCellsFailWithoutExtendedFocus;

    [Test]
    procedure TestSelectCellsRectangular;

    [Test]
    procedure TestClickUnselectsSelectedCells;

    [Test]
    procedure TestCopyHTML1;

    [Test]
    procedure TestCopyHTML2;

    [Test]
    procedure TestCopyPlainText1;

    [Test]
    procedure TestCopyPlainText2;

    [Test]
    procedure TestCopyRTF1;

    [Test]
    procedure TestCopyRTF2;

    /// <summary>
    /// This tests that OnChange is fired when a node is selected
    /// </summary>
    [Test]
    procedure TestOnChange;

    /// <summary>
    /// This tests that OnChange is fired when an empty area is clicked
    /// </summary>
    [Test]
    procedure TestEmptyAreaOnChange;

  end;

implementation

uses
  System.SysUtils, Vcl.Controls, VirtualTrees.Types,
  Vcl.Clipbrd, VirtualTrees.ClipBoard,
  System.Classes, Winapi.ActiveX, Vcl.ClipboardHelper, VirtualTrees.MouseUtils;

type
  TRowData = record
    col1: string;
    col2: string;
    col3: string;
    col4: string;
    col5: string;
  public
    constructor Create(const ACol1, ACol2, ACol3, ACol4, ACol5: string);
    procedure Clear;
  end;

{ TRowData }

procedure TRowData.Clear;
begin
  System.Finalize(Self);
end;

constructor TRowData.Create(const ACol1, ACol2, ACol3, ACol4, ACol5: string);
begin
  col1 := ACol1;
  col2 := ACol2;
  col3 := ACol3;
  col4 := ACol4;
  col5 := ACol5;
end;

const
  col0 = 0;
  col1 = 1;
  col2 = 2;
  col3 = 3;
  col4 = 4;
  col5 = 5;
  // skip the 4th column

{ TCellSelectionTests }

procedure TCellSelectionTests.CloseClipboard;
begin
  if FClipboardAllocated then
  begin
    DeallocateHWnd(FClipboardWindow);
    FClipboardWindow := 0;
    Application.Handle := 0;
    FClipboardAllocated := False;
  end;
end;

// Hacks to make OLE clipboard and VCL clipboard compatible
// within DUnit test framework
procedure TCellSelectionTests.CompleteClipboardCopy;
var
  LResult: HRESULT;
  LErrorMsg: string;
begin
  Application.ProcessMessages;
  FTree.FlushClipboard;
  LResult := Winapi.ActiveX.OleFlushClipboard;
  if Failed(LResult) then
    begin
      LErrorMsg := SysErrorMessage(GetLastError);
    end;
  Application.ProcessMessages;
end;

procedure TCellSelectionTests.OpenClipboard;
begin
  if FClipboardWindow = 0 then
  begin
    FClipboardWindow := AllocateHWnd(MainWndProc);
    Application.Handle := FClipboardWindow;
    FClipboardAllocated := True;
  end;
end;

procedure TCellSelectionTests.MainWndProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self)
    else
      raise;
  end;
end;

procedure TCellSelectionTests.WndProc(var Message: TMessage);
begin
  with Message do
    Result := DefWindowProc(FClipboardWindow, Msg, wParam, lParam);
end;

procedure TCellSelectionTests.AssignChange(
  const AChangeEventProc: TVTChangeEventProc);
begin
  FChangeEventProc := AChangeEventProc;
  FTree.OnChange := DoChangeEvent;
end;

procedure TCellSelectionTests.AssignChange(const AChangeCellEventProc: TVTChangeCellEventProc);
begin
  FChangeCellEventProc := AChangeCellEventProc;
  FTree.OnChangeCell := DoChangeCellEvent;
end;

procedure TCellSelectionTests.DoChangeEvent(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if Assigned(FChangeEventProc) then
    FChangeEventProc(Sender, Node);
end;

procedure TCellSelectionTests.DoChangeCellEvent(Sender: TBaseVirtualTree; const Cells: TVTCellArray);
begin
  if Assigned(FChangeCellEventProc) then
    FChangeCellEventProc(Sender, Cells);
end;

procedure TCellSelectionTests.FreeNodeEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  LRowData: TRowData;
begin
  LRowData := Node.GetData<TRowData>;
  LRowData.Clear;
end;

// End hacks

procedure TCellSelectionTests.Setup;
var
  LTree: TVirtualStringTree;
  LCol1, LCol2, LCol3, LCol4, LCol5: TVirtualTreeColumn;
begin

  OpenClipboard;

  FForm := TForm.Create(nil);
  FRichEdit := TRichEdit.Create(FForm);
  FForm.InsertControl(FRichEdit);
  FTree := TVirtualStringTree.Create(FForm);
  LTree := FTree;
  LTree.Parent := FForm;
  LTree.Align := alClient;
  LTree.OnFreeNode := FreeNodeEvent;
  LTree.OnGetText := VirtualStringTree1GetText;
  LTree.TreeStates := LTree.TreeStates + [tsUseCache];

  try
    OleSetClipboard(nil);
    Clipboard.AsText := '';
  except
  end;

  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_TEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_OEMTEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_UNICODETEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_VRTF));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_HTML));

  // Add three columns
  LCol1 := LTree.Header.Columns.Add;
  LCol2 := LTree.Header.Columns.Add;
  LCol3 := LTree.Header.Columns.Add;
  LCol4 := LTree.Header.Columns.Add;
  LCol5 := LTree.Header.Columns.Add;
  LCol1.Text := 'col1';
  LCol2.Text := 'col2';
  LCol3.Text := 'col3';
  LCol4.Text := 'col4';
  LCol5.Text := 'col5';

  LTree.NodeDataSize := SizeOf(TRowData);

  FNode1 := LTree.AddChild(LTree.RootNode);
  FNode2 := LTree.AddChild(LTree.RootNode);
  FNode3 := LTree.AddChild(LTree.RootNode);
  FNode4 := LTree.AddChild(LTree.RootNode);
  FNode5 := LTree.AddChild(LTree.RootNode);

  FNode1.SetData<TRowData>(
    TRowData.Create('1a', '1b', '1c', '1d', '1e')
  );
  FNode2.SetData<TRowData>(
    TRowData.Create('2a', '2b', '2c', '2d', '2e')
  );
  FNode3.SetData<TRowData>(
    TRowData.Create('3a', '3b', '3c', '3d', '3e')
  );
  FNode4.SetData<TRowData>(
    TRowData.Create('4a', '4b', '4c', '4d', '4e')
  );
  FNode5.SetData<TRowData>(
    TRowData.Create('5a', '5b', '5c', '5d', '5e')
  );
end;

procedure TCellSelectionTests.TearDown;
begin
  CloseClipboard;
  OleSetClipboard(nil);
  FreeAndNil(FForm);
end;

procedure TCellSelectionTests.TestChangeCellEvent;
var
  LTree: TVirtualStringTree;
  n3: PVirtualNode;
  LChangeCellFiredWhenAdding, LChangeCellFiredWhenRemoving: LongBool;
begin
  LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect];

  LChangeCellFiredWhenAdding := False;
  AssignChange(procedure (Sender: TBaseVirtualTree; const Cells: TVTCellArray)
  begin
    LChangeCellFiredWhenAdding := True;
  end);

  n3 := FNode3;
  LTree.SelectCells(n3, 1, n3, 1, False);

  Assert.IsTrue(LChangeCellFiredWhenAdding, 'OnChangeCell event not fired when adding!');

  LChangeCellFiredWhenRemoving := False;
  AssignChange(procedure (Sender: TBaseVirtualTree; const Cells: TVTCellArray)
  begin
    LChangeCellFiredWhenRemoving := True;
  end);
  LTree.ClearCellSelection;

  Assert.IsTrue(LChangeCellFiredWhenRemoving, 'OnChangeCell event not fired when removing!');

end;

procedure TCellSelectionTests.TestClear;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
  LSelectedCells: TVTCellArray;
begin
  LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);
  LSelectedCells := LTree.SelectedCells;

  // How long is not important here, other tests in this suite checks it.
  Assert.IsTrue(Length(LSelectedCells) > 0);

  LTree.Clear;

  LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 0, 'Selected cells are not cleared!');
end;

procedure TCellSelectionTests.TestClearCellSelection;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
  LSelectedCells: TVTCellArray;
begin
  LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 4, 'Length of selected cells is not 4!');

  LTree.ClearCellSelection;
  LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 0, 'Length of selected cells is not 0!');
end;

procedure TCellSelectionTests.TestClickUnselectsSelectedCells;
var
  LTree: TVirtualStringTree;
  n1, n3, n4: PVirtualNode;
  LSelCel1, LSelCel2: TVTCellArray;
begin
  LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  LSelCel1 := LTree.SelectedCells;
  n1 := FNode1;
  LTree.MouseClick(n1);
  LSelCel2 := LTree.SelectedCells;

  // Ensures the above cells are no longer selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');

  Assert.IsTrue(LTree.IsCellSelected(n1, 0), 'n1, col0 should be selected');
end;

procedure TCellSelectionTests.TestCopyHTML1;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
  LText, LExpected: string;
  LTries: Integer;
  LCompareSuccessful: LongBool;
begin
  LTree := FTree;
  LTree.Font.Name := 'Tahoma';
  LTree.Font.Size := 8;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n4, col4
  LText := ''; LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n4, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    Inc(LTries);
    try
      LText := Clipboard.AsHTML;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      begin
        Clipboard.Close;
      end;
      else
        raise;
    end;
    // End unnecessary stuff
  until (LText <> '') or (LTries > MaxTries);
  LExpected := 'Version:1.0'#$D#$A'StartHTML:00000097'#$D#$A'EndHTML:00001737'#$D#$A'StartFragment:00000269'#$D#$A'EndFragment:00001705'#$D#$A +
  '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"><html><head><META http-equiv=Content-Type content="text/html; charset=utf-8"></head><body><!--StartFragment--><META http-equiv="Content-Type" content="text/html; charset=utf-8">'+
  '<style type="text/css">'#$D#$A'.default{font-family: ''Tahoma''; font-size: 8pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}'#$D#$A +
  '.header{font-family: ''Tahoma''; font-size: 8pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}'#$D#$A'.noborder{border-style: none;padding-left: 4px; padding-right: 4px;}'#$D#$A +
  '.normalborder {vertical-align: top; border-right: none; border-left:none; border-top:none; border-bottom: none;border-width: thin; border-style: dotted;padding-left: 4px; padding-right: 4px;}</style>'#$D#$A +
  '<table class="default" style="border-collapse: collapse;" bgcolor=#FFFFFF  border="1" frame=box cellspacing="0">'#$D#$A'<tr class="header" style="padding-left: 4px; padding-right: 4px;">'#$D#$A +
  '<th height="17px" align=left bgcolor=#F0F0F0 width="50px">col2</th><th height="17px" align=left bgcolor=#F0F0F0 width="50px">col3</th><th height="17px" align=left bgcolor=#F0F0F0 width="50px">col4</th></tr>'#$D#$A+
  ' <tr class="default">'#$D#$A' <td class="normalborder"  height="18px" align=left>3b</td> <td class="normalborder"  height="18px" align=left>3c</td> <td class="normalborder"  height="18px" align=left>3d</td> </tr>'#$D#$A+
  ' <tr class="default">'#$D#$A' <td class="normalborder"  height="18px" align=left>4b</td> <td class="normalborder"  height="18px" align=left>4c</td> <td class="normalborder"  height="18px" align=left>4d</td> </tr>'#$D#$A'</table><!--EndFragment--></body></html>';
  LCompareSuccessful := LText = LExpected;
  Assert.IsTrue(LCompareSuccessful, 'Clipboard text is unexpected!');
end;

procedure TCellSelectionTests.TestCopyHTML2;
var
  LTree: TVirtualStringTree;
  n3, n5: PVirtualNode;
  LText, LExpected: string;
  LTries: Integer;
  LCompareSuccessful: LongBool;
begin
  LTree := FTree;
  LTree.Font.Name := 'Tahoma';
  LTree.Font.Size := 10;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n5 := FNode5;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n5, col4
  LText := ''; LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n5, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    Inc(LTries);
    try
      LText := Clipboard.AsHTML;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      begin
        Clipboard.Close;
      end;
      else
        raise;
    end;
    // End unnecessary stuff
  until (LText <> '') or (LTries > MaxTries);
  LExpected := 'Version:1.0'#$D#$A'StartHTML:00000097'#$D#$A'EndHTML:00001947'#$D#$A'StartFragment:00000269'#$D#$A'EndFragment:00001915'#$D#$A'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"><html><head>'+
  '<META http-equiv=Content-Type content="text/html; charset=utf-8"></head><body><!--StartFragment--><META http-equiv="Content-Type" content="text/html; charset=utf-8"><style type="text/css">'#$D#$A+
  '.default{font-family: ''Tahoma''; font-size: 10pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}'#$D#$A+
  '.header{font-family: ''Tahoma''; font-size: 10pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}'#$D#$A'.noborder{border-style: none;padding-left: 4px; padding-right: 4px;}'#$D#$A+
  '.normalborder {vertical-align: top; border-right: none; border-left:none; border-top:none; border-bottom: none;border-width: thin; border-style: dotted;padding-left: 4px; padding-right: 4px;}</style>'#$D#$A +
  '<table class="default" style="border-collapse: collapse;" bgcolor=#FFFFFF  border="1" frame=box cellspacing="0">'#$D#$A'<tr class="header" style="padding-left: 4px; padding-right: 4px;">'#$D#$A+
  '<th height="20px" align=left bgcolor=#F0F0F0 width="50px">col2</th><th height="20px" align=left bgcolor=#F0F0F0 width="50px">col3</th><th height="20px" align=left bgcolor=#F0F0F0 width="50px">col4</th></tr>'#$D#$A+
  ' <tr class="default">'#$D#$A' <td class="normalborder"  height="18px" align=left>3b</td> <td class="normalborder"  height="18px" align=left>3c</td> <td class="normalborder"  height="18px" align=left>3d</td> </tr>'#$D#$A+
  ' <tr class="default">'#$D#$A' <td class="normalborder"  height="18px" align=left>4b</td> <td class="normalborder"  height="18px" align=left>4c</td> <td class="normalborder"  height="18px" align=left>4d</td> </tr>'#$D#$A' <tr class="default">'#$D#$A +
  ' <td class="normalborder"  height="18px" align=left>5b</td> <td class="normalborder"  height="18px" align=left>5c</td> <td class="normalborder"  height="18px" align=left>5d</td> </tr>'#$D#$A'</table><!--EndFragment--></body></html>';
  LCompareSuccessful := LText = LExpected;
  Assert.IsTrue(LCompareSuccessful, 'Clipboard text is unexpected!');
end;

procedure TCellSelectionTests.TestCopyPlainText1;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
  LText: string;
  LTries: Integer;
  LCompareSuccessful: LongBool;
begin
  LTree := FTree;
  LTree.Font.Name := 'Tahoma';
  LTree.Font.Size := 10;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n4, col4
  LText := ''; LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n4, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    Inc(LTries);
    try
      LText := Clipboard.AsText;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      begin
        Clipboard.Close;
      end;
      else
        raise;
    end;
    // End unnecessary stuff
  until (LText <> '') or (LTries > MaxTries);
  LCompareSuccessful := LText = 'col2'#9'col3'#9'col4'#$D#$A'3b'#9'3c'#9'3d'#$D#$A'4b'#9'4c'#9'4d'#$D#$A;
  Assert.IsTrue(LCompareSuccessful, 'Clipboard text is unexpected!');
end;

procedure TCellSelectionTests.TestCopyPlainText2;
var
  LTree: TVirtualStringTree;
  n3, n5: PVirtualNode;
  LText, LExpected: string;
  LTries: Integer;
  LCompareSuccessful: LongBool;
begin
  LTree := FTree;
  LTree.Font.Name := 'Tahoma';
  LTree.Font.Size := 10;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n5 := FNode5;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n5, col4
  LText := ''; LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n5, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    Inc(LTries);
    try
      LText := Clipboard.AsText;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      begin
        Clipboard.Close;
      end;
      else
        raise;
    end;
    // End unnecessary stuff
  until (LText <> '') or (LTries > MaxTries);
  LExpected := 'col2'#9'col3'#9'col4'#$D#$A'3b'#9'3c'#9'3d'#$D#$A'4b'#9'4c'#9'4d'#$D#$A'5b'#9'5c'#9'5d'#$D#$A;
  LCompareSuccessful := LText = LExpected;
  Assert.IsTrue(LCompareSuccessful, 'Clipboard text is unexpected!');
end;

procedure TCellSelectionTests.TestCopyRTF1;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
  LText, LPlainText, LExpected: string;
  LTries: Integer;
  LCompareSuccessful: LongBool;
begin
  LTree := FTree;
  LTree.Font.Name := 'Tahoma';
  LTree.Font.Size := 8;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n4, col4
  LText := ''; LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n4, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    Inc(LTries);
    try
      LText := Clipboard.AsRTF;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      begin
        Clipboard.Close;
      end;
      else
        raise;
    end;
    // End unnecessary stuff
  until (LText <> '') or (LTries > MaxTries);
  LExpected := '{\rtf1\ansi\ansicpg1252\deff0\deflang1043{\fonttbl{\f0 Tahoma;}}{\colortbl;\red0\green0\blue0;}\paperw16840\paperh11907\margl720\margr720\margt720\margb720\uc1\trowd\trgaph70\cellx750\cellx1500\cellx2250\pard\intbl\ql\f0\cf1'+
  '\fs16 \u99\''3f\u111\''3f\u108\''3f\u50\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u51\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u52\''3f\cell\row\pard\intbl \u51\''3f\u98\''3f\cell\pard\intbl \u51\''3f\u99\''3f\cell\pard\intbl \u51\''3f'+
  '\u100\''3f\cell\row'#$D#$A'\pard\intbl \u52\''3f\u98\''3f\cell\pard\intbl \u52\''3f\u99\''3f\cell\pard\intbl \u52\''3f\u100\''3f\cell\row'#$D#$A'\pard\par}';
  FRichEdit.SelText := LText;
  LPlainText := FRichEdit.Text;
  LCompareSuccessful := LText = LExpected;
  Assert.IsTrue(LCompareSuccessful, 'Clipboard text is unexpected!');
end;

procedure TCellSelectionTests.TestCopyRTF2;
var
  LTree: TVirtualStringTree;
  n3, n5: PVirtualNode;
  LText, LPlainText, LExpected: string;
  LTries: Integer;
  LCompareSuccessful: LongBool;
begin
  LTree := FTree;
  LTree.Font.Name := 'Tahoma';
  LTree.Font.Size := 8;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n5 := FNode5;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n5, col4
  LText := ''; LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n5, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    Inc(LTries);
    try
      LText := Clipboard.AsRTF;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      begin
        Clipboard.Close;
      end;
      else
        raise;
    end;
    // End unnecessary stuff
  until (LText <> '') or (LTries > MaxTries);
  LExpected := '{\rtf1\ansi\ansicpg1252\deff0\deflang1043{\fonttbl{\f0 Tahoma;}}{\colortbl;\red0\green0\blue0;}\paperw16840\paperh11907\margl720\margr720\margt720\margb720\uc1\trowd\trgaph70\cellx750\cellx1500\cellx2250\pard\intbl\ql\f0\cf1\fs16 \u99\''3f\u111\''3f\u108\''3' +
  'f\u50\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u51\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u52\''3f\cell\row\pard\intbl \u51\''3f\u98\''3f\cell\pard\intbl \u51\''3f\u99\''3f\cell\pard\intbl \u51\''3f\u100\''3f\cell\row' + sLineBreak +
  '\pard\intbl \u52\''3f\u98\''3f\cell\pard\intbl \u52\''3f\u99\''3f\cell\pard\intbl \u52\''3f\u100\''3f\cell\row' + sLineBreak +
  '\pard\intbl \u53\''3f\u98\''3f\cell\pard\intbl \u53\''3f\u99\''3f\cell\pard\intbl \u53\''3f\u100\''3f\cell\row' + sLineBreak +
  '\pard\par}';
  FRichEdit.SelText := LText;
  LPlainText := FRichEdit.Text;
  LCompareSuccessful := LText = LExpected;
  Assert.IsTrue(LCompareSuccessful, 'Clipboard text is unexpected!');
end;

procedure TCellSelectionTests.TestEmptyAreaOnChange;
var
  LTree: TVirtualStringTree;
  n3: PVirtualNode;
  LOnChangeFired: LongBool;
  I, LColumnCount, LMaxWidth: Integer;
  Rects: TArray<TRect>;
  LargestRect: TRect;
  LTextOnly, LUnclipped, LApplyCellContentMargin: Boolean;
  LastNode, LHitNode: PVirtualNode;
  LEmptyArea: TPoint;
  LHitInfo: THitInfo;
begin
  LTree := FTree;
  n3 := FNode3;


  // Calculate the largest client area for the VirtualTree and set it
  LColumnCount := LTree.Header.Columns.Count;
  LMaxWidth := 0;
  for I := 0 to LColumnCount-1 do
    begin
      if LTree.Header.Columns[I].Width > LMaxWidth then
        LMaxWidth := LTree.Header.Columns[I].Width;
    end;
  I := 0;
  for LTextOnly := False to True do
    for LUnclipped := False to True do
      for LApplyCellContentMargin := False to True do
      begin
        SetLength(Rects, I+1);
        Rects[I] := LTree.GetDisplayRect(n3, LColumnCount-1, LTextOnly, LUnclipped, LApplyCellContentMargin);
        Inc(I);
      end;
  LargestRect := Rects[0];
  for I := 1 to High(Rects) do
    begin
      LargestRect := TRect.Union(LargestRect, Rects[I]);
    end;

  LastNode := LTree.GetLastVisible;

  LTree.ClientHeight := LargestRect.BottomRight.Y + (LastNode.NodeHeight * 2);
  LTree.ClientWidth :=  LargestRect.BottomRight.X + LMaxWidth;

  // This should be an empty area, beyond any visible nodes
  LEmptyArea := Point(LargestRect.BottomRight.X + LMaxWidth, LargestRect.BottomRight.Y + LastNode.NodeHeight);

  // At this point, there should be no nodes selected
  Assert.IsTrue(LTree.SelectedCount = 0);
  LTree.MouseClick(n3);
  // At this point, a node should be selected
  Assert.IsTrue(LTree.SelectedCount = 1);

  LOnChangeFired := False;
  LHitNode := Pointer($FFFFFFFF);
  AssignChange(procedure (Sender: TBaseVirtualTree; ANode: PVirtualNode)
  begin
    LOnChangeFired := True;
    LHitNode := ANode;
  end);

  LTree.GetHitTestInfoAt(LEmptyArea.X, LEmptyArea.Y, True, LHitInfo);
  LTree.MouseClick(LEmptyArea);

  Assert.IsTrue(hiNowhere in LHitInfo.HitPositions, 'Mouse click is not in an unpopulated/empty area!');
  Assert.IsTrue(LOnChangeFired, 'OnChange event not fired!');
  Assert.IsTrue(LHitNode = nil, 'Node is not nil!');
end;

procedure TCellSelectionTests.TestOnChange;
var
  LTree: TVirtualStringTree;
  n3: PVirtualNode;
  LOnChangeFired: LongBool;
begin
  LTree := FTree;
  n3 := FNode3;

  LOnChangeFired := False;
  AssignChange(procedure (Sender: TBaseVirtualTree; ANode: PVirtualNode)
  begin
    LOnChangeFired := True;
  end);

  LTree.MouseClick(n3);
  Assert.IsTrue(LOnChangeFired, 'OnChange event not fired!');
end;

procedure TCellSelectionTests.TestSelectCellsRectangular;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
  LNodes: TArray<PVirtualNode>;
  LNode: PVirtualNode;
  LColumn: Integer;
begin

  LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  // Ensure the non-selected cells are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 0), 'n3, col0 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 3), 'n3, col3 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 0), 'n4, col0 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 3), 'n4, col3 should not be selected');

  LNodes := [FNode1, FNode2, FNode5, FNode6, FNode7, FNode8];
  for LNode in LNodes do
    for LColumn := 0 to 3 do
      Assert.IsFalse(LTree.IsCellSelected(LNode, LColumn),
        Format('Row: $%p Column: %d should not be selected', [Pointer(LNode), LColumn]));
end;

procedure TCellSelectionTests.TestSelectMultipleCellsFailWithoutExtendedFocus;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
begin
  LTree := FTree;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions -
    [toExtendedFocus];

  n3 := FNode3;
  n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  // Ensure the selected cells above are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');
end;

procedure TCellSelectionTests.TestSelectMultipleCellsFailWithoutMultiSelect;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
begin
  LTree := FTree;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions -
    [toMultiSelect];

  n3 := FNode3;
  n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  // Ensure the selected cells above are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');
end;

procedure TCellSelectionTests.TestSelectSingleCell;
var
  LTree: TVirtualStringTree;
  n3: PVirtualNode;
  LSelectedCells: TVTCellArray;
  LNodes: TArray<PVirtualNode>;
  LNode: PVirtualNode;
  LColumn: Integer;
begin
  LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect];

  n3 := FNode3;
  LTree.SelectCells(n3, 1, n3, 1, False);

  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 1, 'Should only have 1 cell selected');
  Assert.IsTrue((LSelectedCells[0].Node = n3) and (LSelectedCells[0].Column = 1));

  LNodes := [FNode1, FNode2, n3, FNode4, FNode5, FNode6, FNode7, FNode8];
  for LNode in LNodes do
    for LColumn := 0 to 3 do
    begin
      if (LNode = n3) and (LColumn = 1) then
        Continue;
      Assert.IsFalse(LTree.IsCellSelected(LNode, LColumn),
        Format('Row: $%p Column: %d should not be selected', [Pointer(LNode), LColumn]));
    end;
end;

procedure TCellSelectionTests.TestShiftClickMultipleCells;
var
  LTree: TVirtualStringTree;
  n3, n4: PVirtualNode;
  LSelectedCells, LNewSelectedCells: TVTCellArray;
begin
  LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  n3 := FNode3;
  n4 := FNode4;

  LTree.MouseClick(n3, 1);
  LSelectedCells := LTree.SelectedCells;
  LTree.ShiftMouseClick(n4, 2);
  LNewSelectedCells := LTree.SelectedCells;

  Assert.IsTrue(Length(LSelectedCells) = 1, 'Length of selected cell is unexpected!');
  Assert.IsTrue((LSelectedCells[0].Node = n3) and (LSelectedCells[0].Column = 1), 'Unexpected cell selection 1');

  Assert.IsTrue(Length(LNewSelectedCells) = 4, 'Length of selected cells is unexpected!');
  Assert.IsTrue((LNewSelectedCells[0].Node = n3) and (LNewSelectedCells[0].Column = 1), 'Unexpected cell selection 0!');
  Assert.IsTrue((LNewSelectedCells[1].Node = n3) and (LNewSelectedCells[1].Column = 2), 'Unexpected cell selection 1!');
  Assert.IsTrue((LNewSelectedCells[2].Node = n4) and (LNewSelectedCells[2].Column = 1), 'Unexpected cell selection 2!');
  Assert.IsTrue((LNewSelectedCells[3].Node = n4) and (LNewSelectedCells[3].Column = 2), 'Unexpected cell selection 3!');
end;

procedure TCellSelectionTests.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  LData: TRowData;
begin
  if not Assigned(Node) then
    Exit;
  LData := Node.GetData<TRowData>;
  case Column of
    col0: begin
      CellText := LData.col1;
    end;
    col1: begin
      CellText := LData.col2;
    end;
    col2: begin
      CellText := LData.col3;
    end;
    col3: begin
      CellText := LData.col4;
    end;
    col4: begin
      CellText := LData.col5;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCellSelectionTests);
end.
