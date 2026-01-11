unit VTCellSelectionTests;

// Virtual Treeview cell selection tests
// Written by CheeWee Chua.

interface

uses
  DUnitX.TestFramework, Vcl.Forms, VirtualTrees, System.Types,
  Winapi.Messages, Winapi.Windows;

type

  [TestFixture]
  TCellSelectionTests = class(TObject)
  strict private
  const MaxTries = 10;
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

    procedure MouseClick(ACursorPos: TPoint); overload;
    procedure MouseClick(ANode: PVirtualNode); overload;
    procedure ShiftMouseClick(ANode: PVirtualNode); overload;

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
    procedure TestSelectSingleCell;

    [Test]
    procedure TestShiftClickMultipleCells;

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

    [Test, RepeatTest(10)]
    procedure TestCopyHTML1;

    [Test, RepeatTest(10)]
    procedure TestCopyHTML2;

    [Test, RepeatTest(10)]
    procedure TestCopyPlainText1;

    [Test, RepeatTest(10)]
    procedure TestCopyPlainText2;

    [Test, RepeatTest(10)]
    procedure TestCopyRTF1;

    [Test, RepeatTest(10)]
    procedure TestCopyRTF2;

  end;

implementation

uses
  System.SysUtils, Vcl.Controls, VirtualTrees.Types,
  Vcl.Clipbrd, VirtualTrees.ClipBoard,
  System.Classes, Winapi.ActiveX, Vcl.ClipboardHelper;

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

procedure TCellSelectionTests.MouseClick(ACursorPos: TPoint);
const
  KEYDOWN = Byte(1 shl 7);
var
  LKeyboardState: TKeyboardState;
begin
  // Click a new cell on the tree...
  var LTree := FTree;
  var LSavedCursorPos := Mouse.CursorPos;
  try
    Mouse.CursorPos := ACursorPos;
    var LWPARAM: WPARAM := MK_LBUTTON;
    if GetKeyboardState(LKeyboardState) then
      begin
        if (LKeyboardState[VK_SHIFT] and KEYDOWN <> 0) or
           (LKeyboardState[VK_LSHIFT] and KEYDOWN <> 0) or
           (LKeyboardState[VK_RSHIFT] and KEYDOWN <> 0) then
          LWPARAM := LWPARAM or MK_SHIFT;
        if (LKeyboardState[VK_CONTROL] and KEYDOWN <> 0) or
           (LKeyboardState[VK_LCONTROL] and KEYDOWN <> 0) or
           (LKeyboardState[VK_RCONTROL] and KEYDOWN <> 0) then
          LWPARAM := LWPARAM or MK_CONTROL;
      end;
    var LPos := MakeLParam(ACursorPos.X, ACursorPos.Y);
    LTree.Perform(WM_LBUTTONDOWN, LWPARAM, LPos);
    LTree.Perform(WM_LBUTTONUP, LWPARAM, LPos);
  finally
    Mouse.CursorPos := LSavedCursorPos;
  end;
end;

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
begin
  Application.ProcessMessages;
  FTree.FlushClipboard;
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

procedure TCellSelectionTests.FreeNodeEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  var LRowData := Node.GetData<TRowData>;
  LRowData.Clear;
end;

// End hacks

procedure TCellSelectionTests.ShiftMouseClick(ANode: PVirtualNode);
var
  LOrigKBState, LNewKBState: TKeyboardState;
begin
  GetKeyboardState(LOrigKBState);
  LNewKBState := LOrigKBState;
  LNewKBState[VK_SHIFT] := LOrigKBState[VK_SHIFT] or (1 shl 7);
  SetKeyboardState(LNewKBState);
  try
    MouseClick(ANode);
  finally
    SetKeyboardState(LOrigKBState);
  end;
end;

procedure TCellSelectionTests.MouseClick(ANode: PVirtualNode);
begin
  var LTree := FTree;
  var LClientRect := LTree.GetDisplayRect(ANode, 0, True);
  MouseClick(LClientRect.TopLeft);
end;

procedure TCellSelectionTests.Setup;
begin

  OpenClipboard;

  FForm := TForm.Create(nil);
  FTree := TVirtualStringTree.Create(FForm);
  FTree.Parent := FForm;
  FTree.Align := alClient;
  FTree.OnFreeNode := FreeNodeEvent;
  FTree.OnGetText := VirtualStringTree1GetText;

  try
    Clipboard.AsText := '';
  except
  end;

  var LTree := FTree;

  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_TEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_OEMTEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_UNICODETEXT));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_VRTF));
  LTree.ClipboardFormats.Add(GetVTClipboardFormatDescription(CF_HTML));

  // Add three columns
  var LCol1 := LTree.Header.Columns.Add;
  var LCol2 := LTree.Header.Columns.Add;
  var LCol3 := LTree.Header.Columns.Add;
  var LCol4 := LTree.Header.Columns.Add;
  var LCol5 := LTree.Header.Columns.Add;
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
  FreeAndNil(FForm);
end;

procedure TCellSelectionTests.TestClearCellSelection;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  var LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 4, 'Length of selected cells is not 4!');

  LTree.ClearCellSelection;
  LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 0, 'Length of selected cells is not 0!');
end;

procedure TCellSelectionTests.TestClickUnselectsSelectedCells;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  Assert.IsTrue(toExtendedFocus in LTree.TreeOptions.SelectionOptions);
  Assert.IsTrue(toMultiSelect in LTree.TreeOptions.SelectionOptions);

  // Ensure the selected cells above are selected
  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n3, 2), 'n3, col2 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 1), 'n4, col1 should be selected');
  Assert.IsTrue(LTree.IsCellSelected(n4, 2), 'n4, col2 should be selected');

  var LSelCel1 := LTree.SelectedCells;
  var n1 := FNode1;
  MouseClick(n1);
  var LSelCel2 := LTree.SelectedCells;

  // Ensures the above cells are no longer selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');

  Assert.IsTrue(LTree.IsCellSelected(n1, 0), 'n1, col0 should be selected');
end;

procedure TCellSelectionTests.TestCopyHTML1;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n4, col4
  var LText := ''; var LTries := 0;
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
      else
        raise;
    end;
  until (LText <> '') and (LTries <= MaxTries);
  Assert.IsTrue(LText = 'Version:1.0' + sLineBreak +
'StartHTML:00000097' + sLineBreak +
'EndHTML:00001737' + sLineBreak +
'StartFragment:00000269' + sLineBreak +
'EndFragment:00001705' + sLineBreak +
'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"><html><head><META http-equiv=Content-Type content="text/html; charset=utf-8"></head><body><!--StartFragment--><META http-equiv="Content-Type" content="text/html; charset=utf-8"><style type="tex' +
't/css">' + sLineBreak +
'.default{font-family: ''Tahoma''; font-size: 8pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}' + sLineBreak +
'.header{font-family: ''Tahoma''; font-size: 8pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}' + sLineBreak +
'.noborder{border-style: none;padding-left: 4px; padding-right: 4px;}' + sLineBreak +
'.normalborder {vertical-align: top; border-right: none; border-left:none; border-top:none; border-bottom: none;border-width: thin; border-style: dotted;padding-left: 4px; padding-right: 4px;}</style>' + sLineBreak +
'<table class="default" style="border-collapse: collapse;" bgcolor=#FFFFFF  border="1" frame=box cellspacing="0">' + sLineBreak +
'<tr class="header" style="padding-left: 4px; padding-right: 4px;">' + sLineBreak +
'<th height="19px" align=left bgcolor=#F0F0F0 width="50px">col2</th><th height="19px" align=left bgcolor=#F0F0F0 width="50px">col3</th><th height="19px" align=left bgcolor=#F0F0F0 width="50px">col4</th></tr>' + sLineBreak +
' <tr class="default">' + sLineBreak +
' <td class="normalborder"  height="18px" align=left>3b</td> <td class="normalborder"  height="18px" align=left>3c</td> <td class="normalborder"  height="18px" align=left>3d</td> </tr>' + sLineBreak +
' <tr class="default">' + sLineBreak +
' <td class="normalborder"  height="18px" align=left>4b</td> <td class="normalborder"  height="18px" align=left>4c</td> <td class="normalborder"  height="18px" align=left>4d</td> </tr>' + sLineBreak +
'</table><!--EndFragment--></body></html>');
end;

procedure TCellSelectionTests.TestCopyHTML2;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n5 := FNode5;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n5, col4
  var LText := ''; var LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n5, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);
    CompleteClipboardCopy;
    try
      LText := Clipboard.AsHTML;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      else
        raise;
    end;
    Inc(LTries);
  until (LText <> '') and (LTries <= MaxTries);
  Assert.IsTrue(LText =
'Version:1.0' + sLineBreak +
'StartHTML:00000097' + sLineBreak +
'EndHTML:00001945' + sLineBreak +
'StartFragment:00000269' + sLineBreak +
'EndFragment:00001913' + sLineBreak +
'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"><html><head><META http-equiv=Content-Type content="text/html; charset=utf-8"></head><body><!--StartFragment--><META http-equiv="Content-Type" content="text/html; charset=utf-8"><style type="tex' +
't/css">' + sLineBreak +
'.default{font-family: ''Tahoma''; font-size: 8pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}' + sLineBreak +
'.header{font-family: ''Tahoma''; font-size: 8pt; font-style: normal; font-weight: normal; text-decoration: none; color: #000000;}' + sLineBreak +
'.noborder{border-style: none;padding-left: 4px; padding-right: 4px;}' + sLineBreak +
'.normalborder {vertical-align: top; border-right: none; border-left:none; border-top:none; border-bottom: none;border-width: thin; border-style: dotted;padding-left: 4px; padding-right: 4px;}</style>' + sLineBreak +
'<table class="default" style="border-collapse: collapse;" bgcolor=#FFFFFF  border="1" frame=box cellspacing="0">' + sLineBreak +
'<tr class="header" style="padding-left: 4px; padding-right: 4px;">' + sLineBreak +
'<th height="19px" align=left bgcolor=#F0F0F0 width="50px">col2</th><th height="19px" align=left bgcolor=#F0F0F0 width="50px">col3</th><th height="19px" align=left bgcolor=#F0F0F0 width="50px">col4</th></tr>' + sLineBreak +
' <tr class="default">' + sLineBreak +
' <td class="normalborder"  height="18px" align=left>3b</td> <td class="normalborder"  height="18px" align=left>3c</td> <td class="normalborder"  height="18px" align=left>3d</td> </tr>' + sLineBreak +
' <tr class="default">' + sLineBreak +
' <td class="normalborder"  height="18px" align=left>4b</td> <td class="normalborder"  height="18px" align=left>4c</td> <td class="normalborder"  height="18px" align=left>4d</td> </tr>' + sLineBreak +
' <tr class="default">' + sLineBreak +
' <td class="normalborder"  height="18px" align=left>5b</td> <td class="normalborder"  height="18px" align=left>5c</td> <td class="normalborder"  height="18px" align=left>5d</td> </tr>' + sLineBreak +
'</table><!--EndFragment--></body></html>');
end;

procedure TCellSelectionTests.TestCopyPlainText1;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n4, col4
  var LText := ''; var LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n4, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    try
      LText := Clipboard.AsText;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      else
        raise;
    end;
    Inc(LTries);
  until (LText <> '') and (LTries <= MaxTries);
  Assert.IsTrue(LText = 'col2	col3	col4' + sLineBreak +
'3b	3c	3d' + sLineBreak +
'4b	4c	4d' + sLineBreak +
'');
end;

procedure TCellSelectionTests.TestCopyPlainText2;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n5 := FNode5;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n5, col4
  var LText := ''; var LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n5, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    try
      LText := Clipboard.AsText;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      else
        raise;
    end;
    Inc(LTries);
  until (LText <> '') and (LTries <= MaxTries);
  Assert.IsTrue(LText = 'col2	col3	col4' + sLineBreak +
'3b	3c	3d' + sLineBreak +
'4b	4c	4d' + sLineBreak +
'5b	5c	5d' + sLineBreak
  );
end;

procedure TCellSelectionTests.TestCopyRTF1;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n4, col4
  var LText := ''; var LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n4, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    try
      LText := Clipboard.AsRTF;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      else
        raise;
    end;
    Inc(LTries);
  until (LText <> '') and (LTries <= MaxTries);
  Assert.IsTrue(LText = '{\rtf1\ansi\ansicpg1252\deff0\deflang1043{\fonttbl{\f0 Tahoma;}}{\colortbl;\red0\green0\blue0;}\paperw16840\paperh11907\margl720\margr720\margt720\margb720\uc1\trowd\trgaph70\cellx750\cellx1500\cellx2250\pard\intbl\ql\f0\cf1\fs16 \u99\''3f\u111\''3f\u108\''3' +
'f\u50\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u51\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u52\''3f\cell\row\pard\intbl \u51\''3f\u98\''3f\cell\pard\intbl \u51\''3f\u99\''3f\cell\pard\intbl \u51\''3f\u100\''3f\cell\row' + sLineBreak +
'\pard\intbl \u52\''3f\u98\''3f\cell\pard\intbl \u52\''3f\u99\''3f\cell\pard\intbl \u52\''3f\u100\''3f\cell\row' + sLineBreak +
'\pard\par}');
end;

procedure TCellSelectionTests.TestCopyRTF2;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n5 := FNode5;

// Occasional failure caused by clipboard copy issues, but this
// is due to interaction of complex interaction of Windows, console and
// DUnit testing, the failure is not seen in actual interacting applications

  // Select rectangle from n3, col2 to n5, col4
  var LText := ''; var LTries := 0;
  repeat
    LTree.SelectCells(n3, 1, n5, 3, False);
    LTree.CopyToClipboard;

    // The following are not necessary in an actual application
    Sleep(0);

    CompleteClipboardCopy;
    try
      LText := Clipboard.AsRTF;
    except
      // Clipboard exception is ok, anything else is not
      on EClipboardException do
      else
        raise;
    end;
    Inc(LTries);
    // End unnecessary stuff
  until (LText <> '') and (LTries <= MaxTries);
  Assert.IsTrue(LText = '{\rtf1\ansi\ansicpg1252\deff0\deflang1043{\fonttbl{\f0 Tahoma;}}{\colortbl;\red0\green0\blue0;}\paperw16840\paperh11907\margl720\margr720\margt720\margb720\uc1\trowd\trgaph70\cellx750\cellx1500\cellx2250\pard\intbl\ql\f0\cf1\fs16 \u99\''3f\u111\''3f\u108\''3' +
'f\u50\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u51\''3f\cell\ql \u99\''3f\u111\''3f\u108\''3f\u52\''3f\cell\row\pard\intbl \u51\''3f\u98\''3f\cell\pard\intbl \u51\''3f\u99\''3f\cell\pard\intbl \u51\''3f\u100\''3f\cell\row' + sLineBreak +
'\pard\intbl \u52\''3f\u98\''3f\cell\pard\intbl \u52\''3f\u99\''3f\cell\pard\intbl \u52\''3f\u100\''3f\cell\row' + sLineBreak +
'\pard\intbl \u53\''3f\u98\''3f\cell\pard\intbl \u53\''3f\u99\''3f\cell\pard\intbl \u53\''3f\u100\''3f\cell\row' + sLineBreak +
'\pard\par}');
end;

procedure TCellSelectionTests.TestSelectCellsRectangular;
begin

  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

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

  var LNodes := [FNode1, FNode2, FNode5, FNode6, FNode7, FNode8];
  for var LNode in LNodes do
    for var LColumn := 0 to 3 do
      Assert.IsFalse(LTree.IsCellSelected(LNode, LColumn),
        Format('Row: $%p Column: %d should not be selected', [Pointer(LNode), LColumn]));
end;

procedure TCellSelectionTests.TestSelectMultipleCellsFailWithoutExtendedFocus;
begin
  var LTree := FTree;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions -
    [toExtendedFocus];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  // Ensure the selected cells above are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');
end;

procedure TCellSelectionTests.TestSelectMultipleCellsFailWithoutMultiSelect;
begin
  var LTree := FTree;
  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions -
    [toMultiSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  // Select rectangle from n3, col2 to n2, col3
  LTree.SelectCells(n3, 1, n4, 2, False);

  // Ensure the selected cells above are not selected
  Assert.IsFalse(LTree.IsCellSelected(n3, 1), 'n3, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n3, 2), 'n3, col2 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 1), 'n4, col1 should not be selected');
  Assert.IsFalse(LTree.IsCellSelected(n4, 2), 'n4, col2 should not be selected');
end;

procedure TCellSelectionTests.TestSelectSingleCell;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect];

  var n3 := FNode3;
  LTree.SelectCells(n3, 1, n3, 1, False);

  Assert.IsTrue(LTree.IsCellSelected(n3, 1), 'n3, col1 should be selected');
  var LSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LSelectedCells) = 1, 'Should only have 1 cell selected');
  Assert.IsTrue((LSelectedCells[0].Node = n3) and (LSelectedCells[0].Column = 1));

  var LNodes := [FNode1, FNode2, n3, FNode4, FNode5, FNode6, FNode7, FNode8];
  for var LNode in LNodes do
    for var LColumn := 0 to 3 do
    begin
      if (LNode = n3) and (LColumn = 1) then
        Continue;
      Assert.IsFalse(LTree.IsCellSelected(LNode, LColumn),
        Format('Row: $%p Column: %d should not be selected', [Pointer(LNode), LColumn]));
    end;
end;

procedure TCellSelectionTests.TestShiftClickMultipleCells;
begin
  var LTree := FTree;

  LTree.TreeOptions.SelectionOptions := LTree.TreeOptions.SelectionOptions +
    [toExtendedFocus, toMultiSelect] - [toFullRowSelect];

  var n3 := FNode3;
  var n4 := FNode4;

  MouseClick(n3);
  var LSelectedCells := LTree.SelectedCells;
  ShiftMouseClick(n4);
  var LNewSelectedCells := LTree.SelectedCells;
  Assert.IsTrue(Length(LNewSelectedCells) = 2);
  Assert.IsTrue((LNewSelectedCells[0].Node = n3) and (LNewSelectedCells[0].Column = 0));
  Assert.IsTrue((LNewSelectedCells[1].Node = n4) and (LNewSelectedCells[0].Column = 0));
end;

procedure TCellSelectionTests.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if not Assigned(Node) then
    Exit;
  var LData := Node.GetData<TRowData>;
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
