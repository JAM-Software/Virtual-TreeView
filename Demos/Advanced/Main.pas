unit Main;

// Advanced demo for Virtual Treeview showing various effects and features in several forms.
// This is the main form which serves as container window for the demo forms.
// Written by Mike Lischke.

interface

// For some things to work we need code, which is classified as being unsafe for .NET.
{$warn UNSAFE_TYPE off}
{$warn UNSAFE_CAST off}
{$warn UNSAFE_CODE off}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, Buttons, ExtCtrls, StdCtrls, ImgList, ActnList,
  StdActns, VirtualTrees;

type
  TMainForm = class(TForm)
    PageScroller1: TPageScroller;
    SpeedDemoButton: TSpeedButton;
    AbilitiesDemoButton: TSpeedButton;
    PropertiesDemoButton: TSpeedButton;
    VisibilityDemoButton: TSpeedButton;
    GridDemoButton: TSpeedButton;
    AlignDemoButton: TSpeedButton;
    QuitButton: TSpeedButton;
    PaintTreeDemoButton: TSpeedButton;
    Bevel1: TBevel;
    MainPanel: TPanel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    StatusBar: TStatusBar;
    ContainerPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    XPDemoButton: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure QuitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DemoButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  MainForm: TMainForm;

procedure ConvertToHighColor(ImageList: TImageList);
procedure LoadUnicodeStrings(Name: string; var Strings: array of UnicodeString);
procedure SetStatusbarText(const S: string);

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  CommCtrl, VirtualTrees.Accessibility,
  SpeedDemo, GeneralAbilitiesDemo, DrawTreeDemo, PropertiesDemo,
  GridDemo, VisibilityDemo, AlignDemo, WindowsXPStyleDemo, MultilineDemo, HeaderCustomDrawDemo,
  States;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

procedure ConvertToHighColor(ImageList: TImageList);

// To show smooth images we have to convert the image list from 16 colors to high color.

var
  IL: TImageList;

begin
  // Have to create a temporary copy of the given list, because the list is cleared on handle creation.
  IL := TImageList.Create(nil);
  IL.Assign(ImageList);

  with ImageList do
    Handle := ImageList_Create(Width, Height, ILC_COLOR16 or ILC_MASK, Count, AllocBy);
  ImageList.Assign(IL);
  IL.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure LoadUnicodeStrings(Name: string; var Strings: array of UnicodeString);

// Loads the Unicode strings from the resource.

var
  Stream: TResourceStream;
  Head, Tail: PWideChar;
  I: Integer;

begin
  Stream := TResourceStream.Create(0, Name, 'Unicode');
  try
    Head := Stream.Memory;
    // Skip byte order mark.
    Inc(Head);
    Tail := Head;
    for I := 0 to High(Strings) do
    begin
      Head := Tail;
      while not (Ord(Tail^) in [0, 13]) do
        Inc(Tail);
      SetString(Strings[I], Head, Tail - Head);
      // Skip carriage return and linefeed.
      Inc(Tail, 2);
    end;
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetStatusbarText(const S: string);

begin
  MainForm.StatusBar.SimpleText := S;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.QuitButtonClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  // Show hints 10 seconds.
  Application.HintHidePause := 10000;
  System.ReportMemoryLeaksOnShutdown:= true;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DemoButtonClick(Sender: TObject);

// This method is a kind of scheduler. Here we switch between the demo forms.

var
  NewDemoClass: TFormClass;
  NewDemo: TForm;

begin
  case (Sender as TSpeedButton).Tag of
    0:
      NewDemoClass := TSpeedForm;
    1:
      NewDemoClass := TGeneralForm;
    2:
      NewDemoClass := TPropertiesForm;
    3:
      NewDemoClass := TVisibilityForm;
    5:
      NewDemoClass := TGridForm;
    6:
      NewDemoClass := TDrawTreeForm;
    7:
      NewDemoClass := TAlignForm;
    8:
      NewDemoClass := TWindowsXPForm;
    9:
      NewDemoClass := TNodeForm;
    10:
      NewDemoClass := THeaderOwnerDrawForm;
  else
    NewDemoClass := nil;
  end;

  if (ContainerPanel.ControlCount = 0) or not (ContainerPanel.Controls[0] is NewDemoClass) then
  begin
    if ContainerPanel.ControlCount > 0 then
      ContainerPanel.Controls[0].Free;

    if Assigned(NewDemoClass) then
    begin
      NewDemo := NewDemoClass.Create(Self);
      NewDemo.BorderStyle := bsNone;
      NewDemo.Align := alClient;
      NewDemo.Parent := ContainerPanel;
      NewDemo.Show;
      {$if CompilerVersion >= 33}
      NewDemo.ScaleForPPI(FCurrentPPI); // See issue #990
      {$endif}
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormShow(Sender: TObject);

begin
  StateForm.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

end.


