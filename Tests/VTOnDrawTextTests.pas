unit VTOnDrawTextTests;

interface

uses
  DUnitX.TestFramework,
  Vcl.Forms,
  VirtualTrees, System.Types;

type

  [TestFixture]
  TVTOnDrawTextTests = class
  strict private
    fTree: TVirtualStringTree;
    fForm: TForm;

    FDrawText1Called: Boolean;
    FDrawTextEx1Called: Boolean;

    FDrawText2Called: Boolean;
    FDrawTextEx2Called: Boolean;

    FDrawText3Called: Boolean;
    FDrawTextEx3Called: Boolean;

    procedure DrawText1Event(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);

    procedure DrawTextEx2Event(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean; var DrawFormat: Cardinal);

    procedure DrawText3Event(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure DrawTextEx3Event(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean; var DrawFormat: Cardinal);

    procedure GetTextEvent(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestOnDrawText;

    [Test]
    procedure TestOnDrawTextOnDrawTextEx;

    [Test]
    procedure TestOnDrawTextEx;
  end;

implementation

uses
  System.SysUtils, VirtualTrees.Types;

const
  colCaption = 0;
  colData    = 1;

procedure TVTOnDrawTextTests.DrawText1Event(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  FDrawText1Called := True;
end;

procedure TVTOnDrawTextTests.DrawText3Event(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  FDrawText3Called := True;
end;

procedure TVTOnDrawTextTests.DrawTextEx2Event(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean;
  var DrawFormat: Cardinal);
begin
  FDrawTextEx2Called := True;
end;

procedure TVTOnDrawTextTests.DrawTextEx3Event(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean;
  var DrawFormat: Cardinal);
begin
  FDrawTextEx3Called := True;
end;

procedure TVTOnDrawTextTests.GetTextEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  case Column of
    colCaption: begin
      CellText := 'Caption';
    end;
    colData: begin
      CellText := 'Data';
    end;
  end;
end;

procedure TVTOnDrawTextTests.Setup;
begin
  FDrawText1Called := False;
  FDrawTextEx1Called := False;

  FDrawText2Called := False;
  FDrawTextEx2Called := False;

  FDrawText3Called := False;
  FDrawTextEx3Called := False;

  fForm := TForm.Create(nil);
  fTree := TVirtualStringTree.Create(fForm);
  fForm.InsertControl(fTree);

  fTree.OnGetText := GetTextEvent;

  var LCol1 := fTree.Header.Columns.Add;
  var LCol2 := fTree.Header.Columns.Add;
  LCol1.Text := 'Caption';
  LCol2.Text := 'Data';

  fTree.AddChild(fTree.RootNode);
  fTree.AddChild(fTree.RootNode);
  fForm.Show;
end;

procedure TVTOnDrawTextTests.TearDown;
begin
  FreeAndNil(fForm);
end;

procedure TVTOnDrawTextTests.TestOnDrawText;
begin
  // This test ensures that OnDrawText event is called when OnDrawText is assigned
  fTree.OnDrawText := DrawText1Event;
  fTree.OnDrawTextEx := nil;
  fTree.Update;

  Assert.IsTrue(FDrawText1Called and not FDrawTextEx1Called);
end;

procedure TVTOnDrawTextTests.TestOnDrawTextEx;
begin
  // This test ensures that OnDrawTextEx event is called when OnDrawTextEx is assigned
  // and that OnDrawText is not called
  fTree.OnDrawText := nil;
  fTree.OnDrawTextEx := DrawTextEx2Event;
  fTree.Update;

  Assert.IsTrue(not FDrawText2Called and FDrawTextEx2Called);
end;

procedure TVTOnDrawTextTests.TestOnDrawTextOnDrawTextEx;
begin
  // This test ensures that only the OnDrawTextEx event is called when both
  // OnDrawText and OnDrawTextEx are assigned and that OnDrawText is not called
  fTree.OnDrawText := DrawText3Event;
  fTree.OnDrawTextEx := DrawTextEx3Event;
  fTree.Update;

  Assert.IsTrue(not FDrawText3Called and FDrawTextEx3Called);
end;

initialization
  TDUnitX.RegisterTestFixture(TVTOnDrawTextTests);
end.
