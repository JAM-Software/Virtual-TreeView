unit HeaderCustomDrawDemo;

// Virtual Treeview sample form demonstrating following features:
//   - Advanced header custom draw.
// Written by Mike Lischke.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, VirtualTrees, StdCtrls, ExtCtrls, VirtualTrees.BaseTree, System.ImageList,
  VirtualTrees.Types;

type
  THeaderOwnerDrawForm = class(TForm)
    Label8: TLabel;
    HeaderCustomDrawTree: TVirtualStringTree;
    HeaderImages: TImageList;
    AnimationTimer: TTimer;
    procedure HeaderCustomDrawTreeHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements);
    procedure HeaderCustomDrawTreeAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      const Elements: THeaderPaintElements);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);
    procedure HeaderCustomDrawTreeHeaderMouseUp(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure HeaderCustomDrawTreeHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure HeaderCustomDrawTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure HeaderCustomDrawTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    FBackBitmap1,
    FBackBitmap2,
    FCheckerBackground: TBitmap;
    FHeaderBitmap: TBitmap;
    FLeftPos: Integer;
    procedure CreateCheckerBackground;
    procedure PaintSelection(Bitmap: TBitmap);
    procedure FillBackground(R: TRect; Target: TCanvas);
  end;

var
  HeaderOwnerDrawForm: THeaderOwnerDrawForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  States, Types,VirtualTrees.Utils;
  
{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.HeaderCustomDrawTreeHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);

// This event tells the tree which part we want to draw ourselves. Don't forget to enable custom drawing in the header
// options and switch the Style property of every column, which we handle here to vsOwnerDraw.

begin
  with PaintInfo do
  begin
    // First check the column member. If it is NoColumn then it's about the header background.
    if Column = nil then
      Elements := [hpeBackground] // No other flag is recognized for the header background.
    else
    begin
      // Using the index here ensures a column, regardless of its position, always has the same draw style.
      // By using the Position member, we could make a certain column place stand out, regardless of the column order.
      // Don't forget to change the AdvancedHeaderDraw event body accordingly after you changed the indicator here.
      case Column.Index of
        0: // Default drawing.
          ;
        1: // Background only customization.
          Include(Elements, hpeBackground);
        2: // Full customization (well, quite).
          Elements := [hpeBackground, hpeText{, hpeDropMark, hpeHeaderGlyph, hpeSortGlyph}];
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.HeaderCustomDrawTreeAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  const Elements: THeaderPaintElements);

var
  S: string;
  Size: TSize;
  SourceRect,
  TargetRect: TRect;

begin
  with PaintInfo do
  begin
    // First check the column member. If it is NoColumn then it's about the header background.
    if Column = nil then
    begin
      if hpeBackground in Elements then
      begin
        TargetCanvas.Brush.Color := clBackground;
        TargetCanvas.FillRect(PaintRectangle);
      end;
    end
    else
    begin
      case Column.Index of
        0: // Will never come here.
          ;
        1: // Background only customization.
          begin
            FBackBitmap1.Width := PaintRectangle.Right - PaintRectangle.Left;
            FBackBitmap1.Height := PaintRectangle.Bottom - PaintRectangle.Top;
            FillBackground(PaintRectangle, FBackbitmap1.Canvas);
            if IsHoverIndex then
              PaintSelection(FBackBitmap1);
            TargetCanvas.Draw(PaintRectangle.Left, Paintrectangle.Top, FBackbitmap1);
          end;
        2: // Full customization. Check elements to learn what must be drawn in the various stages.
          begin
            if hpeBackground in Elements then
              with FBackBitmap2 do
              begin
                Width := PaintRectangle.Right - PaintRectangle.Left;
                Height := PaintRectangle.Bottom - PaintRectangle.Top;
                TargetRect := Rect(0, 0, Width, Height);
                Canvas.Brush.Color := clInfoBk;
                Canvas.FillRect(TargetRect);
                InflateRect(TargetRect, - 10, -10);
                SourceRect := TargetRect;
                OffsetRect(SourceRect, -SourceRect.Left + FLeftPos, -SourceRect.Top);
                Canvas.CopyRect(TargetRect, FHeaderBitmap.Canvas, SourceRect);

                TargetCanvas.Draw(PaintRectangle.Left, Paintrectangle.Top, FBackbitmap2);
              end;
            if hpeText in Elements then
            begin
              TargetCanvas.Font.Name := 'Webdings';
              TargetCanvas.Font.Charset := SYMBOL_CHARSET;
              TargetCanvas.Font.Size := 60;
              if IsHoverIndex then
                TargetCanvas.Font.Color := $80FF;
              S := 'û';
              Size := TargetCanvas.TextExtent(S);
              SetBkMode(TargetCanvas.Handle, TRANSPARENT);
              TargetCanvas.TextOut(PaintRectangle.Left + 10, Paintrectangle.Bottom - Size.cy, S);
            end;
            // Other elements go here.
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.CreateCheckerBackground;

begin
  FCheckerBackground := TBitmap.Create;
  with FCheckerBackground do
  begin
    Width := 16;
    Height := 16;
    Canvas.Brush.Color := clBtnShadow;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Brush.Color := clBtnHighlight;
    Canvas.FillRect(Rect(0, 0, 8, 8));
    Canvas.FillRect(Rect(8, 8, 16, 16));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.PaintSelection(Bitmap: TBitmap);

const
  Alpha = 75;

var
  R: TRect;

begin
  R := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  VirtualTrees.Utils.AlphaBlend(0, Bitmap.Canvas.Handle, R, Point(0, 0), bmConstantAlphaAndColor, Alpha,
    ColorToRGB(clHighlight));
  with Bitmap do
  begin
    Canvas.Pen.Color := clHighlight;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.FillBackground(R: TRect; Target: TCanvas);

// Tiles the background image over the given target bitmap.

var
  X, Y: Integer;
  dX, dY: Integer;

begin
  with Target do
  begin
    dX := FCheckerBackground.Width;
    dY := FCheckerBackground.Height;

    Y := 0;
    while Y < R.Bottom - R.Top do
    begin
      X := 0;
      while X < R.Right - R.Left do
      begin
        Draw(X, Y, FCheckerBackground);
        Inc(X, dX);
      end;
      Inc(Y, dY);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.FormCreate(Sender: TObject);

begin
  FBackBitmap1 := TBitmap.Create;
  FBackBitmap1.PixelFormat := pf32Bit;
  FBackBitmap2 := TBitmap.Create;
  FBackBitmap2.PixelFormat := pf32Bit;
  CreateCheckerBackground;
  FHeaderBitmap := TBitmap.Create;
  FHeaderBitmap.Handle := LoadImage(HInstance, 'Transcriptions', IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.FormDestroy(Sender: TObject);

begin
  FCheckerBackground.Free;
  FBackBitmap1.Free;
  FBackBitmap2.Free;
  FHeaderBitmap.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.AnimationTimerTimer(Sender: TObject);

begin
  FLeftPos := (FLeftPos + FHeaderBitmap.Width div 2000) mod FHeaderBitmap.Width;
  with HeaderCustomDrawTree.Header do
    Invalidate(Columns[2]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.HeaderCustomDrawTreeHeaderMouseUp(Sender: TVTHeader; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  // Reenable animation after a drag operation.
  AnimationTimer.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.HeaderCustomDrawTreeHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  // Stop animation when mouse button is down.
  AnimationTimer.Enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.HeaderCustomDrawTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THeaderOwnerDrawForm.HeaderCustomDrawTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

begin
  CellText := 'Some simple text.';
end;

//----------------------------------------------------------------------------------------------------------------------

end.
