unit VirtualTrees.BaseAncestorFMX;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski                                                                         }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface
uses VirtualTrees.BaseTree;

type
  TVTAncestorFMX = class abstract(TBaseVirtualTree)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    function PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush; override;

    function GetClientHeight: Single; override;
    function GetClientWidth: Single; override;
    function GetClientRect: TRect; override;												   

    //TODO: CopyCutPaste - need to be implemented
    {
    function PasteFromClipboard(): Boolean; override;
    procedure CopyToClipboard(); override;
    procedure CutToClipboard(); override;
    }
  end;

implementation

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); //wymaga BaseTree
Var MM: TWMMouse;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P.X:= X;
  P.Y:= Y;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  FillTWMMouse(MM, Button, Shift, P.X, P.Y, isNC, false);
  if FHeader.HandleMessage(TMessage(MM)) then
    exit;//!!!

  FillTWMMouse(MM, Button, Shift, X, Y, isNC, false);
  // get information about the hit
  GetHitTestInfoAt(X, Y, True, hInfo);

  HandleMouseDown(MM, hInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); //wymaga BaseTree
Var MM: TWMMouse;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P.X:= X;
  P.Y:= Y;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  FillTWMMouse(MM, Button, Shift, P.X, P.Y, isNC, true);
  if FHeader.HandleMessage(TMessage(MM)) then
    exit;//!!!

  FillTWMMouse(MM, Button, Shift, X, Y, isNC, true);
  // get information about the hit
  GetHitTestInfoAt(X, Y, True, hInfo);
  HandleMouseUp(MM, hInfo);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorFMX.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //wymaga BaseTree
Var M: TCMMouseWheel;
  hInfo: THitInfo;
  P: TPoint;
  isNC: Boolean;
begin
  P:= Screen.MousePos;
  if ClientRect.Contains(P) then
    begin
      isNc:= false;
    end else
    begin
      isNC:= true;
      P:= ClientToScreen(P);
    end;
  M.Msg:= CM_MOUSEWHEEL;
  M.ShiftState:= Shift;
  M.WheelDelta:= WheelDelta;
  M.XPos:= P.X;
  M.YPos:= P.Y;
  M.Result:= 0;
  CMMouseWheel(M);
  Handled:= M.Result<>0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.PrepareDottedBrush(CurrentDottedBrush: TBrush; Bits: Pointer; const BitsLinesCount: Word): TBrush;
Var PatternBitmap: TBitmap;
  i_bmp, line, bit: Integer;
begin
  //FMX pattern brush is different then VCL. Where color is derived from current one...
  //We should have 2 brushes 1 for Tree lines 1 for grid lines
  //and recreate it every time when color is changing

  CurrentDottedBrush.Free;
  FDottedBrushGridLines.Free;

  Result := nil;
  for i_bmp:= 1 to 2 do
    begin
      PatternBitmap := TBitmap.Create(8, BitsLinesCount);
      PatternBitmap.Clear(TAlphaColorRec.Null); //fully transparent
      PatternBitmap.Canvas.BeginScene;

      PatternBitmap.Map(TMapAccess.Write, BitmapData);
      try
        {
        DestPitch := PixelFormatBytes[PatternBitmap.PixelFormat];
        System.Move(PAlphaColorArray(BitmapData.Data)[0], PAlphaColorArray(Bits)[0], 8 * 4);
        }
        for line:= 0 to LineLen-1 do
          begin
            for bit:= 0 to 7 do
              begin
                if PWordArray(Bits)^[line] and (1 shl bit)=0 then
                  BitmapData.SetPixel(bit, line, clWhite) else
                  begin
                    if i_bmp=1 then
                      BitmapData.SetPixel(bit, line, TreeColors.TreeLineColor) else
                      BitmapData.SetPixel(bit, line, TreeColors.GridLineColor);
                  end;
              end;
          end;
      finally
        PatternBitmap.UnMap(BitmapData);
      end;

      PatternBitmap.Canvas.EndScene;

      if i_bmp=1 then
        begin
          Result := TStrokeBrush.Create(TBrushKind.Bitmap, clWhite);
          Result.Bitmap.Bitmap.Assign(PatternBitmap);
        end else
        begin
          FDottedBrushGridLines := TStrokeBrush.Create(TBrushKind.Bitmap, clWhite);
          FDottedBrushGridLines.Bitmap.Bitmap.Assign(PatternBitmap);
        end;
      FreeAndNil(PatternBitmap);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.GetClientHeight: Single;
begin
  Result:= ClientRect.Height;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.GetClientWidth: Single;
begin
  Result:= ClientRect.Width;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorFMX.GetClientRect: TRect;
begin
  Result:= ClipRect;
  if Assigned(Header) then
    begin
      if hoVisible in Header.Options then
        Inc(Result.Top, Header.Height);
    end;
  if FVScrollBar.Visible then
    Dec(Result.Right, VScrollBar.Width);
  if HScrollBar.Visible then
    Dec(Result.Bottom, HScrollBar.Height);
    
  if Result.Left>Result.Right then
    Result.Left:= Result.Right;
    
  if Result.Top>Result.Bottom then
    Result.Top:= Result.Bottom;

  //OffsetRect(Result, OffsetX, OffsetY);
  //Dec(Result.Left, -OffsetX); //increase width
  //Dec(Result.Top, -OffsetY);  //increase height
end;

end.
