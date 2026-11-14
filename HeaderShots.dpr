program HeaderShots;

{$R HeaderShotsManifest.res}

// Renders the Virtual TreeView header in several variants into a PNG contact sheet,
// to check the look of hoSortGlyphAboveCaption (issue #1242).

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,
  Winapi.CommCtrl,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.StrUtils,
  Vcl.ImgList,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Imaging.pngimage,
  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.Header,
  VirtualTrees.BaseTree;

type
  TTreeCracker = class(TBaseVirtualTree);
  TColumnsCracker = class(TVirtualTreeColumns);
  TThemeMode = (tmClassic, tmThemed, tmExplorer);

const
  StylesDir = 'C:/Users/Public/Documents/Embarcadero/Studio/37.0/Styles/';
  Zoom = 3;
  HeaderWidth = 420;

var
  Form: TForm;
  Sheet: Vcl.Graphics.TBitmap;
  SheetY, SheetMaxX: Integer;
  OutDir: string;
  ShotHeight: Integer = 19;

procedure AddShot(const ALabel: string; ABmp: Vcl.Graphics.TBitmap);
begin
  Sheet.Canvas.Font.Name := 'Segoe UI';
  Sheet.Canvas.Font.Size := 11;
  Sheet.Canvas.Font.Color := clBlack;
  Sheet.Canvas.Brush.Style := bsClear;
  Sheet.Canvas.TextOut(8, SheetY + 4, ALabel);
  Sheet.Canvas.Brush.Style := bsSolid;
  Inc(SheetY, 26);
  SetStretchBltMode(Sheet.Canvas.Handle, COLORONCOLOR);
  Sheet.Canvas.StretchDraw(Rect(8, SheetY, 8 + ABmp.Width * Zoom, SheetY + ABmp.Height * Zoom), ABmp);
  SheetMaxX := Max(SheetMaxX, 8 + ABmp.Width * Zoom + 8);
  Inc(SheetY, ABmp.Height * Zoom + 14);
end;

procedure Shot(const ALabel: string; AMode: TThemeMode; AAbove: Boolean; AConfigure: TProc<TVirtualStringTree> = nil;
  APPI: Integer = 96);
var
  Tree: TVirtualStringTree;
  Bmp: Vcl.Graphics.TBitmap;
  Col: TVirtualTreeColumn;
begin
  Tree := TVirtualStringTree.Create(Form);
  try
    Tree.Parent := Form;
    Tree.SetBounds(0, 0, HeaderWidth + 40, 150);
    if AMode = tmExplorer then
      Tree.TreeOptions.PaintOptions := Tree.TreeOptions.PaintOptions + [toUseExplorerTheme];
    Col := Tree.Header.Columns.Add;
    Col.Text := 'Name';
    Col.Width := 160;
    Col := Tree.Header.Columns.Add;
    Col.Text := 'Date modified';
    Col.Width := 140;
    Col := Tree.Header.Columns.Add;
    Col.Text := 'Size';
    Col.Width := 120;
    Tree.Header.Options := Tree.Header.Options + [hoVisible, hoShowSortGlyphs];
    if AAbove then
      Tree.Header.Options := Tree.Header.Options + [hoSortGlyphAboveCaption];
    Tree.Header.SortColumn := 0;
    Tree.HandleNeeded;
    Tree.Header.Height := ShotHeight;
    if APPI <> 96 then
      Tree.ScaleForPPI(APPI);
    if Assigned(AConfigure) then
      AConfigure(Tree);
    Application.ProcessMessages;
    if AMode = tmClassic then
      TTreeCracker(Tree).DoStateChange([], [tsUseThemes, tsUseExplorerTheme]);

    Bmp := Vcl.Graphics.TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.SetSize(MulDiv(HeaderWidth, APPI, 96), Tree.Header.Height);
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Tree.Header.Columns.PaintHeader(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height), Point(0, 0));
      AddShot(Format('%s   (header height %d px)', [ALabel, Tree.Header.Height]), Bmp);
    finally
      Bmp.Free;
    end;
  finally
    Tree.Free;
  end;
end;

procedure SaveSheet(const AFileName: string);
var
  Cropped: Vcl.Graphics.TBitmap;
  Png: TPngImage;
begin
  Cropped := Vcl.Graphics.TBitmap.Create;
  Png := TPngImage.Create;
  try
    Cropped.PixelFormat := pf24bit;
    Cropped.SetSize(SheetMaxX, SheetY);
    Cropped.Canvas.CopyRect(Rect(0, 0, SheetMaxX, SheetY), Sheet.Canvas, Rect(0, 0, SheetMaxX, SheetY));
    Png.Assign(Cropped);
    Png.SaveToFile(OutDir + AFileName);
  finally
    Png.Free;
    Cropped.Free;
  end;
end;

procedure NewSheet;
begin
  Sheet.Canvas.Brush.Color := RGB(230, 230, 230);
  Sheet.Canvas.FillRect(Rect(0, 0, Sheet.Width, Sheet.Height));
  SheetY := 4;
  SheetMaxX := 0;
end;

begin
  OutDir := ExtractFilePath(ParamStr(0));
  Application.Initialize;
  Form := TForm.CreateNew(nil);
  Sheet := Vcl.Graphics.TBitmap.Create;
  try
    Form.SetBounds(-2000, -2000, HeaderWidth + 80, 220);
    Form.Font.Name := 'Segoe UI';
    Form.Font.Size := 9;
    Form.Show;
    Sheet.PixelFormat := pf24bit;
    Sheet.SetSize(1700, 3000);

    // Sheet 1: Explorer theme at the default header height, default placement vs. hoSortGlyphAboveCaption.
    NewSheet;
    Shot('Default: sort glyph next to the caption', tmExplorer, False);
    Shot('hoSortGlyphAboveCaption', tmExplorer, True);
    Shot('hoSortGlyphAboveCaption, descending', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin T.Header.SortDirection := sdDescending; end);
    Shot('Narrow sort column (70 px), default', tmExplorer, False,
      procedure(T: TVirtualStringTree) begin T.Header.Columns[1].Width := 70; T.Header.SortColumn := 1; end);
    Shot('Narrow sort column (70 px), hoSortGlyphAboveCaption', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin T.Header.Columns[1].Width := 70; T.Header.SortColumn := 1; end);
    Shot('Centered caption, default', tmExplorer, False,
      procedure(T: TVirtualStringTree) begin T.Header.Columns[1].CaptionAlignment := taCenter; T.Header.SortColumn := 1; end);
    Shot('Centered caption, hoSortGlyphAboveCaption', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin T.Header.Columns[1].CaptionAlignment := taCenter; T.Header.SortColumn := 1; end);
    Shot('Right aligned caption, default', tmExplorer, False,
      procedure(T: TVirtualStringTree) begin T.Header.Columns[2].CaptionAlignment := taRightJustify; T.Header.SortColumn := 2; end);
    Shot('Right aligned caption, hoSortGlyphAboveCaption', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin T.Header.Columns[2].CaptionAlignment := taRightJustify; T.Header.SortColumn := 2; end);
    Shot('Right-to-left, default', tmExplorer, False,
      procedure(T: TVirtualStringTree) begin T.BiDiMode := bdRightToLeft; end);
    Shot('Right-to-left, hoSortGlyphAboveCaption', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin T.BiDiMode := bdRightToLeft; end);
    SaveSheet('issue-1242-explorer-theme.png');

    // Sheet 2: other paint modes and states, all with hoSortGlyphAboveCaption unless noted.
    NewSheet;
    Shot('Themed without Explorer theme, default', tmThemed, False);
    Shot('Themed without Explorer theme, hoSortGlyphAboveCaption', tmThemed, True);
    Shot('Classic (no themes), default', tmClassic, False);
    Shot('Classic (no themes), hoSortGlyphAboveCaption', tmClassic, True);
    Shot('Classic (no themes), hoSortGlyphAboveCaption, descending', tmClassic, True,
      procedure(T: TVirtualStringTree) begin T.Header.SortDirection := sdDescending; end);
    Shot('Explorer theme, hoSortGlyphAboveCaption, hot', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin TColumnsCracker(T.Header.Columns).HoverIndex := 0; end);
    Shot('Explorer theme, hoSortGlyphAboveCaption, pressed', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin TColumnsCracker(T.Header.Columns).DownIndex := 0; end);
    Shot('Explorer theme, hoSortGlyphAboveCaption, header image', tmExplorer, True,
      procedure(T: TVirtualStringTree)
      var
        IL: TImageList;
        B: Vcl.Graphics.TBitmap;
      begin
        IL := TImageList.Create(T);
        B := Vcl.Graphics.TBitmap.Create;
        try
          B.SetSize(16, 16);
          B.Canvas.Brush.Color := clGreen;
          B.Canvas.FillRect(Rect(0, 0, 16, 16));
          IL.Add(B, nil);
        finally
          B.Free;
        end;
        T.Header.Images := IL;
        T.Header.Options := T.Header.Options + [hoShowImages];
        T.Header.Columns[0].ImageIndex := 0;
      end);
    ShotHeight := 24;
    Shot('Explorer theme, hoSortGlyphAboveCaption, centered caption, header height 24 px', tmExplorer, True,
      procedure(T: TVirtualStringTree) begin T.Header.Columns[1].CaptionAlignment := taCenter; T.Header.SortColumn := 1; end);
    ShotHeight := 19;
    SaveSheet('issue-1242-modes-and-states.png');

    // Sheet 3: VCL styles (they change global state, so they come last).
    NewSheet;
    if FileExists(StylesDir + 'Windows10.vsf') then
    begin
      TStyleManager.SetStyle(TStyleManager.LoadFromFile(StylesDir + 'Windows10.vsf'));
      Shot('VCL style Windows10, default', tmThemed, False);
      Shot('VCL style Windows10, hoSortGlyphAboveCaption', tmThemed, True);
    end;
    if FileExists(StylesDir + 'Windows10Dark.vsf') then
    begin
      TStyleManager.SetStyle(TStyleManager.LoadFromFile(StylesDir + 'Windows10Dark.vsf'));
      Shot('VCL style Windows10 Dark, default', tmThemed, False);
      Shot('VCL style Windows10 Dark, hoSortGlyphAboveCaption', tmThemed, True);
    end;
    SaveSheet('issue-1242-vcl-styles.png');
  finally
    Sheet.Free;
    Form.Free;
  end;
end.
