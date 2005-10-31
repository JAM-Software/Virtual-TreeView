unit WidgetEditor;

//----------------------------------------------------------------------------------------------------------------------
// WidgetEditor contains the editor for the color picker widgets used in the Color Picker control.
//
// This software is freeware. You may freely use it in any software, including commercial software, provided
// you accept the following conditions:
//
// 1) The software may not be included into component collections and similar compilations which are sold. If you want
//    to distribute this software for money then contact me first and ask for my permission.
// 2) My copyright notices in the source code may not be removed or modified.
// 3) If you modify and/or distribute the code to any third party then you must not veil the original author. It must
//    always be clearly identifiable that I, Mike Lischke, am the original author.
// Although it is not required it would be a nice move to recognize my work by adding a citation to the application's
// about box or a similar place.
//
// The original code is WidgetEditor.pas, released 1. October 2003.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

{$Include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, StdCtrls, Buttons, ActnList, ColorPicker, ColorPickerTypes, ExtCtrls,
  DesignIntf, CheckLst, ComCtrls, ToolWin, Grids, ImgList, ToolsAPI;

type
  // We use an own drag object to support a nice drag image for the widget list box.
  TWidgetDragObject = class(TBaseDragControlObject)
  private
    FDragImage: TDragImageList;
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure HideDragImage; override;
    procedure PrepareDragImage(const Bitmap: TBitmap; HotSpot: TPoint);
    procedure ShowDragImage; override;
  end;

  TWidgetEditorForm = class(TForm, IDesignNotification)
    GroupBox1: TGroupBox;
    OKButton: TButton;
    WidgetsListbox: TListBox;
    GroupBox2: TGroupBox;
    WidgetGrid: TDrawGrid;
    UtilityImages: TImageList;
    procedure OKButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WidgetsListboxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure WidgetsListboxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
    procedure WidgetsListboxStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure WidgetsListboxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure WidgetsListboxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure WidgetToolBarDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure WidgetGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure WidgetGridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure WidgetGridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure WidgetGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure WidgetsListboxDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure WidgetGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WidgetGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure WidgetGridDblClick(Sender: TObject);
    procedure WidgetGridEndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    FColorPicker: TCustomColorPicker;
    FDesigner: IDesigner;
    FThumbnails: TObjectList;
    FBackBuffer: TBitmap;
    FDragObject: TWidgetDragObject;
    FCurrentDragIndex: Integer;
    FHotDropIndex: Integer;
    FLeftDropSide: Boolean;        // True if left half of thumbnail, otherwise False.
    procedure DrawWidget(Canvas: TCanvas; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ThumbnailNeeded(Widget: TColorPickerWidgetClass; Index: Integer);
    procedure UpdateUsedWidgets;
  public
    procedure Edit(const ColorPicker: TCustomColorPicker; Designer: IDesigner);

    // IDesignNotification
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
  end;

var
  WidgetEditorForm: TWidgetEditorForm;

procedure ShowWidgetEditor(const Picker: TCustomColorPicker; Designer: IDesigner);

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  UsesClause, Types, Math;

resourcestring
  SEditorCaption = 'Color picker widget editor - editing %s';

//----------------------------------------------------------------------------------------------------------------------

procedure ShowWidgetEditor(const Picker: TCustomColorPicker; Designer: IDesigner);

begin
  if Designer.IsSourceReadOnly then
    MessageBox(0, 'Source is read-only. The color picker(s) cannot be edited', 'Information', MB_OK or MB_ICONINFORMATION)
  else
  begin
    if WidgetEditorForm = nil then
      WidgetEditorForm := TWidgetEditorform.Create(nil);
    WidgetEditorForm.Hide;
    WidgetEditorForm.Caption := Format(SEditorCaption, [Picker.Name]);
    WidgetEditorForm.Edit(Picker, Designer);
  end;
end;

//----------------- TWidgetDragObject ----------------------------------------------------------------------------------

constructor TWidgetDragObject.Create(AControl: TControl);

begin
  inherited;
  
  FDragImage := TDragImageList.Create(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TWidgetDragObject.Destroy;

begin
  FDragImage.Free;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWidgetDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;

begin
  if Accepted then
    Result := crArrow
  else
    Result := crNo;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWidgetDragObject.GetDragImages: TDragImageList;

begin
  Result := FDragImage;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetDragObject.HideDragImage;

begin
  FDragImage.HideDragImage;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetDragObject.PrepareDragImage(const Bitmap: TBitmap; HotSpot: TPoint);

begin
  FDragImage.Clear;
  FDragImage.Width := Bitmap.Width;
  FDragImage.Height := Bitmap.Height;
  FDragImage.AddMasked(Bitmap, clWindow);

  FDragImage.SetDragImage(0, HotSpot.X, HotSpot.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetDragObject.ShowDragImage;

begin
  FDragImage.ShowDragImage;
end;

//----------------- TWidgetEditorForm ----------------------------------------------------------------------------------

procedure TWidgetEditorForm.Edit(const ColorPicker: TCustomColorPicker; Designer: IDesigner);

var
  I, Index: Integer;
  WidgetClass: TColorPickerWidgetClass;
  Selection: Integer;

begin
  FColorPicker := ColorPicker;
  FDesigner := Designer;

  Selection := -1;
  WidgetsListbox.Clear;
  FCurrentDragIndex := -1;
  FHotDropIndex := -1;

  // Add all registered widgets to the widgets listbox.
  for I := 0 to RegisteredWidgets.Count - 1 do
  begin
    WidgetClass := TColorPickerWidgetClass(RegisteredWidgets[I]);
    Index := WidgetsListbox.Items.AddObject(WidgetClass.GetSummary, Pointer(WidgetClass));
    if Selection = -1 then
      Selection := Index;
  end;

  WidgetsListbox.ItemIndex := Selection;
  UpdateUsedWidgets;
  WidgetGrid.Col := 0;
  WidgetGrid.Row := 0;
  if FColorPicker.Widgets.Count > 0 then
    FDesigner.SelectComponent(FColorPicker.Widgets[0]);

  Show;
  BringToFront;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  PreviewWidth = 90;
  PreviewHeight = 90;
  PreviewMargin = 4;

procedure TWidgetEditorForm.DrawWidget(Canvas: TCanvas; Index: Integer; Rect: TRect; State: TOwnerDrawState);

var
  Widget: TColorPickerWidgetClass;
  R: TRect;
  Offset: Integer;

begin
  if Index > -1 then
  begin
    with FBackBuffer.Canvas do
    begin
      if FBackBuffer.Width <> (Rect.Right - Rect.Left) then
        FBackBuffer.Width := Rect.Right - Rect.Left;
      if FBackBuffer.Height <> (Rect.Bottom - Rect.Top) then
        FBackBuffer.Height := Rect.Bottom - Rect.Top;

      Font := Canvas.Font;
      // Fill background.
      Brush.Color := clWindow;
      R := Rect;
      OffsetRect(R, -R.Left, -R.Top);
      FillRect(R);
      if odSelected in State then
      begin
        Brush.Color := clHighlight;
        Inc(R.Left, PreviewWidth - PreviewMargin);
        FillRect(R);
      end;

      Widget := TColorPickerWidgetClass(RegisteredWidgets[Index]);

      // Draw thumbnail. Create it if not yet done.
      ThumbnailNeeded(Widget, Index);
      Draw(0, 0, TBitmap(FThumbnails[Index]));

      // Draw summary.
      Font.Style := [fsBold];
      if odSelected in State then
        Font.Color := clWhite
      else
        Font.Color := clMaroon;
      R := Types.Rect(0, 0, ClientWidth - PreviewWidth, 0);
      DrawText(Handle, PChar(Widget.GetSummary), Length(Widget.GetSummary), R, DT_CALCRECT or DT_WORDBREAK);
      Offset := R.Bottom - R.Top;
      R := Rect;
      // Convert to bitmap coordinates.
      OffsetRect(R, -R.Left, -R.Top);
      R.Left := PreviewWidth;
      Inc(R.Top, 4);
      Inc(R.Left, 4);
      DrawText(Handle, PChar(Widget.GetSummary), Length(Widget.GetSummary), R, DT_WORDBREAK);

      // Determine height of the description string.
      Font.Style := [];
      if odSelected in State then
        Font.Color := clWhite
      else
        Font.Color := clGray;
      Inc(R.Top, Offset);
      DrawText(Handle, PChar(Widget.GetDescription), Length(Widget.GetDescription), R, DT_WORDBREAK);

      Canvas.Draw(Rect.Left, Rect.Top, FBackBuffer);
      if [odFocused, odNoFocusRect] * State = [odFocused] then
        Canvas.DrawFocusRect(Rect);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.ThumbnailNeeded(Widget: TColorPickerWidgetClass; Index: Integer);

var
  Bitmap: TBitmap;
  R: TRect;
  
begin
  if FThumbnails[Index] = nil then
  begin
    // The bitmap created here will be freed when the thumbnail cache is freed.
    Bitmap := TBitmap.Create;
    Bitmap.Width := PreviewWidth;
    Bitmap.Height := PreviewHeight;
    Bitmap.TransparentColor := clWhite;
    Bitmap.Transparent := True;
    Bitmap.Canvas.Brush.Color := clWhite;
    R := Rect(0, 0, PreviewWidth, PreviewHeight);
    Bitmap.Canvas.FillRect(R);
    InflateRect(R, -PreviewMargin, -PreviewMargin);
    Widget.RenderPreview(Bitmap.Canvas, R);
    FThumbnails[Index] := Bitmap;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.OKButtonClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  WidgetsListBox.Clear;
  FDesigner := nil;

  Action := caHide;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.FormCreate(Sender: TObject);

begin
  // Create a list thumbnails for the preview window. The thumbnails are created on demand.
  FThumbnails := TObjectList.Create(True);
  FThumbnails.Count := RegisteredWidgets.Count;

  FBackBuffer := TBitmap.Create;
  with WidgetsListBox do
    ControlStyle := ControlStyle + [csDisplayDragImage];
  with WidgetGrid do
  begin
    ControlStyle := ControlStyle + [csDisplayDragImage];
    DefaultColWidth := PreviewWidth + 10;
    DefaultRowHeight := PreviewHeight + 10;
    ClientHeight := DefaultRowHeight;
  end;

  Mouse.DragImmediate := False;

  // Register notifiers with the IDE to know about certain events.
  RegisterDesignNotification(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);

begin
  if FDesigner = ADesigner then
    Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);

begin
  // Nothing to do.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);

begin
  if AItem = FColorPicker then
    Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);

begin
  // Nothing to do.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.ItemsModified(const ADesigner: IDesigner);

begin
  Invalidate;
  FColorPicker.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);

begin
  // Nothing to do.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.FormDestroy(Sender: TObject);

begin
  UnregisterDesignNotification(Self);

  FBackBuffer.Free;
  // The thumb cache owns the bitmaps and frees them implicitely.
  FThumbnails.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetsListboxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);

var
  Widget: TColorPickerWidgetClass;
  R: TRect;
  NewHeight: Integer;

begin
  NewHeight := 0;
  if Index > -1 then
    with Control as TListBox do
    begin
      Canvas.Font := Font;
      Widget := TColorPickerWidgetClass(RegisteredWidgets[Index]);
      with Canvas do
      begin
        // Determine height of the summary string.
        Font.Style := [fsBold];
        R := Rect(0, 0, ClientWidth - PreviewWidth, 0);
        DrawText(Handle, PChar(Widget.GetSummary), Length(Widget.GetSummary), R, DT_CALCRECT or DT_WORDBREAK);
        Inc(NewHeight, R.Bottom - R.Top);

        // Determine height of the description string.
        Font.Style := [];
        R := Rect(0, 0, ClientWidth - PreviewWidth, 0);
        DrawText(Handle, PChar(Widget.GetDescription), Length(Widget.GetDescription), R, DT_CALCRECT or DT_WORDBREAK);
        Inc(NewHeight, R.Bottom - R.Top);
      end;
    end;

  Height := Max(PreviewHeight, NewHeight + 20);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetsListboxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

begin
  if Index > -1 then
    DrawWidget(WidgetsListBox.Canvas, Index, Rect, State);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.FormResize(Sender: TObject);

begin
  WidgetsListbox.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetsListboxStartDrag(Sender: TObject; var DragObject: TDragObject);

var
  R: TRect;
  CursorPos: TPoint;
  HotSpot: TPoint;

begin
  // Create and return our special drag object in order to show a nice drag image.
  FDragObject := TWidgetDragObject.Create(WidgetsListBox);
  DragObject := FDragObject;

  // Determine hotspot in the image (the cursor position relative to the drag image origin).
  FCurrentDragIndex := WidgetsListBox.ItemIndex;
  R := WidgetsListBox.ItemRect(FCurrentDragIndex);
  GetCursorPos(CursorPos);
  CursorPos := WidgetsListBox.ScreenToClient(CursorPos);
  HotSpot.X := CursorPos.X - R.Left;
  HotSpot.Y := CursorPos.Y - R.Top;

  // Create the drag image to show.
  FDragObject.PrepareDragImage(TBitmap(FThumbnails[FCurrentDragIndex]), HotSpot);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetsListboxEndDrag(Sender, Target: TObject; X, Y: Integer);

begin
  FreeAndNil(FDragObject);
  FCurrentDragIndex := -1;
  FHotDropIndex := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetsListboxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

begin
  Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.UpdateUsedWidgets;

begin
  // Note: for some hardly understandable reasons a grid can never be entirely empty. There is always one column.
  // Regardless of what is set here.
  WidgetGrid.ColCount := FColorPicker.Widgets.Count;
  WidgetGrid.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetToolBarDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

begin
  Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);

var
  X, Y: Integer;
  Image: TBitmap;
  Index: Integer;
  WidgetClass: TColorPickerWidgetClass;
  
begin
  if ACol < FColorPicker.Widgets.Count then
  begin
    // Draw widget thumbnail.
    WidgetClass := TColorPickerWidgetClass(FColorPicker.Widgets[ACol].ClassType);
    Index := RegisteredWidgets.IndexOf(WidgetClass);
    if Index > -1 then
    begin
      ThumbnailNeeded(WidgetClass, Index);
      Image := TBitmap(FThumbnails[Index]);
      if Assigned(Image) then
      begin
        X := (Rect.Right + Rect.Left - Image.Width) div 2;
        Y := (Rect.Bottom + Rect.Top - Image.Height) div 2;
        WidgetGrid.Canvas.Draw(X, Y, Image);
      end;
    end;

    // Draw drop mark if dragging is in progress.
    if (ACol = FHotDropIndex) and Assigned(FDragObject) then
    begin
      if FDragObject.DragTargetPos.X < (Rect.Right + Rect.Left) div 2 then
      begin
        // Left half of the widget thumbnail.
        UtilityImages.Draw(WidgetGrid.Canvas, Rect.Left + 4, (Rect.Bottom + Rect.Top - UtilityImages.Height) div 2, 0);
      end
      else
      begin
        // Right half of the widget thumbnail.
        UtilityImages.Draw(WidgetGrid.Canvas, Rect.Right - 4 - UtilityImages.Width, (Rect.Bottom + Rect.Top -
          UtilityImages.Height) div 2, 1);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetGridStartDrag(Sender: TObject; var DragObject: TDragObject);

var
  R: TRect;
  CursorPos: TPoint;
  HotSpot: TPoint;
  WidgetClass: TColorPickerWidgetClass;
  Index: Integer;

begin
  // Create and return our special drag object in order to show a nice drag image.
  FDragObject := TWidgetDragObject.Create(WidgetGrid);
  DragObject := FDragObject;

  // Determine hotspot in the image (the cursor position relative to the drag image origin).
  // FCurrentDragIndex and FHotDropIndex are set already in the mouse down handler.
  R := WidgetGrid.CellRect(FCurrentDragIndex, 0);
  GetCursorPos(CursorPos);
  CursorPos := WidgetGrid.ScreenToClient(CursorPos);
  HotSpot.X := CursorPos.X - R.Left - 5;
  HotSpot.Y := CursorPos.Y - R.Top - 5;

  // Create the drag image to show.
  WidgetClass := TColorPickerWidgetClass(FColorPicker.Widgets[FCurrentDragIndex].ClassType);
  Index := RegisteredWidgets.IndexOf(WidgetClass);
  FDragObject.PrepareDragImage(TBitmap(FThumbnails[Index]), HotSpot);
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TGridCast = class(TDrawGrid);
  
procedure TWidgetEditorForm.WidgetGridDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

var
  Column, Row: Integer;
  R: TRect;
  NewDropSideIsLeft: Boolean;
  
begin
  Accept := True;

  if FColorPicker.Widgets.Count = 0 then
    FHotDropIndex := -1
  else
  begin
    WidgetGrid.MouseToCell(X, Y, Column, Row);
    R := WidgetGrid.CellRect(Column, Row);
    NewDropSideIsLeft := X < (R.Right + R.Left) div 2;
    if (Column <> FHotDropIndex) or (FLeftDropSide <> NewDropSideIsLeft) then
    begin
      FLeftDropSide := NewDropSideIsLeft;
      TGridCast(WidgetGrid).InvalidateCell(FHotDropIndex, Row); // InvalidateCell is protected.
      if Column <> FHotDropIndex then
      begin
        FHotDropIndex := Column;
        TGridCast(WidgetGrid).InvalidateCell(FHotDropIndex, Row); // InvalidateCell is protected.
      end;
      FDragObject.HideDragImage;
      WidgetGrid.Update;
      FDragObject.ShowDragImage;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);

var
  Row, Column: Integer;
  
begin
  WidgetGrid.MouseToCell(X, Y, Column, Row);
  if (Column > -1) and (Row > -1) and (Column < FColorPicker.Widgets.Count) then
  begin
    FCurrentDragIndex := Column;
    FHotDropIndex := Column;
    // Make sure the grid has finished paintig the new cell in a selected fashion before starting the drag operation.
    WidgetGrid.Update;
    WidgetGrid.BeginDrag(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetsListboxDragDrop(Sender, Source: TObject; X, Y: Integer);

begin
  if FDragObject.Control = WidgetGrid then
  begin
    FColorPicker.Widgets.Delete(FCurrentDragIndex);
    FDesigner.Modified;
    UpdateUsedWidgets;
    if FColorPicker.Widgets.Count = 0 then
      FDesigner.NoSelection
    else
      FDesigner.SelectComponent(FColorPicker.Widgets[WidgetGrid.Col]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetGridDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  Widget: TBaseColorPickerWidget;
  ImplFileName,
  IntfFileName,
  FormFileName: string;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;

begin
  Widget := nil;
  
  if FDragObject.Control = WidgetsListBox then
  begin
    Widget := TColorPickerWidgetClass(RegisteredWidgets[FCurrentDragIndex]).Create(FColorPicker);
    if (FHotDropIndex < 0) or (FColorPicker.Widgets.Count = 0) then
      FColorPicker.Widgets.Add(Widget)
    else
      if FLeftDropSide then
        FColorPicker.Widgets.Insert(FHotDropIndex, Widget)
      else
        FColorPicker.Widgets.Insert(FHotDropIndex + 1, Widget);
    UpdateUsedWidgets;
    if FColorPicker.Widgets.Count = 1 then
      FDesigner.SelectComponent(Widget);

    // Make widget linkable if not yet done.
    FDesigner.ModuleFileNames(ImplFileName, IntfFileName, FormFileName);
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    Module := ModuleServices.FindModule(ImplFileName);
    MakeClassLinkable(Module, Widget.ClassType, False);
  end
  else
    if FHotDropIndex <> FCurrentDragIndex then
    begin
      // Source object is the widget grid.
      if FHotDropIndex < 0 then
        FColorPicker.Widgets.Move(FCurrentDragIndex, FColorPicker.Widgets.Count - 1)
      else
        if FLeftDropSide then
        begin
          if FHotDropIndex > FCurrentDragIndex then
            FColorPicker.Widgets.Move(FCurrentDragIndex, FHotDropIndex - 1)
          else
            FColorPicker.Widgets.Move(FCurrentDragIndex, FHotDropIndex);
        end
        else
        begin
          if FHotDropIndex > FCurrentDragIndex then
            FColorPicker.Widgets.Move(FCurrentDragIndex, FHotDropIndex)
          else
            FColorPicker.Widgets.Move(FCurrentDragIndex, Min(FHotDropIndex + 1, FColorPicker.Widgets.Count - 1));
        end;
      UpdateUsedWidgets;
    end;
  FColorPicker.Notify(Widget, wnrStructure);
  FDesigner.Modified;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);

begin
  CanSelect := ACol < FColorPicker.Widgets.Count;
  if CanSelect then
    FDesigner.SelectComponent(FColorPicker.Widgets[ACol]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetGridDblClick(Sender: TObject);

// Opens up the object inspector and brings it to front.
// The widget, which was double clicked will be selected in the OI.

var
  EditorServices: IOTAEditorServices;
  EditActions: IOTAEditActions;

begin
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  if Assigned(EditorServices.TopView) then
  begin
    EditActions := EditorServices.TopView as IOTAEditActions;
    EditActions.ViewObjectInspector;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWidgetEditorForm.WidgetGridEndDrag(Sender, Target: TObject; X, Y: Integer);

begin
  FreeAndNil(FDragObject);
  FCurrentDragIndex := -1;
  FHotDropIndex := -1;
  WidgetGrid.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
finalization
  WidgetEditorForm.Free;
end.
