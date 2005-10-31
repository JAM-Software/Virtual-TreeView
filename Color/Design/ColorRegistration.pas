unit ColorRegistration;

//----------------------------------------------------------------------------------------------------------------------
//
// ColorRegistration.pas contains everything (except the widget editor), which is related to design time support for
// the color picker and colors in general. This includes component and property editors, certain dialogs etc.
//
//----------------------------------------------------------------------------------------------------------------------
//
// This unit is released under the MIT license:
// Copyright (c) 1999-2005 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
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
// The original code is ColorRegistration.pas, released 1. June 2003.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, Messages, Classes, Controls, DesignIntf, DesignEditors, VCLEditors, PropertyCategories, ColorPicker;

{$Include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

procedure Register;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses                           
  Graphics, Contnrs, Dialogs, SysUtils, Registry, TypInfo, Forms,
  WidgetEditor, DefaultWidgets, WebWidgets, ColorSwatchWidgets, HLSWidgets,
  ColorPickerTypes, ColorTools, ColorManagement, LCMS;

type
  TColorPickerEditor = class (TDefaultEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TColorPickerProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  // Property editor to enable an open-file-dialog and a MRU list for a file name.
  TColorFileNameProperty = class(TStringProperty)
  private
    FMRU: TStringList; // List of previously picked file names.
  protected
    procedure AddToMRU(FileName: string); virtual;
    function GetBaseKey: string; virtual; abstract;
    function GetFullFileName(const FileName: string): string; virtual;
    function GetFilterString: string; virtual; abstract;
    procedure ReadMRU;
    procedure WriteMRU;
  public
    destructor Destroy; override;

    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  // Special variant for color swatch file names.
  TSwatchFileNameProperty = class(TColorFileNameProperty)
  protected
    function GetBaseKey: string; override;
    function GetFilterString: string; override;
  end;

  // Special variant for the file name of a color management profile.
  TCMMFileNameProperty = class(TColorFileNameProperty)
  protected
    procedure AddToMRU(FileName: string); override;
    function GetBaseKey: string; override;
    function GetFullFileName(const FileName: string): string; override;
    function GetFilterString: string; override;
  public
    procedure SetValue(const Value: string); override;
  end;

  TLocalizedFloatProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
  end;

  TColorComponentsProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TDynArrayElementProperty = class(TNestedProperty)
  private
    FElement: Integer;
  public
    constructor Create(Parent: TPropertyEditor; Element: Integer); reintroduce;

    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TColorElementProperty = class(TNestedProperty, ICustomPropertyDrawing)
  private
    FComponent: string;
    FShift: Integer;
  public
    constructor Create(Parent: TPropertyEditor; Component: string; Shift: Integer); reintroduce;

    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  // New property editor for TColor.
  TColorProperty = class(TIntegerProperty, ICustomPropertyDrawing)
  private
    FCurrentIndex: Integer;           // Index of the currently active property in the inspector listbox.
    FPicker: TDropDownColorPicker;
    FDroppedDown: Boolean;
    FLastFocused: HWND;               // Used to keep the last focused window handle to restore it when
                                      // the color picker disappears.
  protected
    procedure CloseUp;
    procedure DoDropDown;
    procedure OnColorChange(Sender: TCustomColorPicker; Display: COLORREF; const Info: TColorInfo);
    procedure OnSelectionCancelled(Sender: TObject);
    procedure PreparePicker;
    procedure RemoveHooks;
  public
    destructor Destroy; override;

    procedure Activate; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

  published
    procedure OnDropDown(Sender: TObject; var CanDrop: Boolean);
    procedure OnSelectItem(Sender: TObject);
  end;

  // Help class to support multipe hooking into events.
  TChangeLink = class(TList)
  protected
    function FindEventHandler(Method: TMethod): Integer;
    procedure HookEvent; virtual; abstract;
    procedure UnhookEvent; virtual; abstract;
  public
    destructor Destroy; override;

    procedure AddEventHandler(Method: TMethod);
    procedure RemoveEventHandler(Method: TMethod);
  end;

  TDropDownLink = class(TChangeLink)
  private
    FOldDropDownMethod: TMethod;      // Original drop down method in the OI drop down button.
  protected
    procedure HookEvent; override;
    procedure UnhookEvent; override;
  published // Must be published to allow access via RTTI.
    procedure OnDropDown(Sender: TObject; var CanDrop: Boolean);
  end;

  TSelectItemLink = class(TChangeLink)
  private
    FOldSelectItemMethod: TMethod;    // Original select item method in the OI list box.
  protected
    procedure HookEvent; override;
    procedure UnhookEvent; override;
  published // Must be published to allow access via RTTI.
    procedure OnSelectItem(Sender: TObject);
  end;

var
  FormatSettings: TFormatSettings; // Needed for culturally correct float value display.
  DropDownButton: TWinControl;     // A reference to the drop down button used in the object inspector.
  DropDownLink: TDropDownLink;     // Link for multiple event handlers to the drop down event.
  InspectorListBox: TWinControl;   // A reference to the object inspector list box.
  SelectItemLink: TSelectItemLink; // Link for multiple event handlers to the select item event.

//----------------- Helper functions -----------------------------------------------------------------------------------

function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;

// Access to published properties, which are dynmamic arrays.

asm
        // ->    EAX Pointer to instance         
        //       EDX Pointer to property info
        // <-    ECX pointer to result dyn array

        PUSH  ESI
        PUSH  EDI
        MOV   EDI, EDX

        MOV   EDX, [EDI].TPropInfo.Index       // pass index in EDX
        CMP   EDX, $80000000
        JNE   @@hasIndex
        MOV   EDX, ECX                         // pass value in EDX
@@hasIndex:
        MOV   ESI, [EDI].TPropInfo.GetProc
        CMP   [EDI].TPropInfo.GetProc.Byte[3], $FE
        JA    @@isField
        JB    @@isStaticMethod

@@isVirtualMethod:
        MOVSX ESI,SI                          // sign extend slot offset
        ADD   ESI,[EAX]                       // vmt + slot offset
        CALL  DWORD PTR [ESI]
        JMP   @@exit

@@isStaticMethod:
        CALL  ESI
        JMP   @@exit

@@isField:
        AND   ESI, $00FFFFFF
        LEA   EDX, [EAX + ESI]                // Source
        MOV   EDX, [EDX]
        PUSH  EBX
        MOV   EBX, [ECX]                      // Destination

        // Increment ref count of source if non-nil
        TEST  EDX, EDX
        JE    @@skipInc
   LOCK INC   DWORD PTR [EDX - 8]
@@skipInc:
        // Dec ref count of destination - if it becomes 0, clear dest.
        TEST  EBX,EBX
        JE    @@skipClear
   LOCK DEC   DWORD PTR [EBX - 8]
        JNZ   @@skipClear
        PUSH  EAX
        PUSH  EDX
        MOV   EDX,ECX
        INC   DWORD PTR [EBX-8]
        CALL  System.@DynArrayClear
        POP   EDX
        POP   EAX
@@skipClear:
        // Finally store source into destination
        MOV   [ECX], EDX
        POP   EBX

@@exit:
        POP   EDI
        POP   ESI
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

asm
        // ->    EAX Pointer to instance
        //       EDX Pointer to property info
        //       ECX Pointer to dyn array value   

        PUSH  ESI
        PUSH  EDI
        MOV   ESI,EDX

        MOV   EDX, [ESI].TPropInfo.Index      // pass index in EDX
        CMP   EDX, $80000000
        JNE   @@hasIndex
        MOV   EDX, ECX                        // pass value in EDX
@@hasIndex:
        MOV   EDI,[ESI].TPropInfo.SetProc
        CMP   [ESI].TPropInfo.SetProc.Byte[3], $FE
        JA    @@isField
        JB    @@isStaticMethod

@@isVirtualMethod:
        MOVSX EDI, DI
        ADD   EDI, [EAX]
        CALL  DWORD PTR [EDI]
        JMP   @@exit

@@isStaticMethod:
        CALL  EDI
        JMP   @@exit

@@isField:
        AND   EDI, $00FFFFFF
        ADD   EAX, EDI                        // Destination
        MOV   EDX, [EDX]                      // Source
        PUSH  EBX
        MOV   EBX, [EAX]

        // Increment ref count of source if non-nil
        TEST  EDX, EDX
        JE    @@skipInc
   LOCK INC   DWORD PTR [EDX - 8]
@@skipInc:
        // Dec ref count of destination - if it becomes 0, clear dest.
        TEST  EBX, EBX
        JE    @@skipClear
   LOCK DEC   DWORD PTR [EBX - 8]
        JNZ   @@skipClear
        PUSH  EAX
        PUSH  EDX
        MOV   EDX,ECX
        INC   DWORD PTR [EBX-8]
        CALL  System.@DynArrayClear
        POP   EDX
        POP   EAX
@@skipClear:
        // Finally store source into destination
        MOV   [ECX], EDX
        POP   EBX
@@exit:
        POP   EDI
        POP   ESI
end;

//----------------------------------------------------------------------------------------------------------------------

function FindIDEControlByName(const Name, ClassName: string): TWinControl;

// This function iterates through all components in the application (which is the Delphi IDE at design time) and
// tries to find a control in the object inspector given by either Name or ClassName (not both). 

var
  UseName,
  UseClassName: Boolean;

  //--------------- local function ---------------------------------------------

  function IterateComponents(Parent: TWinControl): TWinControl;

  var
    I: Integer;
    Control: TControl;

  begin
    Result := nil;

    for I := 0 to Parent.ControlCount - 1 do
    begin
      Control := Parent.Controls[I];
      if (UseName and (Control.Name = Name)) or (UseClassName and (Control.ClassName = ClassName)) then
      begin
        Result := Control as TWinControl;
        Break;
      end
      else
      begin
        if Control is TWinControl then
          Result := IterateComponents(TWinControl(Control));
        if Assigned(Result) then
          Break;
      end;
    end;
  end;

  //--------------- end local function -----------------------------------------

var
  I: Integer;

begin
  Result := nil;
  UseName := Name <> '';
  UseClassName := ClassName <> '';

  for I := 0 to Screen.FormCount - 1 do
  begin
    Result := IterateComponents(Screen.Forms[I]);
    if Assigned(Result) then
      Break;
  end;
end;

//----------------- TColorPickerEditor ---------------------------------------------------------------------------------

procedure TColorPickerEditor.Edit;

begin
  ShowWidgetEditor(Component as TCustomColorPicker, Designer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorPickerEditor.ExecuteVerb(Index: Integer);

begin
  case Index of
    0:
      Edit;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerEditor.GetVerb(Index: Integer): string;

begin
  case Index of
    0:
      Result := 'Edit color widgets';
  else
    Result := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerEditor.GetVerbCount: Integer;

begin
  Result := 1;
end;

//----------------- TColorPickerProperty -------------------------------------------------------------------------------

procedure TColorPickerProperty.Edit;

begin
  ShowWidgetEditor(GetComponent(0) as TCustomColorPicker, Designer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerProperty.GetAttributes: TPropertyAttributes;

begin
  Result := inherited GetAttributes - [paSubProperties, paMultiselect] + [paDialog];
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorPickerProperty.GetValue: string;

begin
  Result := '(Widget list)';
end;

//----------------- TColorFileNameProperty -----------------------------------------------------------------------------

destructor TColorFileNameProperty.Destroy;

begin
  FMRU.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorFileNameProperty.AddToMRU(FileName: string);

var
  LastCount: Integer;

begin
  if FMRU = nil then
    ReadMRU;
  LastCount := FMRU.Count;
  FMRU.Add(FileName);
  if LastCount <> FMRU.Count then
    WriteMRU;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorFileNameProperty.GetFullFileName(const FileName: string): string;

begin
  Result := GetValue;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorFileNameProperty.ReadMRU;

var
  Registry: TRegistry;
  Names: TStringList;
  I: Integer;

begin
  FMRU := TStringList.Create;
  FMRU.CaseSensitive := False;
  FMRU.Duplicates := dupIgnore;
  FMRU.Sorted := False;

  Registry := TRegistry.Create;
  try
    Names := TStringList.Create;
    try
      if Registry.OpenKey(GetBaseKey, True) then
      begin
        Registry.GetValueNames(Names);
        for I := 0 to Names.Count - 1 do
          FMRU.Add(Registry.ReadString(Names[I]));
      end;
      FMRU.Sorted := True;
    finally
      Names.Free;
    end;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorFileNameProperty.WriteMRU;

var
  Registry: TRegistry;
  I: Integer;

begin
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(GetBaseKey, True) then
    begin
      for I := 0 to FMRU.Count - 1 do
        Registry.WriteString(Format('File %d', [I]), FMRU[I]);
    end;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorFileNameProperty.Edit;

begin
  with TOpenDialog.Create(nil) do
  try
    FileName := GetFullFileName(GetValue);
    if not FileExists(FileName) and DirectoryExists(FileName) then
      FileName := FileName + '*.*';
    Filter := GetFilterString;
    Options := [ofPathMustExist, ofFileMustExist, ofShareAware, ofNoTestFileCreate, ofEnableIncludeNotify,
      ofEnableSizing];
    if Execute and FileExists(FileName) then
    begin
      AddToMRU(FileName);
      SetValue(FileName);
      Modified;
    end;
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorFileNameProperty.GetAttributes: TPropertyAttributes;

begin
  Result := inherited GetAttributes + [paDialog, paValueList];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorFileNameProperty.GetValues(Proc: TGetStrProc);

var
  I: Integer;

begin
  if FMRU = nil then
    ReadMRU;
  for I := 0 to FMRU.Count - 1 do
    Proc(FMRU[I]);
end;

//----------------- TSwatchFileNameProperty ----------------------------------------------------------------------------

function TSwatchFileNameProperty.GetBaseKey: string;

begin
  Result := 'Software\Soft Gems\Color Picker\Swatch file MRU';
end;

//----------------------------------------------------------------------------------------------------------------------

function TSwatchFileNameProperty.GetFilterString: string;

begin
  Result := 'Color swatch files (*.aco)|*.aco';
end;

//----------------- TCMMFileNameProperty -------------------------------------------------------------------------------

procedure TCMMFileNameProperty.AddToMRU(FileName: string);

var
  Buffer: array[0..MAX_PATH] of Char;
  BufferSize: Cardinal;
  Folder: string;

begin
  // Remove the directory part of the file name if it refers to a file in the system color folder.
  BufferSize := SizeOf(Buffer);
  if GetColorDirectory(nil, Buffer, BufferSize) then
  begin
    Folder := ExtractFilePath(FileName);
    if SameText(IncludeTrailingPathDelimiter(Buffer), Folder) then
      FileName := ExtractFileName(FileName);
  end;

  inherited AddToMRU(FileName);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCMMFileNameProperty.GetBaseKey: string;

begin
  Result := 'Software\Soft Gems\Color Picker\CMM File MRU';
end;

//----------------------------------------------------------------------------------------------------------------------

function TCMMFileNameProperty.GetFullFileName(const FileName: string): string;

var
  Buffer: array[0..MAX_PATH] of Char;
  BufferSize: Cardinal;

begin
  Result := FileName;
  if ExtractFilePath(Result) = '' then
  begin
    // If there is no path given then use the color folder for it.
    BufferSize := SizeOf(Buffer);
    if GetColorDirectory(nil, Buffer, BufferSize) then
      Result := IncludeTrailingPathDelimiter(Buffer) + Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCMMFileNameProperty.GetFilterString: string;

begin
  Result := 'Color profiles (*.icc; *.icm)|*.icc;*.icm';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCMMFileNameProperty.SetValue(const Value: string);

// Check if the given file is really a CMYK color profile.

var
  ColorSpace: TColorSpaceSignature;
  FileName: string;

begin
  if Value <> '' then
  begin
    FileName := GetFullFileName(Value);
    ColorSpace := GetProfileColorSpace(FileName);
    if (ColorSpace <> icSigCmykData) and (ColorSpace <> icSigCmyData) then
      raise Exception.Create(Format('"%s" ist not a valid CMY(K) profile.', [ExtractFileName(Value)]));
      
    // Tell the user that picking a CMYK profile is not necessary if the widget has a different color format.
    if TColorPickerWidget(GetComponent(0)).ColorFormat <> cfCMYK then
      Application.MessageBox('The current widget color format does not need a CMYK profile. The value is stored but ignored.',
        'Information', MB_ICONINFORMATION);
  end;

  inherited;
end;

//----------------- TLocalizedFloatProperty ----------------------------------------------------------------------------

function TLocalizedFloatProperty.GetValue: string;

// TFloatProperty does not use the correct conversion function to produce the display string for float values.
// What we want is a culturally correct display as the same form is used for input.

const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 18);

begin
  Result := FloatToStrF(GetFloatValue, ffGeneral, Precisions[GetTypeData(GetPropType)^.FloatType], 0, FormatSettings);
end;

//----------------- TColorComponentsProperty ---------------------------------------------------------------------------

function TColorComponentsProperty.GetAttributes: TPropertyAttributes;

begin
  Result := [paReadOnly, paSubProperties, paVolatileSubProperties];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorComponentsProperty.GetProperties(Proc: TGetPropProc);

var
  I: Integer;
  Components: TColorComponents;
  Widget: TColorPickerWidget;

begin
  Components := nil;
  Widget := TColorPickerWidget(GetComponent(0));
  if Assigned(Widget.NativeProfile) then
  begin
    Components := GetDynArrayProp(Widget, GetPropInfo);
    if Length(Components) = 0 then
    begin
      SetLength(Components, Widget.NativeProfile.Channels);
      SetDynArrayProp(Widget, GetPropInfo, Components);
    end;
    for I := 0 to High(Components) do
      Proc(TDynArrayElementProperty.Create(Self, I));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorComponentsProperty.GetValue: string;

var
  Widget: TColorPickerWidget;
  Components: TColorComponents;

begin
  Components := nil;
  Result := '';
  Widget := TColorPickerWidget(GetComponent(0));
  if Assigned(Widget.NativeProfile) then
  begin
    Components := GetDynArrayProp(Widget, GetPropInfo);
    Result := TCustomColorPicker(Widget.ColorPicker).ColorToString(Components);
  end;

  if Result = '' then
    Result := '(none)';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorComponentsProperty.SetValue(const Value: string);

var
  Widget: TColorPickerWidget;
  Components: TColorComponents;

begin
  Widget := TColorPickerWidget(GetComponent(0));
  Components := TCustomColorPicker(Widget.ColorPicker).StringToColor(Value);
  SetDynArrayProp(Widget, GetPropInfo, Components);
  Modified;
end;

//----------------- TDynArrayElementProperty ---------------------------------------------------------------------------

constructor TDynArrayElementProperty.Create(Parent: TPropertyEditor; Element: Integer);

begin
  inherited Create(Parent);
  FElement := Element;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDynArrayElementProperty.GetAttributes: TPropertyAttributes;

begin
  Result := [paMultiSelect];
end;

//----------------------------------------------------------------------------------------------------------------------

function TDynArrayElementProperty.GetName: string;

var
  Widget: TColorPickerWidget;

begin
  Widget := TColorPickerWidget(GetComponent(0));
  with TStringList.Create do
  try
    CommaText := ComponentNames[Widget.ColorFormat];
    if FElement < Count then
      Result := Strings[FElement]
    else
      Result := '--';
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDynArrayElementProperty.GetValue: string;

var
  Widget: TColorPickerWidget;
  Components: TColorComponents;

begin
  Widget := TColorPickerWidget(GetComponent(0));
  Components := GetDynArrayProp(Widget, GetPropInfo);
  if FElement < Length(Components) then
    Result := FloatToStrF(Components[FElement], ffFixed, 4, 2, FormatSettings)
  else
  begin
    // Cause designer to recreate all property editors. This is necessary because there is no other means to
    // get it to remove a certain subproperty (as we would need here).
    Designer.ClearSelection;
    Designer.SelectComponent(Widget);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDynArrayElementProperty.SetValue(const Value: string);

var
  FloatValue: Double;
  Widget: TColorPickerWidget;
  Components: TColorComponents;

begin
  Widget := TColorPickerWidget(GetComponent(0));
  Components := GetDynArrayProp(Widget, GetPropInfo);
  if FElement < Length(Components) then
  begin
    FloatValue := StrToFloat(Value);
    Components[FElement] := FloatValue;
    Modified;
  end;
end;

//----------------- TColorElementProperty ------------------------------------------------------------------------------

constructor TColorElementProperty.Create(Parent: TPropertyEditor; Component: string; Shift: Integer);

begin
  inherited Create(Parent);
  
  FComponent := Component;
  FShift := Shift;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorElementProperty.GetAttributes: TPropertyAttributes;

begin
  Result := [paMultiSelect];
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorElementProperty.GetName: string;

begin
  Result := FComponent;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorElementProperty.GetValue: string;

var
  Color: COLORREF;
  Value: Integer;

begin
  Color := ColorToRGB(GetOrdValue);
  Value := (Color shr FShift) and $FF;
  Result := IntToStr(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorElementProperty.SetValue(const Value: string);

var
  Color: COLORREF;
  NewValue: Cardinal;
  Mask: Integer;

begin
  NewValue := StrToInt(Value);
  if NewValue > 255 then
    NewValue := 255;
  // Get old color value.
  Color := ColorToRGB(GetOrdValue);
  // Create a mask to blend out the old color component value.
  Mask := not ($FF shl FShift);
  NewValue := NewValue shl FShift;
  Color := (Color and Mask) or NewValue;
  SetOrdValue(Color);

  Modified;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorElementProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorElementProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

var
  R: TRect;
  S: string;
  Offset: Integer;
  Color: COLORREF;
  Value: Integer;

begin
  // Erase background.
  ACanvas.FillRect(ARect);

  // Draw value as number.
  Color := ColorToRGB(GetOrdValue);
  Value := (Color shr FShift) and $FF;
  S := IntToStr(Value);
  R := ARect;
  InflateRect(R, -2, -2);
  DrawText(ACanvas.Handle, PChar(S), Length(S), R, DT_VCENTER);

  // Draw value as color bar.
  Offset := ACanvas.TextWidth(S);
  R.Left := R.Right - 50;
  if R.Left < ARect.Left + Offset + 3 then
    R.Left := ARect.Left + Offset + 3;
  if not IsRectEmpty(R) then
  begin
    ACanvas.Brush.Color := Color and ($FF shl FShift);
    ACanvas.FillRect(R);
  end;
end;

//----------------- TColorProperty -------------------------------------------------------------------------------------

destructor TColorProperty.Destroy;

begin
  RemoveHooks;

  FPicker.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.CloseUp;

begin
  ReleaseCapture;
  FPicker.FadeOut(100);
  if FDroppedDown then
  begin
    SetFocus(FLastFocused);
    FDroppedDown := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.DoDropDown;

var
  R: TRect;

begin
  if FPicker = nil then
    PreparePicker;

  GetWindowRect(DropDownButton.Handle, R);
  Dec(R.Right, FPicker.Width);
  // By default drop down direction is below the button. But when there is not enough space then use the room
  // above the button.
  if R.Bottom + FPicker.Height > GetSystemMetrics(SM_CYSCREEN) then
    // Not enough space below the button. Use upper direction.
    R.Bottom := R.Top - FPicker.Height;
  FPicker.FadeIn(R.BottomRight, 100);
  FDroppedDown := True;
  SetCapture(FPicker.Handle);
  FLastFocused := GetFocus;
  SetFocus(0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.OnColorChange(Sender: TCustomColorPicker; Display: COLORREF; const Info: TColorInfo);

begin
  Assert(Info.Format = cfRGB, 'TColorProperty expects only RGB colors.');

  SetValue(Info.Name);
  CloseUp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.OnSelectionCancelled(Sender: TObject);

begin
  CloseUp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.PreparePicker;

begin
  FPicker := TDropDownColorPicker.Create(nil);
  with FPicker do
  begin
    Width := 150;
    Height := 200;
    Color := clBtnFace;

    // Beautify the window a bit.
    UseShadow := True;
    BevelInner := bvNone;
    BevelKind := bkNone;
    BevelOuter := bvRaised;
    Margin := 10;

    // Add our color picker widgets and initialize their properties.
    Widgets.Add(TDelphiColorsCpWidget.Create(FPicker));

    // Hook some events we need for proper operation.
    OnSelectionCancelled := Self.OnSelectionCancelled;
    OnColorChange := Self.OnColorChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.RemoveHooks;

var
  Method: TMethod;

begin
  Method.Code := MethodAddress('OnDropDown');
  Method.Data := Self;
  DropDownLink.RemoveEventHandler(Method);

  Method.Code := MethodAddress('OnSelectItem');
  Method.Data := Self;
  SelectItemLink.RemoveEventHandler(Method);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.Activate;

var
  Method: TMethod;

begin
  FCurrentIndex := GetPropValue(InspectorListBox, 'CurIndex', False);

  Method.Code := MethodAddress('OnDropDown');
  Method.Data := Self;
  Assert(Assigned(Method.Code), 'OnDropDown method not found. It must be a published method!');
  DropDownLink.AddEventHandler(Method);

  Method.Code := MethodAddress('OnSelectItem');
  Method.Data := Self;
  Assert(Assigned(Method.Code), 'OnSelect method not found. It must be a published method!');
  SelectItemLink.AddEventHandler(Method);
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorProperty.GetAttributes: TPropertyAttributes;

begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable, paSubProperties, paVolatileSubProperties];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.GetProperties(Proc: TGetPropProc);

begin
  Proc(TColorElementProperty.Create(Self, 'Red', 0));
  Proc(TColorElementProperty.Create(Self, 'Green', 8));
  Proc(TColorElementProperty.Create(Self, 'Blue', 16));
end;

//----------------------------------------------------------------------------------------------------------------------

function TColorProperty.GetValue: string;

begin
  Result := ColorToString(TColor(GetOrdValue));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.SetValue(const Value: string);

var
  Color: Integer;

begin
  if IdentToColor(Value, Color) then
    SetOrdValue(Color)
  else
    inherited;

  Modified;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

  //--------------- local function ---------------------------------------------

  function ColorToBorderColor(AColor: TColor): TColor;

  type
    TColorQuad = packed record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;

  begin
    if (TColorQuad(AColor).Red > 192) or (TColorQuad(AColor).Green > 192) or (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else
      if ASelected then
        Result := clWhite
      else
        Result := AColor;
  end;

  //--------------- end local function -----------------------------------------

var
  Right: Integer;
  OldPenColor,
  OldBrushColor: TColor;
  
begin
  Right := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

    // set things up and do the work
    Brush.Color := StringToColor(Value);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    // restore the things we twiddled with
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.OnDropDown(Sender: TObject; var CanDrop: Boolean);

// Called when the little drop down button in the object inspector is pressed. This is
// the signal for us to show our own special window.
  
begin
  // Prevent the normal list box from appearing but show our own window instead.
  CanDrop := False;
  if FDroppedDown then
    OnSelectionCancelled(nil)
  else
    DoDropDown;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorProperty.OnSelectItem(Sender: TObject);

// Called when a new property is selected in the object inspector. This is the indication for us to
// remove our hooks to let other property editors work as expected.
// This event is however called after "Activate" where we set our hooks. So we must check here
// whether the removal is really due or rather to be skipped.

begin
  // If the index which was used to activate this editor differs now from the actual index then
  // another property has been selected and we must remove our hooks.
  if FCurrentIndex <> GetPropValue(InspectorListBox, 'CurIndex', False) then
  begin
    if Assigned(FPicker) then
      OnSelectionCancelled(nil);
    RemoveHooks;
  end;
end;

//----------------- TChangeLink ----------------------------------------------------------------------------------------

destructor TChangeLink.Destroy;

var
  I: Integer;

begin
  for I := 0 to Count - 1 do
    Dispose(Items[I]);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TChangeLink.FindEventHandler(Method: TMethod): Integer;

// Searchs the internal list for the given method and returns its index if found. Otherwise -1 is returned.

var
  I: Integer;
  Entry: ^TMethod;

begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    Entry := Items[I];
    if (Entry.Code = Method.Code) and (Entry.Data = Method.Data) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TChangeLink.AddEventHandler(Method: TMethod);

var
  Index: Integer;
  Entry: ^TMethod;

begin
  Index := FindEventHandler(Method);
  if Index = -1 then
  begin
    New(Entry);
    Entry^ := Method;
    Add(Entry);

    // If this is the first entry in the list then establish the event hook.
    if Count = 1 then
      HookEvent;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TChangeLink.RemoveEventHandler(Method: TMethod);

var
  Index: Integer;

begin
  Index := FindEventHandler(Method);
  if Index > -1 then
  begin
    Dispose(Items[Index]);
    Delete(Index);

    // If this was the last entry then remove the hook.
    if Count = 0 then
      UnhookEvent;
  end;
end;

//----------------- TDropDownLink --------------------------------------------------------------------------------------

procedure TDropDownLink.HookEvent;

var
  Info: PPropInfo;
  Method: TMethod;

begin
  if FOldDropDownMethod.Code = nil then
  begin
    // Get the current event handler of the OnDropDown event of the drop down button
    // and replace it by our own method.
    Info := TypInfo.GetPropInfo(DropDownButton, 'OnDropDown');
    Assert(Assigned(Info), 'OnDropDown method not found in the object inspector drop down button.');
    FOldDropDownMethod := GetMethodProp(DropDownButton, Info);
    Method.Code := MethodAddress('OnDropDown');
    Method.Data := Self;
    Assert(Assigned(Info), 'OnDropDown method not found. It must be a published method!');
    SetMethodProp(DropDownButton, Info, Method);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownLink.UnhookEvent;

begin
  if Assigned(FOldDropDownMethod.Code) then
  begin
    SetMethodProp(DropDownButton, 'OnDropDown', FOldDropDownMethod);
    FOldDropDownMethod.Code := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDropDownLink.OnDropDown(Sender: TObject; var CanDrop: Boolean);

type
  TDropDownProc = procedure(Sender: TObject; var CanDrop: Boolean) of object;

var
  I: Integer;
  Entry: ^TMethod;

begin
  // Call old event handler.
  if Assigned(FOldDropDownMethod.Code) then
    TDropDownProc(FOldDropDownMethod)(Sender, CanDrop);

  // The event handlers might remove themselves from our list, so go backwards to avoid "index out of bounds" errors.
  I := Count - 1;
  while I > -1 do
  begin
    Entry := Items[I];
    TDropDownProc(Entry^)(Sender, CanDrop);
    Dec(I);
  end;
end;

//----------------- TSelectItemLink ------------------------------------------------------------------------------------

procedure TSelectItemLink.HookEvent;

var
  Info: PPropInfo;
  Method: TMethod;

begin
  if FOldSelectItemMethod.Code = nil then
  begin
    // Get the current event handler of the OnDropDown event of the drop down button
    // and replace it by our own method.
    Info := TypInfo.GetPropInfo(InspectorListBox, 'OnSelectItem');
    Assert(Assigned(Info), 'OnSelectItem method not found in the object inspector listbox.');
    FOldSelectItemMethod := GetMethodProp(InspectorListBox, Info);
    Method.Code := MethodAddress('OnSelectItem');
    Method.Data := Self;
    Assert(Assigned(Info), 'OnSelectItem method not found. It must be a published method!');
    SetMethodProp(InspectorListBox, Info, Method);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSelectItemLink.UnhookEvent;

begin
  if Assigned(FOldSelectItemMethod.Code) then
  begin
    SetMethodProp(InspectorListBox, 'OnSelectItem', FOldSelectItemMethod);
    FOldSelectItemMethod.Code := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSelectItemLink.OnSelectItem(Sender: TObject);

// Called when a new property is selected in the object inspector. This is the indication for us to
// remove our hooks to let other property editors work as expected.
// This event is however called after "Activate" where we set our hooks. So we must check here
// whether the removal is really due or rather to be skipped.

type
  TSelectItemProc = procedure(Sender: TObject) of object;

var
  I: Integer;
  Entry: ^TMethod;

begin
  // Call old event handler. 
  if Assigned(FOldSelectItemMethod.Code) then
    TSelectItemProc(FOldSelectItemMethod)(Sender);

  // The event handlers remove themselves from our list, so go backwards to avoid "index out of bounds" errors.
  I := Count - 1;
  while I > -1 do
  begin
    Entry := Items[I];
    TSelectItemProc(Entry^)(Sender);
    Dec(I);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  DropDownButton := FindIDEControlByName('ListButton', '');
  InspectorListBox := FindIDEControlByName('', 'TInspListBox');

  RegisterComponents('Color tools', [TColorPicker, TColorPickerButton]);
  RegisterComponentEditor(TColorPicker, TColorPickerEditor);
  // TDropDownColorPicker derives from TComponent and is hence registered by default with TComponentProperty.
  // This editor does not allow to edit subproperties so we revert registration back to TClassProperty here.
  RegisterPropertyEditor(Typeinfo(TDropDownColorPicker), TColorPickerButton, '', TClassProperty);
  RegisterPropertyEditor(TypeInfo(TObjectList), TCustomColorPicker, 'Widgets', TColorPickerProperty);
  RegisterPropertyEditor(TypeInfo(string), TColorSwatchWidget, 'SwatchFile', TSwatchFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDiscreteColorPickerWidget, 'CMYKProfileFile', TCMMFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TColorComponents), TColorPickerWidget, '', TColorComponentsProperty);
  RegisterPropertyEditor(TypeInfo(TColor), nil, '', TColorProperty);

  RegisterPropertyEditor(TypeInfo(Single), nil, '', TLocalizedFloatProperty);
  RegisterPropertyEditor(TypeInfo(Double), nil, '', TLocalizedFloatProperty);
  RegisterPropertyEditor(TypeInfo(Extended), nil, '', TLocalizedFloatProperty);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  GetLocaleFormatSettings(GetThreadLocale, FormatSettings);
  // Create helper classes for multiple event links.
  DropDownLink := TDropDownLink.Create;
  SelectItemLink := TSelectItemLink.Create;
finalization
  DropDownLink.Free;
  SelectItemLink.Free;
end.
