unit UCEHighlighter;

//----------------------------------------------------------------------------------------------------------------------
//
// UniCodeEditor, a Unicode Source Code Editor for Delphi.
//
// UniCodeEditor is released under the MIT license:
// Copyright (c) 1999-2004 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
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
//
// Description
//   This unit implements the base functionality for a syntax highlighter which is used
//   in TSyntaxEdit. Some of the code used here was taken form mwEdit.
//
// Changes:
//   Version 2.0, 2003-08-17, Mike Zinner
//     Repackaged for D7
//   Version 1.0, 1999-03-10, Mike Lischke
//     Initial Version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Classes, Graphics, Registry, Controls;

type
  TWindowList = class(TList)
  public
    procedure AddInstance(Control: TWinControl);
    procedure RemoveInstance(Control: TWinControl);
    procedure Invalidate;
  end;

  THighlightData = record
    Foreground,
    Background: TColor;
    Style: TFontStyles;
  end;

  TTokenData = record
    Token: PChar;
    TokenType: Integer;
    Position,
    Length: Integer;
    Background,
    ForeGround: TColor;
    Style: TFontStyles;
  end;

  THighlightAttributes = class(TPersistent)
  private
    FHighlightData: THighlightData;
    FOnChange: TNotifyEvent;
    FName: string;
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
    procedure SetStyle(Value: TFontStyles);
  public
    procedure Assign(Source: TPersistent); override;
    function LoadFromBorlandRegistry(Path: string; Version: Integer): Boolean; virtual;
    function LoadFromRegistry(Reg: TRegistry): Boolean;
    function SaveToRegistry(Reg: TRegistry): Boolean;

    property Name: string read FName;
  published
    constructor Create(Name: string);

    property Background: TColor read FHighlightData.Background write SetBackground;
    property Foreground: TColor read FHighlightData.Foreground write SetForeground;
    property Style: TFontStyles read FHighlightData.Style write SetStyle;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TIdentChars = set of Char;

  THighlighterCapability = (
    hcUserSettings, // supports Enum/UseUserSettings
    hcRegistry      // supports LoadFrom/SaveToRegistry
  );

  THighlighterCapabilities = set of THighlighterCapability;

  TUCEHighlighter = class(TComponent)
  private
    FWindowList: TWindowList;
    FDefaultFilter: string;
  protected
    function GetIdentChars: TIdentChars; virtual;
    function GetLanguageName: string; virtual; abstract;

    function GetAttributeCount: Integer; virtual; abstract;
    function GetAttribute(idx: Integer): THighlightAttributes; virtual; abstract;
    function GetCapabilities: THighlighterCapabilities; virtual;
    function GetDefaultFilter: string; virtual;
    procedure SetDefaultFilter(Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AssignAttributesFrom(const Source: TUCEHighlighter); virtual;
    function EOL: Boolean; virtual; abstract;
    function GetRange: Integer; virtual; abstract;
    function GetToken: string; virtual; abstract;
    function GetTokenInfo: TTokenData; virtual; abstract;
    //function GetTokenPos: Integer; virtual; abstract;
    function HasMoreTokens: Boolean; virtual; abstract;
    procedure Next; virtual; abstract;
    procedure SetLine(const Value: string); virtual; abstract;
    procedure SetRange(Value: Integer); virtual; abstract;
    procedure ResetRange; virtual; abstract;
    function UseUserSettings(Path: string): Boolean; virtual;
    procedure EnumUserSettings(Settings: TStrings); virtual;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; virtual;
    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; virtual;

    property WindowList: TWindowList read FWindowList;
    property IdentChars: TIdentChars read GetIdentChars;
    property LanguageName: string read GetLanguageName;

    property AttributeCount: Integer read GetAttributeCount;
    property Attribute[Index: Integer]: THighlightAttributes read GetAttribute;
    property Capabilities: THighlighterCapabilities read GetCapabilities;
  published
    property DefaultFilter: string read GetDefaultFilter write SetDefaultFilter;
  end;

  THighlighterClass = class of TUCEHighlighter;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------- TWindowList ----------------------------------------------------------------------------------------

procedure TWindowList.AddInstance(Control: TWinControl);

begin
  if IndexOf(Control) = -1 then
    Add(Control);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowList.RemoveInstance(Control: TWinControl);

var
  Index: Integer;

begin
  Index := IndexOf(Control);
  if Index <> -1 then
    Delete(Index);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowList.Invalidate;

var
  I: Integer;

begin
  for I := 0 to Count - 1 do
    TWinControl(Items[I]).Invalidate;
end;

//----------------- THighlightAttributes -----------------------------------------

procedure THighlightAttributes.Assign(Source: TPersistent);

begin
  if Source is THighlightAttributes then
  begin
    FHighlightData := (Source as THighlightAttributes).FHighlightData;
    FName := (Source as THighlightAttributes).FName;
  end
  else
    inherited Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor THighlightAttributes.Create(Name: string);

begin
  inherited Create;
  FHighlightData.Background := clWindow;
  FHighlightData.Foreground := clWindowText;
  FName := Name;
end;

//----------------------------------------------------------------------------------------------------------------------

function THighlightAttributes.LoadFromBorlandRegistry(Path: string; Version: Integer): Boolean;

// Reads the attribute values from the registry assuming the structure used by Borland.
// Path is the path to the attribute sub key to read. It is interpreted relative to HKCU.
// Version determines the way the data must be read. It must be > 4 (for Delphi 4 and higher).

const
  StandardPalette: array[0..15] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clDkGray,
    clLtGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite
  );

var
  Registry: TRegistry;
  UseOldStyle: Boolean;
  AttributeSuffix: string;

  //--------------- local functions --------------------------------------------

  function ReadColor(Name: string): TColor;

  var
    AttributeName: string;
    Index: Integer;
    Color: string;

  begin
    Result := clNone;
    AttributeName := Name + AttributeSuffix;
    if Registry.ValueExists(AttributeName) then
      if UseOldStyle then
      begin
        Index := Registry.ReadInteger(AttributeName);
        if Index in [0..15] then
          Result := StandardPalette[Index];
      end
      else
      begin
        Color := Registry.ReadString(AttributeName);
        Result := StringToColor(Color);
      end;
  end;

  //----------------------------------------------------------------------------

  function ReadBoolean(Name: string): Boolean;

  begin
    Result := False;
    if Registry.ValueExists(Name) then
      Result := StrToBool(Registry.ReadString(Name));
  end;

  //--------------- End local functions ----------------------------------------

var
  Color: TColor;
  Switch: Boolean;
  Style: TFontStyles;

begin
  UseOldStyle := Version < 7;
  if UseOldStyle then
    AttributeSuffix := ''
  else
    AttributeSuffix := ' New';

  Result := False;
  Registry := TRegistry.Create;
  try
    with Registry do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly(Path) then
      begin
        try
          Switch := ReadBoolean('Default Foreground');
          if not Switch then
          begin
            Color := ReadColor('Foreground Color');
            if Color <> clNone then
              Foreground := Color;
          end;
          Switch := ReadBoolean('Default Background');
          if not Switch then
          begin
            Color := ReadColor('Background Color');
            if Color <> clNone then
              Background := Color;
          end;
          Style := [];
          Switch := ReadBoolean('Bold');
          if Switch then
            Style := Style + [fsBold];
          Switch := ReadBoolean('Italic');
          if Switch then
            Style := Style + [fsItalic];
          Switch := ReadBoolean('Underline');
          if Switch then
            Style := Style + [fsUnderline];

          Self.Style := Style;
        
          Result := True;
        finally
          CloseKey;
        end;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THighlightAttributes.SetBackground(Value: TColor);

begin
  with FHighlightData do
    if Background <> Value then
    begin
      Background := Value;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THighlightAttributes.SetForeground(Value: TColor);

begin
  with FHighlightData do
    if Foreground <> Value then
    begin
      Foreground := Value;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THighlightAttributes.SetStyle(Value: TFontStyles);

begin
  with FHighlightData do
    if Style <> Value then
    begin
      Style := Value;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THighlightAttributes.LoadFromRegistry(Reg: TRegistry): Boolean;

var
  Key: string;

begin
  Key := Reg.CurrentPath;
  if Reg.OpenKeyReadOnly(Name) then
  begin
    if Reg.ValueExists('Attributes') then
      Reg.ReadBinaryData('Attributes', FHighlightData, SizeOf(FHighlightData));
    Reg.OpenKeyReadOnly('\' + Key);
    Result := True;
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function THighlightAttributes.SaveToRegistry(Reg: TRegistry): Boolean;

var
  Key: string;

begin
  Key := Reg.CurrentPath;
  if Reg.OpenKey(Name, True) then
  begin
    Reg.WriteBinaryData('Attributes', FHighlightData, SizeOf(FHighlightData));
    Reg.OpenKey('\' + Key, False);
    Result := True;
  end
  else
    Result := False;
end;

//----------------- TUCEHighlighter -----------------------------------------------------------------------------------

constructor TUCEHighlighter.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FWindowList := TWindowList.Create;
  FDefaultFilter := '';
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TUCEHighlighter.Destroy;

begin
  FWindowList.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCEHighlighter.EnumUserSettings(Settings: TStrings);

begin
  Settings.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCEHighlighter.UseUserSettings(Path: string): Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCEHighlighter.GetIdentChars: TIdentChars;

begin
  Result := [#33..#255];
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCEHighlighter.LoadFromRegistry(RootKey: HKEY; Key: string): Boolean;

var
  Reg: TRegistry;
  I: Integer;

begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKeyReadOnly(Key) then
    begin
      Result := True;
      for I := 0 to AttributeCount - 1 do
        Result := Result and Attribute[i].LoadFromRegistry(Reg);
    end
    else
      Result := False;
  finally
    Reg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCEHighlighter.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;

var
  Reg: TRegistry;
  I: Integer;

begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKey(Key, True) then
    begin
      Result := True;
      for I := 0 to AttributeCount - 1 do
        Result := Result and Attribute[i].SaveToRegistry(Reg);
    end
    else
      Result := False;
  finally
    Reg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCEHighlighter.GetCapabilities: THighlighterCapabilities;

begin
  Result := [hcRegistry];
end;

//----------------------------------------------------------------------------------------------------------------------

function TUCEHighlighter.GetDefaultFilter: string;

begin
  Result := FDefaultFilter;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCEHighlighter.SetDefaultFilter(Value: string);

begin
  if FDefaultFilter <> Value then
    FDefaultFilter := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TUCEHighlighter.AssignAttributesFrom(const Source: TUCEHighlighter);

// copies all attribute settings from Source to the local list, only attributes whose names
// do match are copied

var
  I, J: Integer;
  AttrName: string;

begin
  for I := 0 to Source.AttributeCount - 1 do
  begin
    AttrName := Source.Attribute[I].Name;
    for J := 0 to AttributeCount - 1 do
      if CompareText(AttrName, Attribute[J].Name) = 0 then
        Attribute[J].Assign(Source.Attribute[I]);
  end;
  FWindowList.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

end.


