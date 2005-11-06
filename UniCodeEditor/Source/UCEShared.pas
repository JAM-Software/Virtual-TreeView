unit UCEShared;

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
// The UCEShared unit provides commonly used data types and interfaces. This allows descentants to implement their own
// classes without to link in the whole UniCodeEditor class and/or to implement several functionality in one class.
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, Graphics;
  
type
  // Defines the interface for a line marker. That is, an entry in the gutter of the edit.
  IUCELineMarker = interface
    procedure Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);
    function GetSize(Index: Integer): TSize;
  end;

  // Defines the inteface for a custom line style.
  IUCELineStyle = interface
    function GetBackground: TColor;
    function GetFontStyles: TFontStyles;
    function GetForceFontStyles: Boolean;
    function GetForeground: TColor;

    property Background: TColor read GetBackground;
    property Foreground: TColor read GetForeground;
    property FontStyles: TFontStyles read GetFontStyles;
    property ForceFontStyles: Boolean read GetForceFontStyles;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
 
