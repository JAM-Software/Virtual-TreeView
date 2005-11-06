unit UsesClause;

//----------------------------------------------------------------------------------------------------------------------
// Version 1.0.0
//
// UsesClause is a helper unit to manipulate a module's uses clauses at design time.
//
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
// The original code is UsesClause.pas, released 1. July 2003.
//
// The initial developer of the original code is:
//   Dipl. Ing. Mike Lischke, Delphi Gems software solutions (public@delphi-gems.com, www.delphi-gems.com).
//
// Portions created by Delphi Gems are
//   (C) 1999-2003 Delphi Gems. All Rights Reserved.
//
// Credits:
//   Ondrej Kelle (TOndrej) for his work in JclUsesWizard and JclParseUses, which was what got me started.
//
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
  Classes, Sysutils, ToolsAPI;

function MakeClassLinkable(const Module: IOTAModule; UnitName: string; InterfaceSection: Boolean): Boolean; overload;
function MakeClassLinkable(const Module: IOTAModule; AClass: TClass; InterfaceSection: Boolean): Boolean; overload;

function MakeClassLinkable(const Module: TFileName; UnitName: string; InterfaceSection: Boolean): Boolean; overload;
function MakeClassLinkable(const Module: TFileName; AClass: TClass; InterfaceSection: Boolean): Boolean; overload;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Windows, TypInfo, RTLConsts, LexicalTools;

const
  // Do not modify the copyright in any way! Usage of this unit is prohibited without the copyright notice
  // in the compiled binary file.
  Copyright: string = 'UsesClause © 1999-2003 Mike Lischke, Delphi Gems software solutions';

//----------------- Helper functions -----------------------------------------------------------------------------------

procedure ReadEditorBuffer(Reader: IOTAEditReader; const Stream: TStringStream);

const
  BufferSize = 1024;

var
  ReaderPos, Read: Integer;
  Buffer: array[0..BufferSize] of Char;

begin
  ReaderPos := 0;
  repeat
    Read := Reader.GetText(ReaderPos, Buffer, BufferSize);
    Inc(ReaderPos, Read);
    Stream.Write(Buffer, Read);
  until Read < BufferSize;
end;

//----------------- Main functions -------------------------------------------------------------------------------------

function MakeClassLinkable(const Module: IOTAModule; UnitName: string; InterfaceSection: Boolean): Boolean;

// Adds the given unit name to the uses clause of Module. If InterfaceSection is True then the unit is added to the
// uses clause in the interface section. Otherwise it ends up in the implementation section.

var
  Editor: IOTASourceEditor;
  Reader: IOTAEditReader;
  Writer: IOTAEditWriter;

  Source: TStringStream;
  Target: TMemoryStream;

  I: Integer;

begin
  Result := False;

  // Find the source editor.
  Editor := nil;
  for I := 0 to Module.GetModuleFileCount - 1 do
    if Supports(Module.ModuleFileEditors[I], IOTASourceEditor, Editor) then
      Break;

  if Assigned(Editor) then
  begin
    Reader := Editor.CreateReader;
    Source := TStringStream.Create('');
    try
      ReadEditorBuffer(Reader, Source);
      Target := TMemoryStream.Create;
      try
        Result := AddToUsesClause(Source, Target, UnitName, '', InterfaceSection);
        if Result then
        begin
          Target.Position := 0;
          // Explicitely release reader interface. We must not have open read and write interfaces at the same time.
          Reader := nil;
          Writer := Editor.CreateUndoableWriter;
          Writer.DeleteTo(MaxInt);
          Writer.Insert(PAnsiChar(Target.Memory));
        end;
      finally
        Target.Free;
      end;
    finally
      Source.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeClassLinkable(const Module: IOTAModule; AClass: TClass; InterfaceSection: Boolean): Boolean; overload;

// Another variant to add a unit to the uses clause of a given module.
// This method takes a class and determines the unit in which it is located and adds that unit to the uses clause of
// the given module.

var
  SourceUnit: string;
  
begin
  // The source unit is stored in the type data of the class and can easily be used thank RTTI.
  SourceUnit := GetTypeData(AClass.ClassInfo).UnitName;
  if SourceUnit = '' then
    raise Exception.Create('MakeClassLinkable: Source unit name cannot be determined.');
  Result := MakeClassLinkable(Module, SourceUnit, InterfaceSection); 
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeClassLinkable(const Module: TFileName; UnitName: string; InterfaceSection: Boolean): Boolean;

// Another variant to add a unit to the uses clause of a given module.
// This method determines the module interface from the given module name (with or without path) and adds the given
// unit to the module's uses clauses.

var
  TargetUnit: string;
  ModuleInterface: IOTAModule;
  Services: IOTAModuleServices;
  I: Integer;

begin
  Result := False;

  // Convert the given module name into a module interface with which we can work.
  TargetUnit := ExtractFileName(Module);
  // ExtractFileName returns an empty string if no extension was given.
  if TargetUnit = '' then
    raise Exception.Create('MakeClassLinkable: Target unit name cannot be determined.');

  Services := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to Services.ModuleCount - 1 do
  begin
    if ExtractFileName(Services.Modules[I].FileName) = TargetUnit then
    begin
      ModuleInterface := Services.Modules[I];
      Result := MakeClassLinkable(ModuleInterface, UnitName, InterfaceSection);
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeClassLinkable(const Module: TFileName; AClass: TClass; InterfaceSection: Boolean): Boolean;

// Another variant to add a unit to the uses clause of a given module.
// This method takes a class and determines the unit in which it is located and adds that unit to the uses clause of
// the given module.

var                                         
  SourceUnit: string;
  TargetUnit: string;
  ModuleInterface: IOTAModule;
  Services: IOTAModuleServices;
  I: Integer;

begin
  Result := False;

  // The source unit is stored in the type data of the class and can easily be used thanks RTTI.
  SourceUnit := GetTypeData(AClass.ClassInfo).UnitName;
  if SourceUnit = '' then
    raise Exception.Create('MakeClassLinkable: Source unit name cannot be determined.');

  // Now convert the given module name into a module interface with which we can work.
  TargetUnit := ExtractFileName(Module);
  // ExtractFileName returns an empty string if no extension was given.
  if TargetUnit = '' then
    raise Exception.Create('MakeClassLinkable: Target unit name cannot be determined.');

  Services := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to Services.ModuleCount - 1 do
  begin
    if ExtractFileName(Services.Modules[I].FileName) = TargetUnit then
    begin
      ModuleInterface := Services.Modules[I];
      Result := MakeClassLinkable(ModuleInterface, SourceUnit, InterfaceSection);
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
