unit Vcl.ClipboardHelper;

interface

uses
  Vcl.Clipbrd, Winapi.Windows;

type
  TClipboardHelper = class helper for TClipboard
  protected
    function GetAsFormat(uFormat: UINT): string;
    function GetAsHTML: string;
    function GetAsRTF: string;
  public
    property AsHTML: string read GetAsHTML;
    property AsRTF: string read GetAsRTF;
  end;

implementation

uses
  VirtualTrees.ClipBoard;

{ TClipboardHelper }

function TClipboardHelper.GetAsFormat(uFormat: UINT): string;
var
  Data: THandle;
begin
  Open;
  Data := GetClipboardData(uFormat);
  try
    if Data <> 0 then
      Result := string(AnsiString(PAnsiChar(GlobalLock(Data))))
    else
      Result := '';
  finally
    if Data <> 0 then
      GlobalUnlock(Data);
    Close;
  end;
end;

function TClipboardHelper.GetAsHTML: string;
begin
  Result := GetAsFormat(CF_HTML);
end;

function TClipboardHelper.GetAsRTF: string;
begin
  Result := GetAsFormat(CF_VRTF);
end;

end.
