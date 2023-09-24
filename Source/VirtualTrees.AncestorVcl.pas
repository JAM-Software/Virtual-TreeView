unit VirtualTrees.AncestorVCL;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski, look at VirtualTrees.pas as some code moved from there                 }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface

uses
  Winapi.Windows,
  Winapi.oleacc,
  Winapi.ActiveX,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  TVTRenderOLEDataEvent = procedure(Sender: TBaseVirtualTree; const FormatEtcIn: TFormatEtc; out Medium: TStgMedium;
    ForClipboard: Boolean; var Result: HRESULT) of object;

  TVTAncestorVcl = class abstract(TBaseVirtualTree)
  private
    FOnRenderOLEData: TVTRenderOLEDataEvent;     // application/descendant defined clipboard formats

  protected
    function DoRenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HRESULT; override;
    property OnRenderOLEData: TVTRenderOLEDataEvent read FOnRenderOLEData write FOnRenderOLEData;
  public //methods
    function PasteFromClipboard(): Boolean; override;
 end;

implementation
uses
  System.Classes,
  Vcl.AxCtrls,
  VirtualTrees.ClipBoard,
  VirtualTrees.DataObject;

resourcestring
  SClipboardFailed = 'Clipboard operation failed.';

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorVcl.DoRenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HRESULT;
begin
  Result := E_FAIL;
  if Assigned(FOnRenderOLEData) then
    FOnRenderOLEData(Self, FormatEtcIn, Medium, ForClipboard, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorVcl.PasteFromClipboard(): Boolean;

// Reads what is currently on the clipboard into the tree (if the format is supported).
// Note: If the application wants to have text or special formats to be inserted then it must implement
//       its own code (OLE). Here only the native tree format is accepted.

var
  Data: IDataObject;
  Source: TBaseVirtualTree;

begin
  Result := False;
  if not (toReadOnly in TreeOptions.MiscOptions) then
  begin
    if OleGetClipboard(Data) <> S_OK then
      RaiseVTError(SClipboardFailed, hcTFClipboardFailed)
    else
    begin
      // Try to get the source tree of the operation to optimize the operation.
      Source := GetTreeFromDataObject(Data);
      Result := ProcessOLEData(Source, Data, FocusedNode, DefaultPasteMode, Assigned(Source) and
        (tsCutPending in Source.TreeStates));
      if Assigned(Source) then
      begin
        if Source <> Self then
          Source.FinishCutOrCopy
        else
          DoStateChange([], [tsCutPending]);
      end;
    end;
  end;
end;

end.
