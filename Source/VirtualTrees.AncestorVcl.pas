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
    function RenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HResult; override;

    property OnRenderOLEData: TVTRenderOLEDataEvent read FOnRenderOLEData write FOnRenderOLEData;
  public //methods
    function PasteFromClipboard(): Boolean; override;
    procedure CopyToClipboard(); override;
    procedure CutToClipboard(); override;
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

function TVTAncestorVcl.RenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium;
  ForClipboard: Boolean): HResult;

// Returns a memory expression of all currently selected nodes in the Medium structure.
// Note: The memory requirement of this method might be very high. This depends however on the requested storage format.
//       For HGlobal (a global memory block) we need to render first all nodes to local memory and copy this then to
//       the global memory in Medium. This is necessary because we have first to determine how much
//       memory is needed before we can allocate it. Hence for a short moment we need twice the space as used by the
//       nodes alone (plus the amount the nodes need in the tree anyway)!
//       With IStream this does not happen. We directly stream out the nodes and pass the constructed stream along.

  //--------------- local function --------------------------------------------

  procedure WriteNodes(Stream: TStream);

  var
    Selection: TNodeArray;
    I: Integer;

  begin
    if ForClipboard then
      Selection := GetSortedCutCopySet(True)
    else
      Selection := GetSortedSelection(True);
    for I := 0 to High(Selection) do
      WriteNode(Stream, Selection[I]);
  end;

  //--------------- end local function ----------------------------------------

var
  Data: PCardinal;
  ResPointer: Pointer;
  ResSize: Integer;
  OLEStream: IStream;
  VCLStream: TStream;

begin
  ZeroMemory (@Medium, SizeOf(Medium));

  // We can render the native clipboard format in two different storage media.
  if (FormatEtcIn.cfFormat = CF_VIRTUALTREE) and (FormatEtcIn.tymed and (TYMED_HGLOBAL or TYMED_ISTREAM) <> 0) then
  begin
    VCLStream := nil;
    try
      Medium.unkForRelease := nil;
      // Return data in one of the supported storage formats, prefer IStream.
      if FormatEtcIn.tymed and TYMED_ISTREAM <> 0 then
      begin
        // Create an IStream on a memory handle (here it is 0 which indicates to implicitely allocated a handle).
        // Do not use TStreamAdapter as it is not compatible with OLE (when flushing the clipboard OLE wants the HGlobal
        // back which is not supported by TStreamAdapater).
        CreateStreamOnHGlobal(0, True, OLEStream);
        VCLStream := TOLEStream.Create(OLEStream);
        WriteNodes(VCLStream);
        // Rewind stream.
        VCLStream.Position := 0;
        Medium.tymed := TYMED_ISTREAM;
        IUnknown(Medium.stm) := OLEStream;
        Result := S_OK;
      end
      else
      begin
        VCLStream := TMemoryStream.Create;
        WriteNodes(VCLStream);
        ResPointer := TMemoryStream(VCLStream).Memory;
        ResSize := VCLStream.Position;

        // Allocate memory to hold the string.
        if ResSize > 0 then
        begin
          Medium.hGlobal := GlobalAlloc(GHND or GMEM_SHARE, ResSize + SizeOf(Cardinal));
          Data := GlobalLock(Medium.hGlobal);
          // Store the size of the data too, for easy retrival.
          Data^ := ResSize;
          Inc(Data);
          Move(ResPointer^, Data^, ResSize);
          GlobalUnlock(Medium.hGlobal);
          Medium.tymed := TYMED_HGLOBAL;

          Result := S_OK;
        end
        else
          Result := E_FAIL;
      end;
    finally
      // We can free the VCL stream here since it was either a pure memory stream or only a wrapper around
      // the OLEStream which exists independently.
      VCLStream.Free;
    end;
  end
  else // Ask application descendants to render self defined formats.
    Result := DoRenderOLEData(FormatEtcIn, Medium, ForClipboard);
end;

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

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorVcl.CopyToClipboard();

var
  lDataObject: IDataObject;

begin
  if SelectedCount > 0 then
  begin
    lDataObject := TVTDataObject.Create(Self, True);
    if OleSetClipboard(lDataObject) = S_OK then
    begin
      MarkCutCopyNodes;
      DoStateChange([tsCopyPending]);
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTAncestorVcl.CutToClipboard();
var
  lDataObject: IDataObject;
begin
  if (SelectedCount > 0) and not (toReadOnly in TreeOptions.MiscOptions) then
  begin
    lDataObject := TVTDataObject.Create(Self, True);
    if OleSetClipboard(lDataObject) = S_OK then
    begin
      MarkCutCopyNodes;
      DoStateChange([tsCutPending], [tsCopyPending]);
      Invalidate;
    end;
  end;
end;

end.
