unit DrawTreeDemo;

// Virtual Treeview sample form demonstrating following features:
//   - General use of TVirtualDrawTree.
//   - Use of vertical node image alignment.
//   - Effective use of node initialization on demand to load images.
// Written by Mike Lischke.
//
// Note: define the symbol "GraphicEx" if you have my GraphicEx library
// available (see http://www.delphi-gems.com) which allows to load
// more image formats into the application.
// Otherwise disable the conditional symbol to compile this demo.

{.$define GraphicEx}

// For some things to work we need code, which is classified as being unsafe for .NET.
// Additionally, there are a few platform warnings, which also have no meaning at all for this project.
{$warn UNSAFE_TYPE off}
{$warn UNSAFE_CAST off}
{$warn UNSAFE_CODE off}
{$warn SYMBOL_PLATFORM off}
{$warn UNIT_PLATFORM off}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, StdCtrls, {$ifdef GraphicEx} GraphicEx, {$else} JPEG, {$endif}
  ImgList, ComCtrls, UITypes;

type
  TDrawTreeForm = class(TForm)
    VDT1: TVirtualDrawTree;
    Label7: TLabel;
    SystemImages: TImageList;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure VDT1CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure VDT1DrawHint(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; R: TRect; Column: TColumnIndex);
    procedure VDT1DrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
    procedure VDT1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VDT1GetHintSize(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var R: TRect);
    procedure VDT1GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var Index: TImageIndex);
    procedure VDT1GetNodeWidth(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      var NodeWidth: Integer);
    procedure VDT1HeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VDT1InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VDT1InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TrackBar1Change(Sender: TObject);
    procedure VDT1StateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
  private
    FThumbSize: Integer;
    FExtensionsInitialized: Boolean;
    FExtensionList: TStringList;
    FDriveStrings: string;
    function CanDisplay(const Name: String): Boolean;
    function GetDriveString(Index: Integer): string;
    function ReadAttributes(const Name: UnicodeString): Cardinal;
    procedure RescaleImage(Source, Target: TBitmap);
  end;

var
  DrawTreeForm: TDrawTreeForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  FileCtrl, ShellAPI, Mask, ShlObj, ActiveX, States,VirtualTrees.Utils;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

type
  // This data record contains all necessary information about a particular file system object.
  // This can either be a folder (virtual or real) or an image file.
  PShellObjectData = ^TShellObjectData;
  TShellObjectData = record
    FullPath,
    Display: UnicodeString;
    Attributes: Cardinal;
    OpenIndex,
    CloseIndex: Integer;      // image indices into the system image list
    Image: TBitmap;
    Properties: UnicodeString;   // some image properties, preformatted
  end;

//----------------- utility functions ----------------------------------------------------------------------------------

function IncludeTrailingBackslash(const S: string): string;

begin
  if not IsPathDelimiter(S, Length(S)) then
    Result := S + '\'
  else
    Result := S;
end;

//----------------------------------------------------------------------------------------------------------------------

function ExcludeTrailingBackslash(const S: string): string;

begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result) - 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function HasChildren(const Folder: string): Boolean;

// Determines whether folder contains other file objects.

var
  SR: TSearchRec;

begin
  Result := FindFirst(IncludeTrailingBackslash(Folder) + '*.*', faReadOnly or faHidden or faSysFile or faArchive, SR) = 0;
  if Result then
    FindClose(SR);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetIconIndex(Name: string; Flags: Cardinal): Integer;

// Returns the index of the system icon for the given file object.

var
  SFI: TSHFileInfo;

begin
  if SHGetFileInfo(PChar(Name), 0, SFI, SizeOf(TSHFileInfo), Flags) = 0 then
    Result := -1
  else
    Result := SFI.iIcon;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure GetOpenAndClosedIcons(Name: string; var Open, Closed: Integer);

begin
  Closed := GetIconIndex(Name, SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  Open := GetIconIndex(Name, SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON);
end;

//----------------- TDrawTreeForm --------------------------------------------------------------------------------------

procedure TDrawTreeForm.FormCreate(Sender: TObject);

var
  SFI: TSHFileInfo;
  I,
  Count: Integer;
  DriveMap,
  Mask: Cardinal;

begin
  VDT1.NodeDataSize := SizeOf(TShellObjectData);

  // Fill root level of image tree. Determine which drives are mapped.
  Count := 0;
  DriveMap := GetLogicalDrives;
  Mask := 1;
  for I := 0 to 25 do
  begin
    if (DriveMap and Mask) <> 0 then
      Inc(Count);
    Mask := Mask shl 1;
  end;
  VDT1.RootNodeCount := Count;
  // Determine drive strings which are used in the initialization process.
  Count := GetLogicalDriveStrings(0, nil);
  SetLength(FDriveStrings, Count);
  GetLogicalDriveStrings(Count, PChar(FDriveStrings));
  
  SystemImages.Handle := SHGetFileInfo('', 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SystemImages.ShareImages := True;

  FThumbSize := 200;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDrawTreeForm.CanDisplay(const Name: string): Boolean;

// Determines whether the given file is one we can display in the image tree.

var
  Ext: string;
  I: Integer;
  
begin
  if not FExtensionsInitialized then
  begin
    FExtensionsInitialized := True;
    FExtensionList := TStringList.Create;
    {$ifdef GraphicEx}
      FileFormatList.GetExtensionList(FExtensionList);
      for I := 0 to FExtensionList.Count - 1 do
        FExtensionList[I] := '.' + FExtensionList[I];
    {$else}
    // GraphicEx is not used so add some default extensions
    with FExtensionList do
    begin
      Add('.bmp');
      Add('.ico');
      Add('.jpg');
      Add('.jpeg');
      Add('.wmf');
      Add('.emf');
    end;
    {$endif}
    FExtensionList.Sort;
  end;

  Ext := ExtractFileExt(Name);
  Result := FExtensionList.Find(Ext, I);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDrawTreeForm.GetDriveString(Index: Integer): string;

// Helper method to extract a sub string (given by Index) from FDriveStrings.

var
  Head, Tail: PChar;

begin
  Head := PChar(FDriveStrings);
  Result := '';
  repeat
    Tail := Head;
    while Tail^ <> #0 do
      Inc(Tail);
    if Index = 0 then
    begin
      SetString(Result, Head, Tail - Head);
      Break;
    end;
    Dec(Index);
    Head := Tail + 1;
  until Head^ = #0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDrawTreeForm.ReadAttributes(const Name: UnicodeString): Cardinal;

// Determines the attributes of the given shell object (file, folder).

const
  SFGAO_CONTENTSMASK = $F0000000; // This value is wrongly defined in ShlObj.

var
  Desktop: IShellFolder;
  Eaten: Cardinal;
  PIDL: PItemIDList;
  Malloc: IMalloc;

begin
  // Get the root folder of the shell name space.
  SHGetDesktopFolder(Desktop);
  // While parsing the name also the shell object's attributes are determined.
  // These is what we are really interested in.
  Result := SFGAO_DISPLAYATTRMASK or SFGAO_CONTENTSMASK or SFGAO_COMPRESSED;
  Desktop.ParseDisplayName(0, nil, PWideChar(Name), Eaten, PIDL, Result);
  // Don't forget to free the returned PIDL. The shell folder is released automatically.
  SHGetMalloc(Malloc);
  Malloc.Free(PIDL);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.RescaleImage(Source, Target: TBitmap);

// if source is in at least one dimension larger than the thumb size then
// rescale source but keep aspect ratio

var
  NewWidth,
  NewHeight: Integer;
begin
  if (Source.Width > FThumbSize) or (Source.Height > FThumbSize) then
  begin
    if Source.Width > Source.Height then
    begin
      NewWidth := FThumbSize;
      NewHeight := Round(FThumbSize * Source.Height / Source.Width);
    end
    else
    begin
      NewHeight := FThumbSize;
      NewWidth := Round(FThumbSize * Source.Width / Source.Height);
    end;

    Target.Width := NewWidth;
    Target.Height := NewHeight;
    SetStretchBltMode(Target.Canvas.Handle, HALFTONE);
    StretchBlt(Target.Canvas.Handle, 0, 0, NewWidth, NewHeight,
      Source.Canvas.Handle, 0, 0, Source.Width, Source.Height, SRCCOPY);
  end
  else
    Target.Assign(Source);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PShellObjectData;
  Picture: TPicture;

begin
  Data := Sender.GetNodeData(Node);
  if ParentNode = nil then
  begin
    // top level node, initialize first enumeration
    Data.FullPath := GetDriveString(Node.Index);
    Data.Display := Data.FullPath;
    GetOpenAndClosedIcons(Data.FullPath, Data.OpenIndex, Data.CloseIndex);
  end
  else
  begin
    Picture := TPicture.Create;
    Data.Display := ExtractFileName(ExcludeTrailingBackslash(Data.FullPath));
    if (Data.Attributes and SFGAO_FOLDER) = 0 then
    try
      try
        Data.Image := TBitmap.Create;
        Picture.LoadFromFile(Data.FullPath);
        if not (Picture.Graphic is TBitmap) then
        begin
          // Some extra steps needed to keep non TBitmap descentants alive when
          // scaling. This is needed because when accessing Picture.Bitmap all
          // non-TBitmap content will simply be erased (definitly the wrong
          // action, but we can't do anything to prevent this). Hence we
          // must explicitly draw the graphic to a bitmap.
          with Data.Image do
          begin
            Width := Picture.Width;
            Height := Picture.Height;
            Canvas.Draw(0, 0, Picture.Graphic);
          end;
          Picture.Bitmap.Assign(Data.Image);
        end;
        RescaleImage(Picture.Bitmap, Data.Image);

        // Collect some additional image properties.
        Data.Properties := Data.Properties + Format('%d x %d pixels', [Picture.Width, Picture.Height]);
        case Picture.Bitmap.PixelFormat of
          pf1bit:
            Data.Properties := Data.Properties + ', 2 colors';
          pf4bit:
            Data.Properties := Data.Properties + ', 16 colors';
          pf8bit:
            Data.Properties := Data.Properties + ', 256 colors';
          pf15bit:
            Data.Properties := Data.Properties + ', 32K colors';
          pf16bit:
            Data.Properties := Data.Properties + ', 64K colors';
          pf24bit:
            Data.Properties := Data.Properties + ', 16M colors';
          pf32bit:
            Data.Properties := Data.Properties + ', 16M+ colors';
        end;
        if Cardinal(Data.Image.Height) + 4 > TVirtualDrawTree(Sender).DefaultNodeHeight then
            Sender.NodeHeight[Node] := Data.Image.Height + 4;
      except
        Data.Image.Free;
        Data.Image := nil;
      end;
    finally
      Picture.Free;
    end;
  end;
  Data.Attributes := ReadAttributes(Data.FullPath);
  if ((Data.Attributes and SFGAO_HASSUBFOLDER) <> 0) or
    (((Data.Attributes and SFGAO_FOLDER) <> 0) and HasChildren(Data.FullPath)) then
    Include(InitialStates, ivsHasChildren);

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  Data: PShellObjectData;
  
begin
  Data := Sender.GetNodeData(Node);
  Data.Image.Free;
  Finalize(Data^); // Clear string data.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1DrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);

// This is the main paint routine for a node in a draw tree. There is nothing special here. Demonstrating the
// specific features of a draw tree (compared to the string tree) is a bit difficult, since the only difference is
// that the draw tree does not handle node content (captions in the case of the string tree).

var
  Data: PShellObjectData;
  X: Integer;
  S: UnicodeString;
  R: TRect;

begin   
  with Sender as TVirtualDrawTree, PaintInfo do
  begin
    Data := Sender.GetNodeData(Node);
    if (Column = FocusedColumn) and (Selected[Node]) then
      Canvas.Font.Color := clHighlightText
    else
      if (Data.Attributes and SFGAO_COMPRESSED) <> 0 then
        Canvas.Font.Color := clBlue
      else
        Canvas.Font.Color := clWindowText;

    SetBKMode(Canvas.Handle, TRANSPARENT);
    
    R := ContentRect;
    InflateRect(R, -TextMargin, 0);
    Dec(R.Right);
    Dec(R.Bottom);
    S := '';
    case Column of
      0, 2:
        begin
          if Column = 2 then
          begin
            if Assigned(Data.Image) then
              S:= Data.Properties;
          end
          else
            S := Data.Display;
          if Length(S) > 0 then
          begin
            with R do
            begin
              if (NodeWidth - 2 * Margin) > (Right - Left) then
                S := ShortenString(Canvas.Handle, S, Right - Left);
            end;
            DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R, DT_TOP or DT_LEFT or DT_VCENTER or DT_SINGLELINE);
          end;
        end;
      1:
        begin
          if Assigned(Data.Image) then
          begin
            X := ContentRect.Left + (VDT1.Header.Columns[1].Width - Data.Image.Width - Margin) div 2;
            BitBlt(Canvas.Handle, X, ContentRect.Top + 2, Data.Image.Width, Data.Image.Height, Data.Image.Canvas.Handle,
              0, 0, SRCCOPY);
          end;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1GetNodeWidth(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  var NodeWidth: Integer);

// Since the draw tree does not know what is in a cell, we have to return the width of the content (not the entire
// cell width, this could be determined by the column width).

var
  Data: PShellObjectData;
  AMargin: Integer;

begin
  with Sender as TVirtualDrawTree do
    AMargin := TextMargin;

  begin
    Data := Sender.GetNodeData(Node);
    case Column of
      0:
        begin
          if Sender.NodeParent[Node] = nil then
            NodeWidth := Canvas.TextWidth(Data.FullPath) + 2 * AMargin
          else
            NodeWidth := Canvas.TextWidth(ExtractFileName(Data.FullPath)) + 2 * AMargin;
        end;
      1:
        begin
          if Assigned(Data.Image) then
            NodeWidth := Data.Image.Width;
        end;
      2:
        NodeWidth := Canvas.TextWidth(Data.Properties) + 2 * AMargin;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

// Called just before a node with children (only folder nodes can have children) is expanded.

var
  Data,
  ChildData: PShellObjectData;
  SR: TSearchRec;
  ChildNode: PVirtualNode;
  NewName: String;

begin
  Data := Sender.GetNodeData(Node);
  if FindFirst(IncludeTrailingBackslash(Data.FullPath) + '*.*', faAnyFile, SR) = 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          NewName := IncludeTrailingBackslash(Data.FullPath) + SR.Name;
          if (SR.Attr and faDirectory <> 0) or CanDisplay(NewName) then
          begin
            ChildNode := Sender.AddChild(Node);
            ChildData := Sender.GetNodeData(ChildNode);
            ChildData.FullPath := NewName;
            ChildData.Attributes := ReadAttributes(NewName);
            if (ChildData.Attributes and SFGAO_FOLDER) = 0 then
              ChildData.Properties := Format('%n KB, ', [SR.Size / 1024]);
            GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex, ChildData.CloseIndex);

            Sender.ValidateNode(Node, False);
          end;
        end;
      until FindNext(SR) <> 0;
      ChildCount := Sender.ChildCount[Node];

      // finally sort node
      if ChildCount > 0 then
        Sender.Sort(Node, 0, TVirtualStringTree(Sender).Header.SortDirection, False);
    finally
      FindClose(SR);
      Screen.Cursor := crDefault;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex);

// Returns the proper node image which has been determine on initialization time. Also overlay images are
// used properly for shared folders.

var
  Data: PShellObjectData;
  
begin
  if Column = 0 then
  begin
    Data := Sender.GetNodeData(Node);
    case Kind of
      ikNormal,
      ikSelected:
        begin
          if Sender.Expanded[Node] then
            Index := Data.OpenIndex
          else
            Index := Data.CloseIndex;
        end;
      ikOverlay:
        if (Data.Attributes and SFGAO_SHARE) <> 0 then
          Index := 0
        else
          if (Data.Attributes and SFGAO_LINK) <> 0 then
            Index := 1;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1GetHintSize(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var R: TRect);

// Draw trees must manage parts of the hints themselves. Here we return the size of the hint window we want to show
// or an empty rectangle in the case we don't want a hint at all.

var
  Data: PShellObjectData;
  
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) and Assigned(Data.Image) then
    R := Rect(0, 0, 2 * Data.Image.Width, 2 * Data.Image.Height)
  else
    R := Rect(0, 0, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1DrawHint(Sender: TBaseVirtualTree; Canvas: TCanvas; Node: PVirtualNode; R: TRect;
  Column: TColumnIndex);

// Here we actually paint the hint. It is the image in a larger size.

var
  Data: PShellObjectData;

begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) and Assigned(Data.Image) then
  begin
    SetStretchBltMode(Canvas.Handle, HALFTONE);
    StretchBlt(Canvas.Handle, 0, 0, 2 * Data.Image.Width, 2 * Data.Image.Height, Data.Image.Canvas.Handle, 0, 0,
      Data.Image.Width, Data.Image.Height, SRCCOPY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);

// The node comparison routine is the heart of the tree sort. Here we have to tell the caller which node we consider
// being "larger" or "smaller".

var
  Data1,
  Data2: PShellObjectData;

begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  // Folder are always before files. Check if *both* are folders or *both* are non-folders, but not different.
  if ((Data1.Attributes xor Data2.Attributes) and SFGAO_FOLDER) <> 0 then
  begin
    // One of both is a folder the other is a file.
    if (Data1.Attributes and SFGAO_FOLDER) <> 0 then
      Result := -1
    else
      Result := 1;
  end
  else
    // Both are of same type (folder or file). Just compare captions.
    // Note that we use ANSI comparison, while the strings are Unicode. Since this will implicitely convert the captions
    // to ANSI for comparation it might happen that the sort order is wrong for names which contain text in a language
    // other than the current OS language. A full blown Unicode comparison is beyond the scope of this demo.
    Result := CompareText(Data1.FullPath, Data2.FullPath);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1HeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

// Click handler to switch the column on which will be sorted. Since we cannot sort image data sorting is actually
// limited to the main column.

begin
  if HitInfo.Button = mbLeft then
  begin
    with Sender do
    begin
      if HitInfo.Column <> MainColumn then
        SortColumn := NoColumn
      else
      begin
        if SortColumn = NoColumn then
        begin
          SortColumn := HitInfo.Column;
          SortDirection := sdAscending;
        end
        else
          if SortDirection = sdAscending then
            SortDirection := sdDescending
          else
            SortDirection := sdAscending;
        Treeview.SortTree(SortColumn, SortDirection, False);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.TrackBar1Change(Sender: TObject);

// This part has nothing to do with the tree content and is only to show the effect of vertical image alignment for nodes
// (since this does not justify an own demo).
// Btw: look how fast this stuff is. Even with several thousands of nodes you still can adjust the position interactively.

var
  Run: PVirtualNode;

begin
  Label3.Caption := Format('%d%%', [Trackbar1.Position]);
  with VDT1, Trackbar1 do
  begin
    BeginUpdate;
    try
      Run := GetFirst;
      while Assigned(Run) do
      begin
        VerticalAlignment[Run] := Position;
        Run := GetNextVisible(Run);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDrawTreeForm.VDT1StateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
