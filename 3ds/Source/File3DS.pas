unit File3DS;

// Implementation of an universal 3DS file reader (and writer). This is the main file of the
// 3DS import library. Currently only loading of 3DS files (Mesh files  * .3ds, Project files  * .prj
// and Material files  * .mli) is supported.
//
// Note: Be careful when using LoadFromStream, because chunk data (in opposition to the
//       chunk structure) will be loaded on demand, i.e. when it is needed. Therefore the
//       passed stream must be available during load.
//       LoadFromStream does not make a copy of the passed stream, but keeps a reference
//       which must stay valid either during the entire lifetime of TFile3DS or at least
//       'til all chunks have been read (by accessing them all once).
//
// Version    - 1.2
//
// (c) Copyright 1999-2005, Mike Lischke (public@soft-gems.net)

{$ALIGN ON}
{$MINENUMSIZE 4}
{$RANGECHECKS OFF}

interface

uses
  Classes, Types3DS;

type
  TFile3DS = class;

     // ----- support classes -----
     // All structure data of a 3DS file is actually held in TFile3DS.FDatabase as a
     // tree with lots of links across the various chunks.
     // For convinience and speed the data of the chunks is collected into some
     // special structures (FMaterialList etc.) and presented to the user
     // by the following helper classes:

  TMaterialList = class
  private
    FOwner: TFile3DS;
    FLocalList: TList;
    function GetCount: Integer;
    function GetMaterial(Index: Integer): PMaterial3DS;
    function GetMaterialByName(const Name: string): PMaterial3DS;
  public
    constructor Create(AOwner: TFile3DS); virtual;
    destructor Destroy; override;
    procedure ClearList;

    property Count: Integer read GetCount;
    property Material[Index: Integer]: PMaterial3DS read GetMaterial; default;
    property MaterialByName[const Name: string]: PMaterial3DS read GetMaterialByName;
  end;

  TObjectList = class
  private
    FOwner: TFile3DS;
    FMeshList: TList;
    FOmniList: TList;
    FSpotList: TList;
    FCameraList: TList;
    function GetCamera(Index: Integer): PCamera3DS;
    function GetCamCount: Integer;
    function GetMeshObjectCount: Integer;
    function GetMesh(Index: Integer): PMesh3DS;
    function GetOmniCount: Integer;
    function GetOmniLight(Index: Integer): PLight3DS;
    function GetSpotCount: Integer;
    function GetSpotLight(Index: Integer): PLight3DS;
  public
    constructor Create(AOwner: TFile3DS); virtual;
    destructor Destroy; override;

    procedure ClearLists;

    property CameraCount: Integer read GetCamCount;
    property MeshCount: Integer read GetMeshObjectCount;
    property OmniLightCount: Integer read GetOmniCount;
    property SpotLightCount: Integer read GetSpotCount;
    property Mesh[Index: Integer]: PMesh3DS read GetMesh;
    property Camera[Index: Integer]: PCamera3DS read GetCamera;
    property OmniLight[Index: Integer]: PLight3DS read GetOmniLight;
    property SpotLight[Index: Integer]: PLight3DS read GetSpotLight;
  end;

  TKeyFramer = class
  private
    FOwner: TFile3DS;
    FMeshMotionList: TList;
    FOmniMotionList: TList;
    FSpotMotionList: TList;
    FCameraMotionList: TList;
    FAmbientMotion: PKFAmbient3DS;
    function GetAmbientMotion: PKFAmbient3DS;
    function GetCameraMotion(Index: Integer): PKFCamera3DS;
    function GetCamMotionCount: Integer;
    function GetKFSets: TKFSets3DS;
    function GetMeshMotionCount: Integer;
    function GetMeshMotion(Index: Integer): PKFMesh3DS;
    function GetOmniMotionCount: Integer;
    function GetOmniLightMotion(Index: Integer): PKFOmni3DS;
    function GetSpotMotionCount: Integer;
    function GetSpotLightMotion(Index: Integer): PKFSpot3DS;
  public
    constructor Create(AOwner: TFile3DS); virtual;
    destructor Destroy; override;

    procedure ClearLists;

    property AmbientLightMotion: PKFAmbient3DS read GetAmbientMotion;
    property CameraMotionCount: Integer read GetCamMotionCount;
    property MeshMotionCount: Integer read GetMeshMotionCount;
    property OmniLightMotionCount: Integer read GetOmniMotionCount;
    property SpotLightMotionCount: Integer read GetSpotMotionCount;
    property MeshMotion[Index: Integer]: PKFMesh3DS read GetMeshMotion; default;
    property CameraMotion[Index: Integer]: PKFCamera3DS read GetCameraMotion;
    property OmniLightMotion[Index: Integer]: PKFOmni3DS read GetOmniLightMotion;
    property Settings: TKFSets3DS read GetKFSets;
    property SpotLightMotion[Index: Integer]: PKFSpot3DS read GetSpotLightMotion;
  end;

     // TFile3DS is the  main class and supplies the user with all available data
     // from a specific 3DS file. The data is currently read only, but the class might be
     // finished sometime later...
  TFile3DS = class
  private
    FNodeList: PNodeList;
    FDatabase: TDatabase3DS;
    FStream: TStream;
    FMaterialList: TMaterialList;
    FObjectList: TObjectList;
    FKeyFramer: TKeyFramer;
    FFileName: string;
    function GetAtmosphereData: TAtmosphere3DS;
    function GetBackgroundData: TBackground3DS;
    function GetDatabaseType: TDBType3DS;
    function GetMeshSettings: TMeshSet3DS;
    function GetViewportData: TViewport3DS;
    function GetDatabaseRelease: TReleaseLevel;
    function GetMeshRelease: TReleaseLevel;
  protected
    procedure AddToNodeList(Chunk: PChunk3DS);
    procedure AssignParentNames;
    procedure CheckListNodeIDs;
    procedure CreateDatabase;
    function FindNodeByID(ID: SmallInt): PNodeList;
    function GetChunkNodeID(Chunk: PChunk3DS): SmallInt;
    procedure InitDatabase;
    function IsNode(Tag: Word): Boolean;
    procedure KFAddParentName(Chunk: PChunk3DS; Name: string);
    procedure MakeNode(var Node: PNodeList);
    procedure ParseDatabase;
    procedure ReadChildren(Parent: PChunk3DS);
    procedure ReadXDataEntryChildren(Parent: PChunk3DS);
    procedure ReleaseDatabase;
    procedure ReleaseNodeList;
  public
    constructor Create; virtual;
    constructor CreateFromFile(const FileName: string); virtual;
    destructor Destroy; override;
    procedure ClearLists;

    // database methods
    procedure DumpDatabase(Strings: TStrings; DumpLevel: TDumpLevel);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(const AStream: TStream);

    // basic access methods
    function ReadByte: Byte;
    function ReadCardinal: Cardinal;
    procedure ReadChunkData(Chunk: PChunk3DS);
    procedure ReadData(Size: Integer; Data: Pointer);
    function ReadDouble: Double;
    function ReadFace: TFace3DS;
    procedure ReadHeader(var ChunkType: Word; var ChunkSize: Cardinal);
    function ReadInteger: Integer;
    function ReadKeyHeader: TKeyHeader3DS;
    function ReadPoint: TPoint3DS;
    function ReadShort: SmallInt;
    function ReadSingle: Single;
    function ReadString: string;
    function ReadTexVert: TTexVert3DS;
    function ReadTrackHeader: TTrackHeader3DS;
    function ReadWord: Word;

    procedure FinishHeader(StartPos, EndPos: Cardinal);
    function InitChunkData(Chunk: PChunk3DS): Pointer;
    procedure SeekChild(Chunk: PChunk3DS);
    procedure Skip(AValue: Integer);

    procedure WriteByte(AValue: Byte);
    procedure WriteCardinal(AValue: Cardinal);
    procedure WriteData(Size: Integer; Data: Pointer);
    procedure WriteDouble(AValue: Double);
    procedure WriteFace(F: TFace3DS);
    procedure WriteFixedString(const AValue: string; Len: Integer);
    procedure WriteHeader(ChunkType: Word; ChunkSize: Cardinal);
    procedure WriteInteger(AValue: Integer);
    procedure WriteKeyHeader(K: TKeyHeader3DS);
    procedure WritePoint(P: TPoint3DS);
    procedure WriteShort(AValue: SmallInt);
    procedure WriteSingle(AValue: Single);
    procedure WriteString(const AValue: string);
    procedure WriteTexVertex(T: TTexVert3DS);
    procedure WriteTrackHeader(T: TTrackHeader3DS);
    procedure WriteWord(AValue: Word);

    property Atmosphere: TAtmosphere3DS read GetAtmosphereData;
    property Background: TBackground3DS read GetBackgroundData;
    property DatabaseRelease: TReleaseLevel read GetDatabaseRelease;
    property DatabaseType: TDBType3DS read GetDatabaseType;
    property FileName: string read FFileName; // this is only valid if loaded from a file
    property KeyFramer: TKeyFramer read FKeyFramer;
    property Materials: TMaterialList read FMaterialList;
    property MeshRelease: TReleaseLevel read GetMeshRelease;
    property MeshSettings: TMeshSet3DS read GetMeshSettings;
    property Objects: TObjectList read FObjectList;
    property Viewport: TViewport3DS read GetViewportData;
  end;

//---------------------------------------------------------------------------------------------------------------------

implementation

uses
  Const3DS, Utils3DS, SysUtils;

//----------------- TMaterialList -------------------------------------------------------------------------------------

constructor TMaterialList.Create(AOwner: TFile3DS);

begin
  FOwner := AOwner;
  FLocalList := TList.Create;
end;

//---------------------------------------------------------------------------------------------------------------------

destructor TMaterialList.Destroy;

begin
  FLocalList.Free;
  inherited Destroy;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TMaterialList.ClearList;

var
  I: Integer;
  Mat: PMaterial3DS;

begin
  for I := 0 to FLocalList.Count - 1 do
    if FLocalList[I] <> nil then
    begin
      Mat := FLocalList[I];
      // free structure data
      ReleaseMaterial(Mat);
    end;
  FLocalList.Count := 0;
end;

//---------------------------------------------------------------------------------------------------------------------

function TMaterialList.GetCount: Integer;

begin
  if (FLocalList.Count = 0) and
    (FOwner.FDatabase.MatListDirty) then
    FLocalList.Count := GetMaterialCount(FOwner, FOwner.FDatabase);
  ;
  Result := FLocalList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TMaterialList.GetMaterial(Index: Integer): PMaterial3DS;

var
  NewEntry: PMaterial3DS;

begin
  Result := nil;
  if Count = 0 then
    Exit; // force reading the list if it was modified

  if FLocalList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetMaterialByIndex(FOwner, FOwner.FDatabase, Index);
    FLocalList[Index] := NewEntry;
  end;
  Result := FLocalList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

function TMaterialList.GetMaterialByName(const Name: string): PMaterial3DS;

var
  Entry: PMaterial3DS;
  Index: Integer;

begin
  Result := nil;
  for Index := 0 to Count - 1 do
  begin
    Entry := GetMaterial(Index);
    if Entry = nil then
      Continue;
    if CompareText(Entry.Name, Name) = 0 then
    begin
      Result := Entry;
      Break;
    end;
  end;
end;

//----------------- TObjectList ---------------------------------------------------------------------------------------

constructor TObjectList.Create(AOwner: TFile3DS);

begin
  FOwner := AOwner;
  FMeshList := TList.Create;
  FOmniList := TList.Create;
  FSpotList := TList.Create;
  FCameraList := TList.Create;
end;

//---------------------------------------------------------------------------------------------------------------------

destructor TObjectList.Destroy;

begin
  ClearLists;
  FMeshList.Free;
  FOmniList.Free;
  FSpotList.Free;
  FCameraList.Free;
  inherited Destroy;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TObjectList.ClearLists;

var
  I: Integer;

begin
  for I := 0 to FMeshList.Count - 1 do
    ReleaseMeshObj(FMeshList[I]);
  FMeshList.Clear;

  for I := 0 to FOmniList.Count - 1 do
    ReleaseLight(FOmniList[I]);
  FOmniList.Clear;

  for I := 0 to FSpotList.Count - 1 do
    ReleaseLight(FSpotList[I]);
  FSpotList.Clear;

  for I := 0 to FCameraList.Count - 1 do
    ReleaseCamera(FCameraList[I]);
  FCameraList.Clear;
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetCamera(Index: Integer): PCamera3DS;

var
  NewEntry: PCamera3DS;

begin
  Result := nil;
  if CameraCount = 0 then
    Exit; // force reading the list if it was modified

  if FCameraList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetCameraByIndex(FOwner, FOwner.FDatabase, Index);
    FCameraList[Index] := NewEntry;
  end;
  Result := FCameraList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetCamCount: Integer;

begin
  if FCameraList.Count = 0 then
    FCameraList.Count := GetCameraCount(FOwner, FOwner.FDatabase);
  Result := FCameraList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetMeshObjectCount: Integer;

begin
  if FMeshList.Count = 0 then
    FMeshList.Count := GetMeshCount(FOwner, FOwner.FDatabase);
  Result := FMeshList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetMesh(Index: Integer): PMesh3DS;

var
  NewEntry: PMesh3DS;

begin
  Result := nil;
  if MeshCount = 0 then
    Exit; // force reading the list if it was modified

  if FMeshList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetMeshByIndex(FOwner, FOwner.FDatabase, Index);
    FMeshList[Index] := NewEntry;
  end;
  Result := FMeshList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetOmniCount: Integer;

begin
  if FOmniList.Count = 0 then
    FOmniList.Count := GetOmniLightCount(FOwner, FOwner.FDatabase);
  Result := FOmniList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetOmniLight(Index: Integer): PLight3DS;

var
  NewEntry: PLight3DS;

begin
  Result := nil;
  if OmniLightCount = 0 then
    Exit; // force reading the list if it was modified

  if FOmniList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetOmniLightByIndex(FOwner, FOwner.FDatabase, Index);
    FOmniList[Index] := NewEntry;
  end;
  Result := FOmniList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetSpotCount: Integer;

begin
  if FSpotList.Count = 0 then
    FSpotList.Count := GetSpotLightCount(FOwner, FOwner.FDatabase);
  Result := FSpotList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TObjectList.GetSpotLight(Index: Integer): PLight3DS;

var
  NewEntry: PLight3DS;

begin
  Result := nil;
  if SpotLightCount = 0 then
    Exit; // force reading the list if it was modified

  if FSpotList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetSpotLightByIndex(FOwner, FOwner.FDatabase, Index);
    FSpotList[Index] := NewEntry;
  end;
  Result := FSpotList[Index];
end;

//----------------- TKeyFramer ----------------------------------------------------------------------------------------

constructor TKeyFramer.Create(AOwner: TFile3DS);

begin
  FOwner := AOwner;
  FMeshMotionList := TList.Create;
  FOmniMotionList := TList.Create;
  FSpotMotionList := TList.Create;
  FCameraMotionList := TList.Create;
end;

//---------------------------------------------------------------------------------------------------------------------

destructor TKeyFramer.Destroy;

begin
  ClearLists;
  FMeshMotionList.Free;
  FOmniMotionList.Free;
  FSpotMotionList.Free;
  FCameraMotionList.Free;
  inherited;
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetAmbientMotion: PKFAmbient3DS;

begin
  if FAmbientMotion = nil then
  begin
    New(FAmbientMotion);
    FillChar(FAmbientMotion^, SizeOf(FAmbientMotion^), 0);
    FAmbientMotion^ := GetAmbientLightMotion(FOwner, FOwner.FDatabase);
  end;
  Result := FAmbientMotion;
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetCameraMotion(Index: Integer): PKFCamera3DS;

var
  NewEntry: PKFCamera3DS;

begin
  Result := nil;
  if CameraMotionCount = 0 then
    Exit; // force reading the list if it was modified

  if FCameraMotionList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetCameraMotionByIndex(FOwner, FOwner.FDatabase, Index);
    FCameraMotionList[Index] := NewEntry;
  end;
  Result := FCameraMotionList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetCamMotionCount: Integer;

begin
  if FCameraMotionList.Count = 0 then
    FCameraMotionList.Count := GetCameraNodeCount(FOwner, FOwner.FDatabase);
  Result := FCameraMotionList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetKFSets: TKFSets3DS;

begin
  Result := GetKFSettings(FOwner, FOwner.FDatabase);
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetMeshMotionCount: Integer;

begin
  if FMeshMotionList.Count = 0 then
    FMeshMotionList.Count := GetObjectNodeCount(FOwner, FOwner.FDatabase);
  Result := FMeshMotionList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetMeshMotion(Index: Integer): PKFMesh3DS;

var
  NewEntry: PKFMesh3DS;

begin
  Result := nil;
  if MeshMotionCount = 0 then
    Exit; // force reading the list if it was modified

  if FMeshMotionList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetObjectMotionByIndex(FOwner, FOwner.FDatabase, Index);
    FMeshMotionList[Index] := NewEntry;
  end;
  Result := FMeshMotionList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetOmniMotionCount: Integer;

begin
  if FOmniMotionList.Count = 0 then
    FOmniMotionList.Count := GetOmniLightNodeCount(FOwner, FOwner.FDatabase);
  Result := FOmniMotionList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetOmniLightMotion(Index: Integer): PKFOmni3DS;

var
  NewEntry: PKFOmni3DS;

begin
  Result := nil;
  if OmniLightMotionCount = 0 then
    Exit; // force reading the list if it was modified

  if FOmniMotionList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetOmniLightMotionByIndex(FOwner, FOwner.FDatabase, Index);
    FOmniMotionList[Index] := NewEntry;
  end;
  Result := FOmniMotionList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetSpotMotionCount: Integer;

begin
  if FSpotMotionList.Count = 0 then
    FSpotMotionList.Count := GetSpotLightNodeCount(FOwner, FOwner.FDatabase);
  Result := FSpotMotionList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function TKeyFramer.GetSpotLightMotion(Index: Integer): PKFSpot3DS;

var
  NewEntry: PKFSpot3DS;

begin
  Result := nil;
  if SpotLightMotionCount = 0 then
    Exit; // force reading the list if it was modified

  if FSpotMotionList[Index] = nil then
  begin
    New(NewEntry);
    FillChar(NewEntry^, SizeOf(NewEntry^), 0);
    NewEntry^ := GetSpotLightMotionByIndex(FOwner, FOwner.FDatabase, Index);
    FSpotMotionList[Index] := NewEntry;
  end;
  Result := FSpotMotionList[Index];
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TKeyFramer.ClearLists;

var
  I: Integer;

begin
  for I := 0 to FMeshMotionList.Count - 1 do
    ReleaseObjectMotion(FMeshMotionList[I]);
  FMeshMotionList.Clear;

  for I := 0 to FOmniMotionList.Count - 1 do
    ReleaseOmnilightMotion(FOmniMotionList[I]);
  FOmniMotionList.Clear;

  for I := 0 to FSpotMotionList.Count - 1 do
    ReleaseSpotlightMotion(FSpotMotionList[I]);
  FSpotMotionList.Clear;

  for I := 0 to FCameraMotionList.Count - 1 do
    ReleaseCameraMotion(FCameraMotionList[I]);
  FCameraMotionList.Clear;

  if assigned(FAmbientMotion) then
    ReleaseAmbientLightMotion(FAmbientMotion);
  FAmbientMotion := nil;
end;

//----------------- TFile3DS ------------------------------------------------------------------------------------------

constructor TFile3DS.Create;

begin
  FMaterialList := TMaterialList.Create(Self);
  FObjectList := TObjectList.Create(Self);
  FKeyFramer := TKeyFramer.Create(Self);
end;

//---------------------------------------------------------------------------------------------------------------------

constructor TFile3DS.CreateFromFile(const FileName: string);

begin
  Create;
  FFileName := FileName;
  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  InitDatabase;
  CreateDatabase;
end;

//------------------------------------------------------------------------------

destructor TFile3DS.Destroy;

begin
  FKeyFramer.Free;
  FObjectList.Free;
  FMaterialList.Free;
  ReleaseDatabase;
  FStream.Free;
  inherited Destroy;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.AddToNodeList(Chunk: PChunk3DS);

// create a node, put node in list and fill-in structure

var
  NewNode: PNodeList;
  HdrChunk,
    InstChunk: PChunk3DS;

begin
  MakeNode(NewNode);
  if NewNode = nil then
    Exit;

  HdrChunk := FindChunk(Chunk, NODE_HDR);
  if HdrChunk = nil then
    Exit;

  ReadChunkData(HdrChunk);
  if HdrChunk = nil then
    Exit;

  // fill in node Data
  NewNode.Name := HdrChunk.Data.NodeHdr.ObjName;
  NewNode.ID := GetChunkNodeID(Chunk);
  NewNode.Tag := Chunk.Tag;
  NewNode.ParentID := HdrChunk.Data.NodeHdr.ParentIndex;
  NewNode.Next := nil;
  NewNode.Inst := '';

  // check for instance
  if Chunk.Tag = OBJECT_NODE_TAG then
  begin
    InstChunk := FindChunk(Chunk, INSTANCE_NAME);
    if assigned(InstChunk) then
    begin
      ReadChunkData(InstChunk);
      NewNode.Inst := InstChunk.Data.InstanceName^;
      Finalize(InstChunk.Data.Nodehdr^);
      FreeChunkData(InstChunk);
    end;
  end;
  FreeChunkData(HdrChunk);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.AssignParentNames;

// traverse keyframe data and assign parent names to its own chunk PARENT_NAME
// which is a child of NODE_HDR

var
  Chunk,
    KfDataChunk,
    HdrChunk,
    NameChunk,
    IdChunk: PChunk3DS;
  I: Integer;
  IDNode,
    IDParentNode: PNodeList;
  Name, Inst: string;

begin
  KfDataChunk := FindChunk(FDatabase.TopChunk, KFDATA);
  if KfDataChunk = nil then
    Exit;

  // Find chunks in KFRAMER
  for I := 1 to NodeTagCount do
  begin
    Chunk := FindChunk(KfDataChunk, NodeTags[I]);
    while assigned(Chunk) do
    begin
      HdrChunk := FindChunk(Chunk, NODE_HDR);
      if assigned(HdrChunk) then
      begin
        IDChunk := FindChunk(Chunk, NODE_ID);
        if assigned(IDChunk) then
        begin
          ReadChunkData(IDChunk);
          if assigned(IDChunk.Data.KFID) then
          begin
            // Find table entry for node of interest
            IDNode := FindNodeByID(IDChunk.Data.KfID^);
            // no ID (bad) or no parent (ok)
            if assigned(IDNode) and (IDNode.ParentID <> -1) then
            begin
              // find table entry for parent
              IDParentNode := FindNodeByID(IDNode.ParentID);
              if assigned(IDParentNode) then
              begin
                Name := IDParentNode.Name;
                Inst := IDParentNode.Inst;
              end;

              if Length(Name) > 0 then
              begin
                // concatenate names if there is an inst name
                if Length(Inst) > 0 then
                  Name := Name + '.' + Inst;

                // if PARENT chunk exists, copy into it
                NameChunk := FindChunk(HdrChunk, PARENT_NAME);
                if assigned(NameChunk) then
                begin
                  ReadChunkData(NameChunk);
                  if assigned(NameChunk.Data.InstanceName) then
                    NameChunk.Data.InstanceName^ := Name;
                end
                else
                  KFAddParentName(HdrChunk, Name); // create PARENT_NAME chunk
              end;
            end;
          end;
        end;
      end;
      Chunk := FindNextChunk(Chunk.Sibling, NodeTags[I]);
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.CheckListNodeIDs;

// Earlier versions (pre 3) of 3dStudio had no node ids, they simply used the order
// in which they came along, if so put in NODE IDs. Assuming that if one node
// has no ID the whole list get renumbered.

var
  ID: PNodeList;
  Index: SmallInt;

begin
  ID := FNodeList;

  while assigned(ID) do
  begin
    if (ID.ID = KNoID) then // if somebody has no ID renumber list
    begin
      Index := 0;
      ID := FNodeList;
      while assigned(ID) do
      begin
        ID.ID := Index;
        Inc(Index);
        ID := ID.Next;
      end;
      break;
    end;
    ID := ID.Next;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.FindNodeByID(ID: SmallInt): PNodeList;

begin
  Result := FNodeList;
  while assigned(Result) do
  begin
    if Result.ID = ID then
      Break;
    Result := Result.Next;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.DumpDatabase(Strings: TStrings; DumpLevel: TDumpLevel);

// dumps entire database into the given string class

var
  OldSeparator: Char;

begin
  OldSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    if assigned(FDatabase.TopChunk) then
      DumpChunk(Self, Strings, FDatabase.TopChunk, 0, DumpLevel);
  finally
    DecimalSeparator := OldSeparator;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetChunkNodeID(Chunk: PChunk3DS): SmallInt;

var
  IDChunk: PChunk3DS;

begin
  Result := KNoID;
  IDChunk := FindChunk(Chunk, NODE_ID);
  if assigned(IDChunk) then
  begin
    ReadChunkData(IDChunk);
    if assigned(IDChunk.Data.KFID) then
      Result := IDChunk.Data.KFID^;
    FreeChunkData(IDChunk);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.IsNode(Tag: Word): Boolean;

var
  I: Integer;

begin
  Result := False;
  for I := 1 to NodeTagCount do
    if Tag = NodeTags[I] then
    begin
      Result := True;
      Break;
    end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.KFAddParentName(Chunk: PChunk3DS; Name: string);

var
  Temp: PChunk3DS;
  Data: PInstanceName;

begin
  InitChunk(Temp);
  Temp.Tag := PARENT_NAME;
  Data := InitChunkData(Temp);

  Data^ := Name;
  AddChildOrdered(Chunk, Temp);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.MakeNode(var Node: PNodeList);

// add node to linked list (uninitialized)

var
  ID: PNodeList;

begin
  ID := FNodeList;
  Node := AllocMem(SizeOf(TNodeList));
  if assigned(Node) then
  begin
    // first node ?
    if ID = nil then
      FNodeList := Node
    else // add to list
    begin
      while assigned(ID.Next) do
        ID := ID.Next;
      ID.Next := Node;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ParseDatabase;

var
  Chunk, KfDataChunk: PChunk3DS;

begin
  KfDataChunk := FindChunk(FDatabase.TopChunk, KFDATA);
  if assigned(KfDataChunk) then
  begin
    Chunk := KfDataChunk.Children;
    while assigned(Chunk) do
    begin
      if IsNode(Chunk.Tag) then
        AddToNodeList(Chunk);
      Chunk := Chunk.Sibling;
    end;
    CheckListNodeIDs;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ReadXDataEntryChildren(Parent: PChunk3DS);

var
  ParentBody: Cardinal;
  Child: PChunk3DS;

begin
  SeekChild(Parent);
  ParentBody := Parent.Position + Parent.Size;

  // satisfy the D4 compiler by castíng the (longint) position to a cardinal
  while Cardinal(FStream.Position) < ParentBody do
  begin
    Child := nil;
    InitChunk(Child);
    Child.Position := FStream.Position;
    ReadHeader(Child.Tag, Child.Size);
    // Validate the child chunk...
    // First, is it a valid header?
    case Child.Tag of
      XDATA_APPNAME,
        XDATA_STRING,
        XDATA_FLOAT,
        XDATA_DOUBLE,
        XDATA_SHORT,
        XDATA_LONG,
        XDATA_VOID,
        XDATA_GROUP,
        XDATA_RFU6,
        XDATA_RFU5,
        XDATA_RFU4,
        XDATA_RFU3,
        XDATA_RFU2,
        XDATA_RFU1:
        begin
          // second, does the size fit inside the XDATA_ENTRY chunk?
          if (Child.Position + Child.Size) <= ParentBody then
          begin
             // chances are its a good subchunk, so add it in
            AddChild(Parent, Child);
            ReadXDataEntryChildren(Child);
          end
          else
            ReleaseChunk(Child);
        end
    else // must not be a valid chunk, seek to the end of the parent then
      begin
        ReleaseChunk(Child);
        FStream.Position := ParentBody;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ReleaseNodeList;

var
  Next: PNodeList;

begin
  while assigned(FNodeList) do
  begin
    Next := FNodeList.Next;
    Dispose(FNodeList);
    FNodeList := Next;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.CreateDatabase;

begin
  with FDatabase do
  begin
    InitChunk(TopChunk);
    FStream.Position := 0;
    ReadHeader(TopChunk.Tag, TopChunk.Size);

    // test header to determine whether it is a top level chunk type
    if (TopChunk.Tag = M3DMAGIC) or
      (TopChunk.Tag = CMAGIC) or
      (TopChunk.Tag = MLIBMAGIC) then
    begin
      // read database structure
      ReadChildren(TopChunk);
      ParseDatabase;
      AssignParentNames;
      ReleaseNodeList;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.InitDatabase;

begin
  with FDatabase do
  begin
    TopChunk := nil;
    ObjListDirty := True;
    MatListDirty := True;
    NodeListDirty := True;
    ObjList := nil;
    MatList := nil;
    NodeList := nil;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ClearLists;

begin
  FMaterialList.ClearList;
  FObjectList.ClearLists;
  FKeyframer.ClearLists;
  ReleaseDatabase;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.LoadFromFile(const FileName: string);

begin
  ClearLists;
  FStream.Free;
  FFileName := FileName;
  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  InitDatabase;
  CreateDatabase;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.LoadFromStream(const AStream: TStream);

begin
  FStream.Free;
  ClearLists;
  FFileName := '';
  FStream := AStream;
  InitDatabase;
  CreateDatabase;
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetAtmosphereData: TAtmosphere3DS;

begin
  Result := GetAtmosphere(Self, FDatabase);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetBackgroundData: TBackground3DS;

begin
  Result := GetBackground(Self, FDatabase);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetDatabaseType: TDBType3DS;

begin
  case FDatabase.TopChunk.Tag of
    M3DMAGIC:
      Result := dbMeshFile;
    CMAGIC:
      Result := dbProjectFile;
    MLIBMAGIC:
      Result := dbMaterialFile;
  else
    Result := dbUnknown;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetMeshSettings: TMeshSet3DS;

begin
  Result := GetMeshSet(Self, FDatabase);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetViewportData: TViewport3DS;

begin
  Result := GetViewport(Self, FDatabase);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ReadChildren(Parent: PChunk3DS);

var
  ParentBody: Integer;
  Child: PChunk3DS;

begin
  SeekChild(Parent);
  ParentBody := Parent.Position + Parent.Size;

  while FStream.Position < ParentBody do
  begin
    Child := nil;
    InitChunk(Child);
    Child.Position := FStream.Position;
    ReadHeader(Child.Tag, Child.Size);
    AddChild(Parent, Child);
    if Child.Tag = XDATA_ENTRY then
      ReadXDataEntryChildren(Child)
    else
      ReadChildren(Child);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ReleaseDatabase;

begin
  with FDatabase do
  begin
    if assigned(TopChunk) then
      ReleaseChunk(TopChunk);
    if assigned(ObjList) then
      ReleaseChunkList(ObjList);
    if assigned(MatList) then
      ReleaseChunkList(MatList);
    if assigned(NodeList) then
      ReleaseChunkList(NodeList);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.InitChunkData(Chunk: PChunk3DS): Pointer;

begin
  case Chunk.Tag of
    COLOR_F:
      Chunk.Data.ColorF := AllocMem(SizeOf(TColorF));
    LIN_COLOR_F:
      Chunk.Data.LinColorF := AllocMem(SizeOf(TLinColorF));
    COLOR_24:
      Chunk.Data.Color24 := AllocMem(SizeOf(TColor24));
    LIN_COLOR_24:
      Chunk.Data.LinColor24 := AllocMem(SizeOf(TLinColor24));
    INT_PERCENTAGE:
      Chunk.Data.IntPercentage := AllocMem(SizeOf(TIntPercentage));
    FLOAT_PERCENTAGE:
      Chunk.Data.FloatPercentage := AllocMem(SizeOf(TFloatPercentage));
    MAT_MAPNAME:
      Chunk.Data.MatMapname := AllocMem(SizeOf(TMatMapname));
    M3D_VERSION:
      Chunk.Data.M3dVersion := AllocMem(SizeOf(TM3dVersion));
    MESH_VERSION:
      Chunk.Data.MeshVersion := AllocMem(SizeOf(TMeshVersion));
    MASTER_SCALE:
      Chunk.Data.MasterScale := AllocMem(SizeOf(TMasterScale));
    LO_SHADOW_BIAS:
      Chunk.Data.LoShadowBias := AllocMem(SizeOf(TLoShadowBias));
    SHADOW_FILTER:
      Chunk.Data.ShadowFilter := AllocMem(SizeOf(TShadowFilter));
    SHADOW_RANGE:
      Chunk.Data.ShadowRange := AllocMem(SizeOf(TShadowRange));
    HI_SHADOW_BIAS:
      Chunk.Data.HiShadowBias := AllocMem(SizeOf(THiShadowBias));
    RAY_BIAS:
      Chunk.Data.RayBias := AllocMem(SizeOf(TRayBias));
    SHADOW_MAP_SIZE:
      Chunk.Data.ShadowMapSize := AllocMem(SizeOf(TShadowMapSize));
    SHADOW_SAMPLES:
      Chunk.Data.ShadowSamples := AllocMem(SizeOf(TShadowSamples));
    O_CONSTS:
      Chunk.Data.OConsts := AllocMem(SizeOf(TOConsts));
    BIT_MAP:
      Chunk.Data.BitMapName := AllocMem(SizeOf(TBitMapName));
    V_GRADIENT:
      Chunk.Data.VGradient := AllocMem(SizeOf(TVGradient));
    FOG:
      Chunk.Data.Fog := AllocMem(SizeOf(TFog));
    LAYER_FOG:
      Chunk.Data.LayerFog := AllocMem(SizeOf(TLayerFog));
    DISTANCE_CUE:
      Chunk.Data.DistanceCue := AllocMem(SizeOf(TDistanceCue));
    VIEW_TOP,
      VIEW_BOTTOM,
      VIEW_LEFT,
      VIEW_RIGHT,
      VIEW_FRONT,
      VIEW_BACK:
      Chunk.Data.ViewStandard := AllocMem(SizeOf(TViewStandard));
    VIEW_USER:
      Chunk.Data.ViewUser := AllocMem(SizeOf(TViewUser));
    VIEW_CAMERA:
      Chunk.Data.ViewCamera := AllocMem(SizeOf(TViewCamera));
    MAT_NAME:
      Chunk.Data.MatName := AllocMem(SizeOf(TMatName));
    MAT_SHADING:
      Chunk.Data.MatShading := AllocMem(SizeOf(TMatShading));
    MAT_ACUBIC:
      Chunk.Data.MatAcubic := AllocMem(SizeOf(TMatAcubic));
    MAT_SXP_TEXT_DATA,
      MAT_SXP_TEXT2_DATA,
      MAT_SXP_OPAC_DATA,
      MAT_SXP_BUMP_DATA,
      MAT_SXP_SPEC_DATA,
      MAT_SXP_SHIN_DATA,
      MAT_SXP_SELFI_DATA,
      MAT_SXP_TEXT_MASKDATA,
      MAT_SXP_TEXT2_MASKDATA,
      MAT_SXP_OPAC_MASKDATA,
      MAT_SXP_BUMP_MASKDATA,
      MAT_SXP_SPEC_MASKDATA,
      MAT_SXP_SHIN_MASKDATA,
      MAT_SXP_SELFI_MASKDATA,
      MAT_SXP_REFL_MASKDATA,
      PROC_DATA:
      Chunk.Data.IpasData := AllocMem(SizeOf(TIpasData));
    MAT_WIRESIZE:
      Chunk.Data.MatWireSize := AllocMem(SizeOf(TMatWireSize));
    MAT_MAP_TILING:
      Chunk.Data.MatMapTiling := AllocMem(SizeOf(TMatMapTiling));
    MAT_MAP_TEXBLUR:
      Chunk.Data.MatMapTexblur := AllocMem(SizeOf(TMatMapTexblur));
    MAT_MAP_USCALE:
      Chunk.Data.MatMapUScale := AllocMem(SizeOf(TMatMapUScale));
    MAT_MAP_VSCALE:
      Chunk.Data.MatMapVScale := AllocMem(SizeOf(TMatMapVScale));
    MAT_MAP_UOFFSET:
      Chunk.Data.MatMapUOffset := AllocMem(SizeOf(TMatMapUOffset));
    MAT_MAP_VOFFSET:
      Chunk.Data.MatMapVOffset := AllocMem(SizeOf(TMatMapVOffset));
    MAT_MAP_ANG:
      Chunk.Data.MatMapAng := AllocMem(SizeOf(TMatMapAng));
    MAT_MAP_COL1:
      Chunk.Data.MatMapCol1 := AllocMem(SizeOf(TMatMapCol1));
    MAT_MAP_COL2:
      Chunk.Data.MatMapCol2 := AllocMem(SizeOf(TMatMapCol2));
    MAT_MAP_RCOL:
      Chunk.Data.MatMapRCol := AllocMem(SizeOf(TMatMapRCol));
    MAT_MAP_GCOL:
      Chunk.Data.MatMapGCol := AllocMem(SizeOf(TMatMapGCol));
    MAT_MAP_BCOL:
      Chunk.Data.MatMapBCol := AllocMem(SizeOf(TMatMapBCol));
    MAT_BUMP_PERCENT:
      Chunk.Data.MatBumpPercent := AllocMem(SizeOf(TMatBumpPercent));
    NAMED_OBJECT:
      Chunk.Data.NamedObject := AllocMem(SizeOf(TNamedObject));
    POINT_ARRAY:
      Chunk.Data.PointArray := AllocMem(SizeOf(TPointArray));
    POINT_FLAG_ARRAY:
      Chunk.Data.PointFlagArray := AllocMem(SizeOf(TPointFlagArray));
    FACE_ARRAY:
      Chunk.Data.FaceArray := AllocMem(SizeOf(TFaceArray));
    MSH_MAT_GROUP:
      Chunk.Data.MshMatGroup := AllocMem(SizeOf(TMshMatGroup));
    MSH_BOXMAP:
      Chunk.Data.MshBoxmap := AllocMem(SizeOf(TMshBoxmap));
    SMOOTH_GROUP:
      Chunk.Data.SmoothGroup := AllocMem(SizeOf(TSmoothGroup));
    TEX_VERTS:
      Chunk.Data.TexVerts := AllocMem(SizeOf(TTexVerts));
    MESH_MATRIX:
      Chunk.Data.MeshMatrix := AllocMem(SizeOf(TMeshMatrix));
    MESH_COLOR:
      Chunk.Data.MeshColor := AllocMem(SizeOf(TMeshColor));
    MESH_TEXTURE_INFO:
      Chunk.Data.MeshTextureInfo := AllocMem(SizeOf(TMeshTextureInfo));
    PROC_NAME:
      Chunk.Data.ProcName := AllocMem(SizeOf(TProcName));
    N_DIRECT_LIGHT:
      Chunk.Data.NDirectLight := AllocMem(SizeOf(TNDirectLight));
    DL_EXCLUDE:
      Chunk.Data.DlExclude := AllocMem(SizeOf(TDlExclude));
    DL_INNER_RANGE:
      Chunk.Data.DlInnerRange := AllocMem(SizeOf(TDlInnerRange));
    DL_OUTER_RANGE:
      Chunk.Data.DlOuterRange := AllocMem(SizeOf(TDlOuterRange));
    DL_MULTIPLIER:
      Chunk.Data.DlMultiplier := AllocMem(SizeOf(TDlMultiplier));
    DL_SPOTLIGHT:
      Chunk.Data.DlSpotlight := AllocMem(SizeOf(TDlSpotlight));
    DL_LOCAL_SHADOW2:
      Chunk.Data.DlLocalShadow2 := AllocMem(SizeOf(TDlLocalShadow2));
    DL_SPOT_ROLL:
      Chunk.Data.DlSpotRoll := AllocMem(SizeOf(TDlSpotRoll));
    DL_SPOT_ASPECT:
      Chunk.Data.DlSpotAspect := AllocMem(SizeOf(TDlSpotAspect));
    DL_SPOT_PROJECTOR:
      Chunk.Data.DlSpotProjector := AllocMem(SizeOf(TDlSpotProjector));
    DL_RAY_BIAS:
      Chunk.Data.DlRayBias := AllocMem(SizeOf(TDlRayBias));
    N_CAMERA:
      Chunk.Data.NCamera := AllocMem(SizeOf(TNCamera));
    CAM_RANGES:
      Chunk.Data.CamRanges := AllocMem(SizeOf(TCamRanges));
    VIEWPORT_LAYOUT:
      Chunk.Data.ViewportLayout := AllocMem(SizeOf(TViewportLayout));
    VIEWPORT_SIZE:
      Chunk.Data.ViewportSize := AllocMem(SizeOf(TViewportSize));
    VIEWPORT_DATA_3,
      VIEWPORT_DATA:
      Chunk.Data.ViewportData := AllocMem(SizeOf(TViewportData));
    XDATA_ENTRY:
      Chunk.Data.XDataEntry := AllocMem(SizeOf(TXDataEntry));
    XDATA_APPNAME:
      Chunk.Data.XDataAppName := AllocMem(SizeOf(TXDataAppName));
    XDATA_STRING:
      Chunk.Data.XDataString := AllocMem(SizeOf(TXDataString));
    KFHDR:
      Chunk.Data.KFHdr := AllocMem(SizeOf(TKFHdr));
    KFSEG:
      Chunk.Data.KFSeg := AllocMem(SizeOf(TKFSeg));
    KFCURTIME:
      Chunk.Data.KFCurtime := AllocMem(SizeOf(TKFCurtime));
    NODE_ID:
      Chunk.Data.KFId := AllocMem(SizeOf(TKFId));
    NODE_HDR:
      Chunk.Data.NodeHdr := AllocMem(SizeOf(TNodeHdr));
    PIVOT:
      Chunk.Data.Pivot := AllocMem(SizeOf(TPivot));
    INSTANCE_NAME,
      PARENT_NAME:
      Chunk.Data.InstanceName := AllocMem(SizeOf(TInstanceName));
    MORPH_SMOOTH:
      Chunk.Data.MorphSmooth := AllocMem(SizeOf(TMorphSmooth));
    BOUNDBOX:
      Chunk.Data.BoundBox := AllocMem(SizeOf(TBoundBox));
    POS_TRACK_TAG:
      Chunk.Data.PosTrackTag := AllocMem(SizeOf(TPosTrackTag));
    COL_TRACK_TAG:
      Chunk.Data.ColTrackTag := AllocMem(SizeOf(TColTrackTag));
    ROT_TRACK_TAG:
      Chunk.Data.RotTrackTag := AllocMem(SizeOf(TRotTrackTag));
    SCL_TRACK_TAG:
      Chunk.Data.ScaleTrackTag := AllocMem(SizeOf(TScaleTrackTag));
    MORPH_TRACK_TAG:
      Chunk.Data.MorphTrackTag := AllocMem(SizeOf(TMorphTrackTag));
    FOV_TRACK_TAG:
      Chunk.Data.FovTrackTag := AllocMem(SizeOf(TFovTrackTag));
    ROLL_TRACK_TAG:
      Chunk.Data.RollTrackTag := AllocMem(SizeOf(TRollTrackTag));
    HOT_TRACK_TAG:
      Chunk.Data.HotTrackTag := AllocMem(SizeOf(THotTrackTag));
    FALL_TRACK_TAG:
      Chunk.Data.FallTrackTag := AllocMem(SizeOf(TFallTrackTag));
    HIDE_TRACK_TAG:
      Chunk.Data.HideTrackTag := AllocMem(SizeOf(THideTrackTag));
    M3DMAGIC, // Chunks who consist entirely of children
      MLIBMAGIC,
      MDATA,
      AMBIENT_LIGHT,
      SOLID_BGND,
      DEFAULT_VIEW,
      MAT_ENTRY,
      MAT_AMBIENT,
      MAT_DIFFUSE,
      MAT_SPECULAR,
      MAT_SHININESS,
      MAT_SHIN2PCT,
      MAT_SHIN3PCT,
      MAT_TRANSPARENCY,
      MAT_XPFALL,
      MAT_REFBLUR,
      MAT_SELF_ILPCT,
      MAT_TEXMAP,
      MAT_TEXMASK,
      MAT_TEX2MAP,
      MAT_TEX2MASK,
      MAT_OPACMAP,
      MAT_OPACMASK,
      MAT_REFLMAP,
      MAT_REFLMASK,
      MAT_BUMPMAP,
      MAT_BUMPMASK,
      MAT_SPECMAP,
      MAT_SPECMASK,
      MAT_SHINMAP,
      MAT_SHINMASK,
      MAT_SELFIMAP,
      MAT_SELFIMASK,
      N_TRI_OBJECT,
      KFDATA,
      AMBIENT_NODE_TAG,
      OBJECT_NODE_TAG,
      CAMERA_NODE_TAG,
      TARGET_NODE_TAG,
      LIGHT_NODE_TAG,
      SPOTLIGHT_NODE_TAG,
      L_TARGET_NODE_TAG,
      CMAGIC,
      XDATA_SECTION,
      XDATA_GROUP:
      Chunk.Data.Dummy := nil;
  else // A truely hideous thing to do but it helps with unknown chunks
      // Don't mess with dataless chunks
    if Chunk.Size > 6 then
      Chunk.Data.Dummy := AllocMem(Chunk.Size - 6)
    else
      Chunk.Data.Dummy := nil;
  end; // end of case
  Result := Chunk.Data.Dummy; // returns the pointer should someone want it
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteByte(AValue: Byte);

begin
  FStream.WriteBuffer(AValue, 1);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadByte: Byte;

begin
  FStream.ReadBuffer(Result, 1);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteShort(AValue: SmallInt);

begin
  FStream.WriteBuffer(AValue, SizeOf(AValue));
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadShort: SmallInt;

begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadCardinal: Cardinal;

begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadDouble: Double;

begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadInteger: Integer;

begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadSingle: Single;

begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadWord: Word;

begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteCardinal(AValue: Cardinal);

begin
  FStream.WriteBuffer(AValue, SizeOf(AValue));
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteDouble(AValue: Double);

begin
  FStream.WriteBuffer(AValue, SizeOf(AValue));
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteInteger(AValue: Integer);

begin
  FStream.WriteBuffer(AValue, SizeOf(AValue));
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteSingle(AValue: Single);

begin
  FStream.WriteBuffer(AValue, SizeOf(AValue));
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteWord(AValue: Word);

begin
  FStream.WriteBuffer(AValue, SizeOf(AValue));
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteData(Size: Integer; Data: Pointer);

begin
  if assigned(Data) then
    FStream.WriteBuffer(Data^, Size);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ReadData(Size: Integer; Data: Pointer);

begin
  FStream.ReadBuffer(Data^, Size);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.Skip(AValue: Integer);

begin
  FStream.Seek(soFromCurrent, AValue);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteString(const AValue: string);

begin
  WriteData(Length(AValue), @AValue[1]);
  WriteByte(0); // Write a null on the end of the string
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteFixedString(const AValue: string; Len: Integer);

var
  I: Integer;

begin
  // len is the length of the target string space including null
  WriteString(AValue); // 1 null byte will also be written
  for I := 1 to Len - Length(AValue) - 1 do
    WriteByte(0); // fill the remaining space with nulls
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadString: string;

var
  Len: Integer;
  Buffer: array[Byte] of Char;

begin
  Len := 0;
  repeat
    FStream.Read(Buffer[Len], 1);
    Inc(Len);
  until Buffer[Len - 1] = #0;
  SetString(Result, Buffer, Len - 1); // not the null byte
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteHeader(ChunkType: Word; ChunkSize: Cardinal);

begin
  WriteWord(ChunkType);
  WriteCardinal(ChunkSize);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ReadHeader(var ChunkType: Word; var ChunkSize: Cardinal);

begin
  ChunkType := ReadWord;
  ChunkSize := ReadCardinal;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.FinishHeader(StartPos, EndPos: Cardinal);

begin
  FStream.Position := StartPos + 2;
  WriteCardinal(EndPos - StartPos);
  FStream.Position := EndPos;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WritePoint(P: TPoint3DS);

begin
  WriteSingle(P.X);
  WriteSingle(P.Y);
  WriteSingle(P.Z);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadPoint: TPoint3DS;

begin
  Result := DefPoint3DS;
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteTexVertex(T: TTexVert3DS);

begin
  WriteSingle(T.U);
  WriteSingle(T.V);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadTexVert: TTexVert3DS;

begin
  Result := DefTextVert3DS;
  Result.U := ReadSingle;
  Result.V := ReadSingle;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteFace(F: TFace3DS);

begin
  WriteWord(F.v1);
  WriteWord(F.v2);
  WriteWord(F.v3);
  WriteWord(F.flag);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadFace: TFace3DS;

begin
  Result := DefFace3DS;
  Result.v1 := ReadWord;
  Result.v2 := ReadWord;
  Result.v3 := ReadWord;
  Result.flag := ReadWord;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteTrackHeader(T: TTrackHeader3DS);

begin
  WriteWord(T.Flags);
  WriteCardinal(T.nu1);
  WriteCardinal(T.nu2);
  WriteCardinal(T.KeyCount);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadTrackHeader: TTrackHeader3DS;

begin
  Result := DefTrackHeader3DS;
  Result.Flags := ReadWord;
  Result.nu1 := ReadCardinal;
  Result.nu2 := ReadCardinal;
  Result.KeyCount := ReadCardinal;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.WriteKeyHeader(K: TKeyHeader3DS);

begin
  WriteCardinal(K.time);
  WriteWord(K.rflags);
  if (K.rflags and KeyUsesTension3DS) > 0 then
    WriteSingle(K.tension);
  if (K.rflags and KeyUsesCont3DS) > 0 then
    WriteSingle(K.continuity);
  if (K.rflags and KeyUsesBias3DS) > 0 then
    WriteSingle(K.bias);
  if (K.rflags and KeyUsesEaseTo3DS) > 0 then
    WriteSingle(K.easeto);
  if (K.rflags and KeyUsesEaseFrom3DS) > 0 then
    WriteSingle(K.easefrom);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.ReadKeyHeader: TKeyHeader3DS;

begin
  Result := DefKeyHeader3DS;
  Result.time := ReadCardinal;
  Result.rflags := ReadWord;
  if (Result.rflags and KeyUsesTension3DS) > 0 then
    Result.tension := ReadSingle;
  if (Result.rflags and KeyUsesCont3DS) > 0 then
    Result.continuity := ReadSingle;
  if (Result.rflags and KeyUsesBias3DS) > 0 then
    Result.bias := ReadSingle;
  if (Result.rflags and KeyUsesEaseTo3DS) > 0 then
    Result.easeto := ReadSingle;
  if (Result.rflags and KeyUsesEaseFrom3DS) > 0 then
    Result.easefrom := ReadSingle;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.ReadChunkData(Chunk: PChunk3DS);

// Reads the data out of the chunk detailed in Chunk and places a pointer to
// the data into the PChunk3DS structure, it will also return that pointer.

var
  I: Integer;

begin
  if Chunk.Data.Dummy = nil then // don't try to read the data if its already been read
  begin
    // seek to the beginning of the Chunk's data (harmless if the Chunk has no data)
    FStream.Position := Chunk.Position + 6;
    case Chunk.Tag of
      COLOR_F:
        begin
          Chunk.Data.ColorF := AllocMem(SizeOf(TColorF)); // allocate the memory to hold the data
          with Chunk.Data.ColorF^ do
          begin
            Red := ReadSingle; // Read the data out of the file
            Green := ReadSingle;
            Blue := ReadSingle;
          end;
        end;
      LIN_COLOR_F:
        begin
          Chunk.Data.LinColorF := AllocMem(SizeOf(TLinColorF));
          with Chunk.Data.LinColorF^ do
          begin
            Red := ReadSingle;
            Green := ReadSingle;
            Blue := ReadSingle;
          end;
        end;
      COLOR_24:
        begin
          Chunk.Data.Color24 := AllocMem(SizeOf(TColor24));
          with Chunk.Data.Color24^ do
          begin
            Red := ReadByte;
            Green := ReadByte;
            Blue := ReadByte;
          end;
        end;
      LIN_COLOR_24:
        begin
          Chunk.Data.LinColor24 := AllocMem(SizeOf(TLinColor24));
          with Chunk.Data.LinColor24^ do
          begin
            Red := ReadByte;
            Green := ReadByte;
            Blue := ReadByte;
          end;
        end;
      INT_PERCENTAGE:
        begin
          Chunk.Data.IntPercentage := AllocMem(SizeOf(TIntPercentage));
          Chunk.Data.IntPercentage^ := ReadShort;
        end;
      FLOAT_PERCENTAGE:
        begin
          Chunk.Data.FloatPercentage := AllocMem(SizeOf(TFloatPercentage));
          Chunk.Data.FloatPercentage^ := ReadSingle;
        end;
      MAT_MAPNAME:
        begin
          Chunk.Data.MatMapname := AllocMem(SizeOf(TMatMapname));
          Chunk.Data.MatMapname^ := ReadString;
        end;
      M3D_VERSION:
        begin
          Chunk.Data.M3dVersion := AllocMem(SizeOf(TM3dVersion));
          Chunk.Data.M3dVersion^ := ReadInteger;
        end;
      MESH_VERSION:
        begin
          Chunk.Data.MeshVersion := AllocMem(SizeOf(TMeshVersion));
          Chunk.Data.MeshVersion^ := ReadInteger;
        end;
      MASTER_SCALE:
        begin
          Chunk.Data.MasterScale := AllocMem(SizeOf(TMasterScale));
          Chunk.Data.MasterScale^ := ReadSingle;
        end;
      LO_SHADOW_BIAS:
        begin
          Chunk.Data.LoShadowBias := AllocMem(SizeOf(TLoShadowBias));
          Chunk.Data.LoShadowBias^ := ReadSingle;
        end;
      SHADOW_FILTER:
        begin
          Chunk.Data.ShadowFilter := AllocMem(SizeOf(TShadowFilter));
          Chunk.Data.ShadowFilter^ := ReadSingle;
        end;
      SHADOW_RANGE:
        begin
          Chunk.Data.ShadowRange := AllocMem(SizeOf(TShadowRange));
          Chunk.Data.ShadowRange^ := ReadInteger;
        end;
      HI_SHADOW_BIAS:
        begin
          Chunk.Data.HiShadowBias := AllocMem(SizeOf(THiShadowBias));
          Chunk.Data.HiShadowBias^ := ReadSingle;
        end;
      RAY_BIAS:
        begin
          Chunk.Data.RayBias := AllocMem(SizeOf(TRayBias));
          Chunk.Data.RayBias^ := ReadSingle;
        end;
      SHADOW_MAP_SIZE:
        begin
          Chunk.Data.ShadowMapSize := AllocMem(SizeOf(TShadowMapSize));
          Chunk.Data.ShadowMapSize^ := ReadShort;
        end;
      SHADOW_SAMPLES:
        begin
          Chunk.Data.ShadowSamples := AllocMem(SizeOf(TShadowSamples));
          Chunk.Data.ShadowSamples^ := ReadShort;
        end;
      O_CONSTS:
        begin
          Chunk.Data.OConsts := AllocMem(SizeOf(TOConsts));
          Chunk.Data.OConsts^ := ReadPoint;
        end;
      BIT_MAP:
        begin
          Chunk.Data.BitMapName := AllocMem(SizeOf(TBitMapName));
          Chunk.Data.BitMapName^ := ReadString;
        end;
      V_GRADIENT:
        begin
          Chunk.Data.VGradient := AllocMem(SizeOf(TVGradient));
          Chunk.Data.VGradient^ := ReadSingle;
        end;
      FOG:
        begin
          Chunk.Data.Fog := AllocMem(SizeOf(TFog));
          with Chunk.Data.Fog^ do
          begin
            NearPlaneDist := ReadSingle;
            NearPlaneDensity := ReadSingle;
            FarPlaneDist := ReadSingle;
            FarPlaneDensity := ReadSingle;
          end;
        end;
      LAYER_FOG:
        begin
          Chunk.Data.LayerFog := AllocMem(SizeOf(TLayerFog));
          with Chunk.Data.LayerFog^ do
          begin
            ZMin := ReadSingle;
            ZMax := ReadSingle;
            Density := ReadSingle;
            AType := ReadCardinal;
          end;
        end;
      DISTANCE_CUE:
        begin
          Chunk.Data.DistanceCue := AllocMem(SizeOf(TDistanceCue));
          with Chunk.Data.DistanceCue^ do
          begin
            NearPlaneDist := ReadSingle;
            NearPlaneDimming := ReadSingle;
            FarPlaneDist := ReadSingle;
            FarPlaneDimming := ReadSingle;
          end;
        end;
      VIEW_TOP,
        VIEW_BOTTOM,
        VIEW_LEFT,
        VIEW_RIGHT,
        VIEW_FRONT,
        VIEW_BACK:
        begin
          Chunk.Data.ViewStandard := AllocMem(SizeOf(TViewStandard));
          with Chunk.Data.ViewStandard^ do
          begin
            ViewWidth := ReadSingle;
            ViewTargetCoord := ReadPoint;
          end;
        end;
      VIEW_USER:
        begin
          Chunk.Data.ViewUser := AllocMem(SizeOf(TViewUser));
          with Chunk.Data.ViewUser^ do
          begin
            ViewWidth := ReadSingle;
            XYViewAngle := ReadSingle;
            YZViewAngle := ReadSingle;
            BankAngle := ReadSingle;
            ViewTargetCoord := ReadPoint;
          end;
        end;
      VIEW_CAMERA:
        begin
          Chunk.Data.ViewCamera := AllocMem(SizeOf(TViewCamera));
          Chunk.Data.ViewCamera^ := ReadString;
        end;
      MAT_NAME:
        begin
          Chunk.Data.MatName := AllocMem(SizeOf(TMatName));
          Chunk.Data.MatName^ := ReadString;
        end;
      MAT_SHADING:
        begin
          Chunk.Data.MatShading := AllocMem(SizeOf(TMatShading));
          FStream.Position := Chunk.Position + 6;
          Chunk.Data.MatShading^ := ReadShort;
        end;
      MAT_ACUBIC:
        begin
          Chunk.Data.MatAcubic := AllocMem(SizeOf(TMatAcubic));
          with Chunk.Data.MatAcubic^ do
          begin
            ShadeLevel := ReadByte;
            AntiAlias := ReadByte;
            Flags := ReadShort;
            MapSize := ReadCardinal;
            FrameInterval := ReadCardinal;
          end;
        end;
      MAT_SXP_TEXT_DATA,
        MAT_SXP_TEXT2_DATA,
        MAT_SXP_OPAC_DATA,
        MAT_SXP_BUMP_DATA,
        MAT_SXP_SPEC_DATA,
        MAT_SXP_SHIN_DATA,
        MAT_SXP_SELFI_DATA,
        MAT_SXP_TEXT_MASKDATA,
        MAT_SXP_TEXT2_MASKDATA,
        MAT_SXP_OPAC_MASKDATA,
        MAT_SXP_BUMP_MASKDATA,
        MAT_SXP_SPEC_MASKDATA,
        MAT_SXP_SHIN_MASKDATA,
        MAT_SXP_SELFI_MASKDATA,
        MAT_SXP_REFL_MASKDATA,
        PROC_DATA:
        begin
          Chunk.Data.IpasData := AllocMem(SizeOf(TIpasData));
          with Chunk.Data.IpasData^ do
          begin
            Size := Chunk.Size - 6;
            Data := AllocMem(Size);
            ReadData(Size, Data);
          end;
        end;
      MAT_WIRESIZE:
        begin
          Chunk.Data.MatWireSize := AllocMem(SizeOf(TMatWireSize));
          Chunk.Data.MatWireSize^ := ReadSingle;
        end;
      MAT_MAP_TILING:
        begin
          Chunk.Data.MatMapTiling := AllocMem(SizeOf(TMatMapTiling));
          Chunk.Data.MatMapTiling^ := ReadWord;
        end;
      MAT_MAP_TEXBLUR:
        begin
          Chunk.Data.MatMapTexblur := AllocMem(SizeOf(TMatMapTexblur));
          Chunk.Data.MatMapTexblur^ := ReadSingle;
        end;
      MAT_MAP_USCALE:
        begin
          Chunk.Data.MatMapUScale := AllocMem(SizeOf(TMatMapUScale));
          Chunk.Data.MatMapUScale^ := ReadSingle;
        end;
      MAT_MAP_VSCALE:
        begin
          Chunk.Data.MatMapVScale := AllocMem(SizeOf(TMatMapVScale));
          Chunk.Data.MatMapVScale^ := ReadSingle;
        end;
      MAT_MAP_UOFFSET:
        begin
          Chunk.Data.MatMapUOffset := AllocMem(SizeOf(TMatMapUOffset));
          Chunk.Data.MatMapUOffset^ := ReadSingle;
        end;
      MAT_MAP_VOFFSET:
        begin
          Chunk.Data.MatMapVOffset := AllocMem(SizeOf(TMatMapVOffset));
          Chunk.Data.MatMapVOffset^ := ReadSingle;
        end;
      MAT_MAP_ANG:
        begin
          Chunk.Data.MatMapAng := AllocMem(SizeOf(TMatMapAng));
          Chunk.Data.MatMapAng^ := ReadSingle;
        end;
      MAT_MAP_COL1:
        begin
          Chunk.Data.MatMapCol1 := AllocMem(SizeOf(TMatMapCol1));
          with Chunk.Data.MatMapCol1^ do
          begin
            Red := ReadByte;
            Green := ReadByte;
            Blue := ReadByte;
          end;
        end;
      MAT_MAP_COL2:
        begin
          Chunk.Data.MatMapCol2 := AllocMem(SizeOf(TMatMapCol2));
          with Chunk.Data.MatMapCol2^ do
          begin
            Red := ReadByte;
            Green := ReadByte;
            Blue := ReadByte;
          end;
        end;
      MAT_MAP_RCOL:
        begin
          Chunk.Data.MatMapRCol := AllocMem(SizeOf(TMatMapRCol));
          with Chunk.Data.MatMapRCol^ do
          begin
            Red := ReadByte;
            Green := ReadByte;
            Blue := ReadByte;
          end;
        end;
      MAT_MAP_GCOL:
        begin
          Chunk.Data.MatMapGCol := AllocMem(SizeOf(TMatMapGCol));
          with Chunk.Data.MatMapGCol^ do
          begin
            Red := ReadByte;
            Green := ReadByte;
            Blue := ReadByte;
          end;
        end;
      MAT_MAP_BCOL:
        begin
          Chunk.Data.MatMapBCol := AllocMem(SizeOf(TMatMapBCol));
          with Chunk.Data.MatMapBCol^ do
          begin
            Red := ReadByte;
            Green := ReadByte;
            Blue := ReadByte;
          end;
        end;
      MAT_BUMP_PERCENT:
        begin
          Chunk.Data.MatBumpPercent := AllocMem(SizeOf(TMatBumpPercent));
          Chunk.Data.MatBumpPercent^ := ReadShort;
        end;
      NAMED_OBJECT:
        begin
          Chunk.Data.NamedObject := AllocMem(SizeOf(TNamedObject));
          Chunk.Data.NamedObject^ := ReadString;
        end;
      POINT_ARRAY:
        begin
          Chunk.Data.PointArray := AllocMem(SizeOf(TPointArray));
          with Chunk.Data.PointArray^ do
          begin
            Vertices := ReadWord;
            PointList := AllocMem(Vertices * SizeOf(TPoint3DS));
            //for I := 0 to Vertices - 1 do PointList[I] := ReadPoint;
            ReadData(Vertices * SizeOf(TPoint3DS), PointList);
          end;
        end;
      POINT_FLAG_ARRAY:
        begin
          Chunk.Data.PointFlagArray := AllocMem(SizeOf(TPointFlagArray));
          with Chunk.Data.PointFlagArray^ do
          begin
            Flags := ReadWord;
            FlagList := AllocMem(Flags * SizeOf(SmallInt));
          //for I := 0 to Flags - 1 do FlagList[I] := ReadShort;
            ReadData(Flags * SizeOf(SmallInt), FlagList);
          end;
        end;
      FACE_ARRAY:
        begin
          Chunk.Data.FaceArray := AllocMem(SizeOf(TFaceArray));
          with Chunk.Data.FaceArray^ do
          begin
            Faces := ReadWord;
            FaceList := AllocMem(Faces * SizeOf(TFace3DS));
            //for I := 0 to Faces - 1 do FaceList[I] := ReadFace;
            ReadData(Faces * SizeOf(TFace3DS), FaceList);
          end;
        end;
      MSH_MAT_GROUP:
        begin
          Chunk.Data.MshMatGroup := AllocMem(SizeOf(TMshMatGroup));
          with Chunk.Data.MshMatGroup^ do
          begin
            MatName := ReadString;
            Faces := ReadWord;
            if Faces > 0 then
            begin
              FaceList := AllocMem(Faces * SizeOf(Word));
              //for I := 0 to Faces - 1 do FaceList[I] := ReadWord;
              ReadData(Faces * SizeOf(Word), FaceList);
            end
            else
              FaceList := nil;
          end;
        end;
      MSH_BOXMAP:
        begin
          Chunk.Data.MshBoxmap := AllocMem(SizeOf(TMshBoxmap));
          for I := 0 to 5 do
            Chunk.Data.MshBoxmap[I] := ReadString;
        end;
      SMOOTH_GROUP:
        begin
          Chunk.Data.SmoothGroup := AllocMem(SizeOf(TSmoothGroup));
          with Chunk.Data.SmoothGroup^ do
          begin
            Groups := (Chunk.Size - 6) div 4;
            GroupList := AllocMem(Groups * SizeOf(Cardinal));
            //for I := 0 to Groups - 1 do GroupList[I] := ReadCardinal;
            ReadData(Groups * SizeOf(Cardinal), GroupList);
          end;
        end;
      TEX_VERTS:
        begin
          Chunk.Data.TexVerts := AllocMem(SizeOf(TTexVerts));
          with Chunk.Data.TexVerts^ do
          begin
            NumCoords := ReadWord;
            TextVertList := AllocMem(NumCoords * SizeOf(TTexVert3DS));
            //for I := 0 to NumCoords - 1 do TextVertList[I] := ReadTexVert;
            ReadData(NumCoords * SizeOf(TTexVert3DS), TextVertList);
          end;
        end;
      MESH_MATRIX:
        begin
          Chunk.Data.MeshMatrix := AllocMem(SizeOf(TMeshMatrix));
          for I := 0 to 11 do
            Chunk.Data.MeshMatrix[I] := ReadSingle;
        end;
      MESH_COLOR:
        begin
          Chunk.Data.MeshColor := AllocMem(SizeOf(TMeshColor));
          Chunk.Data.MeshColor^ := ReadByte;
        end;
      MESH_TEXTURE_INFO:
        begin
          Chunk.Data.MeshTextureInfo := AllocMem(SizeOf(TMeshTextureInfo));
          with Chunk.Data.MeshTextureInfo^ do
          begin
            MapType := ReadWord;
            XTiling := ReadSingle;
            YTiling := ReadSingle;
            IconPos := ReadPoint();
            IconScaling := ReadSingle;
            for I := 0 to 11 do
              XMatrix[I] := ReadSingle;
            IconWidth := ReadSingle;
            IconHeight := ReadSingle;
            CylIconHeight := ReadSingle;
          end;
        end;
      PROC_NAME:
        begin
          Chunk.Data.ProcName := AllocMem(SizeOf(TProcName));
          Chunk.Data.ProcName^ := ReadString;
        end;
      N_DIRECT_LIGHT:
        begin
          Chunk.Data.NDirectLight := AllocMem(SizeOf(TNDirectLight));
          Chunk.Data.NDirectLight^ := ReadPoint;
        end;
      DL_EXCLUDE:
        begin
          Chunk.Data.DlExclude := AllocMem(SizeOf(TDlExclude));
          Chunk.Data.DlExclude^ := ReadString;
        end;
      DL_INNER_RANGE:
        begin
          Chunk.Data.DlInnerRange := AllocMem(SizeOf(TDlInnerRange));
          Chunk.Data.DlInnerRange^ := ReadSingle;
        end;
      DL_OUTER_RANGE:
        begin
          Chunk.Data.DlOuterRange := AllocMem(SizeOf(TDlOuterRange));
          Chunk.Data.DlOuterRange^ := ReadSingle;
        end;
      DL_MULTIPLIER:
        begin
          Chunk.Data.DlMultiplier := AllocMem(SizeOf(TDlMultiplier));
          Chunk.Data.DlMultiplier^ := ReadSingle;
        end;
      DL_SPOTLIGHT:
        begin
          Chunk.Data.DlSpotlight := AllocMem(SizeOf(TDlSpotlight));
          with Chunk.Data.DlSpotlight^ do
          begin
            SpotlightTarg := ReadPoint;
            HotspotAngle := ReadSingle;
            FalloffAngle := ReadSingle;
          end;
        end;
      DL_LOCAL_SHADOW2:
        begin
          Chunk.Data.DlLocalShadow2 := AllocMem(SizeOf(TDlLocalShadow2));
          with Chunk.Data.DlLocalShadow2^ do
          begin
            LocalShadowBias := ReadSingle;
            LocalShadowFilter := ReadSingle;
            LocalShadowMapSize := ReadShort;
          end;
        end;
      DL_SPOT_ROLL:
        begin
          Chunk.Data.DlSpotRoll := AllocMem(SizeOf(TDlSpotRoll));
          Chunk.Data.DlSpotRoll^ := ReadSingle;
        end;
      DL_SPOT_ASPECT:
        begin
          Chunk.Data.DlSpotAspect := AllocMem(SizeOf(TDlSpotAspect));
          Chunk.Data.DlSpotAspect^ := ReadSingle;
        end;
      DL_SPOT_PROJECTOR:
        begin
          Chunk.Data.DlSpotProjector := AllocMem(SizeOf(TDlSpotProjector));
          Chunk.Data.DlSpotProjector^ := ReadString;
        end;
      DL_RAY_BIAS:
        begin
          Chunk.Data.DlRayBias := AllocMem(SizeOf(TDlRayBias));
          Chunk.Data.DlRayBias^ := ReadSingle;
        end;
      N_CAMERA:
        begin
          Chunk.Data.NCamera := AllocMem(SizeOf(TNCamera));
          with Chunk.Data.NCamera^ do
          begin
            CameraPos := ReadPoint;
            TargetPos := ReadPoint;
            CameraBank := ReadSingle;
            CameraFocalLength := ReadSingle;
          end;
        end;
      CAM_RANGES:
        begin
          Chunk.Data.CamRanges := AllocMem(SizeOf(TCamRanges));
          with Chunk.Data.CamRanges^ do
          begin
            NearPlane := ReadSingle;
            FarPlane := ReadSingle;
          end;
        end;
      VIEWPORT_LAYOUT:
        begin
          Chunk.Data.ViewportLayout := AllocMem(SizeOf(TViewportLayout));
          with Chunk.Data.ViewportLayout^ do
          begin
            Form := ReadShort;
            Top := ReadShort;
            Ready := ReadShort;
            WState := ReadShort;
            SwapWS := ReadShort;
            SwapPort := ReadShort;
            SwapCur := ReadShort;
          end;
        end;
      VIEWPORT_SIZE:
        begin
          Chunk.Data.ViewportSize := AllocMem(SizeOf(TViewportSize));
          with Chunk.Data.ViewportSize^ do
          begin
            XPos := ReadWord;
            YPos := ReadWord;
            Width := ReadWord;
            Height := ReadWord;
          end;
        end;
      VIEWPORT_DATA_3,
        VIEWPORT_DATA:
        begin
          Chunk.Data.ViewportData := AllocMem(SizeOf(TViewportData));
          with Chunk.Data.ViewportData^ do
          begin
            Flags := ReadShort;
            AxisLockout := ReadShort;
            WinXPos := ReadShort;
            WinYPos := ReadShort;
            WinWidth := ReadShort;
            WinHeight := ReadShort;
            View := ReadShort;
            ZoomFactor := ReadSingle;
            Center := ReadPoint;
            HorizAng := ReadSingle;
            VertAng := ReadSingle;
            CamName := ReadString;
          end;
        end;
      XDATA_ENTRY:
        begin
          InitChunkData(Chunk);
          with Chunk.Data.XDataEntry^ do
          begin
            Size := (Chunk.Size) - 6;
            Data := AllocMem(Size);
            ReadData(Size, Data);
          end;
        end;
      XDATA_APPNAME:
        begin
          Chunk.Data.XDataAppName := AllocMem(SizeOf(TXDataAppName));
          Chunk.Data.XDataAppName^ := ReadString;
        end;
      XDATA_STRING:
        begin
          Chunk.Data.XDataString := AllocMem(SizeOf(TXDataString));
          Chunk.Data.XDataString^ := ReadString;
        end;
      KFHDR:
        begin
          Chunk.Data.KFHdr := AllocMem(SizeOf(TKFHdr));
          with Chunk.Data.KFHdr^ do
          begin
            Revision := ReadShort;
            FileName := ReadString;
            AnimLength := ReadInteger;
          end;
        end;
      KFSEG:
        begin
          Chunk.Data.KFSeg := AllocMem(SizeOf(TKFSeg));
          with Chunk.Data.KFSeg^ do
          begin
            First := ReadInteger;
            Last := ReadInteger;
          end;
        end;
      KFCURTIME:
        begin
          Chunk.Data.KFCurtime := AllocMem(SizeOf(TKFCurtime));
          Chunk.Data.KFCurtime^ := ReadInteger;
        end;
      NODE_ID:
        begin
          Chunk.Data.KFId := AllocMem(SizeOf(TKFId));
          Chunk.Data.KFId^ := ReadShort;
        end;
      NODE_HDR:
        begin
          Chunk.Data.NodeHdr := AllocMem(SizeOf(TNodeHdr));
          with Chunk.Data.NodeHdr^ do
          begin
            ObjName := ReadString;
            Flags1 := ReadWord;
            Flags2 := ReadWord;
            ParentIndex := ReadShort;
          end;
        end;
      PIVOT:
        begin
          Chunk.Data.Pivot := AllocMem(SizeOf(TPivot));
          Chunk.Data.Pivot^ := ReadPoint;
        end;
      INSTANCE_NAME:
        begin
          Chunk.Data.InstanceName := AllocMem(SizeOf(TInstanceName));
          Chunk.Data.InstanceName^ := ReadString;
        end;
      PARENT_NAME:
        ; // do nothing
      MORPH_SMOOTH:
        begin
          Chunk.Data.MorphSmooth := AllocMem(SizeOf(TMorphSmooth));
          Chunk.Data.MorphSmooth^ := ReadSingle;
        end;
      BOUNDBOX:
        begin
          Chunk.Data.BoundBox := AllocMem(SizeOf(TBoundBox));
          with Chunk.Data.BoundBox^ do
          begin
            Min := ReadPoint;
            Max := ReadPoint;
          end;
        end;
      POS_TRACK_TAG:
        begin
          Chunk.Data.PosTrackTag := AllocMem(SizeOf(TPosTrackTag));
          with Chunk.Data.PosTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            PositionList := AllocMem(TrackHdr.KeyCount * SizeOf(TPoint3DS));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              PositionList[I] := ReadPoint;
            end;
          end;
        end;
      COL_TRACK_TAG:
        begin
          Chunk.Data.ColTrackTag := AllocMem(SizeOf(TColTrackTag));
          with Chunk.Data.ColTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            ColorList := AllocMem(TrackHdr.KeyCount * SizeOf(TFColor3DS));
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              ColorList[I].R := ReadSingle;
              ColorList[I].G := ReadSingle;
              ColorList[I].B := ReadSingle;
            end;
          end;
        end;
      ROT_TRACK_TAG:
        begin
          Chunk.Data.RotTrackTag := AllocMem(SizeOf(TRotTrackTag));
          with Chunk.Data.RotTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            RotationList := AllocMem(TrackHdr.KeyCount * SizeOf(TKFrotkey3DS));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              RotationList[I].Angle := ReadSingle;
              RotationList[I].X := ReadSingle;
              RotationList[I].Y := ReadSingle;
              RotationList[I].Z := ReadSingle;
            end;
          end;
        end;
      SCL_TRACK_TAG:
        begin
          Chunk.Data.ScaleTrackTag := AllocMem(SizeOf(TScaleTrackTag));
          with Chunk.Data.ScaleTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            ScaleList := AllocMem(TrackHdr.KeyCount * SizeOf(TPoint3DS));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              ScaleList[I].X := ReadSingle;
              ScaleList[I].Y := ReadSingle;
              ScaleList[I].Z := ReadSingle;
            end;
          end;
        end;
      MORPH_TRACK_TAG:
        begin
          Chunk.Data.MorphTrackTag := AllocMem(SizeOf(TMorphTrackTag));
          with Chunk.Data.MorphTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            MorphList := AllocMem(TrackHdr.KeyCount * SizeOf(TKFmorphKey3DS));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              MorphList[I] := ReadString;
            end;
          end;
        end;
      FOV_TRACK_TAG:
        begin
          Chunk.Data.FovTrackTag := AllocMem(SizeOf(TFovTrackTag));
          with Chunk.Data.FovTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            FOVAngleList := AllocMem(TrackHdr.KeyCount * SizeOf(Single));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              FOVAngleList[I] := ReadSingle;
            end;
          end;
        end;
      ROLL_TRACK_TAG:
        begin
          Chunk.Data.RollTrackTag := AllocMem(SizeOf(TRollTrackTag));
          with Chunk.Data.RollTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            RollAngleList := AllocMem(TrackHdr.KeyCount * SizeOf(Single));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              RollAngleList[I] := ReadSingle;
            end;
          end;
        end;
      HOT_TRACK_TAG:
        begin
          Chunk.Data.HotTrackTag := AllocMem(SizeOf(THotTrackTag));
          with Chunk.Data.HotTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            HotspotAngleList := AllocMem(TrackHdr.KeyCount * SizeOf(Single));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              HotspotAngleList[I] := ReadSingle;
            end;
          end;
        end;
      FALL_TRACK_TAG:
        begin
          Chunk.Data.FallTrackTag := AllocMem(SizeOf(TFallTrackTag));
          with Chunk.Data.FallTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            FalloffAngleList := AllocMem(TrackHdr.KeyCount * SizeOf(Single));
            for I := 0 to TrackHdr.KeyCount - 1 do
            begin
              KeyHdrList[I] := ReadKeyHeader;
              FalloffAngleList[I] := ReadSingle;
            end;
          end;
        end;
      HIDE_TRACK_TAG:
        begin
          Chunk.Data.HideTrackTag := AllocMem(SizeOf(THideTrackTag));
          with Chunk.Data.HideTrackTag^ do
          begin
            TrackHdr := ReadTrackHeader;
            KeyHdrList := AllocMem(TrackHdr.KeyCount * SizeOf(TKeyHeader3DS));
            for I := 0 to TrackHdr.KeyCount - 1 do
              KeyHdrList[I] := ReadKeyHeader;
          end;
        end;
      M3DMAGIC, // Chunks that do not contain data, or only contain children
        MLIBMAGIC,
        MDATA,
        AMBIENT_LIGHT,
        SOLID_BGND,
        DEFAULT_VIEW,
        MAT_ENTRY,
        MAT_AMBIENT,
        MAT_DIFFUSE,
        MAT_SPECULAR,
        MAT_SHININESS,
        MAT_SHIN2PCT,
        MAT_SHIN3PCT,
        MAT_TRANSPARENCY,
        MAT_XPFALL,
        MAT_REFBLUR,
        MAT_SELF_ILPCT,
        MAT_TEXMAP,
        MAT_TEXMASK,
        MAT_TEX2MAP,
        MAT_TEX2MASK,
        MAT_OPACMAP,
        MAT_OPACMASK,
        MAT_REFLMAP,
        MAT_REFLMASK,
        MAT_BUMPMAP,
        MAT_BUMPMASK,
        MAT_SPECMAP,
        MAT_SPECMASK,
        MAT_SHINMAP,
        MAT_SHINMASK,
        MAT_SELFIMAP,
        MAT_SELFIMASK,
        N_TRI_OBJECT,
        KFDATA,
        AMBIENT_NODE_TAG,
        OBJECT_NODE_TAG,
        CAMERA_NODE_TAG,
        TARGET_NODE_TAG,
        LIGHT_NODE_TAG,
        SPOTLIGHT_NODE_TAG,
        L_TARGET_NODE_TAG,
        CMAGIC,
        XDATA_SECTION,
        XDATA_GROUP:
        ; // do nothing
    else // a truely hideous thing to do, but it helps with unknown chunks
      if Chunk.Size > 6 then // don't mess with dataless chunks
      begin
        Chunk.Data.Dummy := AllocMem(Chunk.Size - 6);
        ReadData(Chunk.Size - 6, Chunk.Data.Dummy);
      end;
    end; // end of case
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure TFile3DS.SeekChild(Chunk: PChunk3DS);

// Function skips to next Chunk on disk by seeking the next file position

var
  Offset: Integer;
  Dummy: string;

begin
  Offset := 0;
  case Chunk.Tag of
    M3DMAGIC,
      SMAGIC,
      LMAGIC,
      MATMAGIC,
      MLIBMAGIC,
      MDATA,
      AMBIENT_LIGHT,
      SOLID_BGND,
      DEFAULT_VIEW,
      MAT_ENTRY,
      MAT_AMBIENT,
      MAT_DIFFUSE,
      MAT_SPECULAR,
      MAT_SHININESS,
      MAT_SHIN2PCT,
      MAT_SHIN3PCT,
      MAT_TRANSPARENCY,
      MAT_XPFALL,
      MAT_REFBLUR,
      MAT_SELF_ILPCT,
      MAT_TEXMAP,
      MAT_TEXMASK,
      MAT_TEX2MAP,
      MAT_TEX2MASK,
      MAT_OPACMAP,
      MAT_OPACMASK,
      MAT_REFLMAP,
      MAT_REFLMASK,
      MAT_BUMPMAP,
      MAT_BUMPMASK,
      MAT_SPECMAP,
      MAT_SPECMASK,
      MAT_SHINMAP,
      MAT_SHINMASK,
      MAT_SELFIMAP,
      MAT_SELFIMASK,
      N_TRI_OBJECT,
      XDATA_SECTION,
      XDATA_ENTRY,
      KFDATA,
      OBJECT_NODE_TAG,
      CAMERA_NODE_TAG,
      TARGET_NODE_TAG,
      LIGHT_NODE_TAG,
      SPOTLIGHT_NODE_TAG,
      L_TARGET_NODE_TAG,
      AMBIENT_NODE_TAG,
      CMAGIC:
      ; // do nothing
    M3D_VERSION:
      Offset := SizeOf(Integer);
    COLOR_F:
      Offset := 3 * SizeOf(Single);
    COLOR_24:
      Offset := 3 * SizeOf(Byte);
    INT_PERCENTAGE:
      Offset := SizeOf(SmallInt);
    FLOAT_PERCENTAGE:
      Offset := SizeOf(Single);
    MAT_MAPNAME:
      Dummy := ReadString;
    MESH_VERSION:
      Offset := SizeOf(Integer);
    MASTER_SCALE:
      Offset := SizeOf(Single);
    LO_SHADOW_BIAS:
      Offset := SizeOf(Single);
    HI_SHADOW_BIAS:
      Offset := SizeOf(Single);
    SHADOW_MAP_SIZE:
      Offset := SizeOf(SmallInt);
    SHADOW_SAMPLES:
      Offset := SizeOf(SmallInt);
    O_CONSTS:
      Offset := 12;
    V_GRADIENT:
      Offset := SizeOf(Single);
    NAMED_OBJECT:
      Dummy := ReadString;
    BIT_MAP:
      Dummy := ReadString;
    FOG:
      Offset := 4 * SizeOf(Single);
    LAYER_FOG:
      Offset := 3 * SizeOf(Single) + SizeOf(Integer);
    DISTANCE_CUE:
      Offset := 4 * SizeOf(Single);
    N_DIRECT_LIGHT:
      Offset := 12;
    DL_SPOTLIGHT:
      Offset := 12 + 2 * SizeOf(Single);
    N_CAMERA:
      Offset := 24 + 2 * SizeOf(Single);
    VIEWPORT_LAYOUT:
      Offset := 7 * SizeOf(SmallInt);
    VIEW_TOP,
      VIEW_BOTTOM,
      VIEW_LEFT,
      VIEW_RIGHT,
      VIEW_FRONT,
      VIEW_BACK:
      Offset := 12 + SizeOf(Single);
    VIEW_USER:
      Offset := 12 + 4 * SizeOf(Single);
    VIEW_CAMERA:
      Dummy := ReadString;
    MAT_NAME:
      Dummy := ReadString;
    MAT_ACUBIC:
      Offset := 2 * SizeOf(Byte) + 2 * SizeOf(Integer) + SizeOf(SmallInt);
    POINT_ARRAY,
      POINT_FLAG_ARRAY:
      Offset := Chunk.Size - 6;
    FACE_ARRAY:
      Offset := ReadWord * SizeOf(SmallInt) * 4;
    MSH_MAT_GROUP:
      Offset := Chunk.Size - 6;
    SMOOTH_GROUP:
      Offset := Chunk.Size - 6;
    TEX_VERTS:
      Offset := Chunk.Size - 6;
    MESH_MATRIX:
      Offset := 12 * SizeOf(Single);
    MESH_TEXTURE_INFO:
      Offset := Chunk.Size - 6;
    PROC_NAME:
      Dummy := ReadString;
    DL_LOCAL_SHADOW2:
      Offset := 2 * SizeOf(Single) + SizeOf(SmallInt);
    KFHDR:
      begin
        ReadShort;
        Dummy := ReadString;
        ReadInteger;
      end;
    KFSEG:
      Offset := 2 * SizeOf(Integer);
    KFCURTIME:
      Offset := SizeOf(Integer);
    NODE_HDR:
      begin
        Dummy := ReadString;
        Offset := 2 * SizeOf(SmallInt) + SizeOf(SmallInt);
      end;
    NODE_ID:
      Offset := SizeOf(SmallInt);
    PIVOT:
      Offset := 12;
    INSTANCE_NAME:
      Dummy := ReadString;
    MORPH_SMOOTH:
      Offset := SizeOf(Single);
    BOUNDBOX:
      Offset := 24;
    VPDATA:
      Offset := SizeOf(Integer);
  else
    Offset := Chunk.Size - 6;
  end;
  FStream.Seek(Offset, soFromCurrent);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetDatabaseRelease: TReleaseLevel;

begin
  Result := Utils3DS.GetDatabaseRelease(Self, FDatabase);
end;

//---------------------------------------------------------------------------------------------------------------------

function TFile3DS.GetMeshRelease: TReleaseLevel;

begin
  Result := Utils3DS.GetMeshRelease(Self, FDatabase);
end;

//---------------------------------------------------------------------------------------------------------------------

end.

