unit Utils3DS;

// Utility functions for the universal 3DS file reader and writer (TFile3DS). Essentially, the functions
// here are the heart of the import library as they deal actually with the database and chunks.
//
// Last Change - 03. October 1999
//
// (c) Copyright 1999, Dipl. Ing. Mike Lischke (public@lischke-online.de)

interface

{$R-}

uses
  Classes, File3DS, Types3DS;

// functions to retrieve global settings of a specific 3DS database
function GetAtmosphere(const Source: TFile3DS; var DB: TDatabase3DS): TAtmosphere3DS;
function GetBackground(const Source: TFile3DS; var DB: TDatabase3DS): TBackground3DS;
function GetMeshSet(const Source: TFile3DS; var DB: TDatabase3DS): TMeshSet3DS;
function GetViewport(const Source: TFile3DS; var DB: TDatabase3DS): TViewport3DS;

// functions to retrieve/modify data related to materials, lights and objects (meshs)
procedure AddChild(Parent, Child: PChunk3DS);
procedure AddChildOrdered(Parent, Child: PChunk3DS);
function FindChunk(Top: PChunk3DS; Tag: Word): PChunk3DS;
function FindNextChunk(Local: PChunk3DS; Tag: Word): PChunk3DS;
procedure FreeChunkData(var Chunk: PChunk3DS);
function GetCameraByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TCamera3DS;
function GetCameraCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;
function GetChunkValue(Tag: Word): Integer;
function GetMaterialByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TMaterial3DS;
function GetMaterialCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;
function GetMeshByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TMesh3DS;
function GetMeshCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;
function GetOmnilightByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TLight3DS;
function GetSpotlightByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TLight3DS;
function GetOmnilightCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;
function GetSpotlightCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;
procedure InitChunk(var Chunk: PChunk3DS);
procedure ReleaseCamera(Camera: PCamera3DS);
procedure ReleaseChunk(var Chunk: PChunk3DS);
procedure ReleaseChunkList(var List: PChunkList3DS);
procedure ReleaseLight(Light: PLight3DS);
procedure ReleaseMaterial(Mat: PMaterial3DS);
procedure ReleaseMeshObj(Mesh: PMesh3DS);

// functions to retrieve/modify keyframer (animation) data
function GetKFSettings(const Source: TFile3DS; var DB: TDatabase3DS): TKFSets3DS;

procedure ReleaseCameraMotion(Camera: PKFCamera3DS);
procedure GetCameraNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);
function GetCameraNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;
function GetCameraMotion(const Source: TFile3DS; CamChunk, TargetChunk: PChunk3DS): TKFCamera3DS;
function GetCameraMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TKFCamera3DS;

procedure ReleaseAmbientLightMotion(Light: PKFAmbient3DS);
function GetAmbientLightMotion(const Source: TFile3DS; var DB: TDatabase3DS): TKFAmbient3DS;

procedure InitObjectMotion(var Obj: TKFMesh3DS; NewNPKeys, NewNRKeys, NewNSKeys, NewNMKeys, NewNHKeys: Cardinal);
procedure ReleaseObjectMotion(Obj: PKFMesh3DS);
procedure GetObjectNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);
function GetObjectNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;
function GetObjectMotionByName(const Source: TFile3DS; var DB: TDatabase3DS; Name: string): TKFMesh3DS;
function GetObjectMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Cardinal): TKFMesh3DS;

procedure ReleaseOmnilightMotion(Light: PKFOmni3DS);
procedure GetOmnilightNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);
function GetOmnilightNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Cardinal;
function GetOmnilightMotionByName(const Source: TFile3DS; var DB: TDatabase3DS; Name: string): TKFOmni3DS;
function GetOmnilightMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Cardinal): TKFOmni3DS;

procedure ReleaseSpotlightMotion(Spot: PKFSpot3DS);
procedure GetSpotlightNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);
function GetSpotlightNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Cardinal;
function GetSpotlightMotionByName(const Source: TFile3DS; var DB: TDatabase3DS; Name: string): TKFSpot3DS;
function GetSpotlightMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Cardinal): TKFSpot3DS;

// version information
function GetM3dMagicRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;
function GetMeshRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;
function GetKfRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;
function GetDatabaseRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;

// support functions for text output of chunk and database contents
procedure ChunkHeaderReport(var Strings: TStrings; Chunk: PChunk3DS; IndentLevel: Integer);
function ChunkTagToString(Tag: Word): string;
procedure DumpChunk(const Source: TFile3DS; var Strings: TStrings; Chunk: PChunk3DS; IndentLevel: Integer; DumpLevel:
  TDumpLevel);
procedure DumpKeyHeader(Strings: TStrings; Key: TKeyHeader3DS; IndentLevel: Integer);

// support functions for chunk handling
procedure DeleteChunk(var Chunk: PChunk3DS);
function FindNamedObjectByIndex(Source: TFile3DS; DB: TDatabase3DS; AType: Word; Index: Integer): PChunk3DS;

// error message routines
procedure ShowError(ErrorMessage: string);
procedure ShowErrorFormatted(ErrorMessage: string; const Args: array of const);

//---------------------------------------------------------------------------------------------------------------------

implementation

uses
  Windows, Dialogs, SysUtils, Const3DS;

type
  E3DSError = class(Exception);

//----------------- error handling ------------------------------------------------------------------------------------

procedure ShowError(ErrorMessage: string);

begin
  MessageBeep(MB_ICONHAND);
  raise E3DSError.Create(ErrorMessage);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ShowErrorFormatted(ErrorMessage: string; const Args: array of const);

begin
  MessageBeep(MB_ICONHAND);
  raise E3DSError.CreateFmt(ErrorMessage, Args);
end;

//----------------- global settings functions -------------------------------------------------------------------------

function InitMeshSet: TMeshSet3DS;

// initializes a mesh settings structure

begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    MasterScale := 1;
    Shadow.Bias := 1;
    Shadow.RayBias := 1;
    Shadow.MapSize := 512;
    Shadow.Filter := 3;
    AmbientLight.R := 0.39216;
    AmbientLight.G := 0.39216;
    AmbientLight.B := 0.39216;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetMeshSet(const Source: TFile3DS; var DB: TDatabase3DS): TMeshSet3DS;

// retrieves the mesh settings from the database

var
  MDataChunk,
    ColorChunk,
    Chunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  // find the mesh data chunk
  MDataChunk := FindChunk(DB.TopChunk, MDATA);

  // If the mesh data section is found
  if Assigned(MDataChunk) then
  begin
    Result := InitMeshSet;
    with Result do
    begin
      // Search for a master_scale chunk
      Chunk := FindNextChunk(MDataChunk.Children, MASTER_SCALE);
      if Assigned(Chunk) then
      begin
        Source.ReadChunkData(Chunk);
        MasterScale := Chunk.Data.MasterScale^;
        FreeChunkData(Chunk);
      end;

      // search for Lo_Shadow_Bias chunk
      Chunk := FindNextChunk(MDataChunk.Children, LO_SHADOW_BIAS);
      if Assigned(Chunk) then
      begin
        Source.ReadChunkData(Chunk);
        Shadow.Bias := Chunk.Data.LoShadowBias^;
        FreeChunkData(Chunk);
      end;

      // Search for ray_Bias Chunk
      Chunk := FindNextChunk(MDataChunk.Children, RAY_BIAS);
      if Assigned(Chunk) then
      begin
        Source.ReadChunkData(Chunk);
        Shadow.RayBias := Chunk.Data.RayBias^;
        FreeChunkData(Chunk);
      end;

      // search for MapSize Chunk
      Chunk := FindNextChunk(MDataChunk.Children, SHADOW_MAP_SIZE);
      if Assigned(Chunk) then
      begin
        Source.ReadChunkData(Chunk);
        Shadow.MapSize := Chunk.Data.ShadowMapSize^;
        FreeChunkData(Chunk);
      end;

      // search for Shadow_Filter Chunk
      Chunk := FindNextChunk(MDataChunk.Children, SHADOW_FILTER);
      if Assigned(Chunk) then
      begin
        Source.ReadChunkData(Chunk);
        Shadow.Filter := Chunk.Data.ShadowFilter^;
        FreeChunkData(Chunk);
      end;

      // search for ambient_light Chunk
      Chunk := FindNextChunk(MDataChunk.Children, AMBIENT_LIGHT);
      if Assigned(Chunk) then
      begin
        // search for the old style Color chunk inside the ambient Light Chunk
        ColorChunk := FindChunk(Chunk, COLOR_F);
        if Assigned(ColorChunk) then
        begin
          Source.ReadChunkData(ColorChunk);
          AmbientLight.R := ColorChunk.Data.ColorF.Red;
          AmbientLight.G := ColorChunk.Data.ColorF.Green;
          AmbientLight.B := ColorChunk.Data.ColorF.Blue;
          FreeChunkData(ColorChunk);
        end
        else
        begin
          // just for robust completeness, search for the COLOR_24 chunk
          ColorChunk := FindChunk(Chunk, COLOR_24);
          if Assigned(ColorChunk) then
          begin
            Source.ReadChunkData(ColorChunk);
            AmbientLight.R := ColorChunk.Data.Color24.Red / 255;
            AmbientLight.G := ColorChunk.Data.Color24.Green / 255;
            AmbientLight.B := ColorChunk.Data.Color24.Blue / 255;
            FreeChunkData(ColorChunk);
          end;
        end;

        // search for the newer linear Color Chunk inside the ambient Light chunk
        ColorChunk := FindChunk(Chunk, LIN_COLOR_F);
        if Assigned(ColorChunk) then
        begin
          Source.ReadChunkData(ColorChunk);
          AmbientLight.R := ColorChunk.Data.LinColorF.Red;
          AmbientLight.G := ColorChunk.Data.LinColorF.Green;
          AmbientLight.B := ColorChunk.Data.LinColorF.Blue;
          FreeChunkData(ColorChunk);
        end
        else
        begin
          // just for completeness, search for the LIN_COLOR_24 chunk
          ColorChunk := FindChunk(Chunk, LIN_COLOR_24);
          if Assigned(ColorChunk) then
          begin
            Source.ReadChunkData(ColorChunk);
            AmbientLight.R := ColorChunk.Data.LinColorF.Red / 255;
            AmbientLight.G := ColorChunk.Data.LinColorF.Green / 255;
            AmbientLight.B := ColorChunk.Data.LinColorF.Blue / 255;
            FreeChunkData(ColorChunk);
          end;
        end;
      end;

      // Search for the oconst chunk
      Chunk := FindNextChunk(MDataChunk.Children, O_CONSTS);
      if Assigned(Chunk) then
      begin
        Source.ReadChunkData(Chunk);
        oconsts.x := Chunk.Data.OConsts.X;
        oconsts.y := Chunk.Data.OConsts.Y;
        oconsts.z := Chunk.Data.OConsts.Z;
        FreeChunkData(Chunk);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function InitAtmosphere: TAtmosphere3DS;

// initializes a atmosphere structure

begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    Fog.FarPlane := 1000;
    Fog.FarDensity := 100;
    Fog.FogBgnd := True;

    LayerFog.ZMax := 100;
    LayerFog.Density := 50;
    LayerFog.Falloff := lfNoFall;
    LayerFog.Fogbgnd := True;

    DCue.FarPlane := 1000;
    DCue.FarDim := 100;

    ActiveAtmo := atNoAtmo;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetAtmosphere(const Source: TFile3DS; var DB: TDatabase3DS): TAtmosphere3DS;

// retrieves the atmospheric settings from database

var
  MDataChunk,
    FogChunk,
    BgnChunk,
    ColorChunk,
    Chunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  // find the MDATA chunk
  MDataChunk := FindChunk(DB.TopChunk, MDATA);

  // if the MDATA chunk was found, then search for the atmospheric chunks
  if Assigned(MDataChunk) then
  begin
    Result := InitAtmosphere;
    // Search for fog chunk
    FogChunk := FindChunk(MDataChunk, FOG);
    if Assigned(FogChunk) then
      with Result do
      begin
        // read the chunk information
        Source.ReadChunkData(FogChunk);

        // Copy the FogChunk data into the structure
        Fog.NearPlane := FogChunk.Data.Fog.NearPlaneDist;
        Fog.NearDensity := FogChunk.Data.Fog.NearPlaneDensity;
        Fog.FarPlane := FogChunk.Data.Fog.FarPlanedist;
        Fog.FarDensity := FogChunk.Data.Fog.FarPlaneDensity;

        // Search for fog Color chunk
        ColorChunk := FindChunk(FogChunk, COLOR_F);
        if Assigned(ColorChunk) then
        begin
          Source.ReadChunkData(ColorChunk);
          Fog.FogColor.R := ColorChunk.Data.ColorF.Red;
          Fog.Fogcolor.G := ColorChunk.Data.ColorF.Green;
          Fog.Fogcolor.B := ColorChunk.Data.ColorF.Blue;
          FreeChunkData(ColorChunk);
        end;

        // Search for FOG_BGND chunk
        BgnChunk := FindChunk(FogChunk, FOG_BGND);
        if Assigned(BgnChunk) then
          Fog.FogBgnd := True
        else
          Fog.FogBgnd := False;
        FreeChunkData(FogChunk);

        // search for LAYER_FOG chunk
        FogChunk := FindChunk(MDataChunk, LAYER_FOG);
        if Assigned(FogChunk) then
        begin
          Source.ReadChunkData(FogChunk);

          LayerFog.ZMin := FogChunk.Data.LayerFog.ZMin;
          LayerFog.ZMax := FogChunk.Data.LayerFog.ZMax;
          LayerFog.Density := FogChunk.Data.LayerFog.Density;

          if (FogChunk.Data.LayerFog.AType and LayerFogBgnd) <> 0 then
            LayerFog.FogBgnd := True
          else
            LayerFog.FogBgnd := False;

          if (FogChunk.Data.LayerFog.AType and TopFalloff) <> 0 then
            LayerFog.Falloff := lfTopFall
          else
            if (FogChunk.Data.LayerFog.AType and BottomFalloff) <> 0 then
              LayerFog.Falloff := lfBottomFall
            else
              LayerFog.Falloff := lfNoFall;

          ColorChunk := FindChunk(FogChunk, COLOR_F);
          if Assigned(ColorChunk) then
          begin
            Source.ReadChunkData(ColorChunk);
            LayerFog.FogColor.R := ColorChunk.Data.ColorF.Red;
            LayerFog.Fogcolor.G := ColorChunk.Data.ColorF.Green;
            LayerFog.Fogcolor.B := ColorChunk.Data.ColorF.Blue;
            FreeChunkData(ColorChunk);
          end;
          FreeChunkData(FogChunk);
        end;

        // search for DISTANCE_CUE chunk
        Chunk := FindChunk(MDataChunk, DISTANCE_CUE);
        if Assigned(Chunk) then
        begin
          Source.ReadChunkData(Chunk);

          DCue.NearPlane := Chunk.Data.DistanceCue.NearPlaneDist;
          DCue.neardim := Chunk.Data.DistanceCue.NearPlaneDimming;
          DCue.FarPlane := Chunk.Data.DistanceCue.FarPlaneDist;
          DCue.FarDim := Chunk.Data.DistanceCue.FarPlaneDimming;

          BgnChunk := FindChunk(Chunk, DCUE_BGND);
          if Assigned(BgnChunk) then
            DCue.DCueBgnd := True
          else
            DCue.DCueBgnd := False;
          FreeChunkData(Chunk);
        end;

        // search for USE_FOG, USE_LAYER_FOG or USE_DISTANCE_CUE chunk
        Chunk := FindChunk(MDataChunk, USE_FOG);
        if Assigned(Chunk) then
          ActiveAtmo := atUseFog
        else
        begin
          Chunk := FindChunk(MDataChunk, USE_LAYER_FOG);
          if Assigned(Chunk) then
            ActiveAtmo := atUseLayerFog
          else
          begin
            Chunk := FindChunk(MDataChunk, USE_DISTANCE_CUE);
            if Assigned(Chunk) then
              ActiveAtmo := atUseDistanceCue
            else
              ActiveAtmo := atNoAtmo;
          end;
        end;
      end; // with Result do
  end; // if Assigned(MDataChunk)
end;

//---------------------------------------------------------------------------------------------------------------------

function InitBackground: TBackground3DS;

// initializes the TBackground3DS structure

begin
  FillChar(Result, SizeOf(Result), 0);
  Result.VGradient.GradPercent := 0.5;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetBackground(const Source: TFile3DS; var DB: TDatabase3DS): TBackground3DS;

// retrieves the background settings from the database

var
  MDataChunk,
    ColorChunk,
    TopColor,
    MidColor,
    BotColor,
    Chunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  // Find the MDATA chunk
  MDataChunk := FindChunk(DB.TopChunk, MDATA);

  // only continue with structure filling if an MDATA chunk is found
  if Assigned(MDataChunk) then
    with Result do
    begin
      Result := InitBackground;
      // search for bitmap chunk
      Chunk := FindChunk(MDataChunk, BIT_MAP);
      if Assigned(Chunk) then
      begin
        // read the chunk information
        Source.ReadChunkData(Chunk);
        // copy the bitmap filename to the structure
        if Assigned(Chunk.Data.BitmapName) then
          Bitmap := Chunk.Data.BitmapName^
        else
          Bitmap := '';
        FreeChunkData(Chunk);
      end;

      Chunk := FindChunk(MDataChunk, SOLID_BGND);
      if Assigned(Chunk) then
      begin
        ColorChunk := FindChunk(Chunk, COLOR_F);
        if Assigned(ColorChunk) then
        begin
          Source.ReadChunkData(ColorChunk);
          Solid.R := ColorChunk.Data.ColorF.Red;
          Solid.G := ColorChunk.Data.ColorF.Green;
          Solid.B := ColorChunk.Data.ColorF.Blue;
          FreeChunkData(ColorChunk);
        end;

        ColorChunk := FindChunk(Chunk, LIN_COLOR_F);
        if Assigned(ColorChunk) then
        begin
          Source.ReadChunkData(ColorChunk);
          Solid.R := ColorChunk.Data.ColorF.Red;
          Solid.G := ColorChunk.Data.ColorF.Green;
          Solid.B := ColorChunk.Data.ColorF.Blue;
          FreeChunkData(ColorChunk);
        end;
      end;

      Chunk := FindChunk(MDataChunk, V_GRADIENT);
      if Assigned(Chunk) then
      begin
        // the COLOR_F chunks are the old, non-gamma corrected colors
        Source.ReadChunkData(Chunk);
        VGradient.GradPercent := Chunk.Data.VGradient^;
        TopColor := FindChunk(Chunk, COLOR_F);
        if Assigned(TopColor) then
        begin
          Source.ReadChunkData(TopColor);
          VGradient.Top.R := TopColor.Data.ColorF.Red;
          VGradient.Top.G := TopColor.Data.ColorF.Green;
          VGradient.Top.B := TopColor.Data.ColorF.Blue;
          MidColor := FindNextChunk(TopColor.Sibling, COLOR_F);
          if Assigned(MidColor) then
          begin
            Source.ReadChunkData(MidColor);
            VGradient.Mid.R := MidColor.Data.ColorF.Red;
            VGradient.Mid.G := MidColor.Data.ColorF.Green;
            VGradient.Mid.B := MidColor.Data.ColorF.Blue;
            BotColor := FindNextChunk(MidColor.Sibling, COLOR_F);
            if Assigned(BotColor) then
            begin
              Source.ReadChunkData(BotColor);
              VGradient.Bottom.R := MidColor.Data.ColorF.Red;
              VGradient.Bottom.G := MidColor.Data.ColorF.Green;
              VGradient.Bottom.B := MidColor.Data.ColorF.Blue;
              FreeChunkData(BotColor);
            end;
            FreeChunkData(MidColor);
          end;
          FreeChunkData(TopColor);
        end;

         // If the newer, gamma correct colors are available, then use them instead
        TopColor := FindChunk(Chunk, LIN_COLOR_F);
        if Assigned(TopColor) then
        begin
          Source.ReadChunkData(TopColor);
          VGradient.Top.R := TopColor.Data.ColorF.Red;
          VGradient.Top.G := TopColor.Data.ColorF.Green;
          VGradient.Top.B := TopColor.Data.ColorF.Blue;
          MidColor := FindNextChunk(TopColor.Sibling, LIN_COLOR_F);
          if Assigned(MidColor) then
          begin
            Source.ReadChunkData(MidColor);
            VGradient.Mid.R := MidColor.Data.ColorF.Red;
            VGradient.Mid.G := MidColor.Data.ColorF.Green;
            VGradient.Mid.B := MidColor.Data.ColorF.Blue;
            BotColor := FindNextChunk(MidColor.Sibling, LIN_COLOR_F);
            if Assigned(BotColor) then
            begin
              Source.ReadChunkData(BotColor);
              VGradient.Bottom.R := MidColor.Data.ColorF.Red;
              VGradient.Bottom.G := MidColor.Data.ColorF.Green;
              VGradient.Bottom.B := MidColor.Data.ColorF.Blue;
              FreeChunkData(BotColor);
            end;
            FreeChunkData(MidColor);
          end;
          FreeChunkData(TopColor);
        end;
        FreeChunkData(Chunk);
      end;

      // Search for use_bitmap, use_solid_bgnd and use_v_gradient chunks
      Chunk := FindChunk(MDataChunk, USE_BIT_MAP);
      if Assigned(Chunk) then
        BgndUsed := btUseBitmapBgnd
      else
      begin
        Chunk := FindChunk(MDataChunk, USE_SOLID_BGND);
        if Assigned(Chunk) then
          BgndUsed := btUseSolidBgnd
        else
        begin
          Chunk := FindChunk(MDataChunk, USE_V_GRADIENT);
          if Assigned(Chunk) then
            BgndUsed := btUseVGradientBgnd
          else
            BgndUsed := btNoBgnd;
        end;
      end;
    end;
end;

//---------------------------------------------------------------------------------------------------------------------

function InitViewport: TViewport3DS;

begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    AType := vtTopView3DS;
    Ortho.Zoom := 0.7395;
    User.Zoom := 0.7395;
    User.HorAng := 20;
    User.VerAng := 30;
    Camera := '';
    Size.Width := 1000;
    Size.Height := 1000;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetViewportEntry(Source: TFile3DS; Section: PChunk3DS): TViewport3DS;

var
  Chunk,
    VLayout: PChunk3DS;
  PortIndex: Integer;
  foundV3: Boolean;

begin
  Result := InitViewport;
  VLayout := FindNextChunk(Section.Children, VIEWPORT_LAYOUT);

  if Assigned(VLayout) then
    with Result do
    begin
      Source.ReadChunkData(VLayout);

      Chunk := VLayout.Children;
      foundV3 := False;
      PortIndex := 0;
      while Assigned(Chunk) do
      begin
        case Chunk.Tag of
          VIEWPORT_SIZE:
            begin
              Source.ReadChunkData(Chunk);
              Size.XPos := Chunk.Data.ViewportSize.XPos;
              Size.YPos := Chunk.Data.ViewportSize.YPos;
              Size.Width := Chunk.Data.ViewportSize.Width;
              Size.Height := Chunk.Data.ViewportSize.Height;
              FreeChunkData(Chunk);
            end;
          VIEWPORT_DATA_3:
            begin
              foundV3 := True;
              if PortIndex = VLayout.Data.ViewportLayout.Top then
              begin
                Source.ReadChunkData(Chunk);
                case Chunk.Data.ViewportData.View of
                  1:
                    AType := vtTopView3DS;
                  2:
                    AType := vtBottomView3DS;
                  3:
                    AType := vtLeftView3DS;
                  4:
                    AType := vtRightView3DS;
                  5:
                    AType := vtFrontView3DS;
                  6:
                    AType := vtBackView3DS;
                  7:
                    AType := vtUserView3DS;
                  18:
                    AType := vtSpotlightView3DS;
                  $FFFF:
                    AType := vtCameraView3DS;
                else
                  AType := vtNoView3DS;
                end;

                Ortho.Zoom := Chunk.Data.ViewportData.ZoomFactor;
                User.Zoom := Chunk.Data.ViewportData.ZoomFactor;
                Ortho.Center.X := Chunk.Data.ViewportData.Center.X;
                User.Center.X := Chunk.Data.ViewportData.Center.X;
                Ortho.Center.Y := Chunk.Data.ViewportData.Center.Y;
                User.Center.y := Chunk.Data.ViewportData.Center.Y;
                Ortho.Center.Z := Chunk.Data.ViewportData.Center.Z;
                User.Center.z := Chunk.Data.ViewportData.Center.Z;
                User.HorAng := Chunk.Data.ViewportData.HorizAng;
                User.VerAng := Chunk.Data.ViewportData.VertAng;
                Camera := Chunk.Data.ViewportData.CamName;
              end;
              Inc(PortIndex);
            end;
          VIEWPORT_DATA:
            if not foundV3 then
            begin
              if PortIndex = VLayout.Data.ViewportLayout.Top then
              begin
                Source.ReadChunkData(Chunk);
                case Chunk.Data.ViewportData.View of
                  1:
                    AType := vtTopView3DS;
                  2:
                    AType := vtBottomView3DS;
                  3:
                    AType := vtLeftView3DS;
                  4:
                    AType := vtRightView3DS;
                  5:
                    AType := vtFrontView3DS;
                  6:
                    AType := vtBackView3DS;
                  7:
                    AType := vtUserView3DS;
                  18:
                    AType := vtSpotlightView3DS;
                  $FFFF:
                    AType := vtCameraView3DS;
                else
                  AType := vtNoView3DS;
                end;

                Ortho.Zoom := Chunk.Data.ViewportData.ZoomFactor;
                User.Zoom := Chunk.Data.ViewportData.ZoomFactor;
                Ortho.Center.X := Chunk.Data.ViewportData.Center.X;
                User.Center.X := Chunk.Data.ViewportData.Center.X;
                Ortho.Center.Y := Chunk.Data.ViewportData.Center.Y;
                User.Center.y := Chunk.Data.ViewportData.Center.Y;
                Ortho.Center.Z := Chunk.Data.ViewportData.Center.Z;
                User.Center.z := Chunk.Data.ViewportData.Center.Z;
                User.HorAng := Chunk.Data.ViewportData.HorizAng;
                User.VerAng := Chunk.Data.ViewportData.VertAng;
                Camera := Chunk.Data.ViewportData.CamName;
              end;
              Inc(PortIndex);
            end;
        end;
        Chunk := Chunk.Sibling;
      end;
    end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetViewport(const Source: TFile3DS; var DB: TDatabase3DS): TViewport3DS;

var
  Data: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);
  if (DB.TopChunk.Tag = M3DMAGIC) or (DB.TopChunk.Tag = CMAGIC) then
  begin
    Data := FindNextChunk(DB.TopChunk.Children, KFDATA);
    if Assigned(Data) then
      Result := GetViewportEntry(Source, Data)
    else
    begin
      Data := FindChunk(DB.TopChunk.Children, MDATA);
      if Assigned(Data) then
        Result := GetViewportEntry(Source, Data);
    end;
  end;
end;

//----------------- helper funcs for text output ----------------------------------------------------------------------

function ChunkTagToString(Tag: Word): string;

begin
  case Tag of
    NULL_CHUNK: Result := 'NULL_CHUNK';
    ChunkType: Result := 'ChunkType';
    ChunkUnique: Result := 'ChunkUnique';
    NotChunk: Result := 'NotChunk';
    Container: Result := 'Container';
    IsChunk: Result := 'IsChunk';

    // Dummy Chunk that sometimes appears in 3DS files created by prerelease 3D Studio R2
    DUMMY: Result := 'DUMMY';

    // Trick Chunk Types
    POINT_ARRAY_ENTRY: Result := 'POINT_ARRAY_ENTRY';
    POINT_FLAG_ARRAY_ENTRY: Result := 'POINT_FLAG_ARRAY_ENTRY';
    FACE_ARRAY_ENTRY: Result := 'FACE_ARRAY_ENTRY';
    MSH_MAT_GROUP_ENTRY: Result := 'MSH_MAT_GROUP_ENTRY';
    TEX_VERTS_ENTRY: Result := 'TEX_VERTS_ENTRY';
    SMOOTH_GROUP_ENTRY: Result := 'SMOOTH_GROUP_ENTRY';
    POS_TRACK_TAG_KEY: Result := 'POS_TRACK_TAG_KEY';
    ROT_TRACK_TAG_KEY: Result := 'ROT_TRACK_TAG_KEY';
    SCL_TRACK_TAG_KEY: Result := 'SCL_TRACK_TAG_KEY';
    FOV_TRACK_TAG_KEY: Result := 'FOV_TRACK_TAG_KEY';
    ROLL_TRACK_TAG_KEY: Result := 'ROLL_TRACK_TAG_KEY';
    COL_TRACK_TAG_KEY: Result := 'COL_TRACK_TAG_KEY';
    MORPH_TRACK_TAG_KEY: Result := 'MORPH_TRACK_TAG_KEY';
    HOT_TRACK_TAG_KEY: Result := 'HOT_TRACK_TAG_KEY';
    FALL_TRACK_TAG_KEY: Result := 'FALL_TRACK_TAG_KEY';

    // 3DS File Chunk IDs
    M3DMAGIC: Result := 'M3DMAGIC';
    SMAGIC: Result := 'SMAGIC';
    LMAGIC: Result := 'LMAGIC';
    MLIBMAGIC: Result := 'MLIBMAGIC';
    MATMAGIC: Result := 'MATMAGIC';
    M3D_VERSION: Result := 'M3D_VERSION';
    M3D_KFVERSION: Result := 'M3D_KFVERSION';

    // Mesh Chunk Ids
    MDATA: Result := 'MDATA';
    MESH_VERSION: Result := 'MESH_VERSION';
    COLOR_F: Result := 'COLOR_F';
    COLOR_24: Result := 'COLOR_24';
    LIN_COLOR_24: Result := 'LIN_COLOR_24';
    LIN_COLOR_F: Result := 'LIN_COLOR_F';
    INT_PERCENTAGE: Result := 'INT_PERCENTAGE';
    FLOAT_PERCENTAGE: Result := 'FLOAT_PERCENTAGE';

    MASTER_SCALE: Result := 'MASTER_SCALE';

    BIT_MAP: Result := 'BIT_MAP';
    USE_BIT_MAP: Result := 'USE_BIT_MAP';
    SOLID_BGND: Result := 'SOLID_BGND';
    USE_SOLID_BGND: Result := 'USE_SOLID_BGND';
    V_GRADIENT: Result := 'V_GRADIENT';
    USE_V_GRADIENT: Result := 'USE_V_GRADIENT';

    LO_SHADOW_BIAS: Result := 'LO_SHADOW_BIAS';
    HI_SHADOW_BIAS: Result := 'HI_SHADOW_BIAS';
    SHADOW_MAP_SIZE: Result := 'SHADOW_MAP_SIZE';
    SHADOW_SAMPLES: Result := 'SHADOW_SAMPLES';
    SHADOW_RANGE: Result := 'SHADOW_RANGE';
    SHADOW_FILTER: Result := 'SHADOW_FILTER';
    RAY_BIAS: Result := 'RAY_BIAS';

    O_CONSTS: Result := 'O_CONSTS';

    AMBIENT_LIGHT: Result := 'AMBIENT_LIGHT';

    FOG: Result := 'FOG';
    USE_FOG: Result := 'USE_FOG';
    FOG_BGND: Result := 'FOG_BGND';
    DISTANCE_CUE: Result := 'DISTANCE_CUE';
    USE_DISTANCE_CUE: Result := 'USE_DISTANCE_CUE';
    LAYER_FOG: Result := 'LAYER_FOG';
    USE_LAYER_FOG: Result := 'USE_LAYER_FOG';
    DCUE_BGND: Result := 'DCUE_BGND';

    DEFAULT_VIEW: Result := 'DEFAULT_VIEW';
    VIEW_TOP: Result := 'VIEW_TOP';
    VIEW_BOTTOM: Result := 'VIEW_BOTTOM';
    VIEW_LEFT: Result := 'VIEW_LEFT';
    VIEW_RIGHT: Result := 'VIEW_RIGHT';
    VIEW_FRONT: Result := 'VIEW_FRONT';
    VIEW_BACK: Result := 'VIEW_BACK';
    VIEW_USER: Result := 'VIEW_USER';
    VIEW_CAMERA: Result := 'VIEW_CAMERA';
    VIEW_WINDOW: Result := 'VIEW_WINDOW';

    NAMED_OBJECT: Result := 'NAMED_OBJECT';
    OBJ_HIDDEN: Result := 'OBJ_HIDDEN';
    OBJ_VIS_LOFTER: Result := 'OBJ_VIS_LOFTER';
    OBJ_DOESNT_CAST: Result := 'OBJ_DOESNT_CAST';
    OBJ_MATTE: Result := 'OBJ_MATTE';
    OBJ_FAST: Result := 'OBJ_FAST';
    OBJ_PROCEDURAL: Result := 'OBJ_PROCEDURAL';
    OBJ_FROZEN: Result := 'OBJ_FROZEN';
    OBJ_DONT_RCVSHADOW: Result := 'OBJ_DONT_RCVSHADOW';

    N_TRI_OBJECT: Result := 'N_TRI_OBJECT';

    POINT_ARRAY: Result := 'POINT_ARRAY';
    POINT_FLAG_ARRAY: Result := 'POINT_FLAG_ARRAY';
    FACE_ARRAY: Result := 'FACE_ARRAY';
    MSH_MAT_GROUP: Result := 'MSH_MAT_GROUP';
    OLD_MAT_GROUP: Result := 'OLD_MAT_GROUP';
    TEX_VERTS: Result := 'TEX_VERTS';
    SMOOTH_GROUP: Result := 'SMOOTH_GROUP';
    MESH_MATRIX: Result := 'MESH_MATRIX';
    MESH_COLOR: Result := 'MESH_COLOR';
    MESH_TEXTURE_INFO: Result := 'MESH_TEXTURE_INFO';
    PROC_NAME: Result := 'PROC_NAME';
    PROC_DATA: Result := 'PROC_DATA';
    MSH_BOXMAP: Result := 'MSH_BOXMAP';

    N_D_L_OLD: Result := 'N_D_L_OLD';

    N_CAM_OLD: Result := 'N_CAM_OLD';

    N_DIRECT_LIGHT: Result := 'N_DIRECT_LIGHT';
    DL_SPOTLIGHT: Result := 'DL_SPOTLIGHT';
    DL_OFF: Result := 'DL_OFF';
    DL_ATTENUATE: Result := 'DL_ATTENUATE';
    DL_RAYSHAD: Result := 'DL_RAYSHAD';
    DL_SHADOWED: Result := 'DL_SHADOWED';
    DL_LOCAL_SHADOW: Result := 'DL_LOCAL_SHADOW';
    DL_LOCAL_SHADOW2: Result := 'DL_LOCAL_SHADOW2';
    DL_SEE_CONE: Result := 'DL_SEE_CONE';
    DL_SPOT_RECTANGULAR: Result := 'DL_SPOT_RECTANGULAR';
    DL_SPOT_OVERSHOOT: Result := 'DL_SPOT_OVERSHOOT';
    DL_SPOT_PROJECTOR: Result := 'DL_SPOT_PROJECTOR';
    DL_EXCLUDE: Result := 'DL_EXCLUDE';
    DL_RANGE: Result := 'DL_RANGE';
    DL_SPOT_ROLL: Result := 'DL_SPOT_ROLL';
    DL_SPOT_ASPECT: Result := 'DL_SPOT_ASPECT';
    DL_RAY_BIAS: Result := 'DL_RAY_BIAS';
    DL_INNER_RANGE: Result := 'DL_INNER_RANGE';
    DL_OUTER_RANGE: Result := 'DL_OUTER_RANGE';
    DL_MULTIPLIER: Result := 'DL_MULTIPLIER';

    N_AMBIENT_LIGHT: Result := 'N_AMBIENT_LIGHT';

    N_CAMERA: Result := 'N_CAMERA';
    CAM_SEE_CONE: Result := 'CAM_SEE_CONE';
    CAM_RANGES: Result := 'CAM_RANGES';

    HIERARCHY: Result := 'HIERARCHY';
    PARENT_OBJECT: Result := 'PARENT_OBJECT';
    PIVOT_OBJECT: Result := 'PIVOT_OBJECT';
    PIVOT_LIMITS: Result := 'PIVOT_LIMITS';
    PIVOT_ORDER: Result := 'PIVOT_ORDER';
    XLATE_RANGE: Result := 'XLATE_RANGE';

    POLY_2D: Result := 'POLY_2D';

    // Flags in shaper file that tell whether polys make up an ok shape
    SHAPE_OK: Result := 'SHAPE_OK';
    SHAPE_NOT_OK: Result := 'SHAPE_NOT_OK';

    SHAPE_HOOK: Result := 'SHAPE_HOOK';

    PATH_3D: Result := 'PATH_3D';
    PATH_MATRIX: Result := 'PATH_MATRIX';
    SHAPE_2D: Result := 'SHAPE_2D';
    M_SCALE: Result := 'M_SCALE';
    M_TWIST: Result := 'M_TWIST';
    M_TEETER: Result := 'M_TEETER';
    M_FIT: Result := 'M_FIT';
    M_BEVEL: Result := 'M_BEVEL';
    XZ_CURVE: Result := 'XZ_CURVE';
    YZ_CURVE: Result := 'YZ_CURVE';
    INTERPCT: Result := 'INTERPCT';
    DEFORM_LIMIT: Result := 'DEFORM_LIMIT';

    // Flags for Modeler options
    USE_CONTOUR: Result := 'USE_CONTOUR';
    USE_TWEEN: Result := 'USE_TWEEN';
    USE_SCALE: Result := 'USE_SCALE';
    USE_TWIST: Result := 'USE_TWIST';
    USE_TEETER: Result := 'USE_TEETER';
    USE_FIT: Result := 'USE_FIT';
    USE_BEVEL: Result := 'USE_BEVEL';

    // Viewport description chunks
    VIEWPORT_LAYOUT_OLD: Result := 'VIEWPORT_LAYOUT_OLD';
    VIEWPORT_DATA_OLD: Result := 'VIEWPORT_DATA_OLD';
    VIEWPORT_LAYOUT: Result := 'VIEWPORT_LAYOUT';
    VIEWPORT_DATA: Result := 'VIEWPORT_DATA';
    VIEWPORT_DATA_3: Result := 'VIEWPORT_DATA_3';
    VIEWPORT_SIZE: Result := 'VIEWPORT_SIZE';
    NETWORK_VIEW: Result := 'NETWORK_VIEW';

    // External Application Data
    XDATA_SECTION: Result := 'XDATA_SECTION';
    XDATA_ENTRY: Result := 'XDATA_ENTRY';
    XDATA_APPNAME: Result := 'XDATA_APPNAME';
    XDATA_STRING: Result := 'XDATA_STRING';
    XDATA_FLOAT: Result := 'XDATA_FLOAT';
    XDATA_DOUBLE: Result := 'XDATA_DOUBLE';
    XDATA_SHORT: Result := 'XDATA_SHORT';
    XDATA_LONG: Result := 'XDATA_LONG';
    XDATA_VOID: Result := 'XDATA_procedure';
    XDATA_GROUP: Result := 'XDATA_GROUP';
    XDATA_RFU6: Result := 'XDATA_RFU6';
    XDATA_RFU5: Result := 'XDATA_RFU5';
    XDATA_RFU4: Result := 'XDATA_RFU4';
    XDATA_RFU3: Result := 'XDATA_RFU3';
    XDATA_RFU2: Result := 'XDATA_RFU2';
    XDATA_RFU1: Result := 'XDATA_RFU1';

    // Material Chunk IDs
    MAT_ENTRY: Result := 'MAT_ENTRY';
    MAT_NAME: Result := 'MAT_NAME';
    MAT_AMBIENT: Result := 'MAT_AMBIENT';
    MAT_DIFFUSE: Result := 'MAT_DIFFUSE';
    MAT_SPECULAR: Result := 'MAT_SPECULAR';
    MAT_SHININESS: Result := 'MAT_SHININESS';
    MAT_SHIN2PCT: Result := 'MAT_SHIN2PCT';
    MAT_SHIN3PCT: Result := 'MAT_SHIN3PCT';
    MAT_TRANSPARENCY: Result := 'MAT_TRANSPARENCY';
    MAT_XPFALL: Result := 'MAT_XPFALL';
    MAT_REFBLUR: Result := 'MAT_REFBLUR';

    MAT_SELF_ILLUM: Result := 'MAT_SELF_ILLUM';
    MAT_TWO_SIDE: Result := 'MAT_TWO_SIDE';
    MAT_DECAL: Result := 'MAT_DECAL';
    MAT_ADDITIVE: Result := 'MAT_ADDITIVE';
    MAT_SELF_ILPCT: Result := 'MAT_SELF_ILPCT';
    MAT_WIRE: Result := 'MAT_WIRE';
    MAT_SUPERSMP: Result := 'MAT_SUPERSMP';
    MAT_WIRESIZE: Result := 'MAT_WIRESIZE';
    MAT_FACEMAP: Result := 'MAT_FACEMAP';
    MAT_XPFALLIN: Result := 'MAT_XPFALLIN';
    MAT_PHONGSOFT: Result := 'MAT_PHONGSOFT';
    MAT_WIREABS: Result := 'MAT_WIREABS';

    MAT_SHADING: Result := 'MAT_SHADING';

    MAT_TEXMAP: Result := 'MAT_TEXMAP';
    MAT_OPACMAP: Result := 'MAT_OPACMAP';
    MAT_REFLMAP: Result := 'MAT_REFLMAP';
    MAT_BUMPMAP: Result := 'MAT_BUMPMAP';
    MAT_SPECMAP: Result := 'MAT_SPECMAP';
    MAT_USE_XPFALL: Result := 'MAT_USE_XPFALL';
    MAT_USE_REFBLUR: Result := 'MAT_USE_REFBLUR';
    MAT_BUMP_PERCENT: Result := 'MAT_BUMP_PERCENT';

    MAT_MAPNAME: Result := 'MAT_MAPNAME';
    MAT_ACUBIC: Result := 'MAT_ACUBIC';

    MAT_SXP_TEXT_DATA: Result := 'MAT_SXP_TEXT_DATA';
    MAT_SXP_TEXT2_DATA: Result := 'MAT_SXP_TEXT2_DATA';
    MAT_SXP_OPAC_DATA: Result := 'MAT_SXP_OPAC_DATA';
    MAT_SXP_BUMP_DATA: Result := 'MAT_SXP_BUMP_DATA';
    MAT_SXP_SPEC_DATA: Result := 'MAT_SXP_SPEC_DATA';
    MAT_SXP_SHIN_DATA: Result := 'MAT_SXP_SHIN_DATA';
    MAT_SXP_SELFI_DATA: Result := 'MAT_SXP_SELFI_DATA';
    MAT_SXP_TEXT_MASKDATA: Result := 'MAT_SXP_TEXT_MASKDATA';
    MAT_SXP_TEXT2_MASKDATA: Result := 'MAT_SXP_TEXT2_MASKDATA';
    MAT_SXP_OPAC_MASKDATA: Result := 'MAT_SXP_OPAC_MASKDATA';
    MAT_SXP_BUMP_MASKDATA: Result := 'MAT_SXP_BUMP_MASKDATA';
    MAT_SXP_SPEC_MASKDATA: Result := 'MAT_SXP_SPEC_MASKDATA';
    MAT_SXP_SHIN_MASKDATA: Result := 'MAT_SXP_SHIN_MASKDATA';
    MAT_SXP_SELFI_MASKDATA: Result := 'MAT_SXP_SELFI_MASKDATA';
    MAT_SXP_REFL_MASKDATA: Result := 'MAT_SXP_REFL_MASKDATA';
    MAT_TEX2MAP: Result := 'MAT_TEX2MAP';
    MAT_SHINMAP: Result := 'MAT_SHINMAP';
    MAT_SELFIMAP: Result := 'MAT_SELFIMAP';
    MAT_TEXMASK: Result := 'MAT_TEXMASK';
    MAT_TEX2MASK: Result := 'MAT_TEX2MASK';
    MAT_OPACMASK: Result := 'MAT_OPACMASK';
    MAT_BUMPMASK: Result := 'MAT_BUMPMASK';
    MAT_SHINMASK: Result := 'MAT_SHINMASK';
    MAT_SPECMASK: Result := 'MAT_SPECMASK';
    MAT_SELFIMASK: Result := 'MAT_SELFIMASK';
    MAT_REFLMASK: Result := 'MAT_REFLMASK';
    MAT_MAP_TILINGOLD: Result := 'MAT_MAP_TILINGOLD';
    MAT_MAP_TILING: Result := 'MAT_MAP_TILING';
    MAT_MAP_TEXBLUR_OLD: Result := 'MAT_MAP_TEXBLUR_OLD';
    MAT_MAP_TEXBLUR: Result := 'MAT_MAP_TEXBLUR';
    MAT_MAP_USCALE: Result := 'MAT_MAP_USCALE';
    MAT_MAP_VSCALE: Result := 'MAT_MAP_VSCALE';
    MAT_MAP_UOFFSET: Result := 'MAT_MAP_UOFFSET';
    MAT_MAP_VOFFSET: Result := 'MAT_MAP_VOFFSET';
    MAT_MAP_ANG: Result := 'MAT_MAP_ANG';
    MAT_MAP_COL1: Result := 'MAT_MAP_COL1';
    MAT_MAP_COL2: Result := 'MAT_MAP_COL2';
    MAT_MAP_RCOL: Result := 'MAT_MAP_RCOL';
    MAT_MAP_GCOL: Result := 'MAT_MAP_GCOL';
    MAT_MAP_BCOL: Result := 'MAT_MAP_BCOL';

    // Keyframe Chunk IDs
    KFDATA: Result := 'KFDATA';
    KFHDR: Result := 'KFHDR';
    AMBIENT_NODE_TAG: Result := 'AMBIENT_NODE_TAG';
    OBJECT_NODE_TAG: Result := 'OBJECT_NODE_TAG';
    CAMERA_NODE_TAG: Result := 'CAMERA_NODE_TAG';
    TARGET_NODE_TAG: Result := 'TARGET_NODE_TAG';
    LIGHT_NODE_TAG: Result := 'LIGHT_NODE_TAG';
    L_TARGET_NODE_TAG: Result := 'L_TARGET_NODE_TAG';
    SPOTLIGHT_NODE_TAG: Result := 'SPOTLIGHT_NODE_TAG';

    KFSEG: Result := 'KFSEG';
    KFCURTIME: Result := 'KFCURTIME';
    NODE_HDR: Result := 'NODE_HDR';
    PARENT_NAME: Result := 'PARENT_NAME';
    INSTANCE_NAME: Result := 'INSTANCE_NAME';
    PRESCALE: Result := 'PRESCALE';
    PIVOT: Result := 'PIVOT';
    BOUNDBOX: Result := 'BOUNDBOX';
    MORPH_SMOOTH: Result := 'MORPH_SMOOTH';
    POS_TRACK_TAG: Result := 'POS_TRACK_TAG';
    ROT_TRACK_TAG: Result := 'ROT_TRACK_TAG';
    SCL_TRACK_TAG: Result := 'SCL_TRACK_TAG';
    FOV_TRACK_TAG: Result := 'FOV_TRACK_TAG';
    ROLL_TRACK_TAG: Result := 'ROLL_TRACK_TAG';
    COL_TRACK_TAG: Result := 'COL_TRACK_TAG';
    MORPH_TRACK_TAG: Result := 'MORPH_TRACK_TAG';
    HOT_TRACK_TAG: Result := 'HOT_TRACK_TAG';
    FALL_TRACK_TAG: Result := 'FALL_TRACK_TAG';
    HIDE_TRACK_TAG: Result := 'HIDE_TRACK_TAG';
    NODE_ID: Result := 'NODE_ID';

    CMAGIC: Result := 'CMAGIC';

    C_MDRAWER: Result := 'C_MDRAWER';
    C_TDRAWER: Result := 'C_TDRAWER';
    C_SHPDRAWER: Result := 'C_SHPDRAWER';
    C_MODDRAWER: Result := 'C_MODDRAWER';
    C_RIPDRAWER: Result := 'C_RIPDRAWER';
    C_TXDRAWER: Result := 'C_TXDRAWER';
    C_PDRAWER: Result := 'C_PDRAWER';
    C_MTLDRAWER: Result := 'C_MTLDRAWER';
    C_FLIDRAWER: Result := 'C_FLIDRAWER';
    C_CUBDRAWER: Result := 'C_CUBDRAWER';
    C_MFILE: Result := 'C_MFILE';
    C_SHPFILE: Result := 'C_SHPFILE';
    C_MODFILE: Result := 'C_MODFILE';
    C_RIPFILE: Result := 'C_RIPFILE';
    C_TXFILE: Result := 'C_TXFILE';
    C_PFILE: Result := 'C_PFILE';
    C_MTLFILE: Result := 'C_MTLFILE';
    C_FLIFILE: Result := 'C_FLIFILE';
    C_PALFILE: Result := 'C_PALFILE';
    C_TX_STRING: Result := 'C_TX_STRING';
    C_CONSTS: Result := 'C_CONSTS';
    C_SNAPS: Result := 'C_SNAPS';
    C_GRIDS: Result := 'C_GRIDS';
    C_ASNAPS: Result := 'C_ASNAPS';
    C_GRID_RANGE: Result := 'C_GRID_RANGE';
    C_RENDTYPE: Result := 'C_RENDTYPE';
    C_PROGMODE: Result := 'C_PROGMODE';
    C_PREVMODE: Result := 'C_PREVMODE';
    C_MODWMODE: Result := 'C_MODWMODE';
    C_MODMODEL: Result := 'C_MODMODEL';
    C_ALL_LINES: Result := 'C_ALL_LINES';
    C_BACK_TYPE: Result := 'C_BACK_TYPE';
    C_MD_CS: Result := 'C_MD_CS';
    C_MD_CE: Result := 'C_MD_CE';
    C_MD_SML: Result := 'C_MD_SML';
    C_MD_SMW: Result := 'C_MD_SMW';
    C_LOFT_WITH_TEXTURE: Result := 'C_LOFT_WITH_TEXTURE';
    C_LOFT_L_REPEAT: Result := 'C_LOFT_L_REPEAT';
    C_LOFT_W_REPEAT: Result := 'C_LOFT_W_REPEAT';
    C_LOFT_UV_NORMALIZE: Result := 'C_LOFT_UV_NORMALIZE';
    C_WELD_LOFT: Result := 'C_WELD_LOFT';
    C_MD_PDET: Result := 'C_MD_PDET';
    C_MD_SDET: Result := 'C_MD_SDET';
    C_RGB_RMODE: Result := 'C_RGB_RMODE';
    C_RGB_HIDE: Result := 'C_RGB_HIDE';
    C_RGB_MAPSW: Result := 'C_RGB_MAPSW';
    C_RGB_TWOSIDE: Result := 'C_RGB_TWOSIDE';
    C_RGB_SHADOW: Result := 'C_RGB_SHADOW';
    C_RGB_AA: Result := 'C_RGB_AA';
    C_RGB_OVW: Result := 'C_RGB_OVW';
    C_RGB_OVH: Result := 'C_RGB_OVH';
    C_RGB_PICTYPE: Result := 'C_RGB_PICTYPE';
    C_RGB_OUTPUT: Result := 'C_RGB_OUTPUT';
    C_RGB_TODISK: Result := 'C_RGB_TODISK';
    C_RGB_COMPRESS: Result := 'C_RGB_COMPRESS';
    C_JPEG_COMPRESSION: Result := 'C_JPEG_COMPRESSION';
    C_RGB_DISPDEV: Result := 'C_RGB_DISPDEV';
    C_RGB_HARDDEV: Result := 'C_RGB_HARDDEV';
    C_RGB_PATH: Result := 'C_RGB_PATH';
    C_BITMAP_DRAWER: Result := 'C_BITMAP_DRAWER';
    C_RGB_FILE: Result := 'C_RGB_FILE';
    C_RGB_OVASPECT: Result := 'C_RGB_OVASPECT';

    C_RGB_ANIMTYPE: Result := 'C_RGB_ANIMTYPE';
    C_RENDER_ALL: Result := 'C_RENDER_ALL';
    C_REND_FROM: Result := 'C_REND_FROM';
    C_REND_TO: Result := 'C_REND_TO';
    C_REND_NTH: Result := 'C_REND_NTH';
    C_REND_TSTEP: Result := 'C_REND_TSTEP';
    C_VP_TSTEP: Result := 'C_VP_TSTEP';

    C_PAL_TYPE: Result := 'C_PAL_TYPE';
    C_RND_TURBO: Result := 'C_RND_TURBO';
    C_RND_MIP: Result := 'C_RND_MIP';
    C_BGND_METHOD: Result := 'C_BGND_METHOD';
    C_AUTO_REFLECT: Result := 'C_AUTO_REFLECT';
    C_VP_FROM: Result := 'C_VP_FROM';
    C_VP_TO: Result := 'C_VP_TO';
    C_VP_NTH: Result := 'C_VP_NTH';

    C_SRDIAM: Result := 'C_SRDIAM';
    C_SRDEG: Result := 'C_SRDEG';
    C_SRSEG: Result := 'C_SRSEG';
    C_SRDIR: Result := 'C_SRDIR';
    C_HETOP: Result := 'C_HETOP';
    C_HEBOT: Result := 'C_HEBOT';
    C_HEHT: Result := 'C_HEHT';
    C_HETURNS: Result := 'C_HETURNS';
    C_HEDEG: Result := 'C_HEDEG';
    C_HESEG: Result := 'C_HESEG';
    C_HEDIR: Result := 'C_HEDIR';
    C_QUIKSTUFF: Result := 'C_QUIKSTUFF';
    C_SEE_LIGHTS: Result := 'C_SEE_LIGHTS';
    C_SEE_CAMERAS: Result := 'C_SEE_CAMERAS';
    C_SEE_3D: Result := 'C_SEE_3D';
    C_MESHSEL: Result := 'C_MESHSEL';
    C_MESHUNSEL: Result := 'C_MESHUNSEL';
    C_POLYSEL: Result := 'C_POLYSEL';
    C_POLYUNSEL: Result := 'C_POLYUNSEL';
    C_SHPLOCAL: Result := 'C_SHPLOCAL';
    C_MSHLOCAL: Result := 'C_MSHLOCAL';
    C_NUM_FORMAT: Result := 'C_NUM_FORMAT';
    C_ARCH_DENOM: Result := 'C_ARCH_DENOM';
    C_IN_DEVICE: Result := 'C_IN_DEVICE';
    C_MSCALE: Result := 'C_MSCALE';
    C_COMM_PORT: Result := 'C_COMM_PORT';
    C_TAB_BASES: Result := 'C_TAB_BASES';
    C_TAB_DIVS: Result := 'C_TAB_DIVS';
    C_MASTER_SCALES: Result := 'C_MASTER_SCALES';
    C_SHOW_1STVERT: Result := 'C_SHOW_1STVERT';
    C_SHAPER_OK: Result := 'C_SHAPER_OK';
    C_LOFTER_OK: Result := 'C_LOFTER_OK';
    C_EDITOR_OK: Result := 'C_EDITOR_OK';
    C_KEYFRAMER_OK: Result := 'C_KEYFRAMER_OK';
    C_PICKSIZE: Result := 'C_PICKSIZE';
    C_MAPTYPE: Result := 'C_MAPTYPE';
    C_MAP_DISPLAY: Result := 'C_MAP_DISPLAY';
    C_TILE_XY: Result := 'C_TILE_XY';
    C_MAP_XYZ: Result := 'C_MAP_XYZ';
    C_MAP_SCALE: Result := 'C_MAP_SCALE';
    C_MAP_MATRIX_OLD: Result := 'C_MAP_MATRIX_OLD';
    C_MAP_MATRIX: Result := 'C_MAP_MATRIX';
    C_MAP_WID_HT: Result := 'C_MAP_WID_HT';
    C_OBNAME: Result := 'C_OBNAME';
    C_CAMNAME: Result := 'C_CAMNAME';
    C_LTNAME: Result := 'C_LTNAME';
    C_CUR_MNAME: Result := 'C_CUR_MNAME';
    C_CURMTL_FROM_MESH: Result := 'C_CURMTL_FROM_MESH';
    C_GET_SHAPE_MAKE_FACES: Result := 'C_GET_SHAPE_MAKE_FACES';
    C_DETAIL: Result := 'C_DETAIL';
    C_VERTMARK: Result := 'C_VERTMARK';
    C_MSHAX: Result := 'C_MSHAX';
    C_MSHCP: Result := 'C_MSHCP';
    C_USERAX: Result := 'C_USERAX';
    C_SHOOK: Result := 'C_SHOOK';
    C_RAX: Result := 'C_RAX';
    C_STAPE: Result := 'C_STAPE';
    C_LTAPE: Result := 'C_LTAPE';
    C_ETAPE: Result := 'C_ETAPE';
    C_KTAPE: Result := 'C_KTAPE';
    C_SPHSEGS: Result := 'C_SPHSEGS';
    C_GEOSMOOTH: Result := 'C_GEOSMOOTH';
    C_HEMISEGS: Result := 'C_HEMISEGS';
    C_PRISMSEGS: Result := 'C_PRISMSEGS';
    C_PRISMSIDES: Result := 'C_PRISMSIDES';
    C_TUBESEGS: Result := 'C_TUBESEGS';
    C_TUBESIDES: Result := 'C_TUBESIDES';
    C_TORSEGS: Result := 'C_TORSEGS';
    C_TORSIDES: Result := 'C_TORSIDES';
    C_CONESIDES: Result := 'C_CONESIDES';
    C_CONESEGS: Result := 'C_CONESEGS';
    C_NGPARMS: Result := 'C_NGPARMS';
    C_PTHLEVEL: Result := 'C_PTHLEVEL';
    C_MSCSYM: Result := 'C_MSCSYM';
    C_MFTSYM: Result := 'C_MFTSYM';
    C_MTTSYM: Result := 'C_MTTSYM';
    C_SMOOTHING: Result := 'C_SMOOTHING';
    C_MODICOUNT: Result := 'C_MODICOUNT';
    C_FONTSEL: Result := 'C_FONTSEL';
    C_TESS_TYPE: Result := 'C_TESS_TYPE';
    C_TESS_TENSION: Result := 'C_TESS_TENSION';

    C_SEG_START: Result := 'C_SEG_START';
    C_SEG_END: Result := 'C_SEG_END';
    C_CURTIME: Result := 'C_CURTIME';
    C_ANIMLENGTH: Result := 'C_ANIMLENGTH';
    C_PV_FROM: Result := 'C_PV_FROM';
    C_PV_TO: Result := 'C_PV_TO';
    C_PV_DOFNUM: Result := 'C_PV_DOFNUM';
    C_PV_RNG: Result := 'C_PV_RNG';
    C_PV_NTH: Result := 'C_PV_NTH';
    C_PV_TYPE: Result := 'C_PV_TYPE';
    C_PV_METHOD: Result := 'C_PV_METHOD';
    C_PV_FPS: Result := 'C_PV_FPS';
    C_VTR_FRAMES: Result := 'C_VTR_FRAMES';
    C_VTR_HDTL: Result := 'C_VTR_HDTL';
    C_VTR_HD: Result := 'C_VTR_HD';
    C_VTR_TL: Result := 'C_VTR_TL';
    C_VTR_IN: Result := 'C_VTR_IN';
    C_VTR_PK: Result := 'C_VTR_PK';
    C_VTR_SH: Result := 'C_VTR_SH';

    // Material chunks
    C_WORK_MTLS: Result := 'C_WORK_MTLS';
    C_WORK_MTLS_2: Result := 'C_WORK_MTLS_2';
    C_WORK_MTLS_3: Result := 'C_WORK_MTLS_3';
    C_WORK_MTLS_4: Result := 'C_WORK_MTLS_4';
    C_WORK_MTLS_5: Result := 'C_WORK_MTLS_5';
    C_WORK_MTLS_6: Result := 'C_WORK_MTLS_6';
    C_WORK_MTLS_7: Result := 'C_WORK_MTLS_7';
    C_WORK_MTLS_8: Result := 'C_WORK_MTLS_8';
    C_WORKMTL: Result := 'C_WORKMTL';
    C_SXP_TEXT_DATA: Result := 'C_SXP_TEXT_DATA';
    C_SXP_TEXT2_DATA: Result := 'C_SXP_TEXT2_DATA';
    C_SXP_OPAC_DATA: Result := 'C_SXP_OPAC_DATA';
    C_SXP_BUMP_DATA: Result := 'C_SXP_BUMP_DATA';
    C_SXP_SPEC_DATA: Result := 'C_SXP_SPEC_DATA';
    C_SXP_SHIN_DATA: Result := 'C_SXP_SHIN_DATA';
    C_SXP_SELFI_DATA: Result := 'C_SXP_SELFI_DATA';
    C_SXP_TEXT_MASKDATA: Result := 'C_SXP_TEXT_MASKDATA';
    C_SXP_TEXT2_MASKDATA: Result := 'C_SXP_TEXT2_MASKDATA';
    C_SXP_OPAC_MASKDATA: Result := 'C_SXP_OPAC_MASKDATA';
    C_SXP_BUMP_MASKDATA: Result := 'C_SXP_BUMP_MASKDATA';
    C_SXP_SPEC_MASKDATA: Result := 'C_SXP_SPEC_MASKDATA';
    C_SXP_SHIN_MASKDATA: Result := 'C_SXP_SHIN_MASKDATA';
    C_SXP_SELFI_MASKDATA: Result := 'C_SXP_SELFI_MASKDATA';
    C_SXP_REFL_MASKDATA: Result := 'C_SXP_REFL_MASKDATA';

    C_BGTYPE: Result := 'C_BGTYPE';
    C_MEDTILE: Result := 'C_MEDTILE';

    // Contrast
    C_LO_CONTRAST: Result := 'C_LO_CONTRAST';
    C_HI_CONTRAST: Result := 'C_HI_CONTRAST';

    // 3D frozen display
    C_FROZ_DISPLAY: Result := 'C_FROZ_DISPLAY';

    // Booleans
    C_BOOLWELD: Result := 'C_BOOLWELD';
    C_BOOLTYPE: Result := 'C_BOOLTYPE';

    C_ANG_THRESH: Result := 'C_ANG_THRESH';
    C_SS_THRESH: Result := 'C_SS_THRESH';
    C_TEXTURE_BLUR_DEFAULT: Result := 'C_TEXTURE_BLUR_DEFAULT';

    C_MAPDRAWER: Result := 'C_MAPDRAWER';
    C_MAPDRAWER1: Result := 'C_MAPDRAWER1';
    C_MAPDRAWER2: Result := 'C_MAPDRAWER2';
    C_MAPDRAWER3: Result := 'C_MAPDRAWER3';
    C_MAPDRAWER4: Result := 'C_MAPDRAWER4';
    C_MAPDRAWER5: Result := 'C_MAPDRAWER5';
    C_MAPDRAWER6: Result := 'C_MAPDRAWER6';
    C_MAPDRAWER7: Result := 'C_MAPDRAWER7';
    C_MAPDRAWER8: Result := 'C_MAPDRAWER8';
    C_MAPDRAWER9: Result := 'C_MAPDRAWER9';
    C_MAPDRAWER_ENTRY: Result := 'C_MAPDRAWER_ENTRY';

    // system options
    C_BACKUP_FILE: Result := 'C_BACKUP_FILE';
    C_DITHER_256: Result := 'C_DITHER_256';
    C_SAVE_LAST: Result := 'C_SAVE_LAST';
    C_USE_ALPHA: Result := 'C_USE_ALPHA';
    C_TGA_DEPTH: Result := 'C_TGA_DEPTH';
    C_REND_FIELDS: Result := 'C_REND_FIELDS';
    C_REFLIP: Result := 'C_REFLIP';
    C_SEL_ITEMTOG: Result := 'C_SEL_ITEMTOG';
    C_SEL_RESET: Result := 'C_SEL_RESET';
    C_STICKY_KEYINF: Result := 'C_STICKY_KEYINF';
    C_WELD_THRESHOLD: Result := 'C_WELD_THRESHOLD';
    C_ZCLIP_POINT: Result := 'C_ZCLIP_POINT';
    C_ALPHA_SPLIT: Result := 'C_ALPHA_SPLIT';
    C_KF_SHOW_BACKFACE: Result := 'C_KF_SHOW_BACKFACE';
    C_OPTIMIZE_LOFT: Result := 'C_OPTIMIZE_LOFT';
    C_TENS_DEFAULT: Result := 'C_TENS_DEFAULT';
    C_CONT_DEFAULT: Result := 'C_CONT_DEFAULT';
    C_BIAS_DEFAULT: Result := 'C_BIAS_DEFAULT';

    C_DXFNAME_SRC: Result := 'C_DXFNAME_SRC ';
    C_AUTO_WELD: Result := 'C_AUTO_WELD ';
    C_AUTO_UNIFY: Result := 'C_AUTO_UNIFY ';
    C_AUTO_SMOOTH: Result := 'C_AUTO_SMOOTH ';
    C_DXF_SMOOTH_ANG: Result := 'C_DXF_SMOOTH_ANG ';
    C_SMOOTH_ANG: Result := 'C_SMOOTH_ANG ';

    // Special network-use chunks
    C_NET_USE_VPOST: Result := 'C_NET_USE_VPOST';
    C_NET_USE_GAMMA: Result := 'C_NET_USE_GAMMA';
    C_NET_FIELD_ORDER: Result := 'C_NET_FIELD_ORDER';

    C_BLUR_FRAMES: Result := 'C_BLUR_FRAMES';
    C_BLUR_SAMPLES: Result := 'C_BLUR_SAMPLES';
    C_BLUR_DUR: Result := 'C_BLUR_DUR';
    C_HOT_METHOD: Result := 'C_HOT_METHOD';
    C_HOT_CHECK: Result := 'C_HOT_CHECK';
    C_PIXEL_SIZE: Result := 'C_PIXEL_SIZE';
    C_DISP_GAMMA: Result := 'C_DISP_GAMMA';
    C_FBUF_GAMMA: Result := 'C_FBUF_GAMMA';
    C_FILE_OUT_GAMMA: Result := 'C_FILE_OUT_GAMMA';
    C_FILE_IN_GAMMA: Result := 'C_FILE_IN_GAMMA';
    C_GAMMA_CORRECT: Result := 'C_GAMMA_CORRECT';
    C_APPLY_DISP_GAMMA: Result := 'C_APPLY_DISP_GAMMA';
    C_APPLY_FBUF_GAMMA: Result := 'C_APPLY_FBUF_GAMMA';
    C_APPLY_FILE_GAMMA: Result := 'C_APPLY_FILE_GAMMA';
    C_FORCE_WIRE: Result := 'C_FORCE_WIRE';
    C_RAY_SHADOWS: Result := 'C_RAY_SHADOWS';
    C_MASTER_AMBIENT: Result := 'C_MASTER_AMBIENT';
    C_SUPER_SAMPLE: Result := 'C_SUPER_SAMPLE';
    C_OBJECT_MBLUR: Result := 'C_OBJECT_MBLUR';
    C_MBLUR_DITHER: Result := 'C_MBLUR_DITHER';
    C_DITHER_24: Result := 'C_DITHER_24';
    C_SUPER_BLACK: Result := 'C_SUPER_BLACK';
    C_SAFE_FRAME: Result := 'C_SAFE_FRAME';
    C_VIEW_PRES_RATIO: Result := 'C_VIEW_PRES_RATIO';
    C_BGND_PRES_RATIO: Result := 'C_BGND_PRES_RATIO';
    C_NTH_SERIAL_NUM: Result := 'C_NTH_SERIAL_NUM';

    VPDATA: Result := 'VPDATA';

    P_QUEUE_ENTRY: Result := 'P_QUEUE_ENTRY';
    P_QUEUE_IMAGE: Result := 'P_QUEUE_IMAGE';
    P_QUEUE_USEIGAMMA: Result := 'P_QUEUE_USEIGAMMA';
    P_QUEUE_PROC: Result := 'P_QUEUE_PROC';
    P_QUEUE_SOLID: Result := 'P_QUEUE_SOLID';
    P_QUEUE_GRADIENT: Result := 'P_QUEUE_GRADIENT';
    P_QUEUE_KF: Result := 'P_QUEUE_KF';
    P_QUEUE_MOTBLUR: Result := 'P_QUEUE_MOTBLUR';
    P_QUEUE_MB_REPEAT: Result := 'P_QUEUE_MB_REPEAT';
    P_QUEUE_NONE: Result := 'P_QUEUE_NONE';

    P_QUEUE_RESIZE: Result := 'P_QUEUE_RESIZE';
    P_QUEUE_OFFSET: Result := 'P_QUEUE_OFFSET';
    P_QUEUE_ALIGN: Result := 'P_QUEUE_ALIGN';

    P_CUSTOM_SIZE: Result := 'P_CUSTOM_SIZE';

    P_ALPH_NONE: Result := 'P_ALPH_NONE';
    P_ALPH_PSEUDO: Result := 'P_ALPH_PSEUDO';
    P_ALPH_OP_PSEUDO: Result := 'P_ALPH_OP_PSEUDO';
    P_ALPH_BLUR: Result := 'P_ALPH_BLUR';
    P_ALPH_PCOL: Result := 'P_ALPH_PCOL';
    P_ALPH_C0: Result := 'P_ALPH_C0';
    P_ALPH_OP_KEY: Result := 'P_ALPH_OP_KEY';
    P_ALPH_KCOL: Result := 'P_ALPH_KCOL';
    P_ALPH_OP_NOCONV: Result := 'P_ALPH_OP_NOCONV';
    P_ALPH_IMAGE: Result := 'P_ALPH_IMAGE';
    P_ALPH_ALPHA: Result := 'P_ALPH_ALPHA';
    P_ALPH_QUES: Result := 'P_ALPH_QUES';
    P_ALPH_QUEIMG: Result := 'P_ALPH_QUEIMG';
    P_ALPH_CUTOFF: Result := 'P_ALPH_CUTOFF';
    P_ALPHANEG: Result := 'P_ALPHANEG';

    P_TRAN_NONE: Result := 'P_TRAN_NONE';
    P_TRAN_IMAGE: Result := 'P_TRAN_IMAGE';
    P_TRAN_FRAMES: Result := 'P_TRAN_FRAMES';
    P_TRAN_FADEIN: Result := 'P_TRAN_FADEIN';
    P_TRAN_FADEOUT: Result := 'P_TRAN_FADEOUT';
    P_TRANNEG: Result := 'P_TRANNEG';

    P_RANGES: Result := 'P_RANGES';

    P_PROC_DATA: Result := 'P_PROC_DATA'
  else
    Result := 'UNKNOWN_CHUNK';
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

const
  IndentString: PChar = #9#9#9#9#9#9#9#9#9#9#9#9;

function Indent(Level: Integer): PChar;

begin
  Result := IndentString;
  Inc(Result, Length(IndentString) - Level);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ChunkHeaderReport(var Strings: TStrings; Chunk: PChunk3DS; IndentLevel: Integer);

var
  OutString: string;

begin
  OutString := Format('%sChunk %s ($%x), Length is %d ($%3:x)', [Indent(IndentLevel),
    ChunkTagToString(Chunk.Tag), Chunk.Tag, Chunk.Size]);
  Strings.Add(OutString);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure DumpKeyHeader(Strings: TStrings; Key: TKeyHeader3DS; IndentLevel: Integer);

var
  Output: string;

begin
  Output := Format('%sFrame %d', [Indent(IndentLevel), Key.Time]);
  if (Key.rflags and KeyUsesTension3DS) <> 0 then
    Output := Output + Format(', Tens %.2f', [Key.Tension]);
  if (Key.rflags and KeyUsesCont3DS) <> 0 then
    Output := Output + Format(', Cont %.2f', [Key.Continuity]);
  if (Key.rflags and KeyUsesBias3DS) <> 0 then
    Output := Output + Format(', Bias %.2f', [Key.Bias]);
  if (Key.rflags and KeyUsesEaseTo3DS) <> 0 then
    Output := Output + Format(', Ease to %.2f', [Key.EaseTo]);
  if (Key.rflags and KeyUsesEaseFrom3DS) <> 0 then
    Output := Output + Format(', Ease from %.2f', [Key.EaseFrom]);
  Strings.Add(Output);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure DumpChunk(const Source: TFile3DS; var Strings: TStrings; Chunk: PChunk3DS; IndentLevel: Integer; DumpLevel:
  TDumpLevel);

// retrieves the Data for a Chunk from the given Source, formats the Data into
// one or more lines of text and puts the lines into the given Strings parameter

var
  Child: PChunk3DS;
  Output: string;
  ID: string;
  I: Integer;

begin
  ChunkHeaderReport(Strings, Chunk, IndentLevel);
  ID := Indent(IndentLevel) + #9;

  if DumpLevel <> dlTerseDump then
  begin
    case Chunk.Tag of
      MESH_VERSION:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sVersion %d', [ID, Chunk.Data.MeshVersion^]);
          Strings.Add(Output);
        end;
      M3D_VERSION:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sVersion %d', [ID, Chunk.Data.M3DVersion^]);
          Strings.Add(Output);
        end;
      COLOR_F:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R:  %f, ', [ID, Chunk.Data.ColorF.Red]);
          Output := Output + Format(' G:  %f, ', [Chunk.Data.ColorF.Green]);
          Output := Output + Format(' B:  %f', [Chunk.Data.ColorF.Blue]);
          Strings.Add(Output);
        end;
      LIN_COLOR_F:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R:  %f, ', [ID, Chunk.Data.LinColorF.Red]);
          Output := Output + Format(' G:  %f, ', [Chunk.Data.LinColorF.Green]);
          Output := Output + Format(' B:  %f', [Chunk.Data.LinColorF.Blue]);
          Strings.Add(Output);
        end;
      COLOR_24:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R: %d, ', [ID, Chunk.Data.Color24.Red]);
          Output := Output + Format(' G: %d, ', [Chunk.Data.Color24.Green]);
          Output := Output + Format(' B: %d', [Chunk.Data.Color24.Blue]);
          Strings.Add(Output);
        end;
      LIN_COLOR_24:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R: %d, ', [ID, Chunk.Data.LinColor24.Red]);
          Output := Output + Format(' G: %d, ', [Chunk.Data.LinColor24.Green]);
          Output := Output + Format(' B: %d', [Chunk.Data.LinColor24.Blue]);
          Strings.Add(Output);
        end;
      INT_PERCENTAGE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sPercentage of %d%%', [ID, Chunk.Data.IntPercentage^]);
          Strings.Add(Output);
        end;
      FLOAT_PERCENTAGE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sPercentage of  %f%%', [ID, Chunk.Data.FloatPercentage^]);
          Strings.Add(Output);
        end;
      MASTER_SCALE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMaster Scale  %f', [ID, Chunk.Data.MasterScale^]);
          Strings.Add(Output);
        end;
      BIT_MAP:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sBitmap Name %s', [ID, Chunk.Data.BitMapName^]);
          Strings.Add(Output);
        end;
      V_GRADIENT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMidpoint  %f', [ID, Chunk.Data.VGradient^]);
          Strings.Add(Output);
        end;
      LO_SHADOW_BIAS:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sBias of  %f', [ID, Chunk.Data.LoShadowBias^]);
          Strings.Add(Output);
        end;
      HI_SHADOW_BIAS:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sBias of  %f', [ID, Chunk.Data.HiShadowBias^]);
          Strings.Add(Output);
        end;
      RAY_BIAS:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sBias of  %f', [ID, Chunk.Data.RayBias^]);
          Strings.Add(Output);
        end;
      SHADOW_MAP_SIZE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sSize of %d', [ID, Chunk.Data.ShadowMapSize^]);
          Strings.Add(Output);
        end;
      SHADOW_SAMPLES:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sSize of %d', [ID, Chunk.Data.ShadowSamples^]);
          Strings.Add(Output);
        end;
      SHADOW_RANGE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sRange of %d', [ID, Chunk.Data.ShadowRange^]);
          Strings.Add(Output);
        end;
      SHADOW_FILTER:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sFilter of  %f', [ID, Chunk.Data.ShadowFilter^]);
          Strings.Add(Output);
        end;
      O_CONSTS:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sPlane at  %f,  %f,  %f', [ID, Chunk.Data.OConsts.X, Chunk.Data.OConsts.Y,
            Chunk.Data.OConsts.Z]);
          Strings.Add(Output);
        end;
      FOG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sNear plane at  %f', [ID, Chunk.Data.Fog.NearPlaneDist]);
          Strings.Add(Output);
          Output := Format('%sNear Density of  %f', [ID, Chunk.Data.Fog.NearPlaneDensity]);
          Strings.Add(Output);
          Output := Format('%sFar plane at  %f', [ID, Chunk.Data.Fog.FarPlaneDist]);
          Strings.Add(Output);
          Output := Format('%sFar Density of  %f', [ID, Chunk.Data.Fog.FarPlaneDensity]);
          Strings.Add(Output);
        end;
      LAYER_FOG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sFog Z range is  %f to  %f', [ID, Chunk.Data.LayerFog.ZMin, Chunk.Data.LayerFog.ZMax]);
          Strings.Add(Output);
          Output := Format('%sFog Density is  %f', [ID, Chunk.Data.LayerFog.Density]);
          Strings.Add(Output);
          Output := Format('%sFog type of $%x', [ID, Chunk.Data.LayerFog.AType]);
          Strings.Add(Output);
        end;
      DISTANCE_CUE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sNear plane at  %f', [ID, Chunk.Data.DistanceCue.NearPlaneDist]);
          Strings.Add(Output);
          Output := Format('%sNear Density of  %f', [ID, Chunk.Data.DistanceCue.NearPlaneDimming]);
          Strings.Add(Output);
          Output := Format('%sFar plane at  %f', [ID, Chunk.Data.DistanceCue.FarPlaneDist]);
          Strings.Add(Output);
          Output := Format('%sFar Density of  %f', [ID, Chunk.Data.DistanceCue.FarPlaneDimming]);
          Strings.Add(Output);
        end;
      VIEW_TOP,
        VIEW_BOTTOM,
        VIEW_LEFT,
        VIEW_RIGHT,
        VIEW_FRONT,
        VIEW_BACK:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sTarget at  %f,  %f,  %f', [ID, Chunk.Data.ViewStandard.ViewTargetCoord.X,
            Chunk.Data.ViewStandard.ViewTargetCoord.Y,
              Chunk.Data.ViewStandard.ViewTargetCoord.Z]);
          Strings.Add(Output);
          Output := Format('%sView Width of  %f', [ID, Chunk.Data.ViewStandard.ViewWidth]);
          Strings.Add(Output);
        end;
      VIEW_USER:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sTarget at  %f,  %f,  %f', [ID, Chunk.Data.ViewUser.ViewTargetCoord.X,
            Chunk.Data.ViewUser.ViewTargetCoord.Y,
              Chunk.Data.ViewUser.ViewTargetCoord.Z]);
          Strings.Add(Output);
          Output := Format('%sView Width of  %f', [ID, Chunk.Data.ViewUser.ViewWidth]);
          Strings.Add(Output);
          Output := Format('%sHorizontal View angle of  %f', [ID, Chunk.Data.ViewUser.XYViewAngle]);
          Strings.Add(Output);
          Output := Format('%sVertical View angle of  %f', [ID, Chunk.Data.ViewUser.YZViewAngle]);
          Strings.Add(Output);
          Output := Format('%sBank angle of  %f', [ID, Chunk.Data.ViewUser.BankAngle]);
          Strings.Add(Output);
        end;
      VIEW_CAMERA:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sCamera Name %s', [ID, Chunk.Data.ViewCamera^]);
          Strings.Add(Output);
        end;
      NAMED_OBJECT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sName: %s', [ID, Chunk.Data.NamedObject^]);
          Strings.Add(Output);
        end;
      POINT_ARRAY:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Vertices', [ID, Chunk.Data.PointArray.Vertices]);
          Strings.Add(Output);
          if DumpLevel = dlMaximumDump then
            for I := 0 to Chunk.Data.PointArray.Vertices - 1 do
            begin
              Output := Format('%sVertex %d at  %f,  %f,  %f', [ID, I, Chunk.Data.PointArray.PointList[I].X,
                Chunk.Data.PointArray.PointList[I].Y,
                  Chunk.Data.PointArray.PointList[I].Z]);
              Strings.Add(Output);
            end;
        end;
      POINT_FLAG_ARRAY:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sFlags: %d', [ID, Chunk.Data.PointFlagArray.Flags]);
          Strings.Add(Output);
          if DumpLevel = dlMaximumDump then
            for I := 0 to Chunk.Data.PointFlagArray.Flags - 1 do
            begin
              Output := Format('%sFlag %d is %d', [ID, I, Chunk.Data.PointFlagArray.FlagList[I]]);
              Strings.Add(Output);
            end;
        end;
      FACE_ARRAY:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Faces', [ID, Chunk.Data.FaceArray.Faces]);
          Strings.Add(Output);
          if DumpLevel = dlMaximumDump then
            for I := 0 to Chunk.Data.FaceArray.Faces - 1 do
            begin
              Output := Format('%sFace %d Vertices %d, %d, %d and flag $%x', [ID, I,
                Chunk.Data.FaceArray.FaceList[I].V1,
                Chunk.Data.FaceArray.FaceList[I].V2,
                  Chunk.Data.FaceArray.FaceList[I].V3,
                  Chunk.Data.FaceArray.FaceList[I].Flag]);
              Strings.Add(Output);
            end;
        end;
      MSH_MAT_GROUP:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMaterial Name of %s', [ID, Chunk.Data.MshMatGroup.MatName]);
          Strings.Add(Output);
          Output := Format('%sAssigned to %d Faces', [ID, Chunk.Data.MshMatGroup.Faces]);
          Strings.Add(Output);
        end;
      MSH_BOXMAP:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sBoxmap consists of the following materials:', [ID]);
          Strings.Add(Output);
          for I := 0 to 5 do
          begin
            Output := Format('%s%s', [ID, Chunk.Data.MshBoxmap^[I]]);
            Strings.Add(Output);
          end;
        end;
      TEX_VERTS:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Vertices', [ID, Chunk.Data.TexVerts.NumCoords]);
          Strings.Add(Output);
          if DumpLevel = dlMaximumDump then
          begin
            for I := 0 to Chunk.Data.TexVerts.NumCoords - 1 do
            begin
              Output := Format('%sVertex %d with tex vert of  %f,  %f', [ID, I, Chunk.Data.TexVerts.TextVertList[I].U,
                Chunk.Data.TexVerts.TextVertList[I].V]);
              Strings.Add(Output);
            end;
          end;
        end;
      MESH_TEXTURE_INFO:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap Type of %d', [ID, Chunk.Data.MeshTextureInfo.MapType]);
          Strings.Add(Output);
          Output := Format('%sX Tiling of  %f', [ID, Chunk.Data.MeshTextureInfo.XTiling]);
          Strings.Add(Output);
          Output := Format('%sY Tiling of  %f', [ID, Chunk.Data.MeshTextureInfo.YTiling]);
          Strings.Add(Output);
          Output := Format('%sIcon position of  %f,  %f,  %f', [ID, Chunk.Data.MeshTextureInfo.IconPos.X,
            Chunk.Data.MeshTextureInfo.IconPos.Y,
              Chunk.Data.MeshTextureInfo.IconPos.Z]);
          Strings.Add(Output);
          I := 0;
          while I < 12 do
          begin
            Output := Format('%s[%d]  %f [%d]  %f [%d]  %f', [ID, I, Chunk.Data.MeshTextureInfo.XMatrix[I],
              I + 1, Chunk.Data.MeshTextureInfo.XMatrix[I + 1],
                I + 2, Chunk.Data.MeshTextureInfo.XMatrix[I + 2]]);
            Strings.Add(Output);
            Inc(I, 3);
          end;
          Output := Format('%sScaling Value of  %f', [ID, Chunk.Data.MeshTextureInfo.IconScaling]);
          Strings.Add(Output);
          Output := Format('%sPlanar Icon Width of  %f', [ID, Chunk.Data.MeshTextureInfo.IconWidth]);
          Strings.Add(Output);
          Output := Format('%sPlanar Icon Height of  %f', [ID, Chunk.Data.MeshTextureInfo.IconHeight]);
          Strings.Add(Output);
          Output := Format('%sCylinder Icon Height of  %f', [ID, Chunk.Data.MeshTextureInfo.CylIconHeight]);
          Strings.Add(Output);
        end;
      MESH_MATRIX:
        begin
          Source.ReadChunkData(Chunk);
          I := 0;
          while I < 12 do
          begin
            Output := Format('%s[%d]  %f [%d]  %f [%d]  %f', [ID, I, Chunk.Data.MeshMatrix[I],
              I + 1, Chunk.Data.MeshMatrix[I + 1],
                I + 2, Chunk.Data.MeshMatrix[I + 2]]);
            Strings.Add(Output);
            Inc(I, 3);
          end;
        end;
      PROC_NAME:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sProcedure Name of %s', [ID, Chunk.Data.ProcName^]);
          Strings.Add(Output);
        end;
      MESH_COLOR:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor index of %d', [ID, Chunk.Data.MeshColor^]);
          Strings.Add(Output);
        end;
      N_DIRECT_LIGHT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sLight at  %f,  %f,  %f', [ID, Chunk.Data.NDirectLight.X,
            Chunk.Data.NDirectLight.Y,
              Chunk.Data.NDirectLight.Z]);
          Strings.Add(Output);
        end;
      DL_EXCLUDE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sExclude %s', [ID, Chunk.Data.DLExclude^]);
          Strings.Add(Output);
        end;
      DL_OUTER_RANGE,
        DL_INNER_RANGE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sRange of  %f', [ID, Chunk.Data.DlOuterRange^]);
          Strings.Add(Output);
        end;
      DL_MULTIPLIER:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMultiple of  %f', [ID, Chunk.Data.DlMultiplier^]);
          Strings.Add(Output);
        end;
      DL_SPOT_ROLL:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sRoll angle of  %f', [ID, Chunk.Data.DlSpotRoll^]);
          Strings.Add(Output);
        end;
      DL_SPOT_ASPECT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sSpot aspect of  %f', [ID, Chunk.Data.DlSpotAspect^]);
          Strings.Add(Output);
        end;
      DL_SPOT_PROJECTOR:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sFilename of projector is %s', [ID, Chunk.Data.DlSpotProjector^]);
          Strings.Add(Output);
        end;
      DL_RAY_BIAS:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sBias of  %f', [ID, Chunk.Data.DlRayBias^]);
          Strings.Add(Output);
        end;
      DL_SPOTLIGHT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sTarget at  %f, %f, %f', [ID, Chunk.Data.DlSpotlight.SpotlightTarg.X,
            Chunk.Data.DlSpotlight.SpotlightTarg.Y,
              Chunk.Data.DlSpotlight.SpotlightTarg.Z]);
          Strings.Add(Output);
          Output := Format('%sHotspot cone of  %f, ', [ID, Chunk.Data.DlSpotlight.HotspotAngle]);
          Output := Output + Format(' Falloff cone of  %f', [Chunk.Data.DlSpotlight.FalloffAngle]);
          Strings.Add(Output);
        end;
      DL_LOCAL_SHADOW2:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sShadow bias of  %f', [ID, Chunk.Data.DlLocalShadow2.LocalShadowBias]);
          Strings.Add(Output);
          Output := Format('%sShadow filter of  %f', [ID, Chunk.Data.DlLocalShadow2.LocalShadowFilter]);
          Strings.Add(Output);
          Output := Format('%sShadow Map Size of  %f', [ID, Chunk.Data.DlLocalShadow2.LocalShadowMapSize]);
          Strings.Add(Output);
        end;
      N_CAMERA:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sCamera at  %f,  %f,  %f', [ID, Chunk.Data.NCamera.CameraPos.X,
            Chunk.Data.NCamera.CameraPos.Y,
              Chunk.Data.NCamera.CameraPos.Z]);
          Strings.Add(Output);
          Output := Format('%sTarget at  %f,  %f,  %f', [ID, Chunk.Data.NCamera.TargetPos.X,
            Chunk.Data.NCamera.TargetPos.Y,
              Chunk.Data.NCamera.TargetPos.Z]);
          Strings.Add(Output);
          Output := Format('%sBank angle of  %f', [ID, Chunk.Data.NCamera.CameraBank]);
          Output := Output + Format(' and a foc of  %f', [Chunk.Data.NCamera.CameraFocalLength]);
          Strings.Add(Output);
        end;
      CAM_RANGES:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sCamera near range is  %f and far range is  %f', [ID, Chunk.Data.CamRanges.NearPlane,
            Chunk.Data.CamRanges.FarPlane]);
          Strings.Add(Output);
        end;
      VIEWPORT_LAYOUT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sForm of %d', [ID, Chunk.Data.ViewportLayout.Form]);
          Strings.Add(Output);
          Output := Format('%sTop of %d', [ID, Chunk.Data.ViewportLayout.Top]);
          Strings.Add(Output);
          Output := Format('%sReady of %d', [ID, Chunk.Data.ViewportLayout.Ready]);
          Strings.Add(Output);
          Output := Format('%sWState of %d', [ID, Chunk.Data.ViewportLayout.WState]);
          Strings.Add(Output);
          Output := Format('%sSwap WS of %d', [ID, Chunk.Data.ViewportLayout.SwapWS]);
          Strings.Add(Output);
          Output := Format('%sSwap Port of %d', [ID, Chunk.Data.ViewportLayout.SwapPort]);
          Strings.Add(Output);
          Output := Format('%sSwap Cur of %d', [ID, Chunk.Data.ViewportLayout.SwapCur]);
          Strings.Add(Output);
        end;
      VIEWPORT_SIZE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sWork Area X: %d Y: %d W: %d H: %d', [ID, Chunk.Data.ViewportSize.XPos,
            Chunk.Data.ViewportSize.YPos,
              Chunk.Data.ViewportSize.Width,
              Chunk.Data.ViewportSize.Height]);
          Strings.Add(Output);
        end;
      VIEWPORT_DATA_3,
        VIEWPORT_DATA:
        begin
          Source.ReadChunkData(Chunk);
          with Chunk.Data.ViewportData^ do
          begin
            Output := Format('%sFlags: $%x', [ID, Flags]);
            Strings.Add(Output);
            Output := Format('%sAxis Lockouts of $%x', [ID, AxisLockout]);
            Strings.Add(Output);
            Output := Format('%sWindow Position of %d, %d', [ID, WinXPos, WinYPos]);
            Strings.Add(Output);
            Output := Format('%sWindow Size of %d, %d', [ID, WinWidth, WinHeight]);
            Strings.Add(Output);
            Output := Format('%sWindow View of %d', [ID, View]);
            Strings.Add(Output);
            Output := Format('%sZoom Factor of  %f', [ID, ZoomFactor]);
            Strings.Add(Output);
            Output := Format('%sWorld Center of  %f, %f, %f', [ID, Center.X, Center.Y, Center.Z]);
            Strings.Add(Output);
            Output := Format('%sHorizontal Angle of  %f', [ID, HorizAng]);
            Strings.Add(Output);
            Output := Format('%sVertical Angle of  %f', [ID, VertAng]);
            Strings.Add(Output);
            Output := Format('%sCamera Name of %s', [ID, CamName]);
            Strings.Add(Output);
          end;
        end;
      XDATA_APPNAME:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sApplication Name %s', [ID, Chunk.Data.XDataAppName^]);
          Strings.Add(Output);
        end;
      XDATA_STRING:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sString value of %s', [ID, Chunk.Data.XDataString^]);
          Strings.Add(Output);
        end;
      MAT_NAME:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMaterial Name %s', [ID, Chunk.Data.MatName^]);
          Strings.Add(Output);
        end;
      MAT_SHADING:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sShading value of %d', [ID, Chunk.Data.MatShading^]);
          Strings.Add(Output);
        end;
      MAT_ACUBIC:
        begin
          Source.ReadChunkData(Chunk);
          with Chunk.Data.MatAcubic^ do
          begin
            Output := Format('%sShade level of %d', [ID, ShadeLevel]);
            Strings.Add(Output);
            Output := Format('%sAntialias level of %d', [ID, AntiAlias]);
            Strings.Add(Output);
            Output := Format('%sFlags: %d', [ID, Flags]);
            Strings.Add(Output);
            Output := Format('%sMap Size of %d', [ID, MapSize]);
            Strings.Add(Output);
            Output := Format('%sFrame skip of %d', [ID, FrameInterval]);
            Strings.Add(Output);
          end;
        end;
      MAT_MAPNAME:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap Name %s', [ID, Chunk.Data.MatMapname^]);
          Strings.Add(Output);
        end;
      MAT_WIRESIZE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sWire frame Size of  %f', [ID, Chunk.Data.MatWireSize^]);
          Strings.Add(Output);
        end;
      MAT_MAP_TILING:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap Flags: ', [ID]);
          if (Chunk.Data.MatMapTiling^ = 0) then
            Output := Output + ' NONE'
          else
          begin
            if (Chunk.Data.MatMapTiling^ and TEX_DECAL) <> 0 then
              Output := Output + ' TEX_DECAL, ';
            if (Chunk.Data.MatMapTiling^ and TEX_MIRROR) <> 0 then
              Output := Output + ' TEX_MIRROR, ';
            if (Chunk.Data.MatMapTiling^ and TEX_UNUSED1) <> 0 then
              Output := Output + ' TEX_UNUSED1, ';
            if (Chunk.Data.MatMapTiling^ and TEX_INVERT) <> 0 then
              Output := Output + ' TEX_INVERT, ';
            if (Chunk.Data.MatMapTiling^ and TEX_NOWRAP) <> 0 then
              Output := Output + ' TEX_NOWRAP, ';
            if (Chunk.Data.MatMapTiling^ and TEX_SAT) <> 0 then
              Output := Output + ' TEX_SAT, ';
            if (Chunk.Data.MatMapTiling^ and TEX_ALPHA_SOURCE) <> 0 then
              Output := Output + ' TEX_ALPHA_SOURCE, ';
            if (Chunk.Data.MatMapTiling^ and TEX_TINT) <> 0 then
              Output := Output + ' TEX_TINT, ';
            if (Chunk.Data.MatMapTiling^ and TEX_DONT_USE_ALPHA) <> 0 then
              Output := Output + ' TEX_DONT_USE_ALPHA, ';
            if (Chunk.Data.MatMapTiling^ and TEX_RGB_TINT) <> 0 then
              Output := Output + ' TEX_RGB_TINT, ';
            Delete(Output, Length(Output) - 1, 2); // take the last comma out
          end;
          Strings.Add(Output);
        end;
      MAT_MAP_COL1:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R: %d, ', [ID, Chunk.Data.MatMapCol1.Red]);
          Output := Output + Format(' G: %d, ', [Chunk.Data.MatMapCol1.Green]);
          Output := Output + Format(' B: %d', [Chunk.Data.MatMapCol1.Blue]);
          Strings.Add(Output);
        end;
      MAT_MAP_COL2:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R: %d, ', [ID, Chunk.Data.MatMapCol2.Red]);
          Output := Output + Format(' G: %d, ', [Chunk.Data.MatMapCol2.Green]);
          Output := Output + Format(' B: %d', [Chunk.Data.MatMapCol2.Blue]);
          Strings.Add(Output);
        end;
      MAT_MAP_RCOL:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R: %d, ', [ID, Chunk.Data.MatMapRCol.Red]);
          Output := Output + Format(' G: %d, ', [Chunk.Data.MatMapRCol.Green]);
          Output := Output + Format(' B: %d', [Chunk.Data.MatMapRCol.Blue]);
          Strings.Add(Output);
        end;
      MAT_MAP_GCOL:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R: %d, ', [ID, Chunk.Data.MatMapGCol.Red]);
          Output := Output + Format(' G: %d, ', [Chunk.Data.MatMapGCol.Green]);
          Output := Output + Format(' B: %d', [Chunk.Data.MatMapGCol.Blue]);
          Strings.Add(Output);
        end;
      MAT_MAP_BCOL:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sColor R: %d, ', [ID, Chunk.Data.MatMapBCol.Red]);
          Output := Output + Format(' G: %d, ', [Chunk.Data.MatMapBCol.Green]);
          Output := Output + Format(' B: %d', [Chunk.Data.MatMapBCol.Blue]);
          Strings.Add(Output);
        end;
      MAT_MAP_TEXBLUR:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap bluring of  %f', [ID, Chunk.Data.MatMapTexblur^]);
          Strings.Add(Output);
        end;
      MAT_MAP_USCALE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap U scale of  %f', [ID, Chunk.Data.MatMapUScale^]);
          Strings.Add(Output);
        end;
      MAT_MAP_VSCALE:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap V scale of  %f', [ID, Chunk.Data.MatMapVScale^]);
          Strings.Add(Output);
        end;
      MAT_MAP_UOFFSET:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap U offset of  %f', [ID, Chunk.Data.MatMapUOffset^]);
          Strings.Add(Output);
        end;
      MAT_MAP_VOFFSET:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap V offset of  %f', [ID, Chunk.Data.MatMapVOffset^]);
          Strings.Add(Output);
        end;
      MAT_MAP_ANG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMap rotation angle of  %f', [ID, Chunk.Data.MatMapAng^]);
          Strings.Add(Output);
        end;
      MAT_BUMP_PERCENT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sPercentage of %d%%', [ID, Chunk.Data.MatBumpPercent^]);
          Strings.Add(Output);
        end;
      KFHDR:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sRevision level of $%x', [ID, Chunk.Data.KFHdr.Revision]);
          Strings.Add(Output);
          Output := Format('%sFilename %s', [ID, Chunk.Data.KFHdr.FileName]);
          Strings.Add(Output);
          Output := Format('%sAnimation length of %d', [ID, Chunk.Data.KFHdr.AnimLength]);
          Strings.Add(Output);
        end;
      KFSEG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sSegment starts at %d and ends at %d', [ID, Chunk.Data.KFSeg.First,
            Chunk.Data.KFSeg.Last]);
          Strings.Add(Output);
        end;
      KFCURTIME:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sCurrent frame is %d', [ID, Chunk.Data.KFCurtime^]);
          Strings.Add(Output);
        end;
      NODE_ID:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sNode ID: %d', [ID, Chunk.Data.KFID^]);
          Strings.Add(Output);
        end;
      NODE_HDR:
        begin
          Source.ReadChunkData(Chunk);
          Strings.Add(Format('%sObject Name: %s', [ID, Chunk.Data.NodeHdr.ObjName]));
          //--- Flags 1
          Strings.Add(Format('%sFlags 1: $%x', [ID, Chunk.Data.NodeHdr.Flags1]));
          if DumpLevel = dlMaximumDump then
            with Chunk.Data.NodeHdr^ do
            begin
              if (Flags1 and NODE_RENDOB_HIDE) <> 0 then
                Strings.Add(Format('%sNODE_RENDOB_HIDE', [ID]));
              if (Flags1 and NODE_OFF) <> 0 then
                Strings.Add(Format('%sNODE_OFF', [ID]));
              if (Flags1 and ATKEY1) <> 0 then
                Strings.Add(Format('%sATKEY1', [ID]));
              if (Flags1 and ATKEY2) <> 0 then
                Strings.Add(Format('%sATKEY2', [ID]));
              if (Flags1 and ATKEY3) <> 0 then
                Strings.Add(Format('%sATKEY3', [ID]));
              if (Flags1 and ATKEY4) <> 0 then
                Strings.Add(Format('%sATKEY4', [ID]));
              if (Flags1 and ATKEY5) <> 0 then
                Strings.Add(Format('%sATKEY5', [ID]));
              if (Flags1 and ATKEYFLAGS) <> 0 then
                Strings.Add(Format('%sATKEYFLAGS', [ID]));
              if (Flags1 and MARK_NODE) <> 0 then
                Strings.Add(Format('%sMARK_NODE', [ID]));
              if (Flags1 and DISABLE_NODE) <> 0 then
                Strings.Add(Format('%sDISABLE_NODE', [ID]));
              if (Flags1 and HIDE_NODE) <> 0 then
                Strings.Add(Format('%sHIDE_NODE', [ID]));
              if (Flags1 and FAST_NODE) <> 0 then
                Strings.Add(Format('%sFAST_NODE', [ID]));
              if (Flags1 and PRIMARY_NODE) <> 0 then
                Strings.Add(Format('%sPRIMARY_NODE', [ID]));
              if (Flags1 and NODE_CALC_PATH) <> 0 then
                Strings.Add(Format('%sNODE_CALC_PATH', [ID]));
            end;

          //--- Flags 2
          Strings.Add(Format('%sFlags 2: $%x', [ID, Chunk.Data.NodeHdr.Flags2]));
          if DumpLevel = dlMaximumDump then
            with Chunk.Data.NodeHdr^ do
            begin
              if (Flags2 and NODE_HAS_PATH) <> 0 then
                Strings.Add(Format('%sNODE_HAS_PATH', [ID]));
              if (Flags2 and NODE_AUTO_SMOOTH) <> 0 then
                Strings.Add(Format('%sNODE_AUTO_SMOOTH', [ID]));
              if (Flags2 and NODE_FROZEN) <> 0 then
                Strings.Add(Format('%sNODE_FROZEN', [ID]));
              if (Flags2 and NODE_ANI_HIDDEN) <> 0 then
                Strings.Add(Format('%sNODE_ANI_HIDDEN', [ID]));
              if (Flags2 and NODE_MOTION_BLUR) <> 0 then
                Strings.Add(Format('%sNODE_MOTION_BLUR', [ID]));
              if (Flags2 and NODE_BLUR_BRANCH) <> 0 then
                Strings.Add(Format('%sNODE_BLUR_BRANCH', [ID]));
              if (Flags2 and NODE_MORPH_MTL) <> 0 then
                Strings.Add(Format('%sNODE_MORPH_MTL', [ID]));
              if (Flags2 and NODE_MORPH_OB) <> 0 then
                Strings.Add(Format('%sNODE_MORPH_OB', [ID]));
            end;

          if Chunk.Data.NodeHdr.ParentIndex = -1 then
            Strings.Add(Format('%sNo Parent', [ID]))
          else
            Strings.Add(Format('%sParent %d', [ID, Chunk.Data.NodeHdr.ParentIndex]));
        end;
      INSTANCE_NAME:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sInstance Name: %s', [ID, Chunk.Data.InstanceName^]);
          Strings.Add(Output);
        end;
      PARENT_NAME:
        begin
          Source.ReadChunkData(Chunk);
          if Chunk.Data.InstanceName = nil then
            Strings.Add(Format('%sNo Parent', [ID]))
          else
            Strings.Add(Format('%sParent Name: %s', [ID, Chunk.Data.InstanceName^]));
        end;
      PIVOT:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sPivot at  %f,  %f,  %f', [ID, Chunk.Data.Pivot.X,
            Chunk.Data.Pivot.Y,
              Chunk.Data.Pivot.Z]);
          Strings.Add(Output);
        end;
      BOUNDBOX:
        if Assigned(Chunk.Data.Dummy) then
        begin
          Output := Format('%sMinimum at  %f,  %f,  %f', [ID, Chunk.Data.BoundBox.Min.X,
            Chunk.Data.BoundBox.Min.Y,
              Chunk.Data.BoundBox.Min.Z]);
          Strings.Add(Output);
          Output := Format('%sMaximum at  %f,  %f,  %f', [ID, Chunk.Data.BoundBox.Max.X,
            Chunk.Data.BoundBox.Max.Y,
              Chunk.Data.BoundBox.Max.Z]);
          Strings.Add(Output);
        end;
      MORPH_SMOOTH:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%sMorph Smoothing Angle of  %f', [ID, Chunk.Data.MorphSmooth^]);
          Strings.Add(Output);
        end;
      POS_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.PosTrackTag.TrackHdr.KeyCount,
            Chunk.Data.PosTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.PosTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.PosTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sObject at  %f,  %f,  %f', [ID, Chunk.Data.PosTrackTag.PositionList[I].X,
              Chunk.Data.PosTrackTag.PositionList[I].Y,
                Chunk.Data.PosTrackTag.PositionList[I].Z]);
            Strings.Add(Output);
          end;
        end;
      ROT_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.RotTrackTag.TrackHdr.KeyCount,
            Chunk.Data.RotTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.RotTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.RotTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sRotation of  %f', [ID, Chunk.Data.RotTrackTag.RotationList[I].Angle]);
            Strings.Add(Output);
            Output := Format('%sAxis of  %f,  %f,  %f', [ID, Chunk.Data.RotTrackTag.RotationList[I].X,
              Chunk.Data.RotTrackTag.RotationList[I].Y,
                Chunk.Data.RotTrackTag.RotationList[I].Z]);
            Strings.Add(Output);
          end;
        end;
      SCL_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.ScaleTrackTag.TrackHdr.KeyCount,
            Chunk.Data.ScaleTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.ScaleTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.ScaleTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sScale of  %f,  %f,  %f', [ID, Chunk.Data.ScaleTrackTag.ScaleList[I].X,
              Chunk.Data.ScaleTrackTag.ScaleList[I].Y,
                Chunk.Data.ScaleTrackTag.ScaleList[I].Z]);
            Strings.Add(Output);
          end;
        end;
      FOV_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.FovTrackTag.TrackHdr.KeyCount,
            Chunk.Data.FovTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.FovTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.FovTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sCamera FOV of  %f', [ID, Chunk.Data.FovTrackTag.FOVAngleList[I]]);
            Strings.Add(Output);
          end;
        end;
      ROLL_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.RollTrackTag.TrackHdr.KeyCount,
            Chunk.Data.RollTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.RollTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.RollTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sCamera Roll of  %f', [ID, Chunk.Data.RollTrackTag.RollAngleList[I]]);
            Strings.Add(Output);
          end;
        end;
      COL_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.ColTrackTag.TrackHdr.KeyCount,
            Chunk.Data.ColTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.ColTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.ColTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sColor R:  %f, ', [ID, Chunk.Data.ColTrackTag.ColorList[I].B]);
            Output := Output + Format(' G:  %f, ', [Chunk.Data.ColTrackTag.ColorList[I].G]);
            Output := Output + Format(' B:  %f', [Chunk.Data.ColTrackTag.ColorList[I].B]);
            Strings.Add(Output);
          end;
        end;
      MORPH_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.MorphTrackTag.TrackHdr.KeyCount,
            Chunk.Data.MorphTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.MorphTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.MorphTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sMorph to %s', [ID, Chunk.Data.MorphTrackTag.MorphList[I]]);
            Strings.Add(Output);
          end;
        end;
      HOT_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.HotTrackTag.TrackHdr.KeyCount,
            Chunk.Data.HotTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.HotTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.HotTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sHotspot angle of  %f', [ID, Chunk.Data.HotTrackTag.HotspotAngleList[I]]);
            Strings.Add(Output);
          end;
        end;
      FALL_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.FallTrackTag.TrackHdr.KeyCount,
            Chunk.Data.FallTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.FallTrackTag.TrackHdr.KeyCount - 1 do
          begin
            DumpKeyHeader(Strings, Chunk.Data.FallTrackTag.KeyHdrList[I], IndentLevel + 1);
            Output := Format('%sFalloff Angle of  %f', [ID, Chunk.Data.FallTrackTag.FalloffAngleList[I]]);
            Strings.Add(Output);
          end;
        end;
      HIDE_TRACK_TAG:
        begin
          Source.ReadChunkData(Chunk);
          Output := Format('%s%d Keys, Flags: $%x', [ID, Chunk.Data.HideTrackTag.TrackHdr.KeyCount,
            Chunk.Data.HideTrackTag.TrackHdr.Flags]);
          Strings.Add(Output);
          for I := 0 to Chunk.Data.HideTrackTag.TrackHdr.KeyCount - 1 do
            DumpKeyHeader(Strings, Chunk.Data.HideTrackTag.KeyHdrList[I], IndentLevel + 1);
        end;
    end; // end case
  end;

  Child := Chunk.Children;

  while Assigned(Child) do
  begin
    DumpChunk(Source, Strings, Child, IndentLevel + 1, DumpLevel);
    Child := Child.Sibling;
  end;
end;

//----------------- common support function ---------------------------------------------------------------------------

procedure AddChild(Parent, Child: PChunk3DS);

// AddChild puts the chunk at the end of the Sibling list

var
  Current: PChunk3DS;

begin
  if Parent.Children = nil then
    Parent.Children := Child
  else
  begin
    Current := Parent.Children;
    while Assigned(Current.Sibling) do
      Current := Current.Sibling;
    Current.Sibling := Child;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure AddChildOrdered(Parent, Child: PChunk3DS);

// AddChildOrdered will insert the child among its siblings depending
// on the order of occurance set by the 3DS file.

var
  Current, Prev: PChunk3DS;
  ChildValue: Integer;

begin
  ChildValue := GetChunkValue(Child.Tag);

  if Parent.Children = nil then
    Parent.Children := Child
  else
  begin
    Current := Parent.Children;
    Prev := nil;
    while Assigned(Current.Sibling) do
    begin
      if ChildValue > GetChunkValue(Current.Tag) then
        break;
      Prev := Current;
      Current := Current.Sibling;
    end;

    if ChildValue > GetChunkValue(Current.Tag) then
    begin
      Child.Sibling := Current;
      if Assigned(Prev) then
        Prev.Sibling := Child
      else
        Parent.Children := Child;
    end
    else
    begin
      Child.Sibling := Current.Sibling;
      Current.Sibling := Child;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function FindChunk(Top: PChunk3DS; Tag: Word): PChunk3DS;

// searchs the given top Chunk and its children for a match

var
  Child, Match: PChunk3DS;

begin
  Result := nil;
  if Assigned(Top) then
    if Top.Tag = Tag then
      Result := Top
    else
    begin
      Child := Top.Children;
      while Assigned(Child) do
      begin
        Match := FindChunk(Child, Tag);
        if Assigned(Match) then
        begin
          Result := Match;
          Break;
        end;
        Child := Child.Sibling;
      end;
    end;
end;

//---------------------------------------------------------------------------------------------------------------------

function FindNextChunk(Local: PChunk3DS; Tag: Word): PChunk3DS;

var
  Current: PChunk3DS;

begin
  Result := nil;
  Current := Local;
  while Assigned(Current) and (Result = nil) do
  begin
    if Current.Tag = Tag then
      Result := Current;
    Current := Current.Sibling;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure FreeChunkData(var Chunk: PChunk3DS);

begin
  if Assigned(Chunk.Data.Dummy) then
  begin
    // do only care about Chunk.Data fields that contain other pointers
    // that need to be free
    case Chunk.Tag of
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
        FreeMem(Chunk.Data.IpasData.Data);
      POINT_ARRAY:
        FreeMem(Chunk.Data.PointArray.PointList);
      POINT_FLAG_ARRAY:
        FreeMem(Chunk.Data.PointFlagArray.FlagList);
      FACE_ARRAY:
        Freemem(Chunk.Data.FaceArray.FaceList);
      MSH_MAT_GROUP:
        begin
          Chunk.Data.MshMatGroup.MatName := '';
          FreeMem(Chunk.Data.MshMatGroup.FaceList);
        end;
      SMOOTH_GROUP:
        FreeMem(Chunk.Data.SmoothGroup.GroupList);
      TEX_VERTS:
        FreeMem(Chunk.Data.TexVerts.TextVertList);
      XDATA_ENTRY:
        FreeMem(Chunk.Data.XDataEntry.Data);
      POS_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.PosTrackTag.KeyHdrList);
          Freemem(Chunk.Data.PosTrackTag.PositionList);
        end;
      COL_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.ColTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.ColTrackTag.ColorList);
        end;
      ROT_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.RotTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.RotTrackTag.RotationList);
        end;
      SCL_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.ScaleTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.ScaleTrackTag.ScaleList);
        end;
      MORPH_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.MorphTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.MorphTrackTag.MorphList);
        end;
      FOV_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.FovTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.FovTrackTag.FOVAngleList);
        end;
      ROLL_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.RollTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.RollTrackTag.RollAngleList);
        end;
      HOT_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.HotTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.HotTrackTag.HotspotAngleList);
        end;
      FALL_TRACK_TAG:
        begin
          FreeMem(Chunk.Data.FallTrackTag.KeyHdrList);
          FreeMem(Chunk.Data.FallTrackTag.FalloffAngleList);
        end;
      HIDE_TRACK_TAG:
        FreeMem(Chunk.Data.HideTrackTag.KeyHdrList);
      INSTANCE_NAME:
        Chunk.Data.InstanceName^ := '';
      NODE_HDR:
        Chunk.Data.NodeHdr.ObjName := '';
    end;

    // Finally free the data chunk.
    FreeMem(Chunk.Data.Dummy);
    Chunk.Data.Dummy := nil;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure InitChunk(var Chunk: PChunk3DS);

// initializes and allocates memory for a chunk

begin
  New(Chunk);
  if Chunk = nil then
    ShowError(Error3DS_NO_MEM);

  // set default values
  with Chunk^ do
  begin
    Tag := NULL_CHUNK;
    Size := 0;
    Position := 0;
    Data.Dummy := nil;
    Sibling := nil;
    Children := nil;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure InitChunkList(var List: PChunklist3DS; Count: Integer);

begin
  if List = nil then
  begin
    List := AllocMem(SizeOf(TChunklist3DS));
    if List = nil then
      ShowError(Error3DS_NO_MEM);
  end;

  List.Count := Count;

  if Count > 0 then
  begin
    List.List := AllocMem(Count * SizeOf(TChunkListEntry3DS));
    if List.List = nil then
      ShowError(Error3DS_NO_MEM);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function PutGenericNode(TagID: Word; ParentChunk: PChunk3DS): PChunk3DS;

// put a tag into database as a child of ParentChunk

begin
  InitChunk(Result);
  Result.Tag := TagID;
  AddChildOrdered(ParentChunk, Result);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseChunk(var Chunk: PChunk3DS);

var
  Sibling: PChunk3DS;

begin
  // free memory associated with chunk and substructure
  while Assigned(Chunk) do
  begin
    Sibling := Chunk.Sibling;
    ReleaseChunk(Chunk.Children);
    FreeChunkData(Chunk);
    FreeMem(Chunk);
    Chunk := Sibling;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseChunkList(var List: PChunkList3DS);

var
  I: Integer;

begin
  if Assigned(List) then
  begin
    // tell the string management that we don't need these strings any longer
    for I := 0 to List.Count - 1 do
      List.List[I].Name := '';
    if Assigned(List.List) then
      FreeMem(List.List);
    FreeMem(List);
    List := nil;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function CopyChunk(Chunk: PChunk3DS): PChunk3DS;

// copies the structure of Chunk to Result, assigned data of Chunk will not be copied, but
// moved to Result (actually references will be moved)

var
  ChildIn: PChunk3DS;
  ChildOut: ^PChunk3DS;

begin
  if Chunk = nil then
    ShowError(ERROR3DS_INVALID_ARG);

  InitChunk(Result);
  with Result^ do
  begin
    Tag := Chunk.Tag;
    Size := Chunk.Size;
    Position := Chunk.Position;

    if Assigned(Chunk.Data.Dummy) then
    begin
      Data.Dummy := Chunk.Data.Dummy;
      Chunk.Data.Dummy := nil;
    end;

    ChildIn := Chunk.Children;
    ChildOut := @Children;
    while Assigned(ChildIn) do
    begin
      ChildOut^ := CopyChunk(ChildIn);
      ChildIn := ChildIn.Sibling;
      ChildOut := @ChildOut^.Sibling;
    end;
  end;
end;

//----------------- list update routines ------------------------------------------------------------------------------

procedure UpdateMatEntryList(const Source: TFile3DS; var DB: TDatabase3DS);

var
  Parent,
    MatName,
    MatEntry: PChunk3DS;
  I,
    MatCount: Integer;

begin
  if DB.MatlistDirty then
  begin
    ReleaseChunkList(DB.MatList);

    Parent := FindChunk(DB.TopChunk, MDATA);
    if Parent = nil then
      Parent := FindChunk(DB.TopChunk, MLIBMAGIC);

    MatCount := 0;
    if Assigned(Parent) then
    begin
      MatEntry := FindChunk(Parent, MAT_ENTRY);
      while Assigned(MatEntry) do
      begin
        MatEntry := FindNextChunk(MatEntry.Sibling, MAT_ENTRY);
        Inc(MatCount);
      end;
    end;

    InitChunkList(DB.MatList, MatCount);
    if Parent = nil then
      Exit;

    I := 0;
    MatEntry := FindChunk(Parent, MAT_ENTRY);
    while Assigned(MatEntry) do
    begin
      MatName := FindChunk(MatEntry, MAT_NAME);
      Source.ReadChunkData(MatName);
      DB.MatList.List[I].Chunk := MatEntry;
      DB.MatList.List[I].Name := MatName.Data.MatName^;
      MatEntry := FindNextChunk(MatEntry.Sibling, MAT_ENTRY);
      Inc(I);
    end;
    DB.MatlistDirty := False;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure UpdateNamedObjectList(Source: TFile3DS; var DB: TDatabase3DS);

var
  MDataChunk,
    Current: PChunk3DS;
  I: Integer;

begin
  if DB.ObjListDirty then
  begin
    ReleaseChunkList(DB.ObjList);

    MDataChunk := FindChunk(DB.TopChunk, MDATA);

    I := 0;
    if Assigned(MDataChunk) then
    begin
      Current := FindChunk(MDataChunk, NAMED_OBJECT);
      while Assigned(Current) do
      begin
        Inc(I);
        Current := FindNextChunk(Current.Sibling, NAMED_OBJECT);
      end;
    end;

    InitChunkList(DB.ObjList, I);
    if MDataChunk = nil then
      Exit;

    I := 0;
    Current := FindChunk(MDataChunk, NAMED_OBJECT);
    while Assigned(Current) do
    begin
      Source.ReadChunkData(Current);
      DB.ObjList.List[I].Chunk := Current;
      DB.ObjList.List[I].Name := Current.Data.NamedObject^;
      Current := FindNextChunk(Current.Sibling, NAMED_OBJECT);
      Inc(I);
    end;
    DB.ObjListDirty := False;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure UpdateNodeTagList(Source: TFile3DS; var DB: TDatabase3DS);

var
  KFDataChunk,
    Chunk,
    Current: PChunk3DS;
  I: Integer;

begin
  if DB.NodeListDirty then
  begin
    ReleaseChunkList(DB.NodeList);
    KFDataChunk := FindChunk(DB.TopChunk, KFDATA);

    I := 0;
    // if there is a keyframe section then count the number of node tags
    if Assigned(KFDataChunk) then
    begin
      Current := KFDataChunk.Children;
      while Assigned(Current) do
      begin
        case Current.Tag of
          AMBIENT_NODE_TAG,
            OBJECT_NODE_TAG,
            CAMERA_NODE_TAG,
            TARGET_NODE_TAG,
            LIGHT_NODE_TAG,
            L_TARGET_NODE_TAG,
            SPOTLIGHT_NODE_TAG:
            Inc(I);
        end;
        Current := Current.Sibling;
      end;
    end;

    InitChunkList(DB.NodeList, I);
    if I = 0 then
      Exit;

    I := 0;
    Current := KFDataChunk.Children;
    while Assigned(Current) do
    begin
      case Current.Tag of
        AMBIENT_NODE_TAG,
          OBJECT_NODE_TAG,
          CAMERA_NODE_TAG,
          TARGET_NODE_TAG,
          LIGHT_NODE_TAG,
          L_TARGET_NODE_TAG,
          SPOTLIGHT_NODE_TAG:
          begin
            Chunk := FindNextChunk(Current.Children, NODE_HDR);
            if Assigned(Chunk) then
            begin
              Source.ReadChunkData(Chunk);
              DB.NodeList.List[I].Chunk := Current;
              DB.NodeList.List[I].Name := Chunk.Data.NodeHdr.ObjName;
              FreeChunkData(Chunk);
            end;

            // Object tags may have an instance name as well, which gets appended to
            // the object name with a "." seperator
            if Current.Tag = OBJECT_NODE_TAG then
            begin
              Chunk := FindNextChunk(Current.Children, INSTANCE_NAME);
              if Assigned(Chunk) then
              begin
                Source.ReadChunkData(Chunk);
                DB.NodeList.List[I].Name := DB.NodeList.List[I].Name + '.' + Chunk.Data.InstanceName^;
                FreeChunkData(Chunk);
              end;
            end;
            Inc(I); // Increment index counter
          end;
      end;
      Current := Current.Sibling;
    end;

    DB.NodeListDirty := False;
  end;
end;

//----------------- other support function ----------------------------------------------------------------------------

function GetGenericNodeCount(const Source: TFile3DS; var DB: TDatabase3DS; Tag: Word): Integer;

var
  I: Integer;

begin
  UpdateNodeTagList(Source, DB);

  Result := 0;
  for I := 0 to DB.NodeList.Count - 1 do
    if DB.NodeList.List[I].Chunk.Tag = Tag then
      Inc(Result);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure GetGenericNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; TagID: Word; List: TStringList);

var
  I: Cardinal;

begin
  UpdateNodeTagList(Source, DB);
  List.Clear;
  for I := 0 to DB.NodeList.Count - 1 do
    if DB.NodeList.List[I].Chunk.Tag = TagID then
      List.Add(DB.NodeList.List[I].Name);
end;

//---------------------------------------------------------------------------------------------------------------------

function FindNamedAndTaggedChunk(const Source: TFile3DS; var DB: TDatabase3DS; Name: string; TagID: Word): PChunk3DS;

// Look through the keyframer stuff and find named chunk of the tag type TagID.
// Has to be a chunk that has a node header: CAMERA_NODE, LIGHT_NODE, , .

var
  KfChunk,
    NodeHdrChunk: PChunk3DS;

begin
  // find Keyframe Chunk
  KfChunk := FindChunk(DB.TopChunk, KFDATA);

  // look for the target tag
  Result := FindChunk(KfChunk, TagID);
  while Assigned(Result) do
  begin
    NodeHdrChunk := FindNextChunk(Result.Children, NODE_HDR);
    if Assigned(NodeHdrChunk) then
    begin
      Source.ReadChunkData(NodeHdrChunk);
      // match name, set pointer (case sensitive comparation!)
      if CompareStr(Name, NodeHdrChunk.Data.NodeHdr.ObjName) = 0 then
      begin
        FreeChunkData(NodeHdrChunk);
        Break;
      end;
      FreeChunkData(NodeHdrChunk);
    end;
    Result := FindNextChunk(Result.Sibling, TagID);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function FindNodeTagByIndexAndType(const Source: TFile3DS; var DB: TDatabase3DS; Index: Cardinal; AType: Word):
  PChunk3DS;

var
  I, Count: Cardinal;

begin
  Result := nil;
  Count := 0;
  UpdateNodeTagList(Source, DB);
  for I := 0 to DB.NodeList.Count - 1 do
    if DB.NodeList.List[I].Chunk.Tag = AType then
    begin
      if Count = Index then
      begin
        Result := DB.NodeList.List[I].Chunk;
        Break;
      end;
      Inc(Count);
    end;
end;

//---------------------------------------------------------------------------------------------------------------------

function FindNodeTagByNameAndType(const Source: TFile3DS; DB: TDatabase3DS; Name: string; AType: Word): PChunk3DS;

var
  I: Integer;

begin
  Result := nil;
  UpdateNodeTagList(Source, DB);
  for I := 0 to DB.NodeList.Count - 1 do
    if (DB.NodeList.List[I].Chunk.Tag = AType) and
      (CompareStr(Name, DB.NodeList.List[I].Name) = 0) then
      Result := DB.NodeList.List[I].Chunk;
end;

//----------------- material handling ---------------------------------------------------------------------------------

function GetMaterialCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;

begin
  UpdateMatEntryList(Source, DB);
  if DB.MatList = nil then
    Result := 0
  else
    Result := DB.MatList.Count;
end;

//---------------------------------------------------------------------------------------------------------------------

function FindMatEntryByIndex(Source: TFile3DS; DB: TDatabase3DS; Index: Integer): PChunk3DS;

begin
  if DB.TopChunk = nil then
    ShowError(Error3DS_INVALID_DATABASE);
  if (DB.TopChunk.Tag <> MLIBMAGIC) and
    (DB.TopChunk.Tag <> M3DMAGIC) and
    (DB.TopChunk.Tag <> CMAGIC) then
    ShowError(Error3DS_WRONG_DATABASE);

  UpdateMatEntryList(Source, DB);
  if Index < DB.MatList.Count then
    Result := DB.MatList.List[Index].Chunk
  else
    Result := nil;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure InitBitmap(var Map: TBitmap3DS);

begin
  FillChar(Map, SizeOf(Map), 0);
  with Map do
  begin
    UScale := 1;
    VScale := 1;
    Tint2.R := 1;
    Tint2.G := 1;
    Tint2.B := 1;
    RedTint.R := 1;
    GreenTint.G := 1;
    BlueTint.B := 1;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure InitMaterial(var Mat: TMaterial3DS);

begin
  FillChar(Mat, SizeOf(Mat), 0);
  with Mat do
  begin
    WireSize := 1;
    Shading := stPhong;
    Reflect.AutoMap.Size := 100;
    Reflect.AutoMap.nthFrame := 1;
    InitBitmap(Texture.Map);
    InitBitmap(Texture.Mask);
    InitBitmap(Texture2.Map);
    InitBitmap(Texture2.Mask);
    InitBitmap(Opacity.Map);
    InitBitmap(Opacity.Mask);
    InitBitmap(Reflect.Map);
    InitBitmap(Reflect.Mask);
    InitBitmap(Bump.Map);
    InitBitmap(Bump.Mask);
    InitBitmap(SpecMap.Map);
    InitBitmap(SpecMap.Mask);
    InitBitmap(ShinMap.Map);
    InitBitmap(ShinMap.Mask);
    InitBitmap(IllumMap.Map);
    InitBitmap(IllumMap.Mask);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseMaterial(Mat: PMaterial3DS);

begin
  if Assigned(Mat) then
  begin
    FreeMem(Mat.Texture.Map.Data);
    FreeMem(Mat.Texture.Mask.Data);
    FreeMem(Mat.Texture2.Map.Data);
    FreeMem(Mat.Texture2.Mask.Data);
    FreeMem(Mat.Opacity.Map.Data);
    FreeMem(Mat.Opacity.Mask.Data);
    FreeMem(Mat.Reflect.Mask.Data);
    FreeMem(Mat.Bump.Map.Data);
    FreeMem(Mat.Bump.Mask.Data);
    FreeMem(Mat.Specmap.Map.Data);
    FreeMem(Mat.SpecMap.Mask.Data);
    FreeMem(Mat.ShinMap.Map.Data);
    FreeMem(Mat.ShinMap.Mask.Data);
    FreeMem(Mat.IllumMap.Map.Data);
    FreeMem(Mat.IllumMap.Mask.Data);
    Dispose(Mat);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function FindNamedObjectByIndex(Source: TFile3DS; DB: TDatabase3DS; AType: Word; Index: Integer): PChunk3DS;

// searches the database for a named object by index position and object type
// returns the NAMED_OBJECT chunk if found, nil otherwise

var
  Chunk: PChunk3DS;
  I, Count: Integer;

begin
  UpdateNamedObjectList(Source, DB);

  Count := 0;
  Result := nil;
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    if AType = DL_SPOTLIGHT then
    begin
      Chunk := FindChunk(DB.ObjList.List[I].Chunk, N_DIRECT_LIGHT);
      if Assigned(Chunk) then
        Chunk := FindChunk(Chunk, AType);
    end
    else
      Chunk := FindChunk(DB.ObjList.List[I].Chunk, AType);

    if Assigned(Chunk) then
    begin
      if Count = Index then
      begin
        Result := DB.ObjList.List[I].Chunk;
        Break;
      end
      else
        Inc(Count);
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure DeleteChunk(var Chunk: PChunk3DS);

// returns a chunk to its untagged state state, but leaves it
// connected to any siblings it might have

begin
  if Assigned(Chunk) then
  begin
    // release any children
    if Assigned(Chunk.Children) then
      ReleaseChunk(Chunk.Children);
    // release any data
    if Assigned(Chunk.Data.Dummy) then
      FreeChunkData(Chunk);
    // return to a semi-uninitialized state
    Chunk.Tag := NULL_CHUNK;
    Chunk.Size := 0;
    Chunk.Position := 0;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function ReadPercentageChunk(Source: TFile3DS; Chunk: PChunk3DS): Single;

var
  DataChunk: PChunk3DS;

begin
  DataChunk := FindChunk(Chunk, INT_PERCENTAGE);
  if Assigned(DataChunk) then
  begin
    Source.ReadChunkData(DataChunk);
    Result := DataChunk.Data.IntPercentage^ / 100;
    FreeChunkData(DataChunk);
  end
  else
  begin
    DataChunk := FindChunk(Chunk, FLOAT_PERCENTAGE);
    if Assigned(DataChunk) then
    begin
      Source.ReadChunkData(DataChunk);
      Result := DataChunk.Data.FloatPercentage^;
      FreeChunkData(DataChunk);
    end
    else
      Result := 0;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure GetBitmapChunk(const DataSource: TFile3DS; Chunk: PChunk3DS; var Bitmap: TBitmap3DS);

var
  Current: PChunk3DS;

begin
  Current := Chunk.Children;
  while Assigned(Current) do
  begin
    with Bitmap do
    begin
      case Current.Tag of
        INT_PERCENTAGE:
          begin
            DataSource.ReadChunkData(Current);
            Percent := Current.Data.IntPercentage^ / 100;
            FreeChunkData(Current);
          end;
        FLOAT_PERCENTAGE:
          begin
            DataSource.ReadChunkData(Current);
            Percent := Current.Data.FloatPercentage^;
            FreeChunkData(Current);
          end;
        MAT_MAPNAME:
          begin
            DataSource.ReadChunkData(Current);
            Name := Current.Data.MatMapname^;
            FreeChunkData(Current);
          end;
        MAT_MAP_TILING:
          begin
            DataSource.ReadChunkData(Current);
            if (Current.Data.MatMapTiling^ and TEX_DECAL) <> 0 then
              if (Current.Data.MatMapTiling^ and TEX_NOWRAP) <> 0 then
                Tiling := ttDecal
              else
                Tiling := ttBoth
            else
              tiling := ttTile;
            IgnoreAlpha := (Current.Data.MatMapTiling^ and TEX_DONT_USE_ALPHA) <> 0;
            if (Current.Data.MatMapTiling^ and TEX_SAT) <> 0 then
              Filter := ftSummedArea
            else
              Filter := ftPyramidal;
            Mirror := (Current.Data.MatMapTiling^ and TEX_MIRROR) <> 0;
            Negative := (Current.Data.MatMapTiling^ and TEX_INVERT) <> 0;
            if (Current.Data.MatMapTiling^ and TEX_TINT) <> 0 then
              if (Current.Data.MatMapTiling^ and TEX_ALPHA_SOURCE) <> 0 then
                Source := ttAlphaTint
              else
                Source := ttRGBLumaTint
            else
              if (Current.Data.MatMapTiling^ and TEX_RGB_TINT) <> 0 then
                Source := ttRGBTint
              else
                if (Current.Data.MatMapTiling^ and TEX_ALPHA_SOURCE) <> 0 then
                  Source := ttAlpha
                else
                  source := ttRGB;
            FreeChunkData(Current);
          end;
        MAT_MAP_USCALE:
          begin
            DataSource.ReadChunkData(Current);
            UScale := Current.Data.MatMapUScale^;
            FreeChunkData(Current);
          end;
        MAT_MAP_VSCALE:
          begin
            DataSource.ReadChunkData(Current);
            VScale := Current.Data.MatMapVScale^;
            FreeChunkData(Current);
          end;
        MAT_MAP_UOFFSET:
          begin
            DataSource.ReadChunkData(Current);
            UOffset := Current.Data.MatMapUOffset^;
            FreeChunkData(Current);
          end;
        MAT_MAP_VOFFSET:
          begin
            DataSource.ReadChunkData(Current);
            VOffset := Current.Data.MatMapVOffset^;
            FreeChunkData(Current);
          end;
        MAT_MAP_ANG:
          begin
            DataSource.ReadChunkData(Current);
            Rotation := Current.Data.MatMapAng^;
            FreeChunkData(Current);
          end;
        MAT_BUMP_PERCENT:
          ; // value is really stored in TMaterial3DS structure
        MAT_MAP_COL1:
          begin
            DataSource.ReadChunkData(Current);
            Tint1.R := Current.Data.MatMapCol1.Red / 255;
            Tint1.G := Current.Data.MatMapCol1.Green / 255;
            Tint1.B := Current.Data.MatMapCol1.Blue / 255;
            FreeChunkData(Current);
          end;
        MAT_MAP_COL2:
          begin
            DataSource.ReadChunkData(Current);
            Tint2.R := Current.Data.MatMapCol2.Red / 255;
            Tint2.G := Current.Data.MatMapCol2.Green / 255;
            Tint2.B := Current.Data.MatMapCol2.Blue / 255;
            FreeChunkData(Current);
          end;
        MAT_MAP_RCOL:
          begin
            DataSource.ReadChunkData(Current);
            RedTint.R := Current.Data.MatMapRCol.Red / 255;
            RedTint.G := Current.Data.MatMapRCol.Green / 255;
            RedTint.B := Current.Data.MatMapRCol.Blue / 255;
            FreeChunkData(Current);
          end;
        MAT_MAP_GCOL:
          begin
            DataSource.ReadChunkData(Current);
            GreenTint.R := Current.Data.MatMapGCol.Red / 255;
            GreenTint.G := Current.Data.MatMapGCol.Green / 255;
            GreenTint.B := Current.Data.MatMapGCol.Blue / 255;
            FreeChunkData(Current);
          end;
        MAT_MAP_BCOL:
          begin
            DataSource.ReadChunkData(Current);
            BlueTint.R := Current.Data.MatMapBCol.Red / 255;
            BlueTint.G := Current.Data.MatMapBCol.Green / 255;
            BlueTint.B := Current.Data.MatMapBCol.Blue / 255;
            FreeChunkData(Current);
          end;
        MAT_MAP_TEXBLUR:
          begin
            DataSource.ReadChunkData(Current);
            Blur := Current.Data.MatMapTexBlur^; // float percents
            FreeChunkData(Current);
          end;
      end; // case Current.Tag of
      Current := Current.Sibling;
    end; // with Bitmap do
  end; // while Assigned(Current) do
end;

//---------------------------------------------------------------------------------------------------------------------

function ReadMatEntryChunk(Source: TFile3DS; MatEntry: PChunk3DS): TMaterial3DS;

var
  Current,
    DataChunk,
    Color: PChunk3DS;
  MatColor: PFColor3DS;

begin
  if MatEntry.Tag <> MAT_ENTRY then
    ShowError(Error3DS_INVALID_CHUNK);
  InitMaterial(Result);

  with Result do
  begin
    Current := MatEntry.Children;
    while Assigned(Current) do
    begin
      if (Current.Tag and $FF00) <> $8000 then // ignore xdata
        case Current.Tag of
          MAT_NAME:
            begin
              Source.ReadChunkData(Current);
              Name := Current.Data.MatName^;
              FreeChunkData(Current);
            end;
          MAT_AMBIENT,
            MAT_DIFFUSE,
            MAT_SPECULAR:
            begin
              case Current.Tag of
                MAT_DIFFUSE:
                  MatColor := @Diffuse;
                MAT_SPECULAR:
                  MatColor := @Specular;
              else
                MatColor := @Ambient; // MAT_AMBIENT
              end;
              Color := FindChunk(Current, COLOR_24);
              if Assigned(Color) then
              begin
                Source.ReadChunkData(Color);
                MatColor.R := Color.Data.Color24.Red / 255;
                MatColor.G := Color.Data.Color24.Green / 255;
                MatColor.B := Color.Data.Color24.Blue / 255;
                FreeChunkData(Color);
              end;
              Color := FindChunk(Current, LIN_COLOR_24);
              if Assigned(Color) then
              begin
                Source.ReadChunkData(Color);
                MatColor.R := Color.Data.LinColor24.Red / 255;
                MatColor.G := Color.Data.LinColor24.Green / 255;
                MatColor.B := Color.Data.LinColor24.Blue / 255;
                FreeChunkData(Color);
              end;
            end;
          MAT_SHININESS:
            Shininess := ReadPercentageChunk(Source, Current);
          MAT_SHIN2PCT:
            ShinStrength := ReadPercentageChunk(Source, Current);
          MAT_SHIN3PCT:
            ; // just skip for now
          MAT_REFBLUR:
            Blur := ReadPercentageChunk(Source, Current);
          MAT_TRANSPARENCY:
            Transparency := ReadPercentageChunk(Source, Current);
          MAT_XPFALL:
            TransFalloff := ReadPercentageChunk(Source, Current);
          MAT_SELF_ILPCT:
            SelfIllumPct := ReadPercentageChunk(Source, Current);
          MAT_WIRE:
            Shading := stWire;
          MAT_WIREABS:
            UseWireAbs := True;
          MAT_XPFALLIN:
            Transparency := -Transparency;
          MAT_WIRESIZE:
            begin
              Source.ReadChunkData(Current);
              WireSize := Current.Data.MatWireSize^;
              FreeChunkData(Current);
            end;
          MAT_USE_XPFALL:
            UseFall := True;
          MAT_USE_REFBLUR:
            Useblur := True;
          MAT_SELF_ILLUM:
            SelfIllum := True;
          MAT_TWO_SIDE:
            TwoSided := True;
          MAT_ADDITIVE:
            Additive := True;
          MAT_SHADING:
            begin
              Source.ReadChunkData(Current);
              Shading := TShadeType3DS(Current.Data.MatShading^);
              FreeChunkData(Current);
            end;
          MAT_FACEMAP:
            FaceMap := True;
          MAT_PHONGSOFT:
            Soften := True;
          MAT_TEXMAP:
            GetBitmapChunk(Source, Current, Texture.Map);
          MAT_TEXMASK:
            GetBitmapChunk(Source, Current, Texture.Mask);
          MAT_TEX2MAP:
            GetBitmapChunk(Source, Current, Texture2.Map);
          MAT_TEX2MASK:
            GetBitmapChunk(Source, Current, Texture2.Mask);
          MAT_OPACMAP:
            GetBitmapChunk(Source, Current, Opacity.Map);
          MAT_OPACMASK:
            GetBitmapChunk(Source, Current, Opacity.Mask);
          MAT_REFLMAP:
            GetBitmapChunk(Source, Current, Reflect.Map);
          MAT_ACUBIC:
            begin
              Source.ReadChunkData(Current);
              Reflect.UseAuto := True;
              Reflect.AutoMap.FirstFrame := (Current.Data.MatAcubic.Flags and ACubicFirst3DS) <> 0;
              Reflect.AutoMap.Flat := (Current.Data.MatAcubic.Flags and ACubicFlat3DS) <> 0;
              Reflect.AutoMap.Size := Current.Data.MatAcubic.MapSize;
              Reflect.AutoMap.nthFrame := Current.Data.MatAcubic.FrameInterval;
              FreeChunkData(Current);
            end;
          MAT_REFLMASK:
            GetBitmapChunk(Source, Current, Reflect.Mask);
          MAT_BUMPMAP:
            begin
              GetBitmapChunk(Source, Current, Bump.Map);
              DataChunk := FindChunk(Current, MAT_BUMP_PERCENT);
              if Assigned(DataChunk) then
              begin
                Source.ReadChunkData(DataChunk);
                Bump.Map.Percent := DataChunk.Data.MatBumpPercent^ / 100;
                FreeChunkData(DataChunk);
              end;
            end;
          MAT_BUMPMASK:
            GetBitmapChunk(Source, Current, Bump.Mask);
          MAT_SPECMAP:
            GetBitmapChunk(Source, Current, SpecMap.Map);
          MAT_SPECMASK:
            GetBitmapChunk(Source, Current, SpecMap.Mask);
          MAT_SHINMAP:
            GetBitmapChunk(Source, Current, ShinMap.Map);
          MAT_SHINMASK:
            GetBitmapChunk(Source, Current, Shinmap.Mask);
          MAT_SELFIMAP:
            GetBitmapChunk(Source, Current, IllumMap.Map);
          MAT_SELFIMASK:
            GetBitmapChunk(Source, Current, IllumMap.Mask);
          MAT_SXP_TEXT_DATA:
            begin
              Source.ReadChunkData(Current);
              Texture.Map.DataSize := Current.Data.IpasData.Size;
              Texture.Map.Data := Current.Data.IpasData.Data;
              // avoid releasing the data memory
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_TEXT_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              Texture.Mask.DataSize := Current.Data.IpasData.Size;
              Texture.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_TEXT2_DATA:
            begin
              Source.ReadChunkData(Current);
              Texture2.Map.DataSize := Current.Data.IpasData.Size;
              Texture2.Map.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_TEXT2_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              Texture2.Mask.DataSize := Current.Data.IpasData.Size;
              Texture2.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_OPAC_DATA:
            begin
              Source.ReadChunkData(Current);
              Opacity.Map.DataSize := Current.Data.IpasData.Size;
              Opacity.Map.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_OPAC_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              Opacity.Mask.DataSize := Current.Data.IpasData.Size;
              Opacity.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_REFL_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              Reflect.Mask.DataSize := Current.Data.IpasData.Size;
              Reflect.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_BUMP_DATA:
            begin
              Source.ReadChunkData(Current);
              Bump.Map.DataSize := Current.Data.IpasData.Size;
              Bump.Map.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_BUMP_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              Bump.Mask.DataSize := Current.Data.IpasData.Size;
              Bump.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_SPEC_DATA:
            begin
              Source.ReadChunkData(Current);
              SpecMap.Map.DataSize := Current.Data.IpasData.Size;
              SpecMap.Map.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_SPEC_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              Specmap.Mask.DataSize := Current.Data.IpasData.Size;
              Specmap.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_SHIN_DATA:
            begin
              Source.ReadChunkData(Current);
              ShinMap.Map.DataSize := Current.Data.IpasData.Size;
              ShinMap.Map.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_SHIN_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              ShinMap.Mask.DataSize := Current.Data.IpasData.Size;
              ShinMap.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_SELFI_DATA:
            begin
              Source.ReadChunkData(Current);
              IllumMap.Map.DataSize := Current.Data.IpasData.Size;
              IllumMap.Map.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_SXP_SELFI_MASKDATA:
            begin
              Source.ReadChunkData(Current);
              IllumMap.Mask.DataSize := Current.Data.IpasData.Size;
              IllumMap.Mask.Data := Current.Data.IpasData.Data;
              Current.Data.IpasData.Data := nil;
              FreeChunkData(Current);
            end;
          MAT_DECAL:
            ; // don't know what do to with it
        else
          ShowError(Error3DS_INVALID_CHUNK)
        end;
      Current := Current.Sibling;
    end; // while Assigned(Current) do
  end; // with Result do
end;

//---------------------------------------------------------------------------------------------------------------------

function GetChunkValue(Tag: Word): Integer;

// Computes a chunk weighting used to determine proper chunk order,
// higher values appear earlier in the parent than lower values

begin
  // only chunks where an explicit order matters are handled
  Result := 0;

  case Tag of
    NULL_CHUNK:
      Inc(Result); // These should just be ignored
    SMAGIC:
      Inc(Result, 2);
    LMAGIC:
      Inc(Result, 3);
    M3DMAGIC:
      Inc(Result, 4);
    M3D_VERSION:
      Inc(Result, 5);
    MDATA:
      Inc(Result, 6);
    KFDATA:
      Inc(Result, 7);
    COLOR_24:
      Inc(Result, 8);
    LIN_COLOR_24:
      Inc(Result, 9);
    MESH_VERSION:
      Inc(Result, 10);
    MAT_ENTRY:
      Inc(Result, 11);
    KFHDR:
      Inc(Result, 12);
    MASTER_SCALE:
      Inc(Result, 13);
    VIEWPORT_LAYOUT:
      Inc(Result, 14);
    LO_SHADOW_BIAS:
      Inc(Result, 15);
    SHADOW_MAP_SIZE:
      Inc(Result, 16);
    SHADOW_FILTER:
      Inc(Result, 17);
    RAY_BIAS:
      Inc(Result, 18);
    O_CONSTS:
      Inc(Result, 19);
    AMBIENT_LIGHT:
      Inc(Result, 20);
    SOLID_BGND:
      Inc(Result, 21);
    BIT_MAP:
      Inc(Result, 22);
    V_GRADIENT:
      Inc(Result, 23);
    USE_BIT_MAP:
      Inc(Result, 24);
    USE_SOLID_BGND:
      Inc(Result, 25);
    USE_V_GRADIENT:
      Inc(Result, 26);
    FOG:
      Inc(Result, 27);
    LAYER_FOG:
      Inc(Result, 28);
    DISTANCE_CUE:
      Inc(Result, 29);
    DEFAULT_VIEW:
      Inc(Result, 30);
    NAMED_OBJECT:
      Inc(Result, 31);
    KFSEG:
      Inc(Result, 32);
    KFCURTIME:
      Inc(Result, 33);
    TARGET_NODE_TAG,
      L_TARGET_NODE_TAG,
      OBJECT_NODE_TAG,
      CAMERA_NODE_TAG,
      SPOTLIGHT_NODE_TAG:
      Inc(Result, 34);
    AMBIENT_NODE_TAG:
      Inc(Result, 35);
    N_TRI_OBJECT,
      N_CAMERA,
      N_DIRECT_LIGHT:
      Inc(Result);
    OBJ_HIDDEN:
      Inc(Result);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetMaterialByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TMaterial3DS;

var
  Chunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  Chunk := FindMatEntryByIndex(Source, DB, Index);
  if Assigned(Chunk) then
    Result := ReadMatEntryChunk(Source, Chunk)
  else
    ShowErrorFormatted(Error3DS_INVALID_INDEX, [Index]);
end;

//----------------- mesh object handling ------------------------------------------------------------------------------

function GetMeshCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;

// returns the number of mesh objects referenced in the chunk list

var
  I: Integer;
  Chunk: PChunk3DS;

begin
  // update the index to named objects if the list has changed recently
  UpdateNamedObjectList(Source, DB);

  Result := 0;
  if DB.ObjList = nil then
    Exit;

  // scan through the list of named objects
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    Chunk := FindChunk(DB.ObjList.List[I].Chunk, N_TRI_OBJECT);
    if Assigned(Chunk) then
      Inc(Result);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetMeshMatCount(Current: PChunk3DS): Integer;

// aids the GetMeshEntryChunk3DS in determining
// how many materials are defined within the mesh object

var
  Chunk: PChunk3DS;

begin
  Result := 0;
  Chunk := FindChunk(Current, MSH_MAT_GROUP);
  while Assigned(Chunk) do
  begin
    Chunk := FindNextChunk(Chunk.Sibling, MSH_MAT_GROUP);
    Inc(Result);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure RelMeshObjField(var Mesh: TMesh3DS; Field: Integer);

var
  I: Integer;

begin
  if ((Field and RelVertexArray3DS) <> 0) and
    Assigned(Mesh.VertexArray) then
  begin
    FreeMem(Mesh.VertexArray);
    Mesh.VertexArray := nil;
  end;

  if ((Field and RelTextArray3DS) <> 0) and
    Assigned(Mesh.TextArray) then
  begin
    FreeMem(Mesh.TextArray);
    Mesh.TextArray := nil;
  end;

  if ((Field and RelFaceArray3DS) <> 0) and
    Assigned(Mesh.FaceArray) then
  begin
    FreeMem(Mesh.FaceArray);
    Mesh.FaceArray := nil;
  end;

  if ((Field and RelMatArray3DS) <> 0) and
    Assigned(Mesh.MatArray) then
  begin
    for I := 0 to Mesh.NMats - 1 do
      if Assigned(Mesh.MatArray[I].FaceIndex) then
      begin
        FreeMem(Mesh.MatArray[I].FaceIndex);
        Mesh.MatArray[I].FaceIndex := nil;
        Mesh.MatArray[I].Name := '';
      end;
    FreeMem(Mesh.MatArray);
    Mesh.MatArray := nil;
  end;

  if ((Field and RelSmoothArray3DS) <> 0) and
    Assigned(Mesh.SmoothArray) then
  begin
    FreeMem(Mesh.SmoothArray);
    Mesh.SmoothArray := nil;
  end;

  if ((Field and RelProcData3DS) <> 0) and
    Assigned(Mesh.ProcData) then
  begin
    FreeMem(Mesh.ProcData);
    Mesh.ProcData := nil;
  end;

  if ((Field and RelVFlagArray3DS) <> 0) and
    Assigned(Mesh.VFlagArray) then
  begin
    FreeMem(Mesh.VFlagArray);
    Mesh.VFlagArray := nil;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function InitMeshObjField(Field: Integer): TMesh3DS;

var
  I: Integer;

begin
  with Result do
  begin
    // test to see if Vertices are being allocated
    if (Field and InitVertexArray3DS) <> 0 then
    begin
      // if the vertex count is 0 then free the array
      if NVertices = 0 then
        RelMeshObjField(Result, RelVertexArray3DS)
      else
      begin
        // if this is the very first allocation
        if VertexArray = nil then
        begin
          // allocate the new block of memory
          VertexArray := AllocMem(NVertices * SizeOf(TPoint3DS));
          if VertexArray = nil then
            ShowError(Error3DS_NO_MEM);

          // this is done by AllocMem already
          // initialize the new block
          //for I := 0 to NVertices - 1 do VertexArray[I] := DefPoint3DS;
        end
        else // else this is an existing block
        begin
          // just resize it
          ReallocMem(VertexArray, SizeOf(TPoint3DS) * NVertices);
          if VertexArray = nil then
            ShowError(Error3DS_NO_MEM);
        end;
      end;
    end;

    if (Field and InitTextArray3DS) <> 0 then
    begin
      if NTextVerts = 0 then
        RelMeshObjField(Result, RelTextArray3DS)
      else
      begin
        if TextArray = nil then
        begin
          TextArray := Allocmem(NTextVerts * SizeOf(TTexVert3DS));
          if TextArray = nil then
            ShowError(Error3DS_NO_MEM);

          for I := 0 to NTextVerts - 1 do
            TextArray[I] := DefTextVert3DS;
        end
        else
        begin
          Reallocmem(TextArray, SizeOf(TTexVert3DS) * NTextVerts);
          if TextArray = nil then
            ShowError(Error3DS_NO_MEM);
        end;
      end;
    end;

    if (Field and InitFaceArray3DS) <> 0 then
    begin
      if NFaces = 0 then
        RelMeshObjField(Result, RelFaceArray3DS)
      else
      begin
        if FaceArray = nil then
        begin
          FaceArray := AllocMem(NFaces * SizeOf(TFace3DS));
          if FaceArray = nil then
            ShowError(Error3DS_NO_MEM);

          for I := 0 to NFaces - 1 do
            FaceArray[I] := DefFace3DS;
        end
        else
        begin
          ReallocMem(FaceArray, SizeOf(TFace3DS) * NFaces);
          if FaceArray = nil then
            ShowError(Error3DS_NO_MEM);
        end;
      end;
    end;

    if (Field and InitMatArray3DS) <> 0 then
    begin
      if NMats = 0 then
        RelMeshObjField(Result, RelMatArray3DS)
      else
      begin
        if Matarray = nil then
        begin
          MatArray := AllocMem(NMats * SizeOf(TObjmat3DS));
          if MatArray = nil then
            ShowError(Error3DS_NO_MEM);

          for I := 0 to NMats - 1 do
            MatArray[I] := DefObjMat3DS;
        end
        else
        begin
          ReallocMem(MatArray, SizeOf(TObjmat3DS) * NMats);
          if MatArray = nil then
            ShowError(Error3DS_NO_MEM);
        end;
      end;
    end;

    if (Field and InitSmoothArray3DS) <> 0 then
    begin
      if NFaces = 0 then
        RelMeshObjField(Result, RelSmoothArray3DS)
      else
      begin
        if SmoothArray = nil then
        begin
          SmoothArray := AllocMem(NFaces * SizeOf(Integer));
          if SmoothArray = nil then
            ShowError(Error3DS_NO_MEM);

          // done by AllocMem
          // for I := 0 to NFaces - 1 do SmoothArray[I] := 0;
        end
        else
        begin
          ReallocMem(SmoothArray, SizeOf(Integer) * NFaces);
          if SmoothArray = nil then
            ShowError(Error3DS_NO_MEM);
        end;
      end;
    end;

    if (Field and InitProcData3DS) <> 0 then
    begin
      if ProcSize = 0 then
        RelMeshObjField(Result, RelProcData3DS)
      else
      begin
        if ProcData = nil then
        begin
          ProcData := AllocMem(ProcSize * SizeOf(Byte));
          if ProcData = nil then
            ShowError(Error3DS_NO_MEM);
        end
        else
        begin
          ReallocMem(ProcData, SizeOf(Byte) * ProcSize);
          if ProcData = nil then
            ShowError(Error3DS_NO_MEM);
        end;
      end;
    end;

    if (Field and InitVFlagArray3DS) <> 0 then
    begin
      if NVertices = 0 then
        RelMeshObjField(Result, RelVFlagArray3DS)
      else
      begin
        if VFlagArray = nil then
        begin
          VFlagArray := AllocMem(NVertices * SizeOf(Word));
          if VFlagArray = nil then
            ShowError(Error3DS_NO_MEM);
        end
        else
        begin
          ReallocMem(VFlagArray, SizeOf(Word) * NVertices);
          if VFlagArray = nil then
            ShowError(Error3DS_NO_MEM);
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function InitMeshObj(VertexCount, FaceCount, InitFlags: Word): TMesh3DS;

begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    NVertices := VertexCount;
    Map.TileX := 1;
    Map.TileY := 1;
    Map.Scale := 1;
    Map.Matrix[0] := 1;
    Map.Matrix[4] := 1;
    Map.PW := 1;
    Map.PH := 1;
    Map.CH := 1;

    NFaces := FaceCount;

    Result := InitMeshObjField(InitVertexArray3DS or InitFaceArray3DS);

    if (InitFlags and InitTextArray3DS) <> 0 then
    begin
      NTextVerts := VertexCount;
      Result := InitMeshObjField(InitTextArray3DS);
    end;

    if (InitFlags and InitVFlagArray3DS) <> 0 then
    begin
      NVFlags := VertexCount;
      Result := InitMeshObjField(InitVFlagArray3DS);
    end;

    if (InitFlags and InitSmoothArray3DS) <> 0 then
      Result := InitMeshObjField(InitSmoothArray3DS);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseMeshObj(Mesh: PMesh3DS);

begin
  if Assigned(Mesh) then
  begin
    RelMeshObjField(Mesh^, RelAll3DS);
    Dispose(Mesh);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetMeshEntryChunk(const Source: TFile3DS; Chunk: PChunk3DS): TMesh3DS;

var
  NTriChunk,
    FaceArrayChunk,
    DataChunk,
    Current: PChunk3DS;
  I: Integer;

begin
  NTriChunk := FindNextChunk(Chunk.Children, N_TRI_OBJECT);
  if NTriChunk = nil then
    ShowError(Error3DS_WRONG_OBJECT);

  Result := InitMeshObj(0, 0, 0);

  with Result do
  begin
    // get the mesh name
    Source.ReadChunkData(Chunk);
    Name := Chunk.Data.NamedObject^;

    Current := NTriChunk.Children;
    while Assigned(Current) do
    begin
      case Current.Tag of
        POINT_ARRAY:
          begin
            Source.ReadChunkData(Current);
            NVertices := Current.Data.PointArray.Vertices;
            VertexArray := Current.Data.PointArray.PointList;
            // avoid freeing the just allocated memory
            Current.Data.PointArray.PointList := nil;
            FreeChunkData(Current);
          end;
        POINT_FLAG_ARRAY:
          begin
            Source.ReadChunkData(Current);
            NVFlags := Current.Data.PointFlagArray.Flags;
            VFlagArray := Current.Data.PointFlagArray.FlagList;
            Current.Data.PointFlagArray.FlagList := nil;
            FreeChunkData(Current);
          end;
        FACE_ARRAY:
          begin
            Source.ReadChunkData(Current);
            NFaces := Current.Data.FaceArray.Faces;
            FaceArray := Current.Data.FaceArray.FaceList;
            Current.Data.FaceArray.FaceList := nil;

            if Assigned(Current.Children) then
            begin
              // begin search for MESH_MAT_GROUP and SMOOTH_GROUP
              FaceArrayChunk := Current;

              // create a list of all mesh mat groups
              DataChunk := FindChunk(FaceArrayChunk, MSH_MAT_GROUP);
              if Assigned(DataChunk) then
              begin
                NMats := GetMeshMatCount(DataChunk);
                MatArray := AllocMem(NMats * SizeOf(TObjMat3DS));

                for I := 0 to NMats - 1 do
                begin
                  Source.ReadChunkData(DataChunk);
                  MatArray[I].Name := DataChunk.Data.MshMatGroup.MatName;
                  MatArray[I].NFaces := DataChunk.Data.MshMatGroup.Faces;
                  MatArray[I].FaceIndex := DataChunk.Data.MshMatGroup.FaceList;
                  DataChunk.Data.MshMatGroup.FaceList := nil;
                  FreeChunkData(DataChunk);
                  DataChunk := FindNextChunk(DataChunk.Sibling, MSH_MAT_GROUP);
                end;
              end;

              DataChunk := FindNextChunk(FaceArrayChunk.Children, SMOOTH_GROUP);
              if Assigned(DataChunk) then
              begin
                Source.ReadChunkData(DataChunk);
                SmoothArray := DataChunk.Data.SmoothGroup.GroupList;
                DataChunk.Data.SmoothGroup.GroupList := nil;
                FreeChunkData(DataChunk);
              end;

              DataChunk := FindNextChunk(FaceArrayChunk.Children, MSH_BOXMAP);
              if Assigned(DataChunk) then
              begin
                Source.ReadChunkData(DataChunk);
                for I := 0 to 5 do
                  Boxmap[I] := DataChunk.Data.MshBoxmap[I];
                UseBoxmap := True;
                FreeChunkData(DataChunk);
              end;
            end;
            FreeChunkData(Current);
          end;
        TEX_VERTS:
          begin
            Source.ReadChunkData(Current);
            ntextverts := Current.Data.TexVerts.NumCoords;
            TextArray := Current.Data.TexVerts.TextVertList;
            Current.Data.TexVerts.TextVertList := nil;
            FreeChunkData(Current);
          end;
        MESH_MATRIX:
          begin
            Source.ReadChunkData(Current);
            LocMatrix := Current.Data.MeshMatrix^;
            FreeChunkData(Current);
          end;
        MESH_TEXTURE_INFO:
          begin
            UseMapInfo := True;
            Source.ReadChunkData(Current);
            Map.MapType := Current.Data.MeshTextureInfo.MapType;
            Map.TileX := Current.Data.MeshTextureInfo.XTiling;
            Map.TileY := Current.Data.MeshTextureInfo.YTiling;
            Map.CenX := Current.Data.MeshTextureInfo.IconPos.X;
            Map.CenY := Current.Data.MeshTextureInfo.IconPos.Y;
            Map.CenZ := Current.Data.MeshTextureInfo.IconPos.Z;
            Map.Scale := Current.Data.MeshTextureInfo.IconScaling;
            Map.Matrix := Current.Data.MeshTextureInfo.XMatrix;
            Map.PW := Current.Data.MeshTextureInfo.IconWidth;
            Map.PH := Current.Data.MeshTextureInfo.IconHeight;
            Map.CH := Current.Data.MeshTextureInfo.CylIconHeight;
            FreeChunkData(Current);
          end;
        PROC_NAME:
          begin
            Source.ReadChunkData(Current);
            ProcName := Current.Data.ProcName^;
            FreeChunkData(Current);
          end;
        PROC_DATA:
          begin
            Source.ReadChunkData(Current);
            ProcSize := Current.Data.IpasData.Size;
            ProcData := Current.Data.IpasData.Data;
            Current.Data.IpasData.Data := nil;
            FreeChunkData(Current);
          end;
        MESH_COLOR:
          begin
            Source.ReadChunkData(Current);
            MeshColor := Current.Data.MeshColor^;
            FreeChunkData(Current);
          end;
      end;
      Current := Current.Sibling;
    end;

    IsHidden := Assigned(FindNextChunk(Chunk.Children, OBJ_HIDDEN));
    IsVisLofter := Assigned(FindNextChunk(Chunk.Children, OBJ_VIS_LOFTER));
    IsNoCast := Assigned(FindNextChunk(Chunk.Children, OBJ_DOESNT_CAST));
    IsMatte := Assigned(FindNextChunk(Chunk.Children, OBJ_MATTE));
    IsFast := Assigned(FindNextChunk(Chunk.Children, OBJ_FAST));
    IsFrozen := Assigned(FindNextChunk(Chunk.Children, OBJ_FROZEN));
    IsNoRcvShad := Assigned(FindNextChunk(Chunk.Children, OBJ_DONT_RCVSHADOW));
    UseProc := Assigned(FindNextChunk(Chunk.Children, OBJ_PROCEDURAL));
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetMeshByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TMesh3DS;

// fills a mesh structure from the (index)th mesh reference found in DB

var
  Current: PChunk3DS;
  I, Count: Integer;

begin
  FillChar(Result, SizeOf(Result), 0);

  if DB.TopChunk = nil then
    ShowError(Error3DS_INVALID_DATABASE);
  if (DB.TopChunk.Tag <> M3DMAGIC) and (DB.TopChunk.Tag <> CMAGIC) then
    ShowError(Error3DS_WRONG_DATABASE);

  // update the index to named objects if the list has changed recently
  UpdateNamedObjectList(Source, DB);

  // scan through the list of named objects
  Count := 0;
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    // search each named object for a mesh chunk
    Current := FindChunk(DB.ObjList.List[I].Chunk, N_TRI_OBJECT);

    // if a mesh chunk is found
    if Assigned(Current) then
    begin
      // increment the running total
      Inc(Count);
      // if this is the (index)th mesh, fill out the structure
      if (Count - 1) = Index then
      begin
        Result := GetMeshEntryChunk(Source, DB.ObjList.List[I].Chunk);
        Break;
      end;
    end;
  end;
end;

//----------------- spot and omni light handling ----------------------------------------------------------------------

function GetOmnilightCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;

var
  DLite, SpotL: PChunk3DS;
  I: Integer;

begin
  // update the index to named objects if the list has changed recently
  UpdateNamedObjectList(Source, DB);

  Result := 0;
  if DB.ObjList = nil then
    Exit;

  // scan through the list of named objects looking for lights
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    // search each object for a Light chunk
    DLite := FindChunk(DB.ObjList.List[I].chunk, N_DIRECT_LIGHT);

    // if one was found, check to see if its a spotlight
    if Assigned(DLite) then
    begin
      SpotL := FindChunk(DLite, DL_SPOTLIGHT);
      // if it isn't a spotlight then increment the count
      if SpotL = nil then
        Inc(Result);
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetSpotlightCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;

var
  DLite, SpotL: PChunk3DS;
  I: Integer;

begin
  // update the index to named objects if the list has changed recently
  UpdateNamedObjectList(Source, DB);

  Result := 0;
  if DB.ObjList = nil then
    Exit;

  // scan through the list of named objects looking for lights
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    // search each object for a Light chunk
    DLite := FindChunk(DB.ObjList.List[I].Chunk, N_DIRECT_LIGHT);

    // if one was found, check to see if its a spotlight
    if Assigned(DLite) then
    begin
      SpotL := FindChunk(DLite, DL_SPOTLIGHT);
      // if it is a spotlight then increment the count
      if Assigned(SpotL) then
        Inc(Result);
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure InitLight(var Light: TLight3DS);

// Initializes the Light structure

begin
  FillChar(Light, SizeOf(Light), 0);
  with Light do
  begin
    Name := '';
    Color.R := 0.708852;
    Color.G := 0.708852;
    Color.B := 0.708852;
    Multiplier := 1;
    Attenuation.Inner := 10;
    Attenuation.Outer := 100;
    Exclude := TStringList.Create;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseLight(Light: PLight3DS);

begin
  Light.Exclude.Free;
  Light.Exclude := nil;
  if Assigned(Light.Spot) then
  begin
    Light.Spot.Projector.Bitmap := '';
    FreeMem(Light.Spot);
  end;
  Dispose(Light);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure InitSpotLight(var SpotLight: TLight3DS);

begin
  // do the common Light initialization
  InitLight(SpotLight);
  SpotLight.Spot := AllocMem(SizeOf(TSpotLight3DS));

  with SpotLight.Spot^ do
  begin
    Target.X := 1;
    Target.Y := 1;
    Target.Z := 1;
    Hotspot := 44;
    Falloff := 45;
    Aspect := 1;

    Shadows.AType := ssUseShadowMap;
    Shadows.Bias := 1;
    Shadows.Filter := 3;
    Shadows.Mapsize := 512;
    Shadows.RayBias := 1;

    Cone.AType := csCircular;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetLightEntryChunk(const Source: TFile3DS; Chunk: PChunk3DS): TLight3DS;

// fills out the given Light structure with the Light pointed to by Chunk

var
  DLite, SpotChunk, Current: PChunk3DS;

begin
  DLite := FindNextChunk(Chunk.Children, N_DIRECT_LIGHT);
  if DLite = nil then
    ShowError(Error3DS_WRONG_OBJECT);

  DLite := FindChunk(Chunk.Children, N_DIRECT_LIGHT);
  SpotChunk := FindChunk(Chunk, DL_SPOTLIGHT);

  if Assigned(DLite) then
    with Result do
    begin
      // initilize Light
      if SpotChunk = nil then
        InitLight(Result)
      else
        InitSpotLight(Result);

      // read object name
      Source.ReadChunkData(Chunk);
      Name := Chunk.Data.NamedObject^;
      FreeChunkData(Chunk);

      // read Light postion
      Source.ReadChunkData(DLite);
      Pos := DLite.Data.NDirectLight^;

      // scan all the chunks the Light contains
      Current := DLite.Children;
      while Assigned(Current) do
      begin
        case Current.Tag of
          COLOR_F:
            begin
              Source.ReadChunkData(Current);
              Color.R := Current.Data.ColorF.Red;
              Color.G := Current.Data.ColorF.Green;
              Color.B := Current.Data.ColorF.Blue;
              FreeChunkData(Current);
            end;
          COLOR_24:
            begin
              Source.ReadChunkData(Current);
              Color.R := Current.Data.Color24.Red / 255;
              Color.G := Current.Data.Color24.Green / 255;
              Color.B := Current.Data.Color24.Blue / 255;
              FreeChunkData(Current);
            end;
          DL_MULTIPLIER:
            begin
              Source.ReadChunkData(Current);
              Multiplier := Current.Data.DlMultiplier^;
              FreeChunkData(Current);
            end;
          DL_INNER_RANGE:
            begin
              Source.ReadChunkData(Current);
              // assuming since there is a value it is on
              Attenuation.Inner := Current.Data.DlInnerRange^;
              FreeChunkData(Current);
            end;
          DL_OUTER_RANGE:
            begin
              Source.ReadChunkData(Current);
              // assuming since there is a value it is on
              Attenuation.Outer := Current.Data.DlOuterRange^;
              FreeChunkData(Current);
            end;
          DL_EXCLUDE:
            begin
              Source.ReadChunkData(Current);
              Exclude.Add(Current.Data.DlExclude^);
              FreeChunkData(Current);
            end;
          DL_OFF:
            DLOff := True;
          DL_ATTENUATE:
            Attenuation.IsOn := True;
        end;
        Current := Current.Sibling;
      end;

      // DL_SPOTLIGHT chunk
      if Assigned(SpotChunk) then
      begin
        // read spotlight data
        Source.ReadChunkData(SpotChunk);
        Spot.Target := SpotChunk.Data.DlSpotlight.SpotLightTarg;
        Spot.Hotspot := SpotChunk.Data.DlSpotlight.HotspotAngle;
        Spot.Falloff := SpotChunk.Data.DlSpotlight.FalloffAngle;

        // scan all the chunks the spotlight contains
        Current := SpotChunk.Children;
        while Assigned(Current) do
        begin
          case Current.Tag of
            DL_SPOT_ROLL:
              begin
                Source.ReadChunkData(Current);
                Spot.Roll := Current.Data.DlSpotRoll^;
                FreeChunkData(Current);
              end;
            DL_LOCAL_SHADOW:
              Spot.Shadows.Cast := True;
            DL_LOCAL_SHADOW2:
              begin
                Source.ReadChunkData(Current);
                Spot.Shadows.Bias := Current.Data.DlLocalShadow2.LocalShadowBias;
                Spot.Shadows.Filter := Current.Data.DlLocalShadow2.LocalShadowFilter;
                Spot.Shadows.Mapsize := Current.Data.DlLocalShadow2.LocalShadowMapSize;
                Spot.Shadows.Local := True;
                FreeChunkData(Current);
              end;
            DL_SHADOWED:
              Spot.Shadows.Cast := True;
            DL_SPOT_RECTANGULAR:
              Spot.Cone.AType := csRectangular;
            DL_SEE_CONE:
              Spot.Cone.Show := True;
            DL_SPOT_OVERSHOOT:
              Spot.Cone.Overshoot := True;
            DL_SPOT_ASPECT:
              begin
                Source.ReadChunkData(Current);
                Spot.Aspect := Current.Data.DlSpotAspect^;
                FreeChunkData(Current);
              end;
            DL_RAY_BIAS:
              begin
                Source.ReadChunkData(Current);
                Spot.Shadows.RayBias := Current.Data.DlRayBias^;
                FreeChunkData(Current);
              end;
            DL_RAYSHAD:
              Spot.Shadows.AType := ssUseRayTraceShadow;
            DL_SPOT_PROJECTOR:
              begin
                Source.ReadChunkData(Current);
                Spot.Projector.Bitmap := Current.Data.DlSpotProjector^;
                Spot.Projector.Use := True;
                FreeChunkData(Current);
              end;
          end;
          Current := Current.Sibling;
        end;
      end;
    end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetOmnilightByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TLight3DS;

// fills out the omnilight structure from the (index)th mesh reference found in DB

var
  LightChunk,
    SpotChunk: PChunk3DS;
  I, Count: Integer;

begin
  FillChar(Result, SizeOf(Result), 0);

  if (DB.TopChunk = nil) then
    ShowError(Error3DS_INVALID_DATABASE);
  if not (DB.TopChunk.Tag = M3DMAGIC) and not (DB.TopChunk.Tag = CMAGIC) then
    ShowError(Error3DS_WRONG_DATABASE);

  // update the list if it's changed recently
  UpdateNamedObjectList(Source, DB);

  // Scan through the List
  Count := 0;
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    // search for a Light chunk
    LightChunk := FindChunk(DB.ObjList.List[I].Chunk, N_DIRECT_LIGHT);

    // if one was found check to see if its a spot
    if Assigned(LightChunk) then
    begin
      SpotChunk := FindChunk(LightChunk, DL_SPOTLIGHT);
      // if its not a spot then increment the count
      if SpotChunk = nil then
      begin
        Inc(Count);
        // if this is the (index)th Light file out the structure
        if (Count - 1) = Index then
        begin
          Result := GetLightEntryChunk(Source, DB.ObjList.List[I].Chunk);
          Break;
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetSpotlightByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TLight3DS;

// fills out the Spot structure from the (index)th spot reference found in DB

var
  LightChunk,
    SpotChunk: PChunk3DS;
  I, Count: Integer;

begin
  FillChar(Result, SizeOf(Result), 0);

  if (DB.TopChunk = nil) then
    ShowError(Error3DS_INVALID_DATABASE);
  if not (DB.TopChunk.Tag = M3DMAGIC) and not (DB.TopChunk.Tag = CMAGIC) then
    ShowError(Error3DS_WRONG_DATABASE);

  // update the list if it's changed recently
  UpdateNamedObjectList(Source, DB);

  // Scan through the List
  Count := 0;
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    // search for a Light chunk
    LightChunk := FindChunk(DB.ObjList.List[I].Chunk, N_DIRECT_LIGHT);

    // if one was found check to see if its a spot
    if Assigned(LightChunk) then
    begin
      SpotChunk := FindChunk(LightChunk, DL_SPOTLIGHT);
      // if its not a spot then increment the count
      if Assigned(SpotChunk) then
      begin
        Inc(Count);
        // if this is the (index)th Light file out the structure
        if (Count - 1) = Index then
        begin
          Result := GetLightEntryChunk(Source, DB.ObjList.List[I].Chunk);
          Break;
        end;
      end;
    end;
  end;
end;

//----------------- camera handling -----------------------------------------------------------------------------------

procedure InitCamera(var Camera: TCamera3DS);

begin
  FillChar(Camera, SizeOf(Camera), 0);
  with Camera do
  begin
    Target.X := 1;
    Target.Y := 1;
    Target.Z := 1;
    FOV := 45;
    Ranges.CamNear := 10;
    Ranges.CamFar := 1000;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseCamera(Camera: PCamera3DS);

begin
  if Assigned(Camera) then
  begin
    Dispose(Camera);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetCameraCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;

var
  Chunk: PChunk3DS;
  I: Integer;

begin
  UpdateNamedObjectList(Source, DB);
  Result := 0;
  if Assigned(DB.ObjList) then
    for I := 0 to DB.ObjList.Count - 1 do
    begin
      Chunk := FindChunk(DB.ObjList.List[I].Chunk, N_CAMERA);
      if Assigned(Chunk) then
        Inc(Result);
    end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetCameraEntry(const Source: TFile3DS; Chunk: PChunk3DS): TCamera3DS;

var
  Current, Camera: PChunk3DS;

begin
  if Chunk.Tag <> NAMED_OBJECT then
    ShowError(Error3DS_WRONG_OBJECT);

  Camera := FindNextChunk(Chunk.Children, N_CAMERA);
  if Camera = nil then
    ShowError(Error3DS_WRONG_OBJECT);

  with Result do
  begin
    InitCamera(Result);
    Camera := FindNextChunk(Chunk.Children, N_CAMERA);

    Source.ReadChunkData(Chunk);
    Name := Chunk.Data.NamedObject^;
    FreeChunkData(Chunk);

    Source.ReadChunkData(Camera);
    Position.X := Camera.Data.NCamera.CameraPos.X;
    Position.Y := Camera.Data.NCamera.CameraPos.Y;
    Position.Z := Camera.Data.NCamera.CameraPos.Z;
    Target.X := Camera.Data.NCamera.TargetPos.X;
    Target.Y := Camera.Data.NCamera.TargetPos.Y;
    Target.Z := Camera.Data.NCamera.TargetPos.Z;
    Roll := Camera.Data.NCamera.CameraBank;
    FOV := 2400 / Camera.Data.NCamera.CameraFocalLength;
    FreeChunkData(Camera);

    Current := Camera.Children;
    while Assigned(Current) do
    begin
      case Current.Tag of
        CAM_SEE_CONE:
          ShowCone := True;
        CAM_RANGES:
          begin
            Source.ReadChunkData(Current);
            Ranges.CamNear := Current.Data.CamRanges.NearPlane;
            Ranges.CamFar := Current.Data.CamRanges.FarPlane;
            FreeChunkData(Current);
          end;
      end;
      Current := Current.Sibling;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetCameraByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TCamera3DS;

var
  Camera: PChunk3DS;
  I, Count: Integer;

begin
  FillChar(Result, SizeOf(Result), 0);

  UpdateNamedObjectList(Source, DB);

  Count := 0;
  for I := 0 to DB.ObjList.Count - 1 do
  begin
    Camera := FindChunk(DB.ObjList.List[I].Chunk, N_CAMERA);
    if Assigned(Camera) then
    begin
      Inc(Count);
      if (Count - 1) = Index then
        Result := GetCameraEntry(Source, DB.ObjList.List[I].Chunk);
    end;
  end;
end;

//----------------- common animation settings -------------------------------------------------------------------------

procedure InitKfSets(var Key: TKFSets3DS);

begin
  Key.Anim.Length := 30;
  Key.Anim.CurFrame := 0;
  Key.Seg.Use := False;
  Key.Seg.SegBegin := 0;
  Key.Seg.SegEnd := 30;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetKFSeg(TopChunk: PChunk3DS): PChunk3DS;

// all the keyframe information has to go in the appropriate segment KFDATA

begin
  // look for KFDATA
  Result := FindNextChunk(TopChunk.Children, KFDATA);
  if Result = nil then
    Result := PutGenericNode(KFDATA, TopChunk);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetKeyInfo(const Source: TFile3DS; var DB: TDatabase3DS): TKFKeyInfo3DS;

var
  KFData, KFHdrChunk, KFCTChunk: PChunk3DS;

begin
  KFData := GetKfSeg(DB.TopChunk);
  KFHdrChunk := FindNextChunk(KFData.Children, KFHDR);

  if Assigned(KFHdrChunk) then
  begin
    Source.ReadChunkData(KFHdrChunk);
    Result.Length := KFHdrChunk.Data.KFHdr.AnimLength;
    FreeChunkData(KFHdrChunk);
  end;

  KFCTChunk := FindNextChunk(KFData.Children, KFCURTIME);

  if Assigned(KFCTChunk) then
  begin
    Source.ReadChunkData(KFCTChunk);
    Result.CurFrame := KFCTChunk.Data.KFCurTime^;
    FreeChunkData(KFCTChunk);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetKFSegment(const Source: TFile3DS; var DB: TDatabase3DS): TKFSegment3DS;

var
  DataChunk, SegChunk: PChunk3DS;

begin
  DataChunk := GetKFSeg(DB.TopChunk);
  SegChunk := FindNextChunk(DataChunk.Children, KFSEG);

  if Assigned(SegChunk) then
  begin
    Source.ReadChunkData(SegChunk);
    Result.Use := True;
    Result.SegBegin := SegChunk.Data.KFSeg.First;
    Result.SegEnd := SegChunk.Data.KFSeg.Last;
    FreeChunkData(SegChunk);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetKFSettings(const Source: TFile3DS; var DB: TDatabase3DS): TKFSets3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  if DB.TopChunk = nil then
    ShowError(Error3DS_INVALID_DATABASE);
  if (DB.TopChunk.Tag <> M3DMAGIC) and (DB.TopChunk.Tag <> CMAGIC) then
    ShowError(Error3DS_WRONG_DATABASE);

  InitKFSets(Result);
  Result.Anim := GetKeyInfo(Source, DB);
  Result.Seg := GetKFSegment(Source, DB);
end;

//----------------- Camera animation ----------------------------------------------------------------------------------

procedure InitCameraMotion(var Camera: TKFCamera3DS; NewNPKeys, NewNFKeys, NewNRKeys, NewNTKeys: Cardinal);

var
  I: Integer;

begin
  with Camera do
  begin
    // free any previously allocated memory first
    if Assigned(PKeys) then
      FreeMem(PKeys);
    if Assigned(Pos) then
      FreeMem(Pos);
    if Assigned(FKeys) then
      FreeMem(FKeys);
    if Assigned(FOV) then
      FreeMem(FOV);
    if Assigned(RKeys) then
      FreeMem(RKeys);
    if Assigned(Roll) then
      FreeMem(Roll);
    if Assigned(TKeys) then
      FreeMem(TKeys);
    if Assigned(TPos) then
      FreeMem(TPos);

    FillChar(Camera, SizeOf(TKFCamera3DS), 0);
    NPKeys := NewNPKeys;
    NFKeys := NewNFKeys;
    NRKeys := NewNRKeys;
    NTKeys := NewNTKeys;

    if NPKeys <> 0 then
    begin
      NPFlag := TrackSingle3DS;

      PKeys := AllocMem(NPKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NPKeys - 1 do
        PKeys[I] := DefKeyHeader3DS;

      Pos := AllocMem(NPKeys * SizeOf(TPoint3DS));
      for I := 0 to NPKeys - 1 do
        Pos[I] := DefPoint3DS;
    end;

    if NFKeys <> 0 then
    begin
      NFFlag := TrackSingle3DS;

      FKeys := AllocMem(NFKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NFKeys - 1 do
        FKeys[I] := DefKeyHeader3DS;

      FOV := AllocMem(NFKeys * SizeOf(Single));
      for I := 0 to NFKeys - 1 do
        FOV[I] := 60;
    end;

    if NRKeys <> 0 then
    begin
      NRFlag := TrackSingle3DS;

      RKeys := AllocMem(NRKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NRKeys - 1 do
        RKeys[I] := DefKeyHeader3DS;

      Roll := AllocMem(NRKeys * SizeOf(Single));
    end;

    if NTKeys <> 0 then
    begin
      NTFlag := TrackSingle3DS;
      TFlags1 := 0;
      TFlags2 := 0;

      TKeys := AllocMem(NTKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NTKeys - 1 do
        TKeys[I] := DefKeyHeader3DS;

      TPos := AllocMem(NTKeys * SizeOf(TPoint3DS));
      for I := 0 to NTKeys - 1 do
        TPos[I] := DefPoint3DS;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseCameraMotion(Camera: PKFCamera3DS);

begin
  if Assigned(Camera) then
  begin
    with Camera^ do
    begin
      if Assigned(PKeys) then
        FreeMem(PKeys);
      if Assigned(Pos) then
        FreeMem(Pos);
      if Assigned(FKeys) then
        FreeMem(FKeys);
      if Assigned(FOV) then
        FreeMem(FOV);
      if Assigned(RKeys) then
        FreeMem(RKeys);
      if Assigned(Roll) then
        FreeMem(Roll);
      if Assigned(TKeys) then
        FreeMem(TKeys);
      if Assigned(TPos) then
        FreeMem(TPos);
    end;
    Dispose(Camera);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure GetCameraNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);

begin
  GetGenericNodeNameList(Source, DB, CAMERA_NODE_TAG, List);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetCameraNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;

begin
  Result := GetGenericNodeCount(Source, DB, CAMERA_NODE_TAG);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetParentName(const Source: TFile3DS; Chunk: PChunk3DS): string;

// get parent name if there is one

var
  NameChunk: PChunk3DS;

begin
  Result := '';
  NameChunk := FindChunk(Chunk, PARENT_NAME);
  if Assigned(NameChunk) then
  begin
    Source.ReadChunkData(NameChunk);
    Result := NameChunk.Data.NamedObject^;
    FreeChunkData(NameChunk);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetCameraMotion(const Source: TFile3DS; CamChunk, TargetChunk: PChunk3DS): TKFCamera3DS;

// gets camera keyframe information from chunk
// CamChunk    : CAMERA_NODE_TAG chunk to extract data from
// TargetChunk : TARGET_NODE_TAG chunk to extract target data from
// KFCamera    : Structure to fill in with chunk data

var
  NodeHdrChunk,
    PosChunk,
    FovChunk,
    RollChunk,
    TargetPosChunk,
    TargetHdrChunk: PChunk3DS;
  PosKeys,
    FovKeys,
    RollKeys,
    TargetKeys: Integer;

begin
  FillChar(Result, SizeOf(Result), 0);

  TargetPosChunk := nil;
  TargetHdrChunk := nil;
  PosKeys := 0;
  FovKeys := 0;
  RollKeys := 0;
  TargetKeys := 0;

  // get information from chunks
  // search children of camera Chunk
  NodeHdrChunk := FindChunk(CamChunk, NODE_HDR);
  PosChunk := FindChunk(CamChunk, POS_TRACK_TAG);
  FovChunk := FindChunk(CamChunk, FOV_TRACK_TAG);
  RollChunk := FindChunk(CamChunk, ROLL_TRACK_TAG);

  Source.ReadChunkData(NodeHdrChunk);

  if Assigned(PosChunk) then
  begin
    Source.ReadChunkData(PosChunk);
    PosKeys := PosChunk.Data.PosTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(FOVChunk) then
  begin
    Source.ReadChunkData(FOVChunk);
    FovKeys := FOVChunk.Data.FOVTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(RollChunk) then
  begin
    Source.ReadChunkData(RollChunk);
    RollKeys := RollChunk.Data.RollTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(TargetChunk) then
  begin
    TargetHdrChunk := FindChunk(TargetChunk, NODE_HDR);
    if Assigned(TargetHdrChunk) then
      Source.ReadChunkData(TargetHdrChunk);

    TargetPosChunk := FindChunk(TargetChunk, POS_TRACK_TAG);
    if Assigned(TargetPosChunk) then
    begin
      Source.ReadChunkData(TargetPosChunk);
      TargetKeys := TargetPosChunk.Data.PosTrackTag.TrackHdr.KeyCount;
    end;
  end;

  // set-up and fill-in the kfcamera structure
  InitCameraMotion(Result, PosKeys, FOVKeys, RollKeys, TargetKeys);
  with Result do
  begin
    // header Information
    if Assigned(NodeHdrChunk) then
    begin
      Name := NodeHdrChunk.Data.NodeHdr.ObjName;
      Flags1 := NodeHdrChunk.Data.NodeHdr.Flags1;
      Flags2 := NodeHdrChunk.Data.NodeHdr.Flags2;
    end;
    // parents
    Parent := GetParentName(Source, NodeHdrChunk);
    TParent := GetParentName(Source, TargetHdrChunk);

    // target information
    if TargetKeys <> 0 then
    begin
      NTFlag := TargetPosChunk.Data.PosTrackTag.TrackHdr.Flags;
      Move(TargetPosChunk.Data.PosTrackTag.KeyHdrList^, TKeys^, TargetKeys * SizeOf(TKeyHeader3DS));
      Move(TargetPosChunk.Data.PosTrackTag.PositionList^, TPos^, TargetKeys * SizeOf(TPoint3DS));
    end;
    if Assigned(TargetHdrChunk) then
    begin
      TFlags1 := TargetHdrChunk.Data.NodeHdr.Flags1;
      TFlags2 := TargetHdrChunk.Data.NodeHdr.Flags2;
    end;

    // position information
    if PosKeys <> 0 then
    begin
      NPFlag := PosChunk.Data.PosTrackTag.TrackHdr.Flags;
      Move(PosChunk.Data.PosTrackTag.KeyHdrList^, PKeys^, PosKeys * SizeOf(TKeyHeader3DS));
      Move(PosChunk.Data.PosTrackTag.PositionList^, Pos^, PosKeys * SizeOf(TPoint3DS));
    end;

    // field of view information
    if FOVKeys <> 0 then
    begin
      NFFlag := FOVChunk.Data.FOVTrackTag.TrackHdr.Flags;
      Move(FOVChunk.Data.FOVTrackTag.KeyHdrList^, FKeys^, FOVKeys * SizeOf(TKeyHeader3DS));
      Move(FOVChunk.Data.FOVTrackTag.FOVAngleList^, FOV^, FOVKeys * SizeOf(Single));
    end;

    // roll track information
    if RollKeys <> 0 then
    begin
      NRFlag := RollChunk.Data.RollTrackTag.TrackHdr.Flags;
      Move(RollChunk.Data.RollTrackTag.KeyHdrList^, RKeys^, RollKeys * SizeOf(TKeyHeader3DS));
      Move(RollChunk.Data.RollTrackTag.RollangleList^, Roll^, RollKeys * SizeOf(Single));
    end;

    // free chunk data
    if Assigned(PosChunk) then
      FreeChunkData(PosChunk);
    if Assigned(FovChunk) then
      FreeChunkData(FovChunk);
    if Assigned(RollChunk) then
      FreeChunkData(RollChunk);
    if Assigned(NodeHdrChunk) then
      FreeChunkData(NodeHdrChunk);
    if Assigned(TargetPosChunk) then
      FreeChunkData(TargetPosChunk);
    if Assigned(TargetHdrChunk) then
      FreeChunkData(TargetHdrChunk);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetCameraMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Integer): TKFCamera3DS;

var
  CameraChunk,
    TargetChunk: PChunk3DS;
  List: TStringList;

begin
  FillChar(Result, SizeOf(Result), 0);

  List := TStringList.Create;
  try
    GetCameraNodeNameList(Source, DB, List);
    if Index < List.Count then
    begin
      CameraChunk := FindNamedAndTaggedChunk(Source, DB, List[Index], CAMERA_NODE_TAG);
      if Assigned(CameraChunk) then
      begin
        TargetChunk := FindNamedAndTaggedChunk(Source, DB, List[Index], TARGET_NODE_TAG);
        Result := GetCameraMotion(Source, CameraChunk, TargetChunk);
      end;
    end;
  finally
    List.Free;
  end;
end;

//----------------- Ambient Light animation ---------------------------------------------------------------------------

procedure InitAmbientLightMotion(var Light: TKFAmbient3DS; NewNCKeys: Cardinal);

var
  I: Integer;

begin
  with Light do
  begin
    if Assigned(Color) then
      FreeMem(Color);
    if Assigned(CKeys) then
      FreeMem(CKeys);
    FillChar(Light, SizeOf(Light), 0);
    NCKeys := NewNCKeys;

    if NCKeys <> 0 then
    begin
      NCFlag := TrackSingle3DS;
      CKeys := AllocMem(NCKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NCKeys - 1 do
        CKeys[I] := DefKeyHeader3DS;

      Color := AllocMem(NCKeys * SizeOf(TFColor3DS));
      for I := 0 to NCKeys - 1 do
      begin
        Color[I].R := 1;
        Color[I].G := 1;
        Color[I].B := 1;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseAmbientLightMotion(Light: PKFAmbient3DS);

begin
  if Assigned(Light) then
  begin
    with Light^ do
    begin
      if Assigned(CKeys) then
        FreeMem(CKeys);
      if Assigned(Color) then
        FreeMem(Color);
    end;
    Dispose(Light);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetAmbientLightMotionChunk(const Source: TFile3DS; AmbientChunk: PChunk3DS): TKFAmbient3DS;

// AmbientChunk : SPOTAMBIENT_NODE_TAG chunk to extract data from
// TargetChunk  : L_TARGET_NODE_TAG chunk to extract target data from
// KFSpot       : Structure to fill in with chunk data
//
// Gets AmbientLight keyframe information from chunk
//  L_TARGET
//   ...
//  NODE_HDR
//  APP_DATA
//  COL_TRACK

var
  NodeHdrChunk,
    ColChunk: PChunk3DS;
  ColKeys: Integer;

begin
  if AmbientChunk = nil then
    ShowError(ERROR3DS_INVALID_ARG);
  FillChar(Result, SizeOf(Result), 0);

  // get information from chunks
  // search children of AmbientLight chunk
  NodeHdrChunk := FindChunk(AmbientChunk, NODE_HDR);
  ColChunk := FindChunk(AmbientChunk, COL_TRACK_TAG);

  if Assigned(NodeHdrChunk) then
    Source.ReadChunkData(NodeHdrChunk);
  if Assigned(ColChunk) then
  begin
    Source.ReadChunkData(ColChunk);
    ColKeys := ColChunk.Data.ColTrackTag.TrackHdr.KeyCount;
  end
  else
    ColKeys := 0;

  // eat-up and fill-in the PKFAmbient3DS structure
  InitAmbientLightMotion(Result, ColKeys);

  // header information
  if Assigned(NodeHdrChunk) then
  begin
    Result.Flags1 := NodeHdrChunk.Data.NodeHdr.Flags1;
    Result.Flags2 := NodeHdrChunk.Data.NodeHdr.Flags2;
  end;

  // color information
  if Assigned(ColChunk) then
  begin
    if ColKeys <> 0 then
    begin
      Result.NCFlag := ColChunk.Data.ColTrackTag.TrackHdr.Flags;
      Move(ColChunk.Data.ColTrackTag.KeyHdrList^, Result.CKeys^, ColKeys * SizeOf(TKeyHeader3DS));
      Move(ColChunk.Data.ColTrackTag.ColorList^, Result.Color^, ColKeys * SizeOf(TFColor3DS));
    end;
  end;

  // free chunk data
  if Assigned(NodeHdrChunk) then
    FreeChunkData(NodeHdrChunk);
  if Assigned(ColChunk) then
    FreeChunkData(ColChunk);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetAmbientLightMotion(const Source: TFile3DS; var DB: TDatabase3DS): TKFAmbient3DS;

// Ambient Light a special case: only one ambient node per keyframe data chunk.

var
  KFChunk, Chunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);
  // find keyframe chunk
  KFChunk := FindChunk(DB.TopChunk, KFDATA);
  if Assigned(KFChunk) then
  begin
    Chunk := FindChunk(KFChunk, AMBIENT_NODE_TAG);
    if Assigned(Chunk) then
      Result := GetAmbientLightMotionChunk(Source, Chunk);
  end;
end;

//----------------- Mesh object animation -----------------------------------------------------------------------------

procedure InitObjectMotion(var Obj: TKFMesh3DS;
  NewNPKeys, // Number of position keys
  NewNRKeys, // Number of rot keys
  NewNSKeys, // Number of scale keys
  NewNMKeys, // Number of morph keys
  NewNHKeys: Cardinal); // Number of hide keys
var
  I: Integer;

begin
  with Obj do
  begin
    if Assigned(PKeys) then
      FreeMem(PKeys);
    if Assigned(Pos) then
      FreeMem(Pos);
    if Assigned(RKeys) then
      FreeMem(RKeys);
    if Assigned(Rot) then
      FreeMem(Rot);
    if Assigned(SKeys) then
      FreeMem(SKeys);
    if Assigned(Scale) then
      FreeMem(Scale);
    if Assigned(MKeys) then
      FreeMem(MKeys);
    if Assigned(Morph) then
      FreeMem(Morph);
    if Assigned(HKeys) then
      FreeMem(HKeys);

    FillChar(Obj, SizeOf(Obj), 0);
    Pivot := DefPoint3DS;
    BoundMin := DefPoint3DS;
    BoundMax := DefPoint3DS;

    NPKeys := NewNPKeys;
    NRKeys := NewNRKeys;
    NSKeys := NewNSKeys;
    NMKeys := NewNMKeys;
    NHKeys := NewNHKeys;

    MSAngle := 24;

    if NPKeys <> 0 then
    begin
      NPFlag := TrackSingle3DS;

      PKeys := AllocMem(NPKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NPKeys - 1 do
        PKeys[I] := DefKeyHeader3DS;

      Pos := AllocMem(NPKeys * SizeOf(TPoint3DS));
      for I := 0 to NPKeys - 1 do
        Pos[I] := DefPoint3DS;
    end;

    if NRKeys <> 0 then
    begin
      NRFlag := TrackSingle3DS;

      RKeys := AllocMem(NRKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NRKeys - 1 do
        RKeys[I] := DefKeyHeader3DS;

      Rot := AllocMem(NRKeys * SizeOf(TKFRotKey3DS));
      for I := 0 to NRKeys - 1 do
        Rot[I] := DefKfRotKey3DS;
    end;

    if NSKeys <> 0 then
    begin
      NSFlag := TrackSingle3DS;

      SKeys := AllocMem(NSKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NSKeys - 1 do
        SKeys[I] := DefKeyHeader3DS;

      Scale := AllocMem(NSKeys * SizeOf(TPoint3DS));
      for I := 0 to NSKeys - 1 do
      begin
        Scale[I].X := 1;
        Scale[I].Y := 1;
        Scale[I].Z := 1;
      end;
    end;

    if NMKeys <> 0 then
    begin
      NMFlag := TrackSingle3DS;

      MKeys := AllocMem(NMKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NMKeys - 1 do
        MKeys[I] := DefKeyHeader3DS;

      Morph := AllocMem(NMKeys * SizeOf(TKFMorphKey3DS));
      for I := 0 to NMKeys - 1 do
        Morph[I] := ' ';
    end;

    if NHKeys <> 0 then
    begin
      NHFlag := TrackSingle3DS;

      HKeys := AllocMem(NHKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NMKeys - 1 do
        MKeys[I] := DefKeyHeader3DS;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseObjectMotion(Obj: PKFMesh3DS);

begin
  if Assigned(Obj) then
  begin
    with Obj^ do
    begin
      if Assigned(PKeys) then
        FreeMem(PKeys);
      if Assigned(Pos) then
        FreeMem(Pos);
      if Assigned(RKeys) then
        FreeMem(RKeys);
      if Assigned(Rot) then
        FreeMem(Rot);
      if Assigned(SKeys) then
        FreeMem(SKeys);
      if Assigned(Scale) then
        FreeMem(Scale);
      if Assigned(MKeys) then
        FreeMem(MKeys);
      if Assigned(Morph) then
        FreeMem(Morph);
      if Assigned(HKeys) then
        FreeMem(HKeys);
    end;
    Dispose(Obj);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetObjectNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Integer;

begin
  Result := GetGenericNodeCount(Source, DB, OBJECT_NODE_TAG);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure GetObjectNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);

begin
  GetGenericNodeNameList(Source, DB, OBJECT_NODE_TAG, List);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetObjectMotion(const Source: TFile3DS; MeshChunk: PChunk3DS): TKFMesh3DS;

// Gets mesh keyframe information from chunk
//
//  NODE_ID
//  NODE_HDR
//  APP_DATA
//  INSTANCE_NAME
//  PRESCALE a no-op in 3DS code
//  POS_TRACK
//  ROT_TRACK
//  SCL_TRACK
//  MORPH_TRACK
//  MORPH_SMOOTH
//  HIDE_TRACK
//
// This function is really confusing, because instead of loading the MeshChunk and its children, reading the
// data out and freeing it, the chunk structure is copied, its data is moved to the copy (so MeshChunk is then without
// any data), the copy is parsed and then it is freed. I don't know why this is so, but I don't want to change
// the way it works in case this has (or will later have) side effects I don't see yet. ml

var
  NodeHdrChunk,
    InstChunk,
    PivotChunk,
    BboxChunk,
    MsChunk,
    PosChunk,
    RotChunk,
    ScaleChunk,
    MorphChunk,
    HideChunk,
    ObjTag: PChunk3DS;

  PosKeys,
    RotKeys,
    ScaleKeys,
    MorphKeys,
    HideKeys: Integer;

  PivotData: PPivot;
  InstData: PInstanceName;
  BBoxData: PBoundBox;
  MsData: PMorphSmooth;
  PosData: PPosTrackTag;
  RotData: PRotTrackTag;
  ScaleData: PScaleTrackTag;
  MorphData: PMorphTrackTag;
  HideData: PHideTrackTag;

begin
  PosKeys := 0;
  RotKeys := 0;
  ScaleKeys := 0;
  MorphKeys := 0;
  HideKeys := 0;
  PivotData := nil;
  InstData := nil;
  BboxData := nil;
  MsData := nil;
  PosData := nil;
  RotData := nil;
  ScaleData := nil;
  MorphData := nil;
  HideData := nil;

  if MeshChunk.Tag <> OBJECT_NODE_TAG then
    ShowError(ERROR3DS_WRONG_OBJECT);

  ObjTag := CopyChunk(MeshChunk);

  // get information from chunks
  // search children of MeshLight chunk
  NodeHdrChunk := FindChunk(ObjTag, NODE_HDR);
  InstChunk := FindChunk(ObjTag, INSTANCE_NAME);
  PivotChunk := FindChunk(ObjTag, PIVOT);
  BboxChunk := FindChunk(ObjTag, BOUNDBOX);
  MsChunk := FindChunk(ObjTag, MORPH_SMOOTH);
  PosChunk := FindChunk(ObjTag, POS_TRACK_TAG);
  RotChunk := FindChunk(ObjTag, ROT_TRACK_TAG);
  ScaleChunk := FindChunk(ObjTag, SCL_TRACK_TAG);
  MorphChunk := FindChunk(ObjTag, MORPH_TRACK_TAG);
  HideChunk := FindChunk(ObjTag, HIDE_TRACK_TAG);

  Source.ReadChunkData(NodeHdrChunk);

  if Assigned(InstChunk) then
  begin
    Source.ReadChunkData(InstChunk);
    InstData := InstChunk.Data.Dummy;
    InstChunk.Data.Dummy := nil;
  end;

  if Assigned(PivotChunk) then
  begin
    Source.ReadChunkData(PivotChunk);
    PivotData := PivotChunk.Data.Dummy;
    PivotChunk.Data.Dummy := nil;
  end;

  if Assigned(BboxChunk) then
  begin
    Source.ReadChunkData(BboxChunk);
    BboxData := BboxChunk.Data.Dummy;
    BboxChunk.Data.Dummy := nil;
  end;

  if Assigned(MsChunk) then
  begin
    Source.ReadChunkData(MsChunk);
    MsData := MsChunk.Data.Dummy;
    MsChunk.Data.Dummy := nil;
  end;

  if Assigned(PosChunk) then
  begin
    Source.ReadChunkData(PosChunk);
    PosData := PosChunk.Data.Dummy;
    PosKeys := PosData.TrackHdr.KeyCount;
    PosChunk.Data.Dummy := nil;
  end;

  if Assigned(RotChunk) then
  begin
    Source.ReadChunkData(RotChunk);
    RotData := RotChunk.Data.Dummy;
    RotKeys := RotData.TrackHdr.KeyCount;
    RotChunk.Data.Dummy := nil;
  end;

  if Assigned(ScaleChunk) then
  begin
    Source.ReadChunkData(ScaleChunk);
    ScaleData := ScaleChunk.Data.Dummy;
    ScaleKeys := ScaleData.TrackHdr.KeyCount;
    ScaleChunk.Data.Dummy := nil;
  end;

  if Assigned(MorphChunk) then
  begin
    Source.ReadChunkData(MorphChunk);
    MorphData := MorphChunk.Data.Dummy;
    MorphKeys := MorphData.TrackHdr.KeyCount;
    MorphChunk.Data.Dummy := nil;
  end;

  if Assigned(HideChunk) then
  begin
    Source.ReadChunkData(HideChunk);
    HideData := HideChunk.Data.Dummy;
    HideKeys := HideData.TrackHdr.KeyCount;
    HideChunk.Data.Dummy := nil;
  end;

  // set-up and fill-in the TKFMesh3DS structure
  with Result do
  begin
    //--- header Information
    Name := NodeHdrChunk.Data.NodeHdr.ObjName;
    Flags1 := NodeHdrChunk.Data.NodeHdr.Flags1;
    Flags2 := NodeHdrChunk.Data.NodeHdr.Flags2;

    //--- get parent name if there is one
    Parent := GetParentName(Source, NodeHdrChunk);

    //--- Instance
    if Assigned(InstData) then
    begin
      Instance := InstData^;
      Name := Name + '.' + Instance;
      FreeMem(InstData);
    end
    else
      Instance := '';

    //--- Pivot
    if Assigned(PivotData) then
    begin
      Pivot := PivotData^;
      FreeMem(PivotData);
    end
    else
      Pivot := DefPoint3DS;

    //--- Bound
    if Assigned(BboxData) then
    begin
      BoundMin := BboxData.Min;
      BoundMax := BboxData.Max;
      FreeMem(BboxData);
    end
    else
    begin
      BoundMin := DefPoint3DS;
      BoundMax := DefPoint3DS;
    end;

    //--- MorphSmooth Angle
    if Assigned(MsData) then
    begin
      MSAngle := MsData^;
      FreeMem(MsData);
    end
    else
      MSAngle := 0;

    //--- Position
    NPKeys := PosKeys;
    if PosKeys <> 0 then
    begin
      PKeys := PosData.KeyHdrList;
      Pos := PosData.PositionList;
      NPFlag := PosData.TrackHdr.Flags;
      FreeMem(PosData);
    end
    else
    begin
      PKeys := nil;
      Pos := nil;
      NPFlag := 0;
    end;

    //--- Rotation
    NRKeys := RotKeys;
    if RotKeys <> 0 then
    begin
      RKeys := RotData.KeyHdrList;
      Rot := RotData.RotationList;
      NRFlag := RotData.TrackHdr.Flags;
      FreeMem(RotData);
    end
    else
    begin
      RKeys := nil;
      Rot := nil;
      NRFlag := 0;
    end;

    //--- Scale
    NSKeys := ScaleKeys;
    if ScaleKeys <> 0 then
    begin
      SKeys := ScaleData.KeyHdrList;
      Scale := ScaleData.ScaleList;
      NSFlag := ScaleData.TrackHdr.Flags;
      FreeMem(ScaleData);
    end
    else
    begin
      SKeys := nil;
      Scale := nil;
      NSFlag := 0;
    end;

    //--- Morph
    NMKeys := MorphKeys;
    if MorphKeys <> 0 then
    begin
      MKeys := MorphData.KeyHdrList;
      Morph := MorphData.MorphList;
      NMFlag := MorphData.TrackHdr.Flags;
      FreeMem(MorphData);
    end
    else
    begin
      MKeys := nil;
      Morph := nil;
      NMFlag := 0;
    end;

    NHKeys := HideKeys;
    if HideKeys <> 0 then
    begin
      HKeys := HideData.KeyHdrList;
      NHFlag := HideData.TrackHdr.Flags;
      FreeMem(HideData);
    end
    else
    begin
      HKeys := nil;
      NHFlag := 0;
    end;
  end;

  //-- ADDITIONAL Morph INFO HERE

  //--- free chunk data: only free those that arent being copied
  ReleaseChunk(ObjTag);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetObjectMotionByName(const Source: TFile3DS; var DB: TDatabase3DS; Name: string): TKFMesh3DS;

var
  ObjectChunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  ObjectChunk := FindNodeTagByNameAndType(Source, DB, Name, OBJECT_NODE_TAG);
  if Assigned(ObjectChunk) then
    Result := GetObjectMotion(Source, ObjectChunk);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetObjectMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Cardinal): TKFMesh3DS;

var
  Chunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  Chunk := FindNodeTagByIndexAndType(Source, DB, Index, OBJECT_NODE_TAG);
  if Assigned(Chunk) then
    Result := GetObjectMotion(Source, Chunk);
end;

//----------------- Omni Light animation ------------------------------------------------------------------------------

procedure InitOmnilightMotion(var Light: TKFOmni3DS; NewNPKeys, NewNCKeys: Cardinal);

var
  I: Integer;

begin
  with Light do
  begin
    if Assigned(PKeys) then
      FreeMem(PKeys);
    if Assigned(Pos) then
      FreeMem(Pos);
    if Assigned(CKeys) then
      FreeMem(CKeys);
    if Assigned(Color) then
      FreeMem(Color);

    FillChar(Light, SizeOf(Light), 0);
    NPKeys := NewNPKeys;
    NCKeys := NewNCKeys;

    if NPKeys <> 0 then
    begin
      NPFlag := TrackSingle3DS;

      PKeys := AllocMem(NPKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NPKeys - 1 do
        PKeys[I] := DefKeyHeader3DS;

      Pos := AllocMem(NPKeys * SizeOf(TPoint3DS));
      for I := 0 to NPKeys - 1 do
        Pos[I] := DefPoint3DS;
    end;

    if NCKeys <> 0 then
    begin
      NCFlag := TrackSingle3DS;

      CKeys := AllocMem(NCKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NCKeys - 1 do
        CKeys[I] := DefKeyHeader3DS;

      Color := AllocMem(NCKeys * SizeOf(TFColor3DS));
      for I := 0 to NCKeys - 1 do
      begin
        Color[I].R := 1;
        Color[I].G := 1;
        Color[I].B := 1;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseOmnilightMotion(Light: PKFOmni3DS);

begin
  if Assigned(Light) then
  begin
    with Light^ do
    begin
      if Assigned(PKeys) then
        FreeMem(PKeys);
      if Assigned(Pos) then
        FreeMem(Pos);
      if Assigned(CKeys) then
        FreeMem(CKeys);
      if Assigned(Color) then
        FreeMem(Color);
    end;
    Dispose(Light);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetOmnilightNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Cardinal;

begin
  Result := GetGenericNodeCount(Source, DB, LIGHT_NODE_TAG);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure GetOmnilightNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);

begin
  GetGenericNodeNameList(Source, DB, LIGHT_NODE_TAG, List);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetOmnilightMotion(const Source: TFile3DS; OmniChunk: PChunk3DS): TKFOmni3DS;

// Gets Omnilight keyframe information from chunk
//
//  L_TARGET
//  NODE_ID
//  NODE_HDR
//  APP_DATA
//  POS_TRACK
//  COL_TRACK
//  HOT_TRACK
//  FALL_TRACK
//  ROLL_TRACK

var
  NodeHdrChunk,
    PosChunk,
    ColChunk: PChunk3DS;
  PosKeys,
    ColKeys: Cardinal;

begin
  PosKeys := 0;
  ColKeys := 0;

  // get information from chunks
  // search children of OmniLight chunk
  NodeHdrChunk := FindChunk(OmniChunk, NODE_HDR);
  PosChunk := FindChunk(OmniChunk, POS_TRACK_TAG);
  ColChunk := FindChunk(OmniChunk, COL_TRACK_TAG);

  Source.ReadChunkData(NodeHdrChunk);

  if Assigned(PosChunk) then
  begin
    Source.ReadChunkData(PosChunk);
    PosKeys := PosChunk.Data.PosTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(ColChunk) then
  begin
    Source.ReadChunkData(ColChunk);
    ColKeys := ColChunk.Data.ColTrackTag.TrackHdr.KeyCount;
  end;

  // set-up and fill-in the TKFOmni3DS structure
  InitOmnilightMotion(Result, PosKeys, ColKeys);
  with Result do
  begin
    //--- Header Information
    Name := NodeHdrChunk.Data.NodeHdr.ObjName;
    Flags1 := NodeHdrChunk.Data.NodeHdr.Flags1;
    Flags2 := NodeHdrChunk.Data.NodeHdr.Flags2;
    Parent := GetParentName(Source, NodeHdrChunk);

    //--- Position Information
    if PosKeys <> 0 then
    begin
      NPFlag := PosChunk.Data.PosTrackTag.TrackHdr.Flags;
      Move(PosChunk.Data.PosTrackTag.KeyHdrList^, PKeys^, PosKeys * SizeOf(TKeyHeader3DS));
      Move(PosChunk.Data.PosTrackTag.PositionList^, Pos^, PosKeys * SizeOf(TPoint3DS));
    end;

    //--- Color Information
    if ColKeys <> 0 then
    begin
      NCFlag := PosChunk.Data.ColTrackTag.TrackHdr.Flags;
      Move(ColChunk.Data.ColTrackTag.KeyHdrList^, CKeys^, ColKeys * SizeOf(TKeyHeader3DS));
      Move(ColChunk.Data.ColTrackTag.ColorList^, Color^, ColKeys * SizeOf(TFColor3DS));
    end;

    //--- Free Chunk Data
    if Assigned(NodeHdrChunk) then
      FreeChunkData(NodeHdrChunk);
    if Assigned(PosChunk) then
      FreeChunkData(PosChunk);
    if Assigned(ColChunk) then
      FreeChunkData(ColChunk);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetOmnilightMotionByName(const Source: TFile3DS; var DB: TDatabase3DS; Name: string): TKFOmni3DS;

var
  Chunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  Chunk := FindNamedAndTaggedChunk(Source, DB, Name, LIGHT_NODE_TAG);
  if Assigned(Chunk) then
    Result := GetOmnilightMotion(Source, Chunk);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetOmnilightMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Cardinal): TKFOmni3DS;

var
  Chunk: PChunk3DS;
  List: TStringList;

begin
  FillChar(Result, SizeOf(Result), 0);

  List := TStringList.Create;
  try
    GetOmnilightNodeNameList(Source, DB, List);

    if Index < Cardinal(List.Count) then
    begin
      Chunk := FindNamedAndTaggedChunk(Source, DB, List[Index], LIGHT_NODE_TAG);
      if Assigned(Chunk) then
        Result := GetOmnilightMotion(Source, Chunk);
    end;
  finally
    List.Free;
  end;
end;

//----------------- Spot Light animation ------------------------------------------------------------------------------

procedure InitSpotlightMotion(var Spot: TKFSpot3DS;
  NewNPKeys, //  Number of position keys
  NewNCKeys, //  Number of Color keys
  NewNHKeys, //  Number of hot spot angle keys
  NewNFKeys, //  Number of falloff angle keys
  NewNRKeys, //  Number of roll keys
  NewNTKeys: Cardinal); //  Number of target position keys

var
  I: Cardinal;

begin
  with Spot do
  begin
    if Assigned(PKeys) then
      FreeMem(PKeys);
    if Assigned(Pos) then
      FreeMem(Pos);
    if Assigned(CKeys) then
      FreeMem(CKeys);
    if Assigned(Color) then
      FreeMem(Color);
    if Assigned(HKeys) then
      FreeMem(HKeys);
    if Assigned(Hot) then
      FreeMem(Hot);
    if Assigned(FKeys) then
      FreeMem(FKeys);
    if Assigned(Fall) then
      FreeMem(Fall);
    if Assigned(RKeys) then
      FreeMem(RKeys);
    if Assigned(Roll) then
      FreeMem(Roll);
    if Assigned(TKeys) then
      FreeMem(TKeys);
    if Assigned(TPos) then
      FreeMem(TPos);

    FillChar(Spot, SizeOf(Spot), 0);
    NPKeys := NewNPKeys;
    NCKeys := NewNCKeys;
    NFKeys := NewNFKeys;
    NTKeys := NewNTKeys;
    NHKeys := NewNHKeys;
    NRKeys := NewNRKeys;

    //--- POSITION KEYS -----------------------------------------------------
    if NPKeys <> 0 then
    begin
      NPFlag := TrackSingle3DS;

      PKeys := AllocMem(NPKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NPKeys - 1 do
        PKeys[I] := DefKeyHeader3DS;

      Pos := AllocMem(NPKeys * SizeOf(TPoint3DS));
      for I := 0 to NPKeys - 1 do
        Pos[I] := DefPoint3DS;
    end;

    //--- Color KEYS ----------------------------------------------------------
    if NCKeys <> 0 then
    begin
      NCFlag := TrackSingle3DS;
      CKeys := AllocMem(NCKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NCKeys - 1 do
        CKeys[I] := DefKeyHeader3DS;

      Color := AllocMem(NCKeys * SizeOf(TFColor3DS));
      // Initialization is unclear, even the original developers didn't know what's up.
      // They put this part in an '#ifdef LATER #endif' block. ml
      // for I := 0 to NCKeys - 1 do Color[I] := localDColor.bDefFColor3DS;
    end;

    //---Hot-Spot ANGLE KEYS---------------------------------------------------
    if NHKeys <> 0 then
    begin
      NHFlag := TrackSingle3DS;

      HKeys := AllocMem(NHKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NHKeys - 1 do
        HKeys[I] := DefKeyHeader3DS;

      Hot := AllocMem(NHKeys * SizeOf(Single));
      // default Hot Spot ange 90.0 for now, get real value later (1..174.5)
      for I := 0 to NHKeys - 1 do
        Hot[I] := 90;
    end;

    //---FALLOFF ANGLE KEYS----------------------------------------------------
    if NFKeys <> 0 then
    begin
      NFFlag := TrackSingle3DS;

      FKeys := AllocMem(NFKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NFKeys - 1 do
        FKeys[I] := DefKeyHeader3DS;

      Fall := AllocMem(NFKeys * SizeOf(Single));

      // default falloff ange 90.0 for now, get real value later (1..175)
      for I := 0 to NFKeys - 1 do
        Fall[I] := 90;
    end;

    //--- Roll KEYS ----------------------------------------------------------
    if NRKeys <> 0 then
    begin
      NRFlag := TrackSingle3DS;

      RKeys := AllocMem(NRKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NRKeys - 1 do
        RKeys[I] := DefKeyHeader3DS;

      Roll := AllocMem(NRKeys * SizeOf(Single));
      for I := 0 to NRKeys - 1 do
        Roll[I] := 0;
    end;

    //---L_TARGET Pos KEYS ------------------------------------------------
    if NTKeys <> 0 then
    begin
      NTFlag := TrackSingle3DS;

      TKeys := AllocMem(NTKeys * SizeOf(TKeyHeader3DS));
      for I := 0 to NTKeys - 1 do
        TKeys[I] := DefKeyHeader3DS;

      TPos := AllocMem(NTKeys * SizeOf(TPoint3DS));
      // default target position, 0, 0, 0  sjw fix later if necessary
      for I := 0 to NTKeys - 1 do
        TPos[I] := DefPoint3DS;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

procedure ReleaseSpotlightMotion(Spot: PKFSpot3DS);

begin
  if Assigned(Spot) then
  begin
    with Spot^ do
    begin
      if Assigned(PKeys) then
        FreeMem(PKeys);
      if Assigned(Pos) then
        FreeMem(Pos);
      if Assigned(CKeys) then
        FreeMem(CKeys);
      if Assigned(Color) then
        FreeMem(Color);
      if Assigned(HKeys) then
        FreeMem(HKeys);
      if Assigned(Hot) then
        FreeMem(Hot);
      if Assigned(FKeys) then
        FreeMem(FKeys);
      if Assigned(Fall) then
        FreeMem(Fall);
      if Assigned(RKeys) then
        FreeMem(RKeys);
      if Assigned(Roll) then
        FreeMem(Roll);
      if Assigned(TKeys) then
        FreeMem(TKeys);
      if Assigned(TPos) then
        FreeMem(TPos);
    end;
    Dispose(Spot);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetSpotlightNodeCount(const Source: TFile3DS; var DB: TDatabase3DS): Cardinal;

begin
  Result := GetGenericNodeCount(Source, DB, SPOTLIGHT_NODE_TAG);
end;

//---------------------------------------------------------------------------------------------------------------------

procedure GetSpotlightNodeNameList(const Source: TFile3DS; var DB: TDatabase3DS; List: TStringList);

begin
  GetGenericNodeNameList(Source, DB, SPOTLIGHT_NODE_TAG, List);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetSpotlightMotion(const Source: TFile3DS; SpotChunk, TargetChunk: PChunk3DS): TKFSpot3DS;

// gets Spotlight keyframe information from chunk
//
// L_TARGET
//  ...
// NODE_HDR
// APP_DATA
// POS_TRACK
// COL_TRACK
// HOT_TRACK
// FALL_TRACK
// ROLL_TRACK

var
  NodeHdrChunk,
    PosChunk,
    ColChunk,
    TargetPosChunk,
    HotChunk,
    FallChunk,
    RollChunk,
    TargetHdrChunk: PChunk3DS;
  PosKeys,
    ColKeys,
    HotKeys,
    FallKeys,
    RollKeys,
    TargetKeys: Cardinal;

begin
  TargetPosChunk := nil;
  TargetHdrChunk := nil;
  PosKeys := 0;
  ColKeys := 0;
  HotKeys := 0;
  FallKeys := 0;
  RollKeys := 0;
  TargetKeys := 0;

  // get information from chunks
  // search children of Spotlight chunk
  NodeHdrChunk := FindChunk(SpotChunk, NODE_HDR);
  PosChunk := FindChunk(SpotChunk, POS_TRACK_TAG);
  ColChunk := FindChunk(SpotChunk, COL_TRACK_TAG);
  HotChunk := FindChunk(SpotChunk, HOT_TRACK_TAG);
  FallChunk := FindChunk(SpotChunk, FALL_TRACK_TAG);
  RollChunk := FindChunk(SpotChunk, ROLL_TRACK_TAG);

  Source.ReadChunkData(NodeHdrChunk);

  if Assigned(PosChunk) then
  begin
    Source.ReadChunkData(PosChunk);
    PosKeys := PosChunk.Data.PosTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(ColChunk) then
  begin
    Source.ReadChunkData(ColChunk);
    ColKeys := ColChunk.Data.ColTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(HotChunk) then
  begin
    Source.ReadChunkData(HotChunk);
    HotKeys := HotChunk.Data.HotTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(FallChunk) then
  begin
    Source.ReadChunkData(FallChunk);
    FallKeys := FallChunk.Data.FallTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(RollChunk) then
  begin
    Source.ReadChunkData(RollChunk);
    RollKeys := RollChunk.Data.RollTrackTag.TrackHdr.KeyCount;
  end;

  if Assigned(TargetChunk) then
  begin
    TargetHdrChunk := FindChunk(TargetChunk, NODE_HDR);
    if Assigned(TargetHdrChunk) then
      Source.ReadChunkData(TargetHdrChunk);

    TargetPosChunk := FindChunk(TargetChunk, POS_TRACK_TAG);
    if Assigned(TargetPosChunk) then
    begin
      Source.ReadChunkData(TargetPosChunk);
      TargetKeys := TargetPosChunk.Data.PosTrackTag.TrackHdr.KeyCount;
    end;
  end;

  // set-up and fill-in the TKFSpot3DS structure
  InitSpotlightMotion(Result, PosKeys, ColKeys, HotKeys, FallKeys, RollKeys, TargetKeys);

  with Result do
  begin
    // header Information
    Name := NodeHdrChunk.Data.NodeHdr.ObjName;
    Flags1 := NodeHdrChunk.Data.NodeHdr.Flags1;
    Flags2 := NodeHdrChunk.Data.NodeHdr.Flags2;

    // get parent name if there is one
    Parent := GetParentName(Source, NodeHdrChunk);
    TParent := GetParentName(Source, TargetHdrChunk);

    if Assigned(TargetHdrChunk) then
    begin
      TFlags1 := TargetHdrChunk.Data.NodeHdr.Flags1;
      TFlags2 := TargetHdrChunk.Data.NodeHdr.Flags2;
    end
    else
    begin
      TFlags1 := 0;
      TFlags2 := 0;
    end;

    // target information
    if TargetKeys <> 0 then
    begin
      NTFlag := TargetPosChunk.Data.PosTrackTag.TrackHdr.Flags;
      Move(TargetPosChunk.Data.PosTrackTag.KeyHdrList^, TKeys^, TargetKeys * SizeOf(TKeyHeader3DS));
      Move(TargetPosChunk.Data.PosTrackTag.PositionList^, TPos^, TargetKeys * SizeOf(TPoint3DS));
    end;

    // position information
    if PosKeys <> 0 then
    begin
      NPFlag := PosChunk.Data.PosTrackTag.TrackHdr.Flags;
      Move(PosChunk.Data.PosTrackTag.KeyHdrList^, PKeys^, PosKeys * SizeOf(TKeyHeader3DS));
      Move(PosChunk.Data.PosTrackTag.PositionList^, Pos^, PosKeys * SizeOf(TPoint3DS));
    end;

    // color information
    if ColKeys <> 0 then
    begin
      NCFlag := ColChunk.Data.ColTrackTag.TrackHdr.Flags;
      Move(ColChunk.Data.ColTrackTag.KeyHdrList^, CKeys^, ColKeys * SizeOf(TKeyHeader3DS));
      Move(ColChunk.Data.ColTrackTag.ColorList^, Color^, ColKeys * SizeOf(TFColor3DS));
    end;

    // hot spot information
    if HotKeys <> 0 then
    begin
      NHFlag := HotChunk.Data.HotTrackTag.TrackHdr.Flags;
      Move(HotChunk.Data.HotTrackTag.KeyHdrList^, HKeys^, HotKeys * SizeOf(TKeyHeader3DS));
      Move(HotChunk.Data.HotTrackTag.HotSpotAngleList^, Hot^, HotKeys * SizeOf(Single));
    end;

    // falloff information
    if FallKeys <> 0 then
    begin
      NFFlag := FallChunk.Data.FallTrackTag.TrackHdr.Flags;
      Move(FallChunk.Data.FallTrackTag.KeyHdrList^, FKeys^, FallKeys * SizeOf(TKeyHeader3DS));
      Move(FallChunk.Data.FallTrackTag.FallOffAngleList^, Fall^, FallKeys * SizeOf(Single));
    end;

    // roll track Information
    if RollKeys <> 0 then
    begin
      NRFlag := RollChunk.Data.RollTrackTag.TrackHdr.Flags;
      Move(RollChunk.Data.RollTrackTag.KeyHdrList^, RKeys^, RollKeys * SizeOf(TKeyHeader3DS));
      Move(RollChunk.Data.RollTrackTag.RollAngleList^, Roll^, RollKeys * SizeOf(Single));
    end;
  end;

  //--- Free Chunk Data
  if Assigned(NodeHdrChunk) then
    FreeChunkData(NodeHdrChunk);
  if Assigned(PosChunk) then
    FreeChunkData(PosChunk);
  if Assigned(ColChunk) then
    FreeChunkData(ColChunk);
  if Assigned(HotChunk) then
    FreeChunkData(HotChunk);
  if Assigned(FallChunk) then
    FreeChunkData(FallChunk);
  if Assigned(RollChunk) then
    FreeChunkData(RollChunk);
  if Assigned(TargetPosChunk) then
    FreeChunkData(TargetPosChunk);
end;

//---------------------------------------------------------------------------------------------------------------------

function GetSpotlightMotionByName(const Source: TFile3DS; var DB: TDatabase3DS; Name: string): TKFSpot3DS;

var
  SpotlightChunk,
    TargetChunk: PChunk3DS;

begin
  FillChar(Result, SizeOf(Result), 0);

  SpotlightChunk := FindNamedAndTaggedChunk(Source, DB, Name, SPOTLIGHT_NODE_TAG);
  if Assigned(SpotlightChunk) then
  begin
    TargetChunk := FindNamedAndTaggedChunk(Source, DB, Name, L_TARGET_NODE_TAG);
    Result := GetSpotlightMotion(Source, SpotlightChunk, TargetChunk);
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetSpotlightMotionByIndex(const Source: TFile3DS; var DB: TDatabase3DS; Index: Cardinal): TKFSpot3DS;

var
  SpotChunk,
    TargetChunk: PChunk3DS;
  List: TStringList;

begin
  FillChar(Result, SizeOf(Result), 0);

  List := TStringList.Create;
  try
    GetSpotlightNodeNameList(Source, DB, List);
    if Index < Cardinal(List.Count) then
    begin
      SpotChunk := FindNamedAndTaggedChunk(Source, DB, List[Index], SPOTLIGHT_NODE_TAG);
      if Assigned(SpotChunk) then
      begin
        TargetChunk := FindNamedAndTaggedChunk(Source, DB, List[Index], L_TARGET_NODE_TAG);
        if Assigned(TargetChunk) then
          Result := GetSpotlightMotion(Source, SpotChunk, TargetChunk);
      end;
    end;
  finally
    List.Free;
  end;
end;

//----------------- Versioninformation --------------------------------------------------------------------------------

function GetM3dMagicRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;

// Scans the database for M3D_VERSION chunk and returnes its release

var
  Chunk: PChunk3DS;

begin
  Result := rlReleaseNotKnown;
  // If the database is a 3DS file
  if DB.TopChunk.Tag = M3DMAGIC then
  begin
    Chunk := FindChunk(DB.TopChunk, M3D_VERSION);
    if Assigned(Chunk) then
    begin
      Source.ReadChunkData(Chunk);
      case Chunk.Data.M3dVersion^ of
        1:
          Result := rlRelease1;
        2:
          Result := rlRelease2;
        3:
          Result := rlRelease3;
      else
        Result := rlReleaseNotKnown;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetMeshRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;

// Scans the database for MESH_VERSION chunk and returnes its release

var
  Chunk: PChunk3DS;

begin
  Result := rlReleaseNotKnown;
  // If the database is a 3DS file
  if (DB.TopChunk.Tag = M3DMAGIC) or (DB.TopChunk.Tag = CMAGIC) then
  begin
    Chunk := FindChunk(DB.TopChunk, MESH_VERSION);
    if Assigned(Chunk) then
    begin
      Source.ReadChunkData(Chunk);
      case Chunk.Data.MeshVersion^ of
        1:
          Result := rlRelease1;
        2:
          Result := rlRelease2;
        3:
          Result := rlRelease3;
      else
        Result := rlReleaseNotKnown;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetKfRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;

// Scans the database for KFHDR chunk and returnes its release level

var
  KFChunk,
    Chunk: PChunk3DS;

begin
  Result := rlReleaseNotKnown;
   // If the database is a 3DS file
  if (DB.TopChunk.Tag = M3DMAGIC) or (DB.TopChunk.Tag = CMAGIC) then
  begin
    KFChunk := FindChunk(DB.TopChunk, KFDATA);
    if Assigned(KFChunk) then
      Chunk := FindChunk(DB.TopChunk, KFHDR)
    else
      Chunk := nil;
    if Assigned(Chunk) then
    begin
      Source.ReadChunkData(Chunk);
      case Chunk.Data.KFHdr.Revision of
        1:
          Result := rlRelease1;
        2:
          Result := rlRelease2;
        3:
          Result := rlRelease3;
      else
        Result := rlReleaseNotKnown;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

function GetDatabaseRelease(const Source: TFile3DS; var DB: TDatabase3DS): TReleaseLevel;

begin
  case DB.TopChunk.Tag of
    M3DMAGIC:
      Result := GetM3dMagicRelease(Source, DB);
    CMAGIC:
      Result := GetMeshRelease(Source, DB);
    MLIBMAGIC:
      Result := rlRelease3;
  else
    Result := rlReleaseNotKnown;
  end;
end;

//---------------------------------------------------------------------------------------------------------------------

end.

