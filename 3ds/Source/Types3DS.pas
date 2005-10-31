unit Types3DS;

// This unit contains all of the data types used by the core routines. Most of these are only used
// with the internal database, created when a file is loaded.
//
// Last change - 03. October 1999
//
// (c) Copyright 1999, Dipl. Ing. Mike Lischke (public@lischke-online.de)

{$ALIGN ON}
{$MINENUMSIZE 4}

interface                                                               

uses Classes; // for TStringList

//---------------- commonly used structures ----------------------------------

type TDumpLevel = (dlTerseDump, dlMediumDump, dlMaximumDump);

     PWordList = ^TWordList;
     TWordList = array[0..0] of Word;

     PIntegerList = ^TIntegerList;
     TIntegerList = array[0..0] of Integer;

     PSingleList = ^TSingleList;
     TSingleList = array[0..0] of Single;

     PPoint3DS = ^TPoint3DS;                     // 3D point structure
     TPoint3DS = record
       X, Y, Z : Single;
     end;
     PPointList = ^TPointList;
     TPointList = array[0..0] of TPoint3DS;

     PFColor3DS = ^TFColor3DS;                   // RGB Color components
     TFColor3DS = record
       R, G, B: Single;
     end;
     PFColorList = ^TFColorList;
     TFColorList = array[0..0] of TFColor3DS;

     PFace3DS = ^TFace3DS;                       // Face List element
     TFace3DS = packed record
     case Boolean of
       True: (V1, V2, V3, Flag: Word);
       False: (FaceRec: array[0..3] of Word);
     end;
     PFaceList = ^TFaceList;
     TFaceList = array[0..0] of TFace3DS;

     PTexVert3DS = ^TTexVert3DS;                 // Texture assignment coordinate
     TTexVert3DS = record
       U, V: Single;
     end;
     PTexVertList = ^TTexVertList;
     TTexVertList = array[0..0] of TTexVert3DS;

     PTrackHeader3DS = ^TTrackHeader3DS;         // Global track settings
     TTrackHeader3DS = record
       Flags: Word;
       nu1, nu2,
       KeyCount: Integer;                        // Number of keys in the track
     end;

     PKeyHeader3DS = ^TKeyHeader3DS;             // Animation key settings
     TKeyHeader3DS = record
       Time: Integer;                            // Key's frame position
       RFlags: Word;                             // Spline terms used flag
       Tension: Single;                          // Flagged with $01
       Continuity: Single;                       // Flagged with $02
       Bias: Single;                             // Flagged with $04
       EaseTo: Single;                           // Flagged with $08
       EaseFrom: Single;                         // Flagged with $10
     end;
     PKeyHeaderList = ^TKeyHeaderList;
     TKeyHeaderList = array[0..0] of TKeyHeader3DS;

     PKFRotKey3DS = ^TKFRotKey3DS;               // Rotation key
     TKFRotKey3DS = record
       Angle: Single;                            // Angle of Rotation
       X, Y, Z: Single;                          // Rotation axis vector
     end;
     PKFRotKeyList = ^TKFRotKeyList;
     TKFRotKeyList = array[0..0] of TKFRotKey3DS;

     PKFMorphKey3DS = ^TKFMorphKey3DS;           // Object Morph key
     TKFMorphKey3DS = string;                    // Name of Target Morph object

     PKFMorphKeyList = ^TKFMorphKeyList;
     TKFMorphKeyList = array[0..0] of TKFMorphKey3DS;

     PChunk3DS = ^TChunk3DS;                     // internal database representation of file information

     PChunkListEntry3DS = ^TChunkListEntry3DS;   // cross reference between Name and Chunk
     TChunkListEntry3DS = record
       Name: string;                             // chunk name list
       Chunk: PChunk3DS;                         // corresponding pos
     end;
     PChunkList = ^TChunkList;
     TChunkList = array[0..0] of TChunkListEntry3DS;

     PChunkList3DS = ^TChunkList3DS;             // list of cross references
     TChunkList3DS = record
       Count: Integer;                           // number of entries in List
       List: PChunkList;                         // contents of List
     end;

     { replaced by a TStringList
     PNameList = ^TNameList;
     TNameList = array[0..0] of string;

     PNameList3DS = ^TNameList3DS;               // list of database object names
     TNameList3DS = record
                      Count  : Integer;          // how many entries are in list
                      Spaces : Integer;          // how much space for entries
                      Names  : PNameList;        // to access pointers
                    end;}

    // Database type settings
     TDBType3DS = (dbUnknown,                    // database has not been created yet
                   dbMeshFile,                   // 3D Studio .3DS file
                   dbProjectFile,                // 3D Studio .PRJ file
                   dbMaterialFile                // 3D Studio .MLI file
                  );

     PNodeList = ^TNodeList;
     TNodeList = record
       ID: SmallInt;
       Tag: Word;
       Name,
       Inst: string;
       ParentID: SmallInt;
       Next: PNodeList;
     end;

     PDatabase3DS = ^TDatabase3DS;               // file database
     TDatabase3DS = record
       TopChunk: PChunk3DS;                      // Top chunk in the file
       ObjListDirty,                             // If True, than ObjList needs to be recreated
       MatListDirty,
       NodeListDirty: Boolean;
       ObjList,                                  // Quick Cross references between names and database chunks
       MatList,
       NodeList: PChunkList3DS;
     end;

     TViewType3DS = (vtNoView3DS,
                     vtTopView3DS,
                     vtBottomView3DS,
                     vtLeftView3DS,
                     vtRightView3DS,
                     vtFrontView3DS,
                     vtBackView3DS,
                     vtUserView3DS,
                     vtCameraView3DS,
                     vtSpotlightView3DS);

     PViewSize3DS = ^TViewSize3DS;
     TViewSize3DS = record
        XPos,
        YPos,
        Width,
        Height: Word;
      end;

     POrthoView3DS = ^TOrthoView3DS;             // Used to describe Top, Bottom, left, right, front and back views
     TOrthoView3DS = record
       Center: TPoint3DS;                        // Center of orthogonal View
       Zoom: Single;                             // View Zoom factor
     end;

     PUserView3DS = ^TUserView3DS;
     TUserView3DS = record                       // Used to describe User views
       Center: TPoint3DS;                        // Center of User View
       Zoom: Single;                             // View Zoom factor
       HorAng: Single;                           // Horizontal Angle of View
       VerAng: Single;                           // Vertical Angle of View
     end;

     PCameraView3DS = ^TCameraView3DS;           // used to describe camera views
     TCameraView3DS = string;                    // name of the camera used in the view

     PViewport3DS = ^TViewport3DS;               // Viewport structure details the kind of View in a viewport
     TViewport3DS = record
       AType: TViewType3DS;                      // top, bottom, left, right, front, back, user and camera, spot
       Size: TViewSize3DS;                       // size of the viewport
       Ortho: TOrthoView3DS;                     // used for top, bottom, left, right, front, and back views
       User: TUserView3DS;                       // Used for User views
       Camera: TCameraView3DS;                   // used for camera views
     end;

     TShadowStyle3DS = (ssUseShadowMap,
                        ssUseRayTraceShadow);

     PShadowSets3DS = ^TShadowSets3DS;           // global Shadow settings
     TShadowSets3DS = record
       AType: TShadowStyle3DS;                   // either UseShadowMaps or UseRayTraceShadows
       Bias: Single;                             // Shadow Bias factor.
       RayBias: Single;                          // Shadow ray Bias factor, Used in R3
       MapSize: Smallint;                        // Shadow Map Size
       Filter: Single;                           // Shadow Filter
     end;

     PMeshSet3DS = ^TMeshSet3DS;
     TMeshSet3DS = record
       MasterScale: Single;                      // master mesh Scale factor
       Shadow: TShadowSets3DS;                   // Global Shadow settings
       AmbientLight: TFColor3DS;                 // Ambient light Color
       OConsts: TPoint3DS;                       // default object constructing axis
     end;

     TAtmosphereType3DS = (atNoAtmo,             // no active atmospherics
                           atUseFog,             // Fog atmospheric
                           atUseLayerFog,        // layer Fog atmospheric
                           atUseDistanceCue      // distance cue atmospheric
                          );
    
     TLayerFogFalloff3DS = (lfNoFall,            // no FallOff
                            lfTopFall,           // FallOff to the Top
                            lfBottomFall         // FallOff to the Bottom
                           );
    
     PFogSettings3DS = ^TFogSettings3DS;         // Fog atmosphere parameters
     TFogSettings3DS = record
       NearPlane: Single;                        // near radius of Fog effect
       NearDensity: Single;                      // near Fog Density
       FarPlane: Single;                         // far radius of Fog effect
       FarDensity: Single;                       // far Fog Density
       FogColor: TFColor3DS;                     // Color of Fog effect
       FogBgnd: Boolean;                         // "Fog background" Flag
     end;

     PLayerFogSettings3DS = ^TLayerFogSettings3DS; // layered Fog atmosphere parameters
     TLayerFogSettings3DS = record
       ZMin: Single;                             // lower bounds of Fog
       ZMax: Single;                             // upper bounds of Fog
       Density: Single;                          // Fog Density
       FogColor: TFColor3DS;                     // Fog Color
       FallOff: TLayerFogFalloff3DS;             // FallOff style
       FogBgnd: Boolean;                         // "Fog background" Flag
     end;

     PDCueSettings3DS = ^TDCueSettings3DS;       // distance cue atmosphere parameters
     TDCueSettings3DS = record
       NearPlane: Single;                        // near radius of effect
       NearDim: Single;                          // near dimming factor
       FarPlane: Single;                         // far radius of effect
       FarDim: Single;                           // far dimming factor
       DCueBgnd: Boolean;                        // effect the background Flag
     end;

     PAtmosphere3DS = ^TAtmosphere3DS;
     TAtmosphere3DS = record
       Fog: TFogSettings3DS;                     // Fog atmosphere settings
       LayerFog: TLayerFogSettings3DS;           // layered Fog atmosphere parameters
       DCue: TDCueSettings3DS;                   // distance cue atmosphere parameters
       ActiveAtmo: TAtmosphereType3DS;           // the active atmospheric
     end;

     // enumerate list of possible backgrounds used in file
     TBackgroundType3DS = (btNoBgnd,
                           btUseSolidBgnd,
                           btUseVGradientBgnd,
                           btUseBitmapBgnd);

     PBitmapBgnd3DS = ^TBitmapBgnd3DS;
     TBitmapBgnd3DS = string;                    // Name of background Bitmap

     TSolidBgnd3DS = TFColor3DS;                 // Color of Solid background

     PVGradientBgnd3DS = ^TVGradientBgnd3DS;
     TVGradientBgnd3DS = record
        GradPercent: Single;                     // placement of Mid Color band, Ranges from 0-1
        Top: TFColor3DS;                         // color of Top band
        Mid: TFColor3DS;                         // color of Mid background band
        Bottom: TFColor3DS;                      // color of Bottom band
       end;

     PBackground3DS = ^TBackground3DS;
     TBackground3DS = record
       Bitmap: TBitmapBgnd3DS;
       Solid: TSolidBgnd3DS;
       VGradient: TVGradientBgnd3DS;
       BgndUsed: TBackgroundType3DS;             // background in effect
     end;

     // used for shading field in TMaterial3DS structure
     TShadeType3DS = (stWire,
                      stFlat,
                      stGouraud,
                      stPhong,
                      stMetal);

     // used for tiling field in TBitmap3DS structure
     TTileType3DS = (ttTile,
                     ttDecal,
                     ttBoth);

     TFilterType3DS = (ftPyramidal,
                       ftSummedArea);

     TTintType3DS = (ttRGB,
                     ttAlpha,
                     ttRGBLumaTint,
                     ttAlphaTint,
                     ttRGBTint);

     // used by AddMaterial3DS
     PACubic3DS = ^TACubic3DS;
     TACubic3DS = record
       FirstFrame: Boolean;                      // True for First Frame Only
       Flat: Boolean;                            // True for Flat Mirror reflection
       Size: Integer;                            // Map resolution
       nthFrame: Integer;                        // Map update period
     end;

     // Cubic reflection Map defintion
     PBitmap3DS = ^TBitmap3DS;
     TBitmap3DS = record
       Name: string;                             // Bitmap file name
       Percent: Single;                          // Strength percentage
       Tiling: TTileType3DS;                     // Tile/Decal/Both
       IgnoreAlpha: Boolean;
       Filter: TFilterType3DS;                   // Pyramidal/Summed Area
       Blur: Single;
       Mirror: Boolean;
       Negative: Boolean;
       UScale: Single;
       VScale: Single;
       UOffset: Single;
       VOffset: Single;
       Rotation: Single;
       Source: TTintType3DS;                     // RGB/RGB Luma Tint/Alpha Tint/RGB Tint
       Tint1: TFColor3DS;
       Tint2: TFColor3DS;
       RedTint: TFColor3DS;
       GreenTint: TFColor3DS;
       BlueTint: TFColor3DS;
       DataSize: Integer;                        // Size of procedural Data
       Data: Pointer;                            // Procedural Data
     end;


     // Bit Map definition

     // Structure to all Map settings
     PMapSet3DS = ^TMapSet3DS;
     TMapSet3DS = record
       Map: TBitmap3DS;                          // The Map settings
       Mask: TBitmap3DS;                         // The Mask settings
     end;

     TRMapSet3DS = record
       Map: TBitmap3DS;                          // The Map settings
       UseAuto: Boolean;                         // True if automatic reflections are being used
       AutoMap: TACubic3DS;                      // Automatic reflection definitions
       Mask: TBitmap3DS;                         // The Mask settings
     end;

     PMaterial3DS = ^TMaterial3DS;
     TMaterial3DS = record
       Name: string;                             // Name
       Ambient: TFColor3DS;                      // Ambient Light Color
       Diffuse: TFColor3DS;                      // Diffuse Light Color
       Specular: TFColor3DS;                     // Specular Light Color
       Shininess: Single;                        // Shininess factor
       ShinStrength: Single;                     // Shininess strength
       Blur: Single;                             // Blur factor
       Transparency: Single;                     // Trasparency factor
       TransFallOff: Single;                     // Falloff factor
       SelfIllumPct: Single;                     // Self illumination percentage
       WireSize: Single;                         // Width of wireframe
       Shading: TShadeType3DS;                   // Shading type
       UseBlur: Boolean;                         // Blurring Flag
       UseFall: Boolean;                         // Transparency FallOff Flag
       TwoSided: Boolean;                        // Two sided material Flag
       SelFillum: Boolean;                       // Self illumination Flag
       Additive: Boolean;                        // Additive Transparency Flag
       UseWire: Boolean;                         // Use wireframe rendering
       UseWireAbs: Boolean;                      // Wire Size is in units, not pixels.
       FaceMap: Boolean;                         // Face mapping switch
       Soften: Boolean;                          // Soften switch
       Texture: TMapSet3DS;                      // Texture Map settings
       Texture2: TMapSet3DS;                     // Second Texture Map settings
       Opacity: TMapSet3DS;                      // Opacity Map settings
       Bump: TMapSet3DS;                         // Bump Map settings
       SpecMap: TMapSet3DS;                      // Specularity Map settings
       ShinMap: TMapSet3DS;                      // Shininess Map settings
       IllumMap: TMapSet3DS;                     // Self illumination Map settings
       Reflect: TRMapSet3DS;                     // Reflection Map settings
     end;


     // Mesh definition

     PMeshMatrix = ^TMeshMatrix;
     TMeshMatrix = array[0..11] of Single;

     // Texture Map icon placement
     PMapInfo3DS = ^TMapInfo3DS;
     TMapInfo3DS = record
       MapType: Word;                            // icon type
       TileX: Single;                            // tiling
       TileY: Single;
       CenX: Single;                             // position of center
       CenY: Single;
       CenZ: Single;
       Scale: Single;                            // icon scaling factor
       Matrix: TMeshMatrix;                      // orientation matrix
       PW: Single;                               // planar icon width
       PH: Single;                               // planar icon height
       CH: Single;                               // cylinder icon height
     end;

     // Material assignments by face
     PObjMat3DS = ^TObjMat3DS;
     TObjMat3DS = record
       Name: string;                             // material name
       NFaces: Word;                             // number of faces using material
       FaceIndex: PWordList;                     // list of faces using material
     end;

     PObjMatList = ^TObjMatList;
     TObjMatList = array[0..0] of TObjMat3DS;

     // Mesh object definition
     PMesh3DS = ^TMesh3DS;
     TMesh3DS = record
       Name: string;                             // object name
       IsHidden: Boolean;                        // hidden object flag
       IsvisLofter: Boolean;                     // lofter visibility flag
       IsMatte: Boolean;                         // matte object flag
       IsNoCast: Boolean;                        // doesn't cast shadows flag
       IsFast: Boolean;                          // fast display flag
       IsNorcvShad: boolean;                     // doesn't recieve shadows
       IsFrozen: Boolean;                        // frozen object flag
       NVertices: Word;                          // vertice count
       VertexArray: PPointList;                  // list of vertices
       NVFlags: Word;                            // number of vertex flags
       VFlagArray: PWordList;                    // list of vertex flags
       NTextVerts: Word;                         // number of texture vertices
       TextArray: PTexVertList;                  // list of texture coordinates
       UseMapInfo: Boolean;                      // for use of mapping icon information
       Map: TMapInfo3DS;                         // mapping icon info
       LocMatrix: TMeshMatrix;                   // object orientation matrix
       NFaces: Word;                             // face count
       FaceArray: PFaceList;                     // list of faces
       SmoothArray: PIntegerList;                // smoothing group assignment list
       UseBoxMap: Boolean;                       // used to indicate the use of box mapping
       BoxMap: array[0..5] of string;            // material names used in boxmapping
       MeshColor: Byte;                          // UI color assigned to the mesh
       NMats: Word;                              // assigned materials count
       MatArray: PObjMatList;                    // material assignment list
       UseProc: Boolean;                         // use animated stand-in flag
       ProcSize: Integer;                        // size of animated stand-in data
       ProcName: string;                         // name of animated stand-in procedure
       ProcData: Pointer;                        // animated stand-in data
     end;

     // Spotlight projection cone shape
     TConeStyle3DS = (csCircular,
                      csRectangular);

     // Spotlight shadow settings
     PSpotShadow3DS = ^TSpotShadow3DS;
     TSpotShadow3DS = record
       Cast: Boolean;                            // True if spotlight casts shadows
       AType: TShadowStyle3DS;                   // UseShadow or UseRayTrace
       Local: Boolean;                           // True if local shadow settings are being used
       Bias: Single;                             // shadow bias
       Filter: Single;                           // shadow filter
       MapSize: Word;                            // shadow map size
       RayBias: Single;                          // Ray tracing shadow bias
     end;

     // Cone visibility settings
     PSpotCone3DS = ^TSpotCone3DS;
     TSpotCone3DS = record
       AType: TConeStyle3DS;                     // Circular or rectangular light cone
       Show: Boolean;                            // True if cone is visible
       Overshoot: Boolean;                       // True if cone overshoot is on
     end;

     // spotlight projectio Bitmap
     PSpotProjector3DS = ^TSpotProjector3DS;
     TSpotProjector3DS = record
       Use: Boolean;                             // True if using projector
       Bitmap: string;                           // name of projector bitmap
     end;

     // spotlight settings
     PSpotLight3DS = ^TSpotLight3DS;
     TSpotLight3DS = record
       Target: TPoint3DS;                        // Spotlight Target
       Hotspot: Single;                          // Hotspot Angle
       FallOff: Single;                          // Hotspot FallOff
       Roll: Single;                             // Roll Angle
       Aspect: Single;                           // Aspect ratio
       Shadows: TSpotShadow3DS;
       Cone: TSpotCone3DS;
       Projector: TSpotProjector3DS;
     end;

     // Light Attenuation settings
     PLiteAttenuate3DS = ^TLiteAttenuate3DS;
     TLiteAttenuate3DS = record
       IsOn: Boolean;                            // True if Light Attenuation is on
       Inner: Single;                            // Inner range of Attenuation
       Outer: Single;                            // Outer range of Attenuation
     end;

     // omni and spotlight settings
     PLight3DS = ^TLight3DS;
     TLight3DS = record
       Name: string;                             // light name
       Pos: TPoint3DS;                           // light position
       Color: TFColor3DS;                        // light color
       Multiplier: Single;                       // light intensity multiplier
       DLOff: Boolean;                           // True if light is off
       Attenuation: TLiteAttenuate3DS;
       Exclude: TStringList;
       Spot: PSpotLight3DS;                      // if not nil then struct is a spotlight, else omni
     end;

     // Camera atomosphere Ranges
     PCamRanges3DS = ^TCamRanges3DS;
     TCamRanges3DS = record
       CamNear: Single;                          // Nearest effect radius
       CamFar: Single;                           // Farthest effect radius
     end;

     PCamera3DS = ^TCamera3DS;
     TCamera3DS = record
       Name: string;
       Position: TPoint3DS;
       Target: TPoint3DS;
       Roll: Single;
       FOV: Single;
       ShowCone: Boolean;
       Ranges: TCamRanges3DS;
     end;

     PKFKeyInfo3DS = ^TKFKeyInfo3DS;
     TKFKeyInfo3DS = record
        Length: Integer;
        CurFrame: Integer;
      end;

     PKFSegment3DS = ^TKFSegment3DS;
     TKFSegment3DS = record
        Use: Boolean;
        SegBegin: Integer;
        SegEnd: Integer;
      end;

     PKFSets3DS = ^TKFSets3DS;
     TKFSets3DS = record
        Anim: TKFKeyInfo3DS;
        Seg: TKFSegment3DS;
      end;

     PKFCamera3DS = ^TKFCamera3DS;
     TKFCamera3DS = record
       Name: string;                             // Name of Camera object
       Parent: string;                           // Name of Parent object
       Flags1: Word;                             // Flags field from node header -fixup later
       Flags2: Word;                             // Flags2 field from node header -fixup later
       NPKeys: Integer;                          // Number of Camera Position keys
       NPFlag: Word;                             // Loop control Flag for Camera Position keys
       PKeys: PKeyHeaderList;                    // Spline values for Camera Position keys
       Pos: PPointList;                          // Camera Position keys
       NFKeys: Integer;                          // Number of Camera FOV keys
       NFFlag: Word;                             // Loop control Flag for Camera FOV keys
       FKeys: PKeyHeaderList;                    // Spline values for Camera FOV keys
       FOV: PSingleList;                         // Camera FOV keys
       NRKeys: Integer;                          // Number of Camera Roll keys
       NRFlag: Word;                             // Loop control Flag for Camera Roll keys
       RKeys: PKeyHeaderList;                    // Spline values for Camera Roll keys
       Roll: PSingleList;                        // Camera Roll keys
       TParent: string;                          // Index of Parent object for Target
       NTKeys: Integer;                          // Number of Target Position keys
       NTFlag: Word;                             // Loop control Flag for Target Position keys
       TKeys: PKeyHeaderList;                    // Spline values for Target Position keys
       TPos: PPointList;                         // Target Position keys
       TFlags1: Word;                            // Flags field from Target node header
       TFlags2: Word;                            // Flags field from Target node header
     end;

     // Ambient Light animation
     PKFAmbient3DS = ^TKFAmbient3DS;
     TKFAmbient3DS = record
       Flags1: Word;                             // Flags field from node header -fixup later
       Flags2: Word;                             // Flags2 field from node header -fixup later
       NCKeys: Integer;                          // Number of Color keys
       NCFlag: Word;                             // Loop control Flag for Color keys
       CKeys: PKeyHeaderList;                    // Spline values for Position keys
       Color: PFColorList;                       // Color keys
     end;

     // used by ObjectMotion3DS
     PKFMesh3DS = ^TKFMesh3DS;
     TKFMesh3DS = record
       Name: string;                             // Name of mesh
       Parent: string;                           // Name of Parent object
       Flags1: Word;                             // Flags field from node header
       Flags2: Word;                             // Flags2 field from node header
       Pivot: TPoint3DS;                         // Object Pivot point
       Instance: string;                         // Object Instance Name
       BoundMin: TPoint3DS;                      // Minimum bounding box point for dummy objects
       BoundMax: TPoint3DS;                      // Maximum bounding box point for dummy objects
       NPKeys: Integer;                          // Number of Position keys
       NPFlag: Smallint;                         // Loop control Flag for Position keys
       PKeys: PKeyHeaderList;                    // Spline values for Position keys
       Pos: PPointList;                          // Mesh Position keys
       NRKeys: Integer;                          // Number of Rotation keys
       NRFlag: Smallint;                         // Loop control Flag for Rotation keys
       RKeys: PKeyHeaderList;                    // Spline values for Rotation keys
       Rot: PKFRotKeyList;                       // Rotation keys
       NSKeys: Integer;                          // Number of scaling keys
       NSFlag: Smallint;                         // Loop control Flag for scaling keys
       SKeys: PKeyHeaderList;                    // Spline values for scaling
       Scale: PPointList;                        // Mesh scaling keys
       NMKeys: Integer;                          // Number of Morph keys
       NMFlag: Smallint;                         // Loop control Flag for Morph keys
       MKeys: PKeyHeaderList;                    // Spline values for Morph keys
       Morph: PKFMorphKeyList;                   // Morph keys
       NHKeys: Integer;                          // Number of hide keys
       NHFlag: Smallint;                         // Loop control Flag for hide keys
       HKeys: PKeyHeaderList;                    // Spline values for hide keys
       MSAngle: Single;                          // Morph smoothing group Angle
     end;

     // used by OmnilightMotion3DS
     PKFOmni3DS = ^TKFOmni3DS;
     TKFOmni3DS = record
       Name: string;                             // Name of the Light object node
       Parent: string;                           // Name of the Parent object
       Flags1: Word;                             // Flags field from node header -fixup later
       Flags2: Word;                             // Flags2 field from node header -fixup later
       NPKeys: Integer;                          // Number of Position keys
       NPFlag: Word;                             // Loop control Flag for Position keys
       PKeys: PKeyHeaderList;                    // Spline values for Position keys
       Pos: PPointList;                          // Position keys
       NCKeys: Integer;                          // Number of Color keys
       NCFlag: Word;                             // Loop control Flag for Color keys
       CKeys: PKeyHeaderList;                    // Spline values for Position keys
       Color: PFColorList;                       // Color keys
     end;

     PKFSpot3DS = ^TKFSpot3DS;
     TKFSpot3DS = record
       Name: string;                             // Name of Camera object
       Parent: string;                           // Parent Name
       Flags1: Word;                             // Flags field from node header -fixup later
       Flags2: Word;                             // Flags2 field from node header -fixup later

       {$ifdef broken}
       visible: Smallint;                        // Flags to control visibility
       {$endif}

       NPKeys: Integer;                          // Number of Light Position keys
       NPFlag: Word;                             // Loop control Flag for Position keys
       PKeys: PKeyHeaderList;                    // Spline values for Light Position keys
       Pos: PPointList;                          // Light Position keys
       NCKeys: Integer;                          // Number of Color keys
       NCFlag: Word;                             // Loop control Flag Color keys
       CKeys: PKeyHeaderList;                    // Spline values for Color keys
       Color: PFColorList;                       // Color keys
       NHKeys: Integer;                          // Number of Hotspot Angle keys
       NHFlag: Word;                             // Loop control Flag for Hotspot Angle keys
       HKeys: PKeyHeaderList;                    // Spline values for Hotspot Angle keys
       Hot: PSingleList;                         // Hotspot Angle keys
       NFKeys: Integer;                          // Number of FallOff Angle keys
       NFFlag: Word;                             // Loop control Flag for FallOff Angle keys
       FKeys: PKeyHeaderList;                    // Spline values for FallOff Angle keys
       Fall: PSingleList;                        // FallOff Angle keys
       NRKeys: Integer;                          // Number of Light Roll keys
       NRFlag: Word;                             // Loop control Flag for Light Roll keys
       RKeys: PKeyHeaderList;                    // Spline values for Light Roll keys
       Roll: PSingleList;                        // Light Roll keys
       TParent: string;                          // Name of Target's Parent object
       NTKeys: Integer;                          // Number of Target Position keys
       NTFlag: Word;                             // Loop control Flag for Target Position keys
       TKeys: PKeyHeaderList;                    // Spline values for Target Position keys
       TPos: PPointList;                         // Target Position keys
       TFlags1: Word;                            // Flags field from Target node header
       TFlags2: Word;                            // Flags field from Target node header
     end;

     PXDataRaw3DS = ^TXDataRaw3DS;
     TXDataRaw3DS = record
       Size: Integer;
       Data: Pointer;
     end;

     TTargetType3DS = (ttLightTarget,ttCameraTarget);

     PM3dVersion = ^TM3dVersion;
     TM3dVersion = Cardinal;


     // inner datatypes not followed by a '3DS' to show they are locally used
     // (mostly as a part of another chunk or while collecting specific data)

     PColorF = ^TColorF;
     TColorF = record
       Red, Green, Blue: Single;
     end;

     PLinColorF = ^TLinColorF;
     TLinColorF = TColorF;

     PColor24 = ^TColor24;
     TColor24 = record
       Red, Green, Blue: Byte;
     end;

     PLinColor24 = ^TLinColor24;
     TLinColor24 = TColor24;

     PMatMapRCol = ^TMatMapRCol;
     TMatMapRCol = TLinColor24;

     PMatMapGCol = ^TMatMapGCol;
     TMatMapGCol = TMatMapRCol;

     PMatMapBCol = ^TMatMapBCol;
     TMatMapBCol = TMatMapGCol;

     PMatMapCol1 = ^TMatMapCol1;
     TMatMapCol1 = TMatMapBCol;

     PMatMapCol2 = ^TMatMapCol2;
     TMatMapCol2 = TMatMapCol1;

     PIntPercentage = ^TIntPercentage;
     TIntPercentage = SmallInt;

     PMatBumpPercent = ^TMatBumpPercent;
     TMatBumpPercent = TIntPercentage;

     PFloatPercentage = ^TFloatPercentage;
     TFloatPercentage = Single;

     PMatMapname = ^TMatMapname;
     TMatMapname = string;

     PMeshVersion = ^TMeshVersion;
     TMeshVersion = Integer;

     PMasterScale = ^TMasterScale;
     TMasterScale = Single;

     PLoShadowBias = ^TLoShadowBias;
     TLoShadowBias = Single;

     PHiShadowBias = ^THiShadowBias;
     THiShadowBias = TLoShadowBias;

     PRayBias = ^TRayBias;
     TRayBias = THiShadowBias;

     PShadowMapSize = ^TShadowMapSize;
     TShadowMapSize = SmallInt;

     PShadowSamples = ^TShadowSamples;
     TShadowSamples = SmallInt;

     PShadowRange = ^TShadowRange;
     TShadowRange = Integer;

     PShadowFilter = ^TShadowFilter;
     TShadowFilter = Single;

     POConsts = ^TOConsts;
     TOConsts = TPoint3DS;

     PBitMapName = ^TBitMapName;
     TBitMapName = string;

     PVGradient = ^TVGradient;
     TVGradient = Single;

     PFog = ^TFog;
     TFog = record
       NearPlaneDist: Single;
       NearPlaneDensity: Single;
       FarPlaneDist: Single;
       FarPlaneDensity: Single;
     end;

     PLayerFog = ^TLayerFog;
     TLayerFog = record
       ZMin: Single;
       ZMax: Single;
       Density: Single;
       AType: Cardinal;
     end;

     PDistanceCue = ^TDistanceCue;
     TDistanceCue = record
       NearPlaneDist: Single;
       NearPlaneDimming: Single;
       FarPlaneDist: Single;
       FarPlaneDimming: Single;
     end;

     PViewStandard = ^TViewStandard;
     TViewStandard = record
       ViewTargetCoord: TPoint3DS;
       ViewWidth: Single;
     end;

     PViewUser = ^TViewUser;
     TViewUser = record
       ViewTargetCoord: TPoint3DS;
       ViewWidth: Single;
       XYViewangle: Single;
       YZViewangle: Single;
       BankAngle: Single;
     end;

     PViewCamera = ^TViewCamera;
     TViewCamera = string;

     PMatName = ^TMatName;
     TMatName = string;

     PMatShading = ^TMatShading;
     TMatShading = SmallInt;

     PMatAcubic = ^TMatAcubic;
     TMatAcubic = record
       ShadeLevel: Byte;
       Antialias: Byte;
       Flags: SmallInt;
       MapSize: Cardinal;
       FrameInterval: Cardinal;
     end;

     PIpasData = ^TIpasData;
     TIpasData = record
       Size: Integer;
       Data: Pointer;
     end;

     PMatWireSize = ^TMatWireSize;
     TMatWireSize = Single;

     PMatMapTiling = ^TMatMapTiling;
     TMatMapTiling = Word;

     PMatMapTexblur = ^TMatMapTexblur;
     TMatMapTexblur = Single;

     PMatMapUScale = ^TMatMapUScale;
     TMatMapUScale = Single;

     PMatMapVScale = ^TMatMapVScale;
     TMatMapVScale = TMatMapUScale;

     PMatMapUOffset = ^TMatMapUOffset;
     TMatMapUOffset = Single;

     PMatMapVOffset = ^TMatMapVOffset;
     TMatMapVOffset = TMatMapUOffset;

     PMatMapAng = ^TMatMapAng;
     TMatMapAng = Single;

     PNamedObject = ^TNamedObject;
     TNamedObject = string;

     PPointArray = ^TPointArray;
     TPointArray = record
       Vertices: Word;
       PointList: PPointList;
     end;

     PPointFlagArray = ^TPointFlagArray;
     TPointFlagArray = record
       Flags: Word;
       FlagList: PWordList;
     end;

     PFaceArray = ^TFaceArray;
     TFaceArray = record
       Faces: Word;
       FaceList: PFaceList;
     end;

     PMshMatGroup = ^TMshMatGroup;
     TMshMatGroup = record
       MatName: string;
       Faces: Word;
       FaceList: PWordList;
     end;

     PMshBoxmap = ^TMshBoxmap;
     TMshBoxmap = array[0..5] of string;

     PSmoothGroup = ^TSmoothGroup;
     TSmoothGroup = record
       Groups: Word;
       GroupList: PIntegerList;
     end;

     PTexVerts = ^TTexVerts;
     TTexVerts = record
       NumCoords: Word;
       TextVertList: PTexVertList;
     end;

     PMeshColor = ^TMeshColor;
     TMeshColor = Byte;

     PMeshTextureInfo = ^TMeshTextureInfo;
     TMeshTextureInfo = record
       MapType: Word;
       XTiling: Single;
       YTiling: Single;
       IconPos: TPoint3DS;
       IconScaling: Single;
       XMatrix: TMeshMatrix;
       IconWidth: Single;
       IconHeight: Single;
       CylIconHeight: Single;
     end;

     PProcName = ^TProcName;
     TProcName = string;

     PNDirectLight = ^TNDirectLight;
     TNDirectLight = TPoint3DS;

     PDlExclude = ^TDlExclude;
     TDlExclude =string;

     PDlSpotlight = ^TDlSpotlight;
     TDlSpotlight = record
       SpotLightTarg: TPoint3DS;
       HotspotAngle: Single;
       FalloffAngle: Single;
     end;

     PDlOuterRange = ^TDlOuterRange;
     TDlOuterRange = Single;

     PDlInnerRange = ^TDlInnerRange;
     TDlInnerRange = TDlOuterRange;

     PDlMultiplier = ^TDlMultiplier;
     TDlMultiplier = Single;

     PDlSpotRoll = ^TDlSpotRoll;
     TDlSpotRoll = Single;

     PDlSpotAspect = ^TDlSpotAspect;
     TDlSpotAspect = Single;

     PDlSpotProjector = ^TDlSpotProjector;
     TDlSpotProjector = string;

     PDlRayBias = ^TDlRayBias;
     TDlRayBias = Single;

     PDlLocalShadow2 = ^TDlLocalShadow2;
     TDlLocalShadow2 = record
       LocalShadowBias: Single;
       LocalShadowFilter: Single;
       LocalShadowMapSize: SmallInt
     end;

     PNCamera = ^TNCamera;
     TNCamera = record
       CameraPos: TPoint3DS;
       TargetPos: TPoint3DS;
       CameraBank: Single;
       CameraFocalLength: Single;
     end;

     PCamRanges = ^TCamRanges;
     TCamRanges = record
       NearPlane: Single;
       FarPlane: Single;
     end;

     PViewportLayout = ^TViewportLayout;
     TViewportLayout = record
       Form: SmallInt;                           // 0 = single window
                                                 // 1 = 2 split verticle
                                                 // 2 = 2 split horizontal
                                                 // 3 = 4 equal squares
                                                 // 4 = 2 squares left & 1 rect right
                                                 // 5 = 1 rect at Top & 2 sqr on bot
                                                 // 6 = 1 rect left & 2 sqr right
                                                 // 7 = 2 sqr Top & 1 rect bot
                                                 // 8 = 3 split vertical
                                                 // 9 = 2 split horiz
                                                 // 10 = 3 sqr left and 1 rect right
                                                 // 11 = 1 rect left & 3 sqr. right
                                                 // Form becomes 0 during swap and its preswapped value is stored in the SwapPort field
       Top: SmallInt;                            // Active window index of 0 to 5
       Ready: SmallInt;
       WState: SmallInt;                         // 0 if no swap window, 1 if in swaped "w" state. During a swap, the old 0 window gets stored as the 4 window
       SwapWS: SmallInt;
       SwapPort: SmallInt;                       // The preswapped value from the Form field
       SwapCur: SmallInt;                        // The window index that was swapped
     end;

     PViewportSize = ^TViewportSize;
     TViewportSize = record                      // Values given for 1024x768 resolution
       XPos: Word;                               // 0
       YPos: Word;                               // 14
       Width: Word;                              // 895
       Height: Word;                             // 725
     end;

     PViewportData = ^TViewportData;
     TViewportData = record
       Flags: Word;
       AxisLockout: Word;
       WinXPos: Word;
       WinYPos: Word;
       WinWidth: Word;
       WinHeight: Word;
       View: Word;                               // 0 = No View
                                                 // 1 = Top View
                                                 // 2 = Bottom View
                                                 // 3 = Left View
                                                 // 4 = Right View
                                                 // 5 = Front View
                                                 // 6 = Back View
                                                 // 7 = User View
                                                 // 18 = Spotlight View
                                                 // 65535 = Camera View
       ZoomFactor: Single;
       Center: TPoint3DS;
       HorizAng: Single;
       VertAng: Single;
       CamName: string;
     end;

     PViewportData3 = ^TViewportData3;
     TViewportData3 = TViewportData;

     PKFHdr = ^TKFHdr;
     TKFHdr = record
       Revision: SmallInt;
       Filename: string;
       AnimLength: Integer;
     end;

     PKFId = ^TKFId;
     TKFId = SmallInt;

     PKFSeg = ^TKFSeg;
     TKFSeg = record
       First: Integer;
       Last: Integer;
     end;

     PKFCurtime = ^TKFCurtime;
     TKFCurtime = Integer;

     PNodeHdr = ^TNodeHdr;
     TNodeHdr = record
       ObjName: string;
       Flags1: Word;
       Flags2: Word;
       ParentIndex: SmallInt;
     end;

     PPivot = ^TPivot;
     TPivot = TPoint3DS;

     PInstanceName = ^TInstanceName;
     TInstanceName = string;

     PMorphSmooth = ^TMorphSmooth;
     TMorphSmooth = Single;

     PBoundBox = ^TBoundBox;
     TBoundBox = record
       Min: TPoint3DS;
       Max: TPoint3DS;
     end;

     PPosTrackTag = ^TPosTrackTag;
     TPosTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       PositionList: PPointList;
     end;

     PColTrackTag = ^TColTrackTag;
     TColTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       ColorList: PFColorList;
     end;

     PRotTrackTag = ^TRotTrackTag;
     TRotTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       RotationList: PKFRotKeyList;
     end;

     PScaleTrackTag = ^TScaleTrackTag;
     TScaleTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       ScaleList: PPointList;
     end;

     PMorphTrackTag = ^TMorphTrackTag;
     TMorphTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       MorphList: PKFMorphKeyList;
     end;

     PHideTrackTag = ^THideTrackTag;
     THideTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
     end;

     PFovTrackTag = ^TFovTrackTag;
     TFovTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       FOVAngleList: PSingleList;
     end;

     PRollTrackTag = ^TRollTrackTag;
     TRollTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       RollAngleList: PSingleList;
     end;

     PHotTrackTag = ^THotTrackTag;
     THotTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       HotspotAngleList: PSingleList;
     end;

     PFallTrackTag = ^TFallTrackTag;
     TFallTrackTag = record
       TrackHdr: TTrackHeader3DS;
       KeyHdrList: PKeyHeaderList;
       FalloffAngleList: PSingleList;
     end;

     PXDataEntry = ^TXDataEntry;
     TXDataEntry = record
       Size: Integer;
       Data: Pointer;
     end;

     PXDataAppName = ^TXDataAppName;
     TXDataAppName = string;

     PXDataString = ^TXDataString;
     TXDataString = string;

     PXDataFloat = ^TXDataFloat;
     TXDataFloat = Single;

     PXDataDouble = ^TXDataDouble;
     TXDataDouble = Double;

     PXDataShort = ^TXDataShort;
     TXDataShort = SmallInt;

     PXDataLong = ^TXDataLong;
     TXDataLong = Integer;

     PXDataVoid = ^TXDataVoid;
     TXDataVoid = Pointer;

     TReleaseLevel = (rlRelease1,
                      rlRelease2,
                      rlRelease3,
                      rlReleaseNotKnown);

     // to avoid zillion type casts, we use this variant record for
     // chunk data, effectively this defines the same pointer differently
     // for different chunk types
     // this is only possible because all types are just pointers
     TChunkData = record
       case Integer of
          0 : (ColorF: PColorF);
          1 : (LinColorF: PLinColorF);
          2 : (Color24: PColor24);
          3 : (LinColor24: PLinColor24);
          4 : (IntPercentage: PIntPercentage);
          5 : (FloatPercentage: PFloatPercentage);
          6 : (MatMapname: PMatMapname);
          7 : (M3dVersion: PM3dVersion);
          8 : (MeshVersion: PMeshVersion);
          9 : (MasterScale: PMasterScale);
         10 : (LoShadowBias: PLoShadowBias);
         11 : (ShadowFilter: PShadowFilter);
         12 : (ShadowRange: PShadowRange);
         13 : (HiShadowBias: PHiShadowBias);
         14 : (RayBias: PRayBias);
         15 : (ShadowMapSize: PShadowMapSize);
         16 : (ShadowSamples: PShadowSamples);
         17 : (OConsts: POConsts);
         18 : (BitMapName: PBitMapName);
         19 : (VGradient: PVGradient);
         20 : (Fog: PFog);
         21 : (LayerFog: PLayerFog);
         22 : (DistanceCue: PDistanceCue);
         23 : (ViewStandard: PViewStandard);
         24 : (ViewUser: PViewUser);
         25 : (ViewCamera: PViewCamera);
         26 : (MatName: PMatName);
         27 : (MatShading: PMatShading);
         28 : (MatAcubic: PMatAcubic);
         29 : (IpasData: PIpasData);
         30 : (MatWireSize: PMatWireSize);
         31 : (MatMapTiling: PMatMapTiling);
         32 : (MatMapTexblur: PMatMapTexblur);
         33 : (MatMapUScale: PMatMapUScale);
         34 : (MatMapVScale: PMatMapVScale);
         35 : (MatMapUOffset: PMatMapUOffset);
         36 : (MatMapVOffset: PMatMapVOffset);
         37 : (MatMapAng: PMatMapAng);
         38 : (MatMapCol1: PMatMapCol1);
         39 : (MatMapCol2: PMatMapCol2);
         40 : (MatMapRCol: PMatMapRCol);
         41 : (MatMapGCol: PMatMapGCol);
         42 : (MatMapBCol: PMatMapBCol);
         43 : (MatBumpPercent: PMatBumpPercent);
         44 : (NamedObject: PNamedObject);
         45 : (PointArray: PPointArray);
         46 : (PointFlagArray: PPointFlagArray);
         47 : (FaceArray: PFaceArray);
         48 : (MshMatGroup: PMshMatGroup);
         49 : (MshBoxmap: PMshBoxmap);
         50 : (SmoothGroup: PSmoothGroup);
         51 : (TexVerts: PTexVerts);
         52 : (MeshMatrix: PMeshMatrix);
         53 : (MeshColor: PMeshColor);
         54 : (MeshTextureInfo: PMeshTextureInfo);
         55 : (ProcName: PProcName);
         56 : (NDirectLight: PNDirectLight);
         57 : (DlExclude: PDlExclude);
         58 : (DlInnerRange: PDlInnerRange);
         59 : (DlOuterRange: PDlOuterRange);
         60 : (DlMultiplier: PDlMultiplier);
         61 : (DlSpotlight: PDlSpotlight);
         62 : (DlLocalShadow2: PDlLocalShadow2);
         63 : (DlSpotRoll: PDlSpotRoll);
         64 : (DlSpotAspect: PDlSpotAspect);
         65 : (DlSpotProjector: PDlSpotProjector);
         66 : (DlRayBias: PDlRayBias);
         67 : (NCamera: PNCamera);
         68 : (CamRanges: PCamRanges);
         69 : (ViewportLayout: PViewportLayout);
         70 : (ViewportSize: PViewportSize);
         71 : (ViewportData: PViewportData);
         72 : (XDataEntry: PXDataEntry);
         73 : (XDataAppName: PXDataAppName);
         74 : (XDataString: PXDataString);
         75 : (KFHdr: PKFHdr);
         76 : (KFSeg: PKFSeg);
         77 : (KFCurtime: PKFCurtime);
         78 : (KFId: PKFId);
         79 : (NodeHdr: PNodeHdr);
         80 : (Pivot: PPivot);
         81 : (InstanceName: PInstanceName);
         82 : (MorphSmooth: PMorphSmooth);
         83 : (BoundBox: PBoundBox);
         84 : (PosTrackTag: PPosTrackTag);
         85 : (ColTrackTag: PColTrackTag);
         86 : (RotTrackTag: PRotTrackTag);
         87 : (ScaleTrackTag: PScaleTrackTag);
         88 : (MorphTrackTag: PMorphTrackTag);
         89 : (FovTrackTag: PFovTrackTag);
         90 : (RollTrackTag: PRollTrackTag);
         91 : (HotTrackTag: PHotTrackTag);
         92 : (FallTrackTag: PFallTrackTag);
         93 : (HideTrackTag: PHideTrackTag);
         99 : (Dummy: Pointer);
       end;

     // finally the chunk definition
     TChunk3DS = record
       Tag: Word;                                // Type of Chunk
       Size: Cardinal;                           // Number of bytes used by Chunk
       Position: Cardinal;                       // Offset in Source file
       Data: TChunkData;                         // Memory copy of file Data
       Sibling: PChunk3DS;                       // Next Chunk in database
       Children: PChunk3DS;                      // Chunks contained within this Chunk
     end;

//---------------------------------------------------------------------------------------------------------------------

implementation

//---------------------------------------------------------------------------------------------------------------------

end.


