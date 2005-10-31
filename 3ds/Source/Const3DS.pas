unit Const3DS;

// All 3DS constant definitions used by the various routines (mainly in Utils3DS.pas). About one thousand
// defined constants herein. The guys at Autodesk must be crazy...
//
// Last Change - 03. October 1999
//
// (c) Copyright 1999, Dipl. Ing. Mike Lischke (public@lischke-online.de)

interface

uses Types3DS;

resourcestring
  // error messages
  Error3DS_NO_MEM = 'Not enough memory to complete operation.';
  Error3DS_INVALID_ARG = 'The argument passed to the function is invalid.'#13+
                         'Usually caused by a nil pointer or an out of range numeric argument.';
  Error3DS_INVALID_DATA = 'The structure passed as an argument to the function has'#13+
                          'invalid or out of range data in its fields.';
  Error3DS_INVALID_CHUNK = 'An invalid Chunk structure was encountered while reading'#13+
                           'the database. Usually caused by a corrupt database or file.';
  Error3DS_INVALID_DATABASE = 'The database passed as an argument has not be created yet.';
  Error3DS_WRONG_DATABASE = 'The database passed as an argument is the wrong kind of'#13+
                            'database for this function.';
  Error3DS_UNFOUND_CHUNK = 'The database is missing important file chunks needed to'#13+
                           'fill out the requested structure. Usually caused by a corrupt database or file.';
  Error3DS_WRONG_OBJECT = 'The Name passed to the functions exists but is not the type of object asked for.'#13+
                          'For example asking for a mesh object with the GetCameraByName3DS function.';
  Error3DS_NO_SUCH_FILE = 'The FileName passed as an argument for reading does not exist.';
  Error3DS_INIT_FAILED = 'Failed to initialize structure passed as an argument.';
  Error3DS_OPENING_FILE = 'Could not open requested file.';
  Error3DS_CLOSING_FILE = 'Could not close requested file.';
  Error3DS_READING_FILE = 'Error occured while reading file.';
  Error3DS_CREATING_DATABASE = 'Error occured while creating database.';
  Error3DS_READING_DATABASE = 'Error occured while reading database.';
  Error3DS_WRITING_DATABASE = 'Error occured while writing database.';
  Error3DS_WRITING_FILE = 'Error occured while writing file.';
  Error3DS_STRING_TOO_LONG = 'String encountered in file structure or as an argument was longer than expected.'#13+
                             'Possibly caused by an uninitialed pointer corrupt file or database.';
  Error3DS_GET_FAIL = 'Failed to get new data from database.';
  Error3DS_PUT_FAIL = 'Failed to add new data to the database.';
  Error3DS_INVALID_INDEX = 'Invalid index %d.';

// ---------------- Constants for commonly used structures --------------------

const // Flags used by the flag field of the Mesh3DS structure
      FaceCAVisable3DS = $0001;                  // flags the CA edge as visible
      FaceBCVisable3DS = $0002;                  // flags the BC edge as visible
      FaceABVisable3DS = $0004;                  // flags the AB edge as visible
      FaceUWrap3DS     = $0008;                  // flags the face as being at
                                                 // a texture coord u wrap seam
      FaceVWrap3DS     = $0010;                  // flags the face as being at
                                                 // a texture coord v wrap seam

      // flags used by the rflags field of the keyheader3DS structure
      KeyUsesTension3DS  = $01;
      KeyUsesCont3DS     = $02;
      KeyUsesBias3DS     = $04;
      KeyUsesEaseTo3DS   = $08;
      KeyUsesEaseFrom3DS = $10;

      // flags used by the track flags field
      TrackSingle3DS  = $0000;
      TrackLoops3DS   = $0003;
      TrackRepeats3DS = $0002;
      TrackLockX3DS   = $0008;
      TrackLockY3DS   = $0010;
      TrackLockZ3DS   = $0020;
      TrackNoLinkX3DS = $0100;
      TrackNoLinkY3DS = $0200;
      TrackNoLinkZ3DS = $0400;

      // Basic structure default contents for ease of initialization 
      DefPoint3DS       : TPoint3DS       = (X: 0; Y: 0; Z: 0);
      DefTextVert3DS    : TTexVert3DS     = (U: 0; V: 0);
      DefFace3DS        : TFace3DS        = (v1: 0; v2: 1; v3: 2; Flag: 0);
      DefTrackHeader3DS : TTrackHeader3DS = (Flags: 0; nu1: 0; nu2: 0; KeyCount: 1);
      DefKeyHeader3DS   : TKeyHeader3DS   = (Time: 0; RFlags: 0; Tension: 0; Continuity: 0; Bias: 0; EaseTo: 0; EaseFrom: 0);
      DefObjMat3DS      : TObjMat3DS      = (Name: ''; NFaces: 0; FaceIndex: nil);
      DefKFRotKey3DS    : TKFRotKey3DS    = (Angle: 0; X: 0; Y: 0; Z: 1);


      // Fog Flags
      LayerFogBgnd    = $100000;
      NoFalloff       = $0;
      TopFalloff      = $2;
      BottomFalloff   = $1;

      // Flags for initflags parameter
      InitNoExtras3DS    = $0000;
      InitVertexArray3DS = $0001;
      InitTextArray3DS   = $0002;
      InitFaceArray3DS   = $0004;
      InitMatArray3DS    = $0008;
      InitSmoothArray3DS = $0010;
      InitProcData3DS    = $0020;
      InitVFlagArray3DS  = $0040;

      // field codes for RelMeshObjField3ds
      RelVertexArray3ds  = $0001;
      RelTextArray3ds    = $0002;
      RelFaceArray3ds    = $0004;
      RelMatArray3ds     = $0008;
      RelSmoothArray3ds  = $0010;
      RelProcData3ds     = $0020;
      RelVFlagArray3ds   = $0040;
      RelAll3DS          = $FFFF;

      // Smoothing group Flags used in the smootharray field of the TMesh3DS structure
      Smooth01Group3DS = $00000001;
      Smooth02Group3DS = $00000002;
      Smooth03Group3DS = $00000004;
      Smooth04Group3DS = $00000008;
      Smooth05Group3DS = $00000010;
      Smooth06Group3DS = $00000020;
      Smooth07Group3DS = $00000030;
      Smooth08Group3DS = $00000080;
      Smooth09Group3DS = $00000100;
      Smooth10Group3DS = $00000200;
      Smooth11Group3DS = $00000400;
      Smooth12Group3DS = $00000800;
      Smooth13Group3DS = $00001000;
      Smooth14Group3DS = $00002000;
      Smooth15Group3DS = $00004000;
      Smooth16Group3DS = $00008000;
      Smooth17Group3DS = $00010000;
      Smooth18Group3DS = $00020000;
      Smooth19Group3DS = $00040000;
      Smooth20Group3DS = $00080000;
      Smooth21Group3DS = $00100000;
      Smooth22Group3DS = $00200000;
      Smooth23Group3DS = $00400000;
      Smooth24Group3DS = $00800000;
      Smooth25Group3DS = $01000000;
      Smooth26Group3DS = $02000000;
      Smooth27Group3DS = $04000000;
      Smooth28Group3DS = $08000000;
      Smooth29Group3DS = $10000000;
      Smooth30Group3DS = $20000000;
      Smooth31Group3DS = $40000000;
      Smooth32Group3DS = $80000000;

      DummyName3DS     = '$$$DUMMY';

      // flag settings for TKFMesh3DS, TKFOmni3DS, TKFCamera3DS, TKFAmbient and TKFSpot3DS
      // ml: these flags correspond directly to NODE_RENDOB_HIDE etc. (see below), I don't know
      //     what these duplications are for...

      // for the flags field
      KfNodeOff3DS        = 1 shl 3;
      KfHideNode3DS       = 1 shl 11;
      KfFastNode3DS       = 1 shl 12;

      // For the flags2 field 
      KfNodeHasPath3DS    = 1;
      KfNodeAutoSmooth3DS = 1 shl 1;
      KfNodeFrozen3DS     = 1 shl 2;
      KfMotionBlur3DS     = 1 shl 4;
      KfBlurBranch3DS     = 1 shl 5;
      KfMorphMtl3DS       = 1 shl 6;
      KfMorphOb3DS        = 1 shl 7;

//-------------- constants that define various value ranges for specific chunks

      // 3DS filename 
      FileNameStrMax3DS     = 512;

      // 3DS file attributes
      FileAttrStrMax3DS     = 128;

      // MASTER_SCALE chunk 
      MasterScaleMin3DS     = 0;                 // noninclusive minimum value for master scale 

      // LO_SHADOW_BIAS chunk 
      LoShadowBiasMin3DS    = 0;                 // noninclusive minimum value for low shadow bias setting 

      // HI_SHADOW_BIAS chunk
      HiShadowBiasMin3DS    = 0;                 // noninclusive minimum value for high shadow bias setting 

      // SHADOW_MAP_SIZE chunk 
      ShadowMapSizeMin3DS   = 0;                 // noninclusive minimum value for shadow map size 

      // SHADOW_SAMPLES chunk
      ShadowSamplesMin3DS   = 0;                 // noninclusive minimum value for shadow samples

      // SHADOW_RANGE chunk
      ShadowRangeMin3DS     = 0;                 // noninclusive minimum value for shadow range

      // SHADOW_FILTER chunk
      ShadowFilterMin3DS    = 1;                 // inclusive minimum value for shadow filter 
      ShadowFilterMax3DS    = 10;                // inclusive maximum value for shadow filter

      // BITMAP chunk
      BitMapStrMax3DS       = 12;                // maximum string length for filename

      // V_GRADIENT chunk 
      VGradientMin3DS       = 0;                 // minimum value for gradient midpoint
      VGradientMax3DS       = 1;                 // maximum value for gradient midpoint

      // FOG chunk 
      FogMin3DS             = 0;                 // minimum value for fogging plane density
      FogMax3DS             = 1;                 // maximum value for fogging plane density 

      // DISTANCE_CUE 
      DistanceCueMin3DS     = 0;                 // minimum value for dimming factor
      DistanceCueMax3DS     = 1;                 // maximum value for dimming factor

      // VIEW_CAMERA 
      ViewCameraStrMax3DS   = 10;                // maximum string length for filename

      // MAT_NAME
      MatNameStrMax3DS      = 16;                // maximum string length for material name

      // MAT_SHADING
      MatShadingMin3DS      = 0;                 // minimum shading value
      MatShadingMax3DS      = 3;                 // maximum shading value 

      // MAT_ACUBIC_FMIN 
      MatACubicFMin3DS      = 1;                 // minimum frame skip count
      MatACubicAMin3DS      = 0;                 // minimum reflection map aliasing
      MatACubicAMax3DS      = 3;                 // maximum reflection map aliasing

      // used with TAcubic3DS structure
      ACubicFirst3DS        = $09;
      ACubicFlat3DS         = $11;

      // POINT_ARRAY
      PointArrayMin3DS      = 3;                 // minimum number of vertices

      // FACE_ARRAY
      FaceArrayMin3DS       = 1;                 // minimum number of faces 

      // MshMatGroup3DS 
      MshMatGroupMin3DS     = 1;                 // minimum number of faces per material
      MshMatGroupStrMax3DS  = 16;                // maximim string length for MshMatGroup 

      // PROC_NAME
      ProcNameStrMax3DS     = 12;                // maximum string length for axp process 

      // DL_SPOTLIGHT 
      DLSpotlightMin3DS     = 0;                 // minimum for hotspot and falloff cones
      DLSpotlightMax3DS     = 160;               // maximum for hotspot and falloff cones

      // DL_LOCAL_SHADOW2
      DLLocalShadow2SMin3DS = 10;                // minimum shadow map size
      DLLocalShadow2SMax3DS = 4096;              // maximum shadow map size 
      DLLocalShadow2FMin3DS = 1;                 // minimum shadow map size
      DLLocalShadow2FMax3DS = 10;                // maximum shadow map size 

      // COLOR_F 
      ColorFMin3DS          = 0;                 // minimum color value in a channel
      ColorFMax3DS          = 1;                 // maximum color value in a channel

      // INT_PERCENTAGE
      IntPercentageMax3DS   = 100;               // Maximum integer percentage

      // FLOAT_PERCENTAGE
      FloatPercentageMax3DS = 1;                 // Maximum floating point percentage 

      // MAT_MAPNAME 
      MatMapNameStrMax3DS   = 12;                // Maximum map name string size

      // NAMED_OBJECT
      NamedObjectStrMax3DS  = 10;                // Maximum named object string size

      // N_CAMERA
      NCameraFOVMin3DS      = 0.00025;           // Minimum field of view for camera
      NCameraFOVMax3DS      = 160;               // Maximum field of view for camera
      NCameraFocMin3DS      = 10.7813;           // Minimum lens size for camera
      NCameraFocMax3DS      = 10000000;          // Maximum lens size for camera

      // KFHDR
      KFHdrStrMax3DS        = 12;                // Maximum keyframe header name string size

      // NODE_HDR
      NodeHdrStrMax3DS      = 10;                // Maximum node name string size

      // INSTANCE_NAME
      InstanceNameStrMax3DS = 10;                // Maximum instance name string size

      // MORPH_TRACK
      MorphTrackStrMax3DS   = 10;                // Maximum morph object name string size

      // MORPH_SMOOTH
      MorphSmoothMin3DS     = 0;                 // Minimum morph smoothing angle
      MorphSmoothMax3DS     = 360;               // Maximum morph smoothing angle

      // Keyframe Spline Limits
      KFTensionMin3DS       = -1;                // Minimum key spline tension 
      KFTensionMax3DS       = 1;                 // Maximum key spline tension
      KFContinuityMin3DS    = -1;                // Minimum key spline continuity
      KFContinuityMax3DS    = 1;                 // Maximum key spline continuity
      KFBiasMin3DS          = -1;                // Minimum key spline bias
      KFBiasMax3DS          = 1;                 // Maximum key spline bias
      KFEaseToMin3DS        = 0;                 // Minimum key spline ease to
      KFEaseToMax3DS        = 1;                 // Maximum key spline ease to
      KFEaseFromMin3DS      = 0;                 // Minimum key spline ease from
      KFEaseFromMax3DS      = 1;                 // Maximum key spline ease from

      // Track header Limits
      TrackHeaderKeysMin3DS = 1;                 // Minimum number of keys in a track

      // COL_TRACK_TAG_KEY
      ColTrackTagMin3DS     = 0;                 // Minimum color value
      ColTrackTagMax3DS     = 1;                 // Maximum color value

      // FOV_TRACK_TAG_KEY
      FOVTrackTagMin3DS     = NCameraFOVMin3DS;  // Minimum camera FOV
      FOVTrackTagMax3DS     = NCameraFOVMax3DS;  // Maximum camera FOV

      // HOT_TRACK_TAG_KEY
      HotTrackTagMin3DS     = 0;                 // Minimum hot spot angle
      HotTrackTagMax3DS     = 160;               // Maximum hot spot angle

      // FALL_TRACK_TAG_KEY
      FallTrackTagMin3DS    = 0;                 // Minimum fall off angle
      FallTrackTagMax3DS    = 160;               // Maximum fall off angle

      KNoID                 = -1;
      
      // MAT_TILING
      TEX_DECAL             = 1;
      TEX_MIRROR            = 1 shl 1;
      TEX_UNUSED1           = 1 shl 2;
      TEX_INVERT            = 1 shl 3;
      TEX_NOWRAP            = 1 shl 4;
      TEX_SAT               = 1 shl 5;           // summed area table
      TEX_ALPHA_SOURCE      = 1 shl 6;           // use ALPHA instead of RGB of map
      TEX_TINT              = 1 shl 7;           // tint for color
      TEX_DONT_USE_ALPHA    = 1 shl 8;           // don't use map alpha
      TEX_RGB_TINT          = 1 shl 9;           // do RGB color transform

      // Values for keyframer flags1
      NODE_RENDOB_HIDE      = 1 shl  2;
      NODE_OFF 	            = 1 shl  3;
      ATKEY1		    = 1 shl  4;
      ATKEY2		    = 1 shl  5;
      ATKEY3		    = 1 shl  6;
      ATKEY4		    = 1 shl  7;
      ATKEY5		    = 1 shl  8;
      ATKEYFLAGS            = ATKEY1 or ATKEY2 or ATKEY3 or ATKEY4 or ATKEY5;
      MARK_NODE	            = 1 shl  9;
      DISABLE_NODE          = 1 shl 10;
      HIDE_NODE 	    = 1 shl 11;
      FAST_NODE 	    = 1 shl 12;          // draw node quickdraw style
      PRIMARY_NODE          = 1 shl 14;          // corresponds to mesh
      NODE_CALC_PATH        = 1 shl 15;

      // Values for keyframer flags2
      NODE_HAS_PATH         = 1;
      NODE_AUTO_SMOOTH      = 1 shl 1;
      NODE_FROZEN           = 1 shl 2;
      NODE_ANI_HIDDEN       = 1 shl 3;
      NODE_MOTION_BLUR      = 1 shl 4;
      NODE_BLUR_BRANCH      = 1 shl 5;
      NODE_MORPH_MTL        = 1 shl 6;
      NODE_MORPH_OB         = 1 shl 7;

//----------------- List of all chunk IDs -------------------------------------

      NULL_CHUNK             = $0000;

      // Trick Chunk Flags For ChunkSyntax function
      ChunkType              = $0995;
      ChunkUnique            = $0996;
      NotChunk               = $0997;
      Container              = $0998;
      IsChunk                = $0999;

      // Dummy Chunk that sometimes appears in 3DS files created by prerelease 3D Studio R2
      DUMMY                  = $FFFF;

      // Trick Chunk Types For Open, Write, Close functions
      POINT_ARRAY_ENTRY      = $F110;
      POINT_FLAG_ARRAY_ENTRY = $F111;
      FACE_ARRAY_ENTRY       = $F120;
      MSH_MAT_GROUP_ENTRY    = $F130;
      TEX_VERTS_ENTRY        = $F140;
      SMOOTH_GROUP_ENTRY     = $F150;
      POS_TRACK_TAG_KEY      = $F020;
      ROT_TRACK_TAG_KEY      = $F021;
      SCL_TRACK_TAG_KEY      = $F022;
      FOV_TRACK_TAG_KEY      = $F023;
      ROLL_TRACK_TAG_KEY     = $F024;
      COL_TRACK_TAG_KEY      = $F025;
      MORPH_TRACK_TAG_KEY    = $F026;
      HOT_TRACK_TAG_KEY      = $F027;
      FALL_TRACK_TAG_KEY     = $F028;

      // 3DS file Chunk IDs
      M3DMAGIC               = $4D4D;
      SMAGIC                 = $2D2D;
      LMAGIC                 = $2D3D;
      MLIBMAGIC              = $3DAA;
      MATMAGIC               = $3DFF;
      M3D_VERSION            = $0002;
      M3D_KFVERSION          = $0005;

      // Mesh Chunk Ids
      MDATA                  = $3D3D;
      MESH_VERSION           = $3D3E;
      COLOR_F                = $0010;
      COLOR_24               = $0011;
      LIN_COLOR_24           = $0012;
      LIN_COLOR_F            = $0013;
      INT_PERCENTAGE         = $0030;
      FLOAT_PERCENTAGE       = $0031;
      MASTER_SCALE           = $0100;
      BIT_MAP                = $1100;
      USE_BIT_MAP            = $1101;
      SOLID_BGND             = $1200;
      USE_SOLID_BGND         = $1201;
      V_GRADIENT             = $1300;
      USE_V_GRADIENT         = $1301;
      LO_SHADOW_BIAS         = $1400;
      HI_SHADOW_BIAS         = $1410;
      SHADOW_MAP_SIZE        = $1420;
      SHADOW_SAMPLES         = $1430;
      SHADOW_RANGE           = $1440;
      SHADOW_FILTER          = $1450;
      RAY_BIAS               = $1460;
      O_CONSTS               = $1500;
      AMBIENT_LIGHT          = $2100;
      FOG                    = $2200;
      USE_FOG                = $2201;
      FOG_BGND               = $2210;
      DISTANCE_CUE           = $2300;
      USE_DISTANCE_CUE       = $2301;
      LAYER_FOG              = $2302;
      USE_LAYER_FOG          = $2303;
      DCUE_BGND              = $2310;
      DEFAULT_VIEW           = $3000;
      VIEW_TOP               = $3010;
      VIEW_BOTTOM            = $3020;
      VIEW_LEFT              = $3030;
      VIEW_RIGHT             = $3040;
      VIEW_FRONT             = $3050;
      VIEW_BACK              = $3060;
      VIEW_USER              = $3070;
      VIEW_CAMERA            = $3080;
      VIEW_WINDOW            = $3090;
      NAMED_OBJECT           = $4000;
      OBJ_HIDDEN             = $4010;
      OBJ_VIS_LOFTER         = $4011;
      OBJ_DOESNT_CAST        = $4012;
      OBJ_MATTE              = $4013;
      OBJ_FAST               = $4014;
      OBJ_PROCEDURAL         = $4015;
      OBJ_FROZEN             = $4016;
      OBJ_DONT_RCVSHADOW     = $4017;
      N_TRI_OBJECT           = $4100;
      POINT_ARRAY            = $4110;
      POINT_FLAG_ARRAY       = $4111;
      FACE_ARRAY             = $4120;
      MSH_MAT_GROUP          = $4130;
      OLD_MAT_GROUP          = $4131;
      TEX_VERTS              = $4140;
      SMOOTH_GROUP           = $4150;
      MESH_MATRIX            = $4160;
      MESH_COLOR             = $4165;
      MESH_TEXTURE_INFO      = $4170;
      PROC_NAME              = $4181;
      PROC_DATA              = $4182;
      MSH_BOXMAP             = $4190;
      N_D_L_OLD              = $4400;
      N_CAM_OLD              = $4500;
      N_DIRECT_LIGHT         = $4600;
      DL_SPOTLIGHT           = $4610;
      DL_OFF                 = $4620;
      DL_ATTENUATE           = $4625;
      DL_RAYSHAD             = $4627;
      DL_SHADOWED            = $4630;
      DL_LOCAL_SHADOW        = $4640;
      DL_LOCAL_SHADOW2       = $4641;
      DL_SEE_CONE            = $4650;
      DL_SPOT_RECTANGULAR    = $4651;
      DL_SPOT_OVERSHOOT      = $4652;
      DL_SPOT_PROJECTOR      = $4653;
      DL_EXCLUDE             = $4654;
      DL_RANGE               = $4655;

      // Not used in R3
      DL_SPOT_ROLL           = $4656;
      DL_SPOT_ASPECT         = $4657;
      DL_RAY_BIAS            = $4658;
      DL_INNER_RANGE         = $4659;
      DL_OUTER_RANGE         = $465A;
      DL_MULTIPLIER          = $465B;
      N_AMBIENT_LIGHT        = $4680;
      N_CAMERA               = $4700;
      CAM_SEE_CONE           = $4710;
      CAM_RANGES             = $4720;
      HIERARCHY              = $4F00;
      PARENT_OBJECT          = $4F10;
      PIVOT_OBJECT           = $4F20;
      PIVOT_LIMITS           = $4F30;
      PIVOT_ORDER            = $4F40;
      XLATE_RANGE            = $4F50;
      POLY_2D                = $5000;

      // Flags in shaper AFile that tell whether polys make up an ok shape
      SHAPE_OK               = $5010;
      SHAPE_NOT_OK           = $5011;
      SHAPE_HOOK             = $5020;
      PATH_3D                = $6000;
      PATH_MATRIX            = $6005;
      SHAPE_2D               = $6010;
      M_SCALE                = $6020;
      M_TWIST                = $6030;
      M_TEETER               = $6040;
      M_FIT                  = $6050;
      M_BEVEL                = $6060;
      XZ_CURVE               = $6070;
      YZ_CURVE               = $6080;
      INTERPCT               = $6090;
      DEFORM_LIMIT           = $60A0;

      // Flags for Modeler options
      USE_CONTOUR            = $6100;
      USE_TWEEN              = $6110;
      USE_SCALE              = $6120;
      USE_TWIST              = $6130;
      USE_TEETER             = $6140;
      USE_FIT                = $6150;
      USE_BEVEL              = $6160;

      // Viewport description chunks
      VIEWPORT_LAYOUT_OLD    = $7000;
      VIEWPORT_DATA_OLD      = $7010;
      VIEWPORT_LAYOUT        = $7001;
      VIEWPORT_DATA          = $7011;
      VIEWPORT_DATA_3        = $7012;
      VIEWPORT_SIZE          = $7020;
      NETWORK_VIEW           = $7030;

      // External Application Data
      XDATA_SECTION          = $8000;
      XDATA_ENTRY            = $8001;
      XDATA_APPNAME          = $8002;
      XDATA_STRING           = $8003;
      XDATA_FLOAT            = $8004;
      XDATA_DOUBLE           = $8005;
      XDATA_SHORT            = $8006;
      XDATA_LONG             = $8007;
      XDATA_VOID             = $8008;
      XDATA_GROUP            = $8009;
      XDATA_RFU6             = $800A;
      XDATA_RFU5             = $800B;
      XDATA_RFU4             = $800C;
      XDATA_RFU3             = $800D;
      XDATA_RFU2             = $800E;
      XDATA_RFU1             = $800F;
      PARENT_NAME            = $80F0;

      // Material Chunk IDs
      MAT_ENTRY              = $AFFF;
      MAT_NAME               = $A000;
      MAT_AMBIENT            = $A010;
      MAT_DIFFUSE            = $A020;
      MAT_SPECULAR           = $A030;
      MAT_SHININESS          = $A040;
      MAT_SHIN2PCT           = $A041;
      MAT_SHIN3PCT           = $A042;
      MAT_TRANSPARENCY       = $A050;
      MAT_XPFALL             = $A052;
      MAT_REFBLUR            = $A053;
      MAT_SELF_ILLUM         = $A080;
      MAT_TWO_SIDE           = $A081;
      MAT_DECAL              = $A082;
      MAT_ADDITIVE           = $A083;
      MAT_SELF_ILPCT         = $A084;
      MAT_WIRE               = $A085;
      MAT_SUPERSMP           = $A086;
      MAT_WIRESIZE           = $A087;
      MAT_FACEMAP            = $A088;
      MAT_XPFALLIN           = $A08A;
      MAT_PHONGSOFT          = $A08C;
      MAT_WIREABS            = $A08E;
      MAT_SHADING            = $A100;
      MAT_TEXMAP             = $A200;
      MAT_OPACMAP            = $A210;
      MAT_REFLMAP            = $A220;
      MAT_BUMPMAP            = $A230;
      MAT_SPECMAP            = $A204;
      MAT_USE_XPFALL         = $A240;
      MAT_USE_REFBLUR        = $A250;
      MAT_BUMP_PERCENT       = $A252;
      MAT_MAPNAME            = $A300;
      MAT_ACUBIC             = $A310;
      MAT_SXP_TEXT_DATA      = $A320;
      MAT_SXP_TEXT2_DATA     = $A321;
      MAT_SXP_OPAC_DATA      = $A322;
      MAT_SXP_BUMP_DATA      = $A324;
      MAT_SXP_SPEC_DATA      = $A325;
      MAT_SXP_SHIN_DATA      = $A326;
      MAT_SXP_SELFI_DATA     = $A328;
      MAT_SXP_TEXT_MASKDATA  = $A32A;
      MAT_SXP_TEXT2_MASKDATA = $A32C;
      MAT_SXP_OPAC_MASKDATA  = $A32E;
      MAT_SXP_BUMP_MASKDATA  = $A330;
      MAT_SXP_SPEC_MASKDATA  = $A332;
      MAT_SXP_SHIN_MASKDATA  = $A334;
      MAT_SXP_SELFI_MASKDATA = $A336;
      MAT_SXP_REFL_MASKDATA  = $A338;
      MAT_TEX2MAP            = $A33A;
      MAT_SHINMAP            = $A33C;
      MAT_SELFIMAP           = $A33D;
      MAT_TEXMASK            = $A33E;
      MAT_TEX2MASK           = $A340;
      MAT_OPACMASK           = $A342;
      MAT_BUMPMASK           = $A344;
      MAT_SHINMASK           = $A346;
      MAT_SPECMASK           = $A348;
      MAT_SELFIMASK          = $A34A;
      MAT_REFLMASK           = $A34C;
      MAT_MAP_TILINGOLD      = $A350;
      MAT_MAP_TILING         = $A351;
      MAT_MAP_TEXBLUR_OLD    = $A352;
      MAT_MAP_TEXBLUR        = $A353;
      MAT_MAP_USCALE         = $A354;
      MAT_MAP_VSCALE         = $A356;
      MAT_MAP_UOFFSET        = $A358;
      MAT_MAP_VOFFSET        = $A35A;
      MAT_MAP_ANG            = $A35C;
      MAT_MAP_COL1           = $A360;
      MAT_MAP_COL2           = $A362;
      MAT_MAP_RCOL           = $A364;
      MAT_MAP_GCOL           = $A366;
      MAT_MAP_BCOL           = $A368;

      // Keyframe Chunk IDs
      KFDATA                 = $B000;
      KFHDR                  = $B00A;
      AMBIENT_NODE_TAG       = $B001;
      OBJECT_NODE_TAG        = $B002;
      CAMERA_NODE_TAG        = $B003;
      TARGET_NODE_TAG        = $B004;
      LIGHT_NODE_TAG         = $B005;
      L_TARGET_NODE_TAG      = $B006;
      SPOTLIGHT_NODE_TAG     = $B007;
      KFSEG                  = $B008;
      KFCURTIME              = $B009;
      NODE_HDR               = $B010;
      INSTANCE_NAME          = $B011;
      PRESCALE               = $B012;
      PIVOT                  = $B013;
      BOUNDBOX               = $B014;
      MORPH_SMOOTH           = $B015;
      POS_TRACK_TAG          = $B020;
      ROT_TRACK_TAG          = $B021;
      SCL_TRACK_TAG          = $B022;
      FOV_TRACK_TAG          = $B023;
      ROLL_TRACK_TAG         = $B024;
      COL_TRACK_TAG          = $B025;
      MORPH_TRACK_TAG        = $B026;
      HOT_TRACK_TAG          = $B027;
      FALL_TRACK_TAG         = $B028;
      HIDE_TRACK_TAG         = $B029;
      NODE_ID                = $B030;
      CMAGIC                 = $C23D;
      C_MDRAWER              = $C010;
      C_TDRAWER              = $C020;
      C_SHPDRAWER            = $C030;
      C_MODDRAWER            = $C040;
      C_RIPDRAWER            = $C050;
      C_TXDRAWER             = $C060;
      C_PDRAWER              = $C062;
      C_MTLDRAWER            = $C064;
      C_FLIDRAWER            = $C066;
      C_CUBDRAWER            = $C067;
      C_MFILE                = $C070;
      C_SHPFILE              = $C080;
      C_MODFILE              = $C090;
      C_RIPFILE              = $C0A0;
      C_TXFILE               = $C0B0;
      C_PFILE                = $C0B2;
      C_MTLFILE              = $C0B4;
      C_FLIFILE              = $C0B6;
      C_PALFILE              = $C0B8;
      C_TX_STRING            = $C0C0;
      C_CONSTS               = $C0D0;
      C_SNAPS                = $C0E0;
      C_GRIDS                = $C0F0;
      C_ASNAPS               = $C100;
      C_GRID_RANGE           = $C110;
      C_RENDTYPE             = $C120;
      C_PROGMODE             = $C130;
      C_PREVMODE             = $C140;
      C_MODWMODE             = $C150;
      C_MODMODEL             = $C160;
      C_ALL_LINES            = $C170;
      C_BACK_TYPE            = $C180;
      C_MD_CS                = $C190;
      C_MD_CE                = $C1A0;
      C_MD_SML               = $C1B0;
      C_MD_SMW               = $C1C0;
      C_LOFT_WITH_TEXTURE    = $C1C3;
      C_LOFT_L_REPEAT        = $C1C4;
      C_LOFT_W_REPEAT        = $C1C5;
      C_LOFT_UV_NORMALIZE    = $C1C6;
      C_WELD_LOFT            = $C1C7;
      C_MD_PDET              = $C1D0;
      C_MD_SDET              = $C1E0;
      C_RGB_RMODE            = $C1F0;
      C_RGB_HIDE             = $C200;
      C_RGB_MAPSW            = $C202;
      C_RGB_TWOSIDE          = $C204;
      C_RGB_SHADOW           = $C208;
      C_RGB_AA               = $C210;
      C_RGB_OVW              = $C220;
      C_RGB_OVH              = $C230;
      C_RGB_PICTYPE          = $C240;
      C_RGB_OUTPUT           = $C250;
      C_RGB_TODISK           = $C253;
      C_RGB_COMPRESS         = $C254;
      C_JPEG_COMPRESSION     = $C255;
      C_RGB_DISPDEV          = $C256;
      C_RGB_HARDDEV          = $C259;
      C_RGB_PATH             = $C25A;
      C_BITMAP_DRAWER        = $C25B;
      C_RGB_FILE             = $C260;
      C_RGB_OVASPECT         = $C270;
      C_RGB_ANIMTYPE         = $C271;
      C_RENDER_ALL           = $C272;
      C_REND_FROM            = $C273;
      C_REND_TO              = $C274;
      C_REND_NTH             = $C275;
      C_PAL_TYPE             = $C276;
      C_RND_TURBO            = $C277;
      C_RND_MIP              = $C278;
      C_BGND_METHOD          = $C279;
      C_AUTO_REFLECT         = $C27A;
      C_VP_FROM              = $C27B;
      C_VP_TO                = $C27C;
      C_VP_NTH               = $C27D;
      C_REND_TSTEP           = $C27E;
      C_VP_TSTEP             = $C27F;
      C_SRDIAM               = $C280;
      C_SRDEG                = $C290;
      C_SRSEG                = $C2A0;
      C_SRDIR                = $C2B0;
      C_HETOP                = $C2C0;
      C_HEBOT                = $C2D0;
      C_HEHT                 = $C2E0;
      C_HETURNS              = $C2F0;
      C_HEDEG                = $C300;
      C_HESEG                = $C310;
      C_HEDIR                = $C320;
      C_QUIKSTUFF            = $C330;
      C_SEE_LIGHTS           = $C340;
      C_SEE_CAMERAS          = $C350;
      C_SEE_3D               = $C360;
      C_MESHSEL              = $C370;
      C_MESHUNSEL            = $C380;
      C_POLYSEL              = $C390;
      C_POLYUNSEL            = $C3A0;
      C_SHPLOCAL             = $C3A2;
      C_MSHLOCAL             = $C3A4;
      C_NUM_FORMAT           = $C3B0;
      C_ARCH_DENOM           = $C3C0;
      C_IN_DEVICE            = $C3D0;
      C_MSCALE               = $C3E0;
      C_COMM_PORT            = $C3F0;
      C_TAB_BASES            = $C400;
      C_TAB_DIVS             = $C410;
      C_MASTER_SCALES        = $C420;
      C_SHOW_1STVERT         = $C430;
      C_SHAPER_OK            = $C440;
      C_LOFTER_OK            = $C450;
      C_EDITOR_OK            = $C460;
      C_KEYFRAMER_OK         = $C470;
      C_PICKSIZE             = $C480;
      C_MAPTYPE              = $C490;
      C_MAP_DISPLAY          = $C4A0;
      C_TILE_XY              = $C4B0;
      C_MAP_XYZ              = $C4C0;
      C_MAP_SCALE            = $C4D0;
      C_MAP_MATRIX_OLD       = $C4E0;
      C_MAP_MATRIX           = $C4E1;
      C_MAP_WID_HT           = $C4F0;
      C_OBNAME               = $C500;
      C_CAMNAME              = $C510;
      C_LTNAME               = $C520;
      C_CUR_MNAME            = $C525;
      C_CURMTL_FROM_MESH     = $C526;
      C_GET_SHAPE_MAKE_FACES = $C527;
      C_DETAIL               = $C530;
      C_VERTMARK             = $C540;
      C_MSHAX                = $C550;
      C_MSHCP                = $C560;
      C_USERAX               = $C570;
      C_SHOOK                = $C580;
      C_RAX                  = $C590;
      C_STAPE                = $C5A0;
      C_LTAPE                = $C5B0;
      C_ETAPE                = $C5C0;
      C_KTAPE                = $C5C8;
      C_SPHSEGS              = $C5D0;
      C_GEOSMOOTH            = $C5E0;
      C_HEMISEGS             = $C5F0;
      C_PRISMSEGS            = $C600;
      C_PRISMSIDES           = $C610;
      C_TUBESEGS             = $C620;
      C_TUBESIDES            = $C630;
      C_TORSEGS              = $C640;
      C_TORSIDES             = $C650;
      C_CONESIDES            = $C660;
      C_CONESEGS             = $C661;
      C_NGPARMS              = $C670;
      C_PTHLEVEL             = $C680;
      C_MSCSYM               = $C690;
      C_MFTSYM               = $C6A0;
      C_MTTSYM               = $C6B0;
      C_SMOOTHING            = $C6C0;
      C_MODICOUNT            = $C6D0;
      C_FONTSEL              = $C6E0;
      C_TESS_TYPE            = $C6f0;
      C_TESS_TENSION         = $C6f1;
      C_SEG_START            = $C700;
      C_SEG_END              = $C705;
      C_CURTIME              = $C710;
      C_ANIMLENGTH           = $C715;
      C_PV_FROM              = $C720;
      C_PV_TO                = $C725;
      C_PV_DOFNUM            = $C730;
      C_PV_RNG               = $C735;
      C_PV_NTH               = $C740;
      C_PV_TYPE              = $C745;
      C_PV_METHOD            = $C750;
      C_PV_FPS               = $C755;
      C_VTR_FRAMES           = $C765;
      C_VTR_HDTL             = $C770;
      C_VTR_HD               = $C771;
      C_VTR_TL               = $C772;
      C_VTR_IN               = $C775;
      C_VTR_PK               = $C780;
      C_VTR_SH               = $C785;

      // Material chunks
      C_WORK_MTLS            = $C790;            // Old-style -- now ignored
      C_WORK_MTLS_2          = $C792;            // Old-style -- now ignored
      C_WORK_MTLS_3          = $C793;            // Old-style -- now ignored
      C_WORK_MTLS_4          = $C794;            // Old-style -- now ignored
      C_WORK_MTLS_5          = $CB00;            // Old-style -- now ignored
      C_WORK_MTLS_6          = $CB01;            // Old-style -- now ignored
      C_WORK_MTLS_7          = $CB02;            // Old-style -- now ignored
      C_WORK_MTLS_8          = $CB03;            // Old-style -- now ignored
      C_WORKMTL              = $CB04;
      C_SXP_TEXT_DATA        = $CB10;
      C_SXP_TEXT2_DATA       = $CB20;
      C_SXP_OPAC_DATA        = $CB11;
      C_SXP_BUMP_DATA        = $CB12;
      C_SXP_SPEC_DATA        = $CB24;
      C_SXP_SHIN_DATA        = $CB13;
      C_SXP_SELFI_DATA       = $CB28;
      C_SXP_TEXT_MASKDATA    = $CB30;
      C_SXP_TEXT2_MASKDATA   = $CB32;
      C_SXP_OPAC_MASKDATA    = $CB34;
      C_SXP_BUMP_MASKDATA    = $CB36;
      C_SXP_SPEC_MASKDATA    = $CB38;
      C_SXP_SHIN_MASKDATA    = $CB3A;
      C_SXP_SELFI_MASKDATA   = $C3CB;
      C_SXP_REFL_MASKDATA    = $CB3E;
      C_BGTYPE               = $C7A1;
      C_MEDTILE              = $C7B0;

      // Contrast
      C_LO_CONTRAST          = $C7D0;
      C_HI_CONTRAST          = $C7D1;

      // 3d frozen display
      C_FROZ_DISPLAY         = $C7E0;

      // Booleans
      C_BOOLWELD             = $C7f0;
      C_BOOLTYPE             = $C7f1;
      C_ANG_THRESH           = $C900;
      C_SS_THRESH            = $C901;
      C_TEXTURE_BLUR_DEFAULT = $C903;
      C_MAPDRAWER            = $CA00;
      C_MAPDRAWER1           = $CA01;
      C_MAPDRAWER2           = $CA02;
      C_MAPDRAWER3           = $CA03;
      C_MAPDRAWER4           = $CA04;
      C_MAPDRAWER5           = $CA05;
      C_MAPDRAWER6           = $CA06;
      C_MAPDRAWER7           = $CA07;
      C_MAPDRAWER8           = $CA08;
      C_MAPDRAWER9           = $CA09;
      C_MAPDRAWER_ENTRY      = $CA10;

      // system options
      C_BACKUP_FILE          = $CA20;
      C_DITHER_256           = $CA21;
      C_SAVE_LAST            = $CA22;
      C_USE_ALPHA            = $CA23;
      C_TGA_DEPTH            = $CA24;
      C_REND_FIELDS          = $CA25;
      C_REFLIP               = $CA26;
      C_SEL_ITEMTOG          = $CA27;
      C_SEL_RESET            = $CA28;
      C_STICKY_KEYINF        = $CA29;
      C_WELD_THRESHOLD       = $CA2A;
      C_ZCLIP_POINT          = $CA2B;
      C_ALPHA_SPLIT          = $CA2C;
      C_KF_SHOW_BACKFACE     = $CA30;
      C_OPTIMIZE_LOFT        = $CA40;
      C_TENS_DEFAULT         = $CA42;
      C_CONT_DEFAULT         = $CA44;
      C_BIAS_DEFAULT         = $CA46;
      C_DXFNAME_SRC          = $CA50;
      C_AUTO_WELD            = $CA60;
      C_AUTO_UNIFY           = $CA70;
      C_AUTO_SMOOTH          = $CA80;
      C_DXF_SMOOTH_ANG       = $CA90;
      C_SMOOTH_ANG           = $CAA0;

      // Special network-use chunks
      C_NET_USE_VPOST        = $CC00;
      C_NET_USE_GAMMA        = $CC10;
      C_NET_FIELD_ORDER      = $CC20;
      C_BLUR_FRAMES          = $CD00;
      C_BLUR_SAMPLES         = $CD10;
      C_BLUR_DUR             = $CD20;
      C_HOT_METHOD           = $CD30;
      C_HOT_CHECK            = $CD40;
      C_PIXEL_SIZE           = $CD50;
      C_DISP_GAMMA           = $CD60;
      C_FBUF_GAMMA           = $CD70;
      C_FILE_OUT_GAMMA       = $CD80;
      C_FILE_IN_GAMMA        = $CD82;
      C_GAMMA_CORRECT        = $CD84;
      C_APPLY_DISP_GAMMA     = $CD90;            // OBSOLETE
      C_APPLY_FBUF_GAMMA     = $CDA0;            // OBSOLETE
      C_APPLY_FILE_GAMMA     = $CDB0;            // OBSOLETE
      C_FORCE_WIRE           = $CDC0;
      C_RAY_SHADOWS          = $CDD0;
      C_MASTER_AMBIENT       = $CDE0;
      C_SUPER_SAMPLE         = $CDF0;
      C_OBJECT_MBLUR         = $CE00;
      C_MBLUR_DITHER         = $CE10;
      C_DITHER_24            = $CE20;
      C_SUPER_BLACK          = $CE30;
      C_SAFE_FRAME           = $CE40;
      C_VIEW_PRES_RATIO      = $CE50;
      C_BGND_PRES_RATIO      = $CE60;
      C_NTH_SERIAL_NUM       = $CE70;

      // Video Post
      VPDATA                 = $D000;
      P_QUEUE_ENTRY          = $D100;
      P_QUEUE_IMAGE          = $D110;
      P_QUEUE_USEIGAMMA      = $D114;
      P_QUEUE_PROC           = $D120;
      P_QUEUE_SOLID          = $D130;
      P_QUEUE_GRADIENT       = $D140;
      P_QUEUE_KF             = $D150;
      P_QUEUE_MOTBLUR        = $D152;
      P_QUEUE_MB_REPEAT      = $D153;
      P_QUEUE_NONE           = $D160;
      P_QUEUE_RESIZE         = $D180;
      P_QUEUE_OFFSET         = $D185;
      P_QUEUE_ALIGN          = $D190;
      P_CUSTOM_SIZE          = $D1a0;
      P_ALPH_NONE            = $D210;
      P_ALPH_PSEUDO          = $D220;            // Old Chunk
      P_ALPH_OP_PSEUDO       = $D221;            // Old Chunk
      P_ALPH_BLUR            = $D222;            // Replaces pseudo 
      P_ALPH_PCOL            = $D225;
      P_ALPH_C0              = $D230;
      P_ALPH_OP_KEY          = $D231;
      P_ALPH_KCOL            = $D235;
      P_ALPH_OP_NOCONV       = $D238;
      P_ALPH_IMAGE           = $D240;
      P_ALPH_ALPHA           = $D250;
      P_ALPH_QUES            = $D260;
      P_ALPH_QUEIMG          = $D265;
      P_ALPH_CUTOFF          = $D270;
      P_ALPHANEG             = $D280;
      P_TRAN_NONE            = $D300;
      P_TRAN_IMAGE           = $D310;
      P_TRAN_FRAMES          = $D312;
      P_TRAN_FADEIN          = $D320;
      P_TRAN_FADEOUT         = $D330;
      P_TRANNEG              = $D340;
      P_RANGES               = $D400;
      P_PROC_DATA            = $D500;

                                                  
      NodeTagCount = 6;                          // number of entries in node tag list
      NodeTags: array[1..NodeTagCount] of Word =
        (TARGET_NODE_TAG,
         OBJECT_NODE_TAG,
         CAMERA_NODE_TAG,
         LIGHT_NODE_TAG,
         L_TARGET_NODE_TAG,
         SPOTLIGHT_NODE_TAG
        );

//---------------------------------------------------------------------------------------------------------------------

implementation

//---------------------------------------------------------------------------------------------------------------------

end.
