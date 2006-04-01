unit TIFF;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is TIFF.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleiﬂa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.

{$Include Compilers.inc}

{$Z4}      // enum size = dword                                
// Align record structures to 4 byte boundaries.                   
{$ifdef COMPILER_7_UP}
  {$Align 4}
{$else}
  {$Align On}
{$endif COMPILER_7_UP}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

interface

uses
  Windows;

const
  TIFF_VERSION      = 42;
  TIFF_BIGENDIAN    = $4D4D;
  TIFF_LITTLEENDIAN = $4949;

  // TIFF Tag Definitions.
  // NB: In the comments below,
  // - items marked with a + are obsoleted by revision 5.0,
  // - items marked with a ! are introduced in revision 6.0.
  // - items marked with a % are introduced post revision 6.0.
  // - items marked with a $ are obsoleted by revision 6.0.
  TIFFTAG_SUBFILETYPE           = 254;           // subfile data descriptor
    FILETYPE_REDUCEDIMAGE       = 1;             // reduced resolution version
    FILETYPE_PAGE               = 2;             // one page of many
    FILETYPE_MASK               = 4;             // transparency mask
  TIFFTAG_OSUBFILETYPE          = 255;           // +kind of data in subfile
    OFILETYPE_IMAGE             = 1;             // full resolution image data
    OFILETYPE_REDUCEDIMAGE      = 2;             // reduced size image data
    OFILETYPE_PAGE              = 3;             // one page of many
  TIFFTAG_IMAGEWIDTH            = 256;           // image width in pixels
  TIFFTAG_IMAGELENGTH           = 257;           // image height in pixels
  TIFFTAG_BITSPERSAMPLE         = 258;           // bits per channel (sample)
  TIFFTAG_COMPRESSION           = 259;           // data compression technique
    COMPRESSION_NONE            = 1;             // dump mode
    COMPRESSION_CCITTRLE        = 2;             // CCITT modified Huffman RLE
    COMPRESSION_CCITTFAX3       = 3;             // CCITT Group 3 fax encoding
    COMPRESSION_CCITT_T4        = 3;             // CCITT T.4 (TIFF 6 name)
    COMPRESSION_CCITTFAX4       = 4;             // CCITT Group 4 fax encoding
    COMPRESSION_CCITT_T6        = 4;             // CCITT T.6 (TIFF 6 name)
    COMPRESSION_LZW             = 5;             // Lempel-Ziv  & Welch
    COMPRESSION_OJPEG           = 6;             // !6.0 JPEG
    COMPRESSION_JPEG            = 7;             // %JPEG DCT compression
    COMPRESSION_NEXT            = 32766;         // NeXT 2-bit RLE
    COMPRESSION_CCITTRLEW       = 32771;         // #1 w/ word alignment
    COMPRESSION_PACKBITS        = 32773;         // Macintosh RLE
    COMPRESSION_THUNDERSCAN     = 32809;         // ThunderScan RLE
    // Codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com).
    COMPRESSION_IT8CTPAD        = 32895;         // IT8 CT w/padding
    COMPRESSION_IT8LW           = 32896;         // IT8 Linework RLE
    COMPRESSION_IT8MP           = 32897;         // IT8 Monochrome picture
    COMPRESSION_IT8BL           = 32898;         // IT8 Binary line art
    // Compression codes 32908-32911 are reserved for Pixar.
    COMPRESSION_PIXARFILM       = 32908;         // Pixar companded 10bit LZW
    COMPRESSION_PIXARLOG        = 32909;         // Pixar companded 11bit ZIP
    COMPRESSION_DEFLATE         = 32946;         // Deflate compression
    COMPRESSION_ADOBE_DEFLATE   = 8;             // Deflate compression, as recognized by Adobe
    // Compression code 32947 is reserved for Oceana Matrix <dev@oceana.com>.
    COMPRESSION_DCS             = 32947;         // Kodak DCS encoding
    COMPRESSION_JBIG            = 34661;         // ISO JBIG
    COMPRESSION_SGILOG          = 34676;         // SGI Log Luminance RLE
    COMPRESSION_SGILOG24        = 34677;         // SGI Log 24-bit packed
  TIFFTAG_PHOTOMETRIC           = 262;           // photometric interpretation
    PHOTOMETRIC_MINISWHITE      = 0;             // min value is white
    PHOTOMETRIC_MINISBLACK      = 1;             // min value is black
    PHOTOMETRIC_RGB             = 2;             // RGB color model
    PHOTOMETRIC_PALETTE         = 3;             // color map indexed
    PHOTOMETRIC_MASK            = 4;             // $holdout mask
    PHOTOMETRIC_SEPARATED       = 5;             // !color separations
    PHOTOMETRIC_YCBCR           = 6;             // !CCIR 601
    PHOTOMETRIC_CIELAB          = 8;             // !1976 CIE L*a*b*
    PHOTOMETRIC_ITULAB          = 10;            // ITU L*a*b*
    PHOTOMETRIC_LOGL            = 32844;         // CIE Log2(L)
    PHOTOMETRIC_LOGLUV          = 32845;         // CIE Log2(L) (u',v')
  TIFFTAG_THRESHHOLDING         = 263;           // +thresholding used on data
    THRESHHOLD_BILEVEL          = 1;             // b&w art scan
    THRESHHOLD_HALFTONE         = 2;             // or dithered scan
    THRESHHOLD_ERRORDIFFUSE     = 3;             // usually floyd-steinberg
  TIFFTAG_CELLWIDTH             = 264;           // +dithering matrix width
  TIFFTAG_CELLLENGTH            = 265;           // +dithering matrix height
  TIFFTAG_FILLORDER             = 266;           // data order within a byte
    FILLORDER_MSB2LSB           = 1;             // most significant -> least
    FILLORDER_LSB2MSB           = 2;             // least significant -> most
  TIFFTAG_DOCUMENTNAME          = 269;           // name of doc. image is from
  TIFFTAG_IMAGEDESCRIPTION      = 270;           // info about image
  TIFFTAG_MAKE                  = 271;           // scanner manufacturer name
  TIFFTAG_MODEL                 = 272;           // scanner model name/number
  TIFFTAG_STRIPOFFSETS          = 273;           // offsets to data strips
  TIFFTAG_ORIENTATION           = 274;           // +image orientation
    ORIENTATION_TOPLEFT         = 1;             // row 0 top, col 0 lhs
    ORIENTATION_TOPRIGHT        = 2;             // row 0 top, col 0 rhs
    ORIENTATION_BOTRIGHT        = 3;             // row 0 bottom, col 0 rhs
    ORIENTATION_BOTLEFT         = 4;             // row 0 bottom, col 0 lhs
    ORIENTATION_LEFTTOP         = 5;             // row 0 lhs, col 0 top
    ORIENTATION_RIGHTTOP        = 6;             // row 0 rhs, col 0 top
    ORIENTATION_RIGHTBOT        = 7;             // row 0 rhs, col 0 bottom
    ORIENTATION_LEFTBOT         = 8;             // row 0 lhs, col 0 bottom
  TIFFTAG_SAMPLESPERPIXEL       = 277;           // samples per pixel
  TIFFTAG_ROWSPERSTRIP          = 278;           // rows per strip of data
  TIFFTAG_STRIPBYTECOUNTS       = 279;           // bytes counts for strips
  TIFFTAG_MINSAMPLEVALUE        = 280;           // +minimum sample value
  TIFFTAG_MAXSAMPLEVALUE        = 281;           // +maximum sample value
  TIFFTAG_XRESOLUTION           = 282;           // pixels/resolution in x
  TIFFTAG_YRESOLUTION           = 283;           // pixels/resolution in y
  TIFFTAG_PLANARCONFIG          = 284;           // storage organization
    PLANARCONFIG_CONTIG         = 1;             // single image plane
    PLANARCONFIG_SEPARATE       = 2;             // separate planes of data
  TIFFTAG_PAGENAME              = 285;           // page name image is from
  TIFFTAG_XPOSITION             = 286;           // x page offset of image lhs
  TIFFTAG_YPOSITION             = 287;           // y page offset of image lhs
  TIFFTAG_FREEOFFSETS           = 288;           // +byte offset to free block
  TIFFTAG_FREEBYTECOUNTS        = 289;           // +sizes of free blocks
  TIFFTAG_GRAYRESPONSEUNIT      = 290;           // $gray scale curve accuracy
    GRAYRESPONSEUNIT_10S        = 1;             // tenths of a unit
    GRAYRESPONSEUNIT_100S       = 2;             // hundredths of a unit
    GRAYRESPONSEUNIT_1000S      = 3;             // thousandths of a unit
    GRAYRESPONSEUNIT_10000S     = 4;             // ten-thousandths of a unit
    GRAYRESPONSEUNIT_100000S    = 5;             // hundred-thousandths
  TIFFTAG_GRAYRESPONSECURVE     = 291;           // $gray scale response curve
  TIFFTAG_GROUP3OPTIONS         = 292;           // 32 flag bits
  TIFFTAG_T4OPTIONS             = 292;           // TIFF 6.0 proper name alias
    GROUP3OPT_2DENCODING        = $1;            // 2-dimensional coding
    GROUP3OPT_UNCOMPRESSED      = $2;            // data not compressed
    GROUP3OPT_FILLBITS          = $4;            // fill to byte boundary
  TIFFTAG_GROUP4OPTIONS         = 293;           // 32 flag bits
  TIFFTAG_T6OPTIONS             = 293;           // TIFF 6.0 proper name
    GROUP4OPT_UNCOMPRESSED      = $2;            // data not compressed
  TIFFTAG_RESOLUTIONUNIT        = 296;           // units of resolutions
    RESUNIT_NONE                = 1;             // no meaningful units
    RESUNIT_INCH                = 2;             // english
    RESUNIT_CENTIMETER          = 3;             // metric
  TIFFTAG_PAGENUMBER            = 297;           // page numbers of multi-page
  TIFFTAG_COLORRESPONSEUNIT     = 300;           // $color curve accuracy
    COLORRESPONSEUNIT_10S       = 1;             // tenths of a unit
    COLORRESPONSEUNIT_100S      = 2;             // hundredths of a unit
    COLORRESPONSEUNIT_1000S     = 3;             // thousandths of a unit
    COLORRESPONSEUNIT_10000S    = 4;             // ten-thousandths of a unit
    COLORRESPONSEUNIT_100000S   = 5;             // hundred-thousandths
  TIFFTAG_TRANSFERFUNCTION      = 301;           // !colorimetry info
  TIFFTAG_SOFTWARE              = 305;           // name & release
  TIFFTAG_DATETIME              = 306;           // creation date and time
  TIFFTAG_ARTIST                = 315;           // creator of image
  TIFFTAG_HOSTCOMPUTER          = 316;           // machine where created
  TIFFTAG_PREDICTOR             = 317;           // prediction scheme w/ LZW
  TIFFTAG_WHITEPOINT            = 318;           // image white point
  TIFFTAG_PRIMARYCHROMATICITIES = 319;           // !primary chromaticities
  TIFFTAG_COLORMAP              = 320;           // RGB map for pallette image
  TIFFTAG_HALFTONEHINTS         = 321;           // !highlight+shadow info
  TIFFTAG_TILEWIDTH             = 322;           // !rows/data tile
  TIFFTAG_TILELENGTH            = 323;           // !cols/data tile
  TIFFTAG_TILEOFFSETS           = 324;           // !offsets to data tiles
  TIFFTAG_TILEBYTECOUNTS        = 325;           // !byte counts for tiles
  TIFFTAG_BADFAXLINES           = 326;           // lines w/ wrong pixel count
  TIFFTAG_CLEANFAXDATA          = 327;           // regenerated line info
    CLEANFAXDATA_CLEAN          = 0;             // no errors detected
    CLEANFAXDATA_REGENERATED    = 1;             // receiver regenerated lines
    CLEANFAXDATA_UNCLEAN        = 2;             // uncorrected errors exist
  TIFFTAG_CONSECUTIVEBADFAXLINE = 328;           // max consecutive bad lines
  TIFFTAG_SUBIFD                = 330;           // subimage descriptors
  TIFFTAG_INKSET                = 332;           // !inks in separated image
    INKSET_CMYK                 = 1;             // !cyan-magenta-yellow-black
  TIFFTAG_INKNAMES              = 333;           // !ascii names of inks
  TIFFTAG_NUMBEROFINKS          = 334;           // !number of inks
  TIFFTAG_DOTRANGE              = 336;           // !0% and 100% dot codes
  TIFFTAG_TARGETPRINTER         = 337;           // !separation target
  TIFFTAG_EXTRASAMPLES          = 338;           // !info about extra samples
    EXTRASAMPLE_UNSPECIFIED     = 0;             // !unspecified data
    EXTRASAMPLE_ASSOCALPHA      = 1;             // !associated alpha data
    EXTRASAMPLE_UNASSALPHA      = 2;             // !unassociated alpha data
  TIFFTAG_SAMPLEFORMAT          = 339;           // !data sample format
    SAMPLEFORMAT_UINT           = 1;             // !unsigned integer data
    SAMPLEFORMAT_INT            = 2;             // !signed integer data
    SAMPLEFORMAT_IEEEFP         = 3;             // !IEEE floating point data
    SAMPLEFORMAT_VOID           = 4;             // !untyped data
    SAMPLEFORMAT_COMPLEXINT     = 5;             // !complex signed int
    SAMPLEFORMAT_COMPLEXIEEEFP  = 6;             // !complex ieee floating
  TIFFTAG_SMINSAMPLEVALUE       = 340;           // !variable MinSampleValue
  TIFFTAG_SMAXSAMPLEVALUE       = 341;           // !variable MaxSampleValue
  TIFFTAG_JPEGTABLES            = 347;           // %JPEG table stream
  // Tags 512-521 are obsoleted by Technical Note #2 which specifies a revised JPEG-in-TIFF scheme.
  TIFFTAG_JPEGPROC              = 512;           // !JPEG processing algorithm
    JPEGPROC_BASELINE           = 1;             // !baseline sequential
    JPEGPROC_LOSSLESS           = 14;            // !Huffman coded lossless
  TIFFTAG_JPEGIFOFFSET          = 513;           // !pointer to SOI marker
  TIFFTAG_JPEGIFBYTECOUNT       = 514;           // !JFIF stream length
  TIFFTAG_JPEGRESTARTINTERVAL   = 515;           // !restart interval length
  TIFFTAG_JPEGLOSSLESSPREDICTOR = 517;           // !lossless proc predictor
  TIFFTAG_JPEGPOINTTRANSFORM    = 518;           // !lossless point transform
  TIFFTAG_JPEGQTABLES           = 519;           // !Q matrice offsets
  TIFFTAG_JPEGDCTABLES          = 520;           // !DCT table offsets
  TIFFTAG_JPEGACTABLES          = 521;           // !AC coefficient offsets
  TIFFTAG_YCBCRCOEFFICIENTS     = 529;           // !RGB -> YCbCr transform
  TIFFTAG_YCBCRSUBSAMPLING      = 530;           // !YCbCr subsampling factors
  TIFFTAG_YCBCRPOSITIONING      = 531;           // !subsample positioning
    YCBCRPOSITION_CENTERED      = 1;             // !as in PostScript Level 2
    YCBCRPOSITION_COSITED       = 2;             // !as in CCIR 601-1
  TIFFTAG_REFERENCEBLACKWHITE   = 532;           // !colorimetry info
  // Tags 32952-32956 are private tags registered to Island Graphics.
  TIFFTAG_REFPTS                = 32953;         // image reference points
  TIFFTAG_REGIONTACKPOINT       = 32954;         // region-xform tack point
  TIFFTAG_REGIONWARPCORNERS     = 32955;         // warp quadrilateral
  TIFFTAG_REGIONAFFINE          = 32956;         // affine transformation mat
  // Tags 32995-32999 are private tags registered to SGI.
  TIFFTAG_MATTEING              = 32995;         // $use ExtraSamples
  TIFFTAG_DATATYPE              = 32996;         // $use SampleFormat
  TIFFTAG_IMAGEDEPTH            = 32997;         // z depth of image
  TIFFTAG_TILEDEPTH             = 32998;         // z depth/data tile
  // Tags 33300-33309 are private tags registered to Pixar.
  // TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH
  // are set when an image has been cropped out of a larger image.
  // They reflect the size of the original uncropped image.
  // The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used
  // to determine the position of the smaller image in the larger one.
  TIFFTAG_PIXAR_IMAGEFULLWIDTH  = 33300;         // full image size in x
  TIFFTAG_PIXAR_IMAGEFULLLENGTH = 33301;         // full image size in y
  // Tags 33302-33306 are used to identify special image modes and data used by Pixar's texture formats.
  TIFFTAG_PIXAR_TEXTUREFORMAT   = 33302;         // texture map format
  TIFFTAG_PIXAR_WRAPMODES       = 33303;         // s & t wrap modes
  TIFFTAG_PIXAR_FOVCOT          = 33304;         // cotan(fov) for env. maps
  TIFFTAG_PIXAR_MATRIX_WORLDTOSCREEN = 33305;
  TIFFTAG_PIXAR_MATRIX_WORLDTOCAMERA = 33306;
  // Tag 33405 is a private tag registered to Eastman Kodak.
  TIFFTAG_WRITERSERIALNUMBER    = 33405;         // device serial number
  // Tag 33432 is listed in the 6.0 spec w/ unknown ownership.
  TIFFTAG_COPYRIGHT             = 33432;         // copyright string
  // IPTC TAG from RichTIFF specifications.
  TIFFTAG_RICHTIFFIPTC          = 33723;
  // Tags 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@etsinc.com).
  TIFFTAG_IT8SITE               = 34016;         // site name
  TIFFTAG_IT8COLORSEQUENCE      = 34017;         // color seq. [RGB,CMYK,etc]
  TIFFTAG_IT8HEADER             = 34018;         // DDES Header
  TIFFTAG_IT8RASTERPADDING      = 34019;         // raster scanline padding
  TIFFTAG_IT8BITSPERRUNLENGTH   = 34020;         // # of bits in short run
  TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH = 34021;   // # of bits in long run
  TIFFTAG_IT8COLORTABLE         = 34022;         // LW colortable
  TIFFTAG_IT8IMAGECOLORINDICATOR = 34023;        // BP/BL image color switch
  TIFFTAG_IT8BKGCOLORINDICATOR  = 34024;         // BP/BL bg color switch
  TIFFTAG_IT8IMAGECOLORVALUE    = 34025;         // BP/BL image color value
  TIFFTAG_IT8BKGCOLORVALUE      = 34026;         // BP/BL bg color value
  TIFFTAG_IT8PIXELINTENSITYRANG = 34027;         // MP pixel intensity value
  TIFFTAG_IT8TRANSPARENCYINDICATOR = 34028;      // HC transparency switch
  TIFFTAG_IT8COLORCHARACTERIZATION = 34029;      // color character. table
  // Tags 34232-34236 are private tags registered to Texas Instruments.
  TIFFTAG_FRAMECOUNT            = 34232;         // Sequence Frame Count
  // Tag 34750 is a private tag registered to Adobe?
  TIFFTAG_ICCPROFILE            = 34675;         // ICC profile data
  // Tag 34377 is private tag registered to Adobe for PhotoShop.
  TIFFTAG_PHOTOSHOP             = 34377;
  // Tag 34750 is a private tag registered to Pixel Magic.
  TIFFTAG_JBIGOPTIONS           = 34750;         // JBIG options
  // Tags 34908-34914 are private tags registered to SGI.
  TIFFTAG_FAXRECVPARAMS         = 34908;         // encoded Class 2 ses. parms
  TIFFTAG_FAXSUBADDRESS         = 34909;         // received SubAddr string
  TIFFTAG_FAXRECVTIME           = 34910;         // receive time (secs)
  // Tags 37439-37443 are registered to SGI <gregl@sgi.com>.
  TIFFTAG_STONITS               = 37439;         // Sample value to Nits
  // Tag 34929 is a private tag registered to FedEx.
  TIFFTAG_FEDEX_EDR             = 34929;         // unknown use
  // Tag 65535 is an undefined tag used by Eastman Kodak.
  TIFFTAG_DCSHUESHIFTVALUES     = 65535;         // hue shift correction data

  // The following are ``pseudo tags'' that can be
  // used to control codec-specific functionality.
  // These tags are not written to file.  Note that
  // these values start at $ffff+1 so that they'll
  // never collide with Aldus-assigned tags.
  // If you want your private pseudo tags ``registered''
  // (i.e. added to this file), send mail to sam@sgi.com
  // with the appropriate C definitions to add.
  TIFFTAG_FAXMODE               = 65536;         // Group 3/4 format control
  FAXMODE_CLASSIC               = $0000;         // default, include RTC
  FAXMODE_NORTC                 = $0001;         // no RTC at end of data
  FAXMODE_NOEOL                 = $0002;         // no EOL code at end of row
  FAXMODE_BYTEALIGN             = $0004;         // byte align row
  FAXMODE_WORDALIGN             = $0008;         // word align row
  FAXMODE_CLASSF                = FAXMODE_NORTC; // TIFF Class F
  TIFFTAG_JPEGQUALITY           = 65537;         // Compression quality level
  // Note: quality level is on the IJG 0-100 scale.  Default value is 75.
  TIFFTAG_JPEGCOLORMODE         = 65538;         // Auto RGB<=>YCbCr convert?
    JPEGCOLORMODE_RAW           = $0000;         // no conversion (default)
    JPEGCOLORMODE_RGB           = $0001;         // do auto conversion
  TIFFTAG_JPEGTABLESMODE        = 65539;         // What to put in JPEGTables
    JPEGTABLESMODE_QUANT        = $0001;         // include quantization tbls
    JPEGTABLESMODE_HUFF         = $0002;         // include Huffman tbls
  // Note: default is JPEGTABLESMODE_QUANT or JPEGTABLESMODE_HUFF.
  TIFFTAG_FAXFILLFUNC           = 65540;         // G3/G4 fill function
  TIFFTAG_PIXARLOGDATAFMT       = 65549;         // PixarLogCodec I/O data sz
    PIXARLOGDATAFMT_8BIT        = 0;             // regular u_char samples
    PIXARLOGDATAFMT_8BITABGR    = 1;             // ABGR-order u_chars
    PIXARLOGDATAFMT_11BITLOG    = 2;             // 11-bit log-encoded (raw)
    PIXARLOGDATAFMT_12BITPICIO  = 3;             // as per PICIO (1.0==2048)
    PIXARLOGDATAFMT_16BIT       = 4;             // signed short samples
    PIXARLOGDATAFMT_FLOAT       = 5;             // IEEE float samples
  // Tags 65550-65556 are allocated to Oceana Matrix <dev@oceana.com>.
  TIFFTAG_DCSIMAGERTYPE         = 65550;         // imager model & filter
    DCSIMAGERMODEL_M3           = 0;             // M3 chip (1280 x 1024)
    DCSIMAGERMODEL_M5           = 1;             // M5 chip (1536 x 1024)
    DCSIMAGERMODEL_M6           = 2;             // M6 chip (3072 x 2048)
    DCSIMAGERFILTER_IR          = 0;             // infrared filter
    DCSIMAGERFILTER_MONO        = 1;             // monochrome filter
    DCSIMAGERFILTER_CFA         = 2;             // color filter array
    DCSIMAGERFILTER_OTHER       = 3;             // other filter
  TIFFTAG_DCSINTERPMODE         = 65551;         // interpolation mode
    DCSINTERPMODE_NORMAL        = $0;            // whole image, default
    DCSINTERPMODE_PREVIEW       = $1;            // preview of image (384x256)
  TIFFTAG_DCSBALANCEARRAY       = 65552;         // color balance values
  TIFFTAG_DCSCORRECTMATRIX      = 65553;         // color correction values
  TIFFTAG_DCSGAMMA              = 65554;         // gamma value
  TIFFTAG_DCSTOESHOULDERPTS     = 65555;         // toe & shoulder points
  TIFFTAG_DCSCALIBRATIONFD      = 65556;         // calibration file desc
  // Note: quality level is on the ZLIB 1-9 scale. Default value is -1.
  TIFFTAG_ZIPQUALITY            = 65557;         // compression quality level
  TIFFTAG_PIXARLOGQUALITY       = 65558;         // PixarLog uses same scale
  // Tags 65559 is allocated to Oceana Matrix <dev@oceana.com>.
  TIFFTAG_DCSCLIPRECTANGLE      = 65559;         // area of image to acquire
  TIFFTAG_SGILOGDATAFMT         = 65560;         // SGILog user data format
    SGILOGDATAFMT_FLOAT         = 0;             // IEEE float samples
    SGILOGDATAFMT_16BIT         = 1;             // 16-bit samples
    SGILOGDATAFMT_RAW           = 2;             // uninterpreted data
    SGILOGDATAFMT_8BIT          = 3;             // 8-bit RGB monitor values
  TIFFTAG_SGILOGENCODE          = 65561;         // SGILog data encoding control
    SGILOGENCODE_NODITHER       = 0;             // do not dither encoded values
    SGILOGENCODE_RANDITHER      = 1;             // randomly dither encd values

type
  {$ifndef COMPILER_6_UP}
    PPointer = ^Pointer;
  {$endif COMPILER_6_UP}
  
  // TIFF is an opague record only known to the lib itself.
  PTIFF = ^TTIFF;
  TTIFF = record
  end;                                                  

  // For TIFFReassignTagToIgnore                             
  TTIFFIgnoreSense = (// IGNORE tag table
    TIS_STORE,
    TIS_EXTRACT,
    TIS_EMPTY                                
  );

  TTIFFHeader = record
    tiff_magic: Word;      // magic number (defines byte order)
    tiff_version: Word;    // TIFF version number
    tiff_diroff: Cardinal; // byte offset to first directory
  end;

  // TIFF Image File Directories are comprised of a table of field descriptors of the form shown
  // below.  The table is sorted in ascending order by tag.  The values associated with each entry
  // are disjoint and may appear anywhere in the file (so long as they are placed on a word boundary).
  //
  // If the value is 4 bytes or less, then it is placed in the offset field to save space.  If the value
  // is less than 4 bytes, it is left-justified in the offset field.

  TTIFFDirEntry = record
    tdir_tag: Word;        // see below
    tdir_type: Word;       // data type; see below
    tdir_count: Cardinal;  // number of items; length in spec
    tdir_offset: Cardinal; // byte offset to field data
  end;

  // Tag data type information.
  // Note: RATIONALs are the ratio of two 32-bit integer values.
  TTIFFDataType = (
    TIFF_NOTYPE,    // 0, placeholder
    TIFF_BYTE,      // 1, 8-bit unsigned integer
    TIFF_ASCII,     // 2, 8-bit bytes w/ last byte null
    TIFF_SHORT,     // 3, 16-bit unsigned integer
    TIFF_LONG,      // 4, 32-bit unsigned integer
    TIFF_RATIONAL,  // 5, 64-bit unsigned fraction
    TIFF_SBYTE,     // 6, !8-bit signed integer
    TIFF_UNDEFINED, // 7, !8-bit untyped data
    TIFF_SSHORT,    // 8, !16-bit signed integer
    TIFF_SLONG,     // 9. !32-bit signed integer
    TIFF_SRATIONAL, // 10, !64-bit signed fraction
    TIFF_FLOAT,     // 11, !32-bit IEEE floating point
    TIFF_DOUBLE     // 12, !64-bit IEEE floating point
  );

  tsample_t = Word;
  thandle_t = THandle;
  tsize_t = Cardinal;
  tdata_t = Pointer;
  toff_t = Cardinal;
  ttag_t = Cardinal;
  tdir_t = Word;       // directory index
  tstrip_t = Cardinal; // strip number

  TTIFFReadWriteProc = function(fd: thandle_t; buf: tdata_t; size: tsize_t): tsize_t;
  TTIFFSeekProc = function(fd: thandle_t; off: toff_t; whence: Integer): toff_t;
  TTIFFCloseProc = function(fd: thandle_t): Integer;
  TTIFFSizeProc = function(fd: thandle_t): toff_t;
  TTIFFMapFileProc = function(fd: thandle_t; var pbase: tdata_t; var psize: toff_t): Integer;
  TTIFFUnmapFileProc = procedure(fd: thandle_t; base: tdata_t; size: toff_t);

  TIFFErrorHandler = procedure(Module: PChar; const Format: PChar; Params: va_list);

function TIFFClientOpen(name, mode: PChar; clientdata: thandle_t; readproc: TTIFFReadWriteProc;
  writeproc: TTIFFReadWriteProc; seekproc: TTIFFSeekProc; closeproc: TTIFFCloseProc; sizeproc: TTIFFSizeProc;
  mapproc: TTIFFMapFileProc; unmapproc: TTIFFUnmapFileProc): PTIFF;
procedure TIFFClose(tif: PTIFF);
function TIFFCreateDirectory(tif: PTIFF): Integer;
function TIFFDefaultDirectory(tif: PTIFF): Integer;
function TIFFFlushData(tif: PTIFF): Integer;
function TIFFOpen(name, mode: PChar): PTIFF;
function TIFFReadRGBAImage(tif: PTIFF; rwidth, rheight: Cardinal; raster: Pointer; stop: LONGBOOL): BOOL;
function TIFFReassignTagToIgnore(task: TTIFFIgnoreSense; TIFFtagID: Integer): Integer;
function TIFFSetCompressionScheme(tif: PTIFF; scheme: Integer): Integer;
function TIFFWriteDirectory(tif: PTIFF): Integer;
function TIFFNumberOfDirectories(tif: PTIFF): tdir_t;
function TIFFSetDirectory(tif: PTIFF; dirn: tdir_t): Integer;
function TIFFReadTile(tif: PTIFF; buf: tdata_t; x, y, z: Cardinal; s: tsample_t): tsize_t;
function TIFFReadEncodedStrip(tif: PTIFF; strip: tstrip_t; buf: tdata_t; size: tsize_t): tsize_t;
function TIFFTileSize(tif: PTIFF): tsize_t;
function TIFFTileRowSize(tif: PTIFF): tsize_t;
function TIFFStripSize(tif: PTIFF): tsize_t;
function TIFFScanlineSize(tif: PTIFF): tsize_t;
function TIFFComputeStrip(tif: PTIFF; row: Cardinal; sample: tsample_t): tstrip_t;

// Variable argument list functions. Must be used carefully.
function TIFFVGetField(tif: PTIFF; tag: ttag_t; Values: va_list): Integer;
function TIFFVSetField(tif: PTIFF; tag: ttag_t; Values: va_list): Integer;
function TIFFVGetFieldDefaulted(tif: PTIFF; tag: ttag_t; Values: va_list): Integer;

{$ifdef DELPHI_7_UP}
  function TIFFGetField(tif: PTIFF; tag: ttag_t): Integer; cdecl; varargs;
{$else}
  function TIFFGetField(tif: PTIFF; tag: ttag_t; Value: Pointer): Integer; cdecl;
{$endif DELPHI_7_UP}
function TIFFSetField(tif: PTIFF; tag: ttag_t; Value: Pointer): Integer; cdecl;
{$ifdef DELPHI_7_UP}
  function TIFFGetFieldDefaulted(tif: PTIFF; tag: ttag_t): Integer; cdecl; varargs;
{$else}
  function TIFFGetFieldDefaulted(tif: PTIFF; tag: ttag_t; Values: va_list): Integer; cdecl;
{$endif DELPHI_7_UP}

function TIFFSetWarningHandler(Handler: TIFFErrorHandler): TIFFErrorHandler;
function TIFFSetErrorHandler(Handler: TIFFErrorHandler): TIFFErrorHandler;

function TIFFIsTiled(tif: PTIFF): LONGBOOL;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, GraphicEx, LibStub, JPG, GXzLib;

procedure _TIFFBuiltinCODECS; external;    // Not really a procedure but a structure.
procedure tiffDataWidth; external;         // Not really a procedure but a structure.
procedure TIFFFaxMainTable; external;      // Not really a procedure but a structure.
procedure TIFFFaxWhiteTable; external;     // Not really a procedure but a structure.
procedure TIFFFaxBlackTable; external;     // Not really a procedure but a structure.

function _TIFFgetMode(mode, module: PChar): Integer; external;
procedure _TIFFMergeFieldInfo(tif: PTIFF; const info; n: Integer); external;
function _TIFFNoPreCode(tif: TTIFF; s: tsample_t): Integer; external;
function _TIFFSampleToTagType(tif: PTIFF): TTIFFDataType; external;
procedure _TIFFSetDefaultCompressionState; external;
procedure _TIFFsetString(cpp: PPChar; cp: PChar); external;
procedure _TIFFsetByteArray(vpp: PPointer; vp: Pointer; n: Integer); external;

function TIFFClientOpen(name, mode: PChar; clientdata: thandle_t; readproc: TTIFFReadWriteProc;
  writeproc: TTIFFReadWriteProc; seekproc: TTIFFSeekProc; closeproc: TTIFFCloseProc; sizeproc: TTIFFSizeProc;
  mapproc: TTIFFMapFileProc; unmapproc: TTIFFUnmapFileProc): PTIFF; external;
procedure TIFFClose(tif: PTIFF); external;
function TIFFCreateDirectory(tif: PTIFF): Integer; external;
function TIFFDefaultDirectory(tif: PTIFF): Integer; external;
function TIFFFlushData(tif: PTIFF): Integer; external;
function TIFFOpen(name, mode: PChar): PTIFF; external;
function TIFFReadRGBAImage(tif: PTIFF; rwidth, rheight: Cardinal; raster: Pointer; stop: LONGBOOL): BOOL; external;
function TIFFReassignTagToIgnore(task: TTIFFIgnoreSense; TIFFtagID: Integer): Integer; external;
function TIFFSetCompressionScheme(tif: PTIFF; scheme: Integer): Integer; external;
function TIFFWriteDirectory(tif: PTIFF): Integer; external;
function TIFFNumberOfDirectories(tif: PTIFF): tdir_t; external;
function TIFFSetDirectory(tif: PTIFF; dirn: tdir_t): Integer; external;
function TIFFReadTile(tif: PTIFF; buf: tdata_t; x, y, z: Cardinal; s: tsample_t): tsize_t; external;
function TIFFReadEncodedStrip(tif: PTIFF; strip: tstrip_t; buf: tdata_t; size: tsize_t): tsize_t; external;
function TIFFTileSize(tif: PTIFF): tsize_t; external;
function TIFFTileRowSize(tif: PTIFF): tsize_t; external;
function TIFFStripSize(tif: PTIFF): tsize_t; external;
function TIFFScanlineSize(tif: PTIFF): tsize_t; external;
function TIFFComputeStrip(tif: PTIFF; row: Cardinal; sample: tsample_t): tstrip_t; external;

function TIFFVGetField(tif: PTIFF; tag: ttag_t; Values: va_list): Integer; external;
function TIFFVSetField(tif: PTIFF; tag: ttag_t; Values: va_list): Integer; external;
function TIFFVGetFieldDefaulted(tif: PTIFF; tag: ttag_t; Values: va_list): Integer; external;

{$ifdef DELPHI_7_UP}
  function TIFFGetField(tif: PTIFF; tag: ttag_t): Integer; external;
{$else}
  function TIFFGetField(tif: PTIFF; tag: ttag_t; Value: Pointer): Integer; external;
{$endif DELPHI_7_UP}
function TIFFSetField(tif: PTIFF; tag: ttag_t; Value: Pointer): Integer; external;
{$ifdef DELPHI_7_UP}
  function TIFFGetFieldDefaulted(tif: PTIFF; tag: ttag_t): Integer; external;
{$else}
  function TIFFGetFieldDefaulted(tif: PTIFF; tag: ttag_t; Values: va_list): Integer; external;
{$endif DELPHI_7_UP}

function TIFFSetWarningHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; external;
function TIFFSetErrorHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; external;

function TIFFIsTiled(tif: PTIFF): LONGBOOL; external;

{$L fax3sm_winnt.obj}
{$L tif_aux.obj}
{$L tif_close.obj}
{$L tif_codec.obj}
{$L tif_compress.obj}
{$L tif_dir.obj}
{$L tif_dirinfo.obj}
{$L tif_dirread.obj}
{$L tif_dirwrite.obj}
{$L tif_dumpmode.obj}
{$L tif_error.obj}
{$L tif_fax3.obj}
{$L tif_flush.obj}
{$L tif_getimage.obj}
{$L tif_jpeg.obj}
{$L tif_luv.obj}
{$L tif_lzw.obj}
{$L tif_ojpeg.obj}
{$L tif_open.obj}
{$L tif_packbits.obj}
{$L tif_pixarlog.obj}
{$L tif_predict.obj}
{$L tif_print.obj}
{$L tif_read.obj}
{$L tif_strip.obj}
{$L tif_swab.obj}
{$L tif_thunder.obj}
{$L tif_tile.obj}
{$L tif_version.obj}
{$L tif_warning.obj}
{$L tif_win32.obj}
{$L tif_write.obj}
{$L tif_zip.obj}
{$L tif_next.obj}

//----------------------------------------------------------------------------------------------------------------------

procedure GraphicExTIFFError(Module: PChar; const Format: PChar; Params: va_list);

var
  Buffer: array[0..1000] of Char;
  
begin
  wvsprintf(Buffer, Format, Params);
  GraphicExError(Buffer);
end;                                     

//----------------------------------------------------------------------------------------------------------------------

procedure GraphicExTIFFWarning(Module: PChar; const Format: PChar; Params: va_list);

var
  Buffer: array[0..1000] of Char;

begin                                                   
  wvsprintf(Buffer, Format, Params);
  OutputDebugString(Buffer);     
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  TIFFSetWarningHandler(GraphicExTIFFWarning);
  TIFFSetErrorHandler(GraphicExTIFFError);
finalization
end.



