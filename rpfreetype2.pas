(*******************************************************************
 *
 *  FreeType2.Pas
 *  Pas file generated from freetype.h Freetype2 library
 *  http://freetype.sourceforge.net
 *
 ******************************************************************)

unit rpfreetype2;

interface

{$I rpconf.inc}

uses
{$IFDEF LINUX}
 Libc,
{$ENDIF}
{$IFDEF MSWINDOWS}
 Windows,
{$ENDIF}
 SysUtils,rptypes;

const
{$IFDEF LINUX}
 C_FREETYPE='libfreetype.so';
{$ENDIF}
{$IFDEF MSWINDOWS}
 C_FREETYPE='freetype6.dll';
{$ENDIF}
 FT_LOAD_DEFAULT=$0;
 FT_LOAD_NO_SCALE=$1;
 FT_LOAD_NO_HINTING=$2;
 FT_LOAD_RENDER=$4;
 FT_LOAD_NO_BITMAP=$8;
 FT_LOAD_VERTICAL_LAYOUT=$10;
 FT_LOAD_FORCE_AUTOHINT=$20;
 FT_LOAD_CROP_BITMAP=$40;
 FT_LOAD_PEDANTIC=$80;
 FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH=$200;
 FT_LOAD_NO_RECURSE=$400;
 FT_LOAD_IGNORE_TRANSFORM=$800;
 FT_LOAD_MONOCHROME=$1000;
 FT_LOAD_LINEAR_DESIGN=$2000;

 FT_FACE_FLAG_SFNT=8;
 FT_FACE_FLAG_FIXED_WIDTH=4;

 FT_STYLE_FLAG_ITALIC=1;
 FT_STYLE_FLAG_BOLD=2;

type

  FT_Library     = record z : Pointer; end;
  FT_SubGlyph= record z:Pointer;end;
  FT_Slot_Internal=record z:Pointer;end;
  FT_Size_Internal=record z:Pointer;end;
  FT_Driver=record z:Pointer;end;
  FT_Stream=record z:Pointer;end;
  FT_Int32=integer;
  FT_memory=record z:Pointer;end;
  FT_ListRec=record z:Pointer;end;
  FT_Face_Internal=record z:Pointer;end;
  FT_Int    = Integer;
  FT_Error = FT_Int;
  FT_Byte = Byte;
  PFT_Byte = ^Byte;
  FT_String = char;
  PFT_String = PChar;
  FT_Long = longint;
  FT_Pointer = Pointer;
  FT_ULong = longint;
  FT_UInt = Cardinal;
  FT_Short=smallint;
  FT_UShort=Word;
  FT_Pos=integer;
  FT_Fixed=Integer;
  FT_Bitmap_Size=record
    height:FT_Short;
    width:FT_Short;
    size:FT_Pos;
    x_ppem:FT_Pos;
    y_ppem:FT_Pos;
  end;
  PFT_Bitmap_Size=^FT_Bitmap_Size;


  FT_Generic_Finalizer=procedure (aobject:Pointer);cdecl;
  FT_Generic=record
    data:Pointer;
    finalizer:FT_Generic_Finalizer;
  end;

  FT_Size_Metrics=record
    x_ppem:FT_UShort;      // horizontal pixels per EM               */
    y_ppem:FT_UShort;      // vertical pixels per EM                 */

    x_scale:FT_Fixed   ;     // two scales used to convert font units  */
    y_scale:FT_Fixed   ;     // to 26.6 frac. pixel coordinates        */

    ascender:FT_Pos     ;    // ascender in 26.6 frac. pixels          */
    descender:FT_Pos     ;   // descender in 26.6 frac. pixels         */
    height:FT_Pos     ;      // text height in 26.6 frac. pixels       */
    max_advance:FT_Pos     ; // max horizontal advance, in 26.6 pixels */
  end;

  FT_Glyph_Metrics=record
    width:FT_Pos;           // glyph width  */
    height:FT_Pos;        // glyph height */

    horiBearingX:FT_Pos;  // left side bearing in horizontal layouts */
    horiBearingY:FT_Pos;  // top side bearing in horizontal layouts  */
    horiAdvance:FT_Pos;   // advance width for horizontal layout     */

    vertBearingX:FT_Pos;  // left side bearing in vertical layouts */
    vertBearingY:FT_Pos ;  // top side bearing in vertical layouts  */
    vertAdvance:FT_Pos;   // advance height for vertical layout    */
  end;

  FT_Vector=record
    x:FT_Pos;
    y:FT_Pos;
  end;

  const
   FT_ENCODING_NONE=0;
//   FT_ENCODING_MS_SYMBOL=Ord('s') shl 24+Ord('y') shl 16 +Ord('m') shl 8+Ord('b');
   FT_ENCODING_UNICODE=Cardinal(Ord('u')) shl 24+Cardinal(Ord('n')) shl 16 +Cardinal(Ord('i')) shl 8+Cardinal(Ord('c'));

   FT_ENCODING_ADOBE_STANDARD=Cardinal(Ord('A')) shl 24+Cardinal(Ord('D')) shl 16 +Cardinal(Ord('O')) shl 8+Cardinal(Ord('B'));
//   FT_ENCODING_ADOBE_EXPERT=Ord('A') shl 24+Ord('D') shl 16 +Ord('B') shl 8+Ord('E');
//   FT_ENCODING_ADOBE_CUSTOM=Ord('A') shl 24+Ord('D') shl 16 +Ord('B') shl 8+Ord('C');
   FT_ENCODING_ADOBE_LATIN_1=(Cardinal(Ord('l')) shl 24)+Cardinal(Cardinal(Ord('a')) shl 16) +Cardinal(Cardinal(Ord('t')) shl 8)+Cardinal(Ord('1'));
   FT_ENCODING_OLD_LATIN_2=Cardinal(Ord('l') shl 24)+Cardinal(Ord('a') shl 16) +Cardinal(Ord('t') shl 8)+Cardinal(Ord('2'));
   FT_ENCODING_APLE_ROMAN=Cardinal(Ord('a')) shl 24+Cardinal(Ord('r')) shl 16 +Cardinal(Ord('m')) shl 8+Cardinal(Ord('n'));


  // None, Composite,bitmap,outline,plotter
  type
   FT_Glyph_Format=integer;
//  (0,1);
//   Ord('c') shl 24+Ord('o') shl 16 +Ord('m') shl 8+Ord('p')
//   );
   FT_Encoding=integer;

  FT_Bitmap=record
   rows:integer;
   width:integer;
   pitch:integer;
   buffer:PByte;
   num_grays:smallint;
   pixel_mode: char;
   palette_mode:char;
   palette:Pointer;
  end;

  FT_Face     = ^FT_FaceRec;

  PFT_Vector=^FT_Vector;
  FT_Outline=record
    n_contours:smallint;      //* number of contours in glyph        */
    n_points:smallint;        //* number of points in the glyph      */
    points:PFT_Vector;          //* the outline's points               */
    tags:Pchar;            //* the points flags                   */
    contours:^smallint ;        //* the contour end points             */

    flags:integer;           // outline masks                      */
  end;

  FT_GlyphSlot=^FT_GlyphSlotRec;

  FT_GlyphSlotRec=record
    alibrary:FT_Library;
    face:FT_Face;
    next:FT_GlyphSlot;
    reserved:FT_UInt;       //* retained for binary compatibility */
    generic:FT_Generic;
    metrics:FT_Glyph_Metrics;
    linearHoriAdvance:FT_Fixed;
    linearVertAdvance:FT_Fixed;
    advance:FT_Vector;

    format:FT_Glyph_Format;

    bitmap:FT_Bitmap;
    bitmap_left:FT_Int;
    bitmap_top:FT_Int;
    outline:FT_Outline;

    num_subglyphs:FT_UInt;
    subglyphs:FT_SubGlyph;

    control_data:Pointer;
    control_len:longint;

    other:Pointer;

    internal:FT_Slot_Internal;
  end;

  FT_BBox=record
    xMin, yMin:FT_Pos;
    xMax, yMax:FT_Pos  ;
  end;

  FT_SizeRec=record
    face:FT_Face;                 // parent face object              */
    generic:FT_Generic;   // generic pointer for client uses */
    metrics:FT_Size_Metrics;   // size metrics                    */
    internal:FT_Size_Internal;
  end;

  FT_Size=^FT_SizeRec;

  FT_CharMap  = ^FT_CharMapRec;

  FT_CharMapRec=record
    face:FT_Face;
    encoding:FT_Encoding;
    platform_id:FT_UShort;
    encoding_id:FT_UShort;
  end;
  PFT_CharMap  = ^FT_CharMap;

  FT_FaceRec=record
    num_faces:FT_Long;
    face_index:FT_Long;
    face_flags:FT_Long;
    style_flags:FT_Long;
    num_glyphs:FT_Long;
    family_name:PFT_String;
    style_name:PFT_String;
    num_fixed_sizes:FT_Int;
    available_sizes:PFT_Bitmap_Size;
    num_charmaps:FT_Int;
    charmaps:PFT_CharMap;
    generic:FT_Generic;

    //# the following are only relevant to scalable outlines */
    bbox:FT_BBox;
    units_per_EM:FT_UShort;
    ascender:FT_Short;
    descender:FT_Short;
    height:FT_Short;

    max_advance_width:FT_Short;
    max_advance_height:FT_Short;

    underline_position:FT_Short;
    underline_thickness:FT_Short;

    glyph:FT_GlyphSlot;
    size:FT_Size;
    charmap:FT_CharMap;

    //*@private begin */

    driver:FT_Driver         ;
    memory:FT_Memory         ;
    stream:FT_Stream         ;

    sizes_list:FT_ListRec        ;

    autohint:FT_Generic        ;
    extensions:Pointer;

    internal:FT_Face_Internal  ;

    //*@private end */
  end;



  FT_Module     = record z : Pointer; end;

  TFT_Init_FreeType =function (var alibrary:FT_Library):FT_Error;cdecl;
  TFT_Done_FreeType=function (alibrary:FT_Library):FT_Error;cdecl;
  TFT_New_Face=function (alibrary:FT_Library;filepathname:PChar;
               face_index:FT_Long;var aface:FT_Face):FT_Error;cdecl;
  TFT_Done_Face=function  (aface : FT_Face ) : FT_Error;cdecl;


  FT_Parameter=record
    tag:FT_ULong;
    data:FT_Pointer;
  end;
  PFT_Parameter=^FT_Parameter;


  FT_Open_Args=record
    flags:FT_UInt;
    memory_base:PFT_Byte;
    memory_size:FT_Long;
    pathname:PFT_String;
    stream:FT_Stream;
    driver:FT_Module;
    num_params:FT_Int;
    params:PFT_Parameter;
   end;

  TFT_Set_Charmap=function (face:FT_Face; charmap:FT_CharMap  ):FT_Error;cdecl;
  TFT_Select_Charmap=function (face:FT_Face;encoding:FT_Encoding):FT_Error;cdecl;



  TFT_Get_Char_Index=function (face:FT_Face;charcode:FT_ULong):FT_UInt;cdecl;

  TFT_Load_Glyph=function (face:FT_Face;glyph_index:FT_UInt;
                 load_flags:FT_Int32 ):FT_Error;cdecl;





  TT_Int    = Integer;
  TT_Engine     = record z : Pointer; end;

  TT_Long   = longint;
  TT_ULong  = longint;  (* there are no unsigned longs in Pascal :-(        *)
                        (* it will probably be a good idea to use cardinals *)
                        (* with Delphi and Virtual a bit later..            *)
  TT_Short  = integer;
  TT_UShort = word;

  TT_Fixed  = LongInt;  (* Signed Fixed 16.16 Float *)

  TT_FWord  = Integer;  (* Distance in FUnits *)
  TT_UFWord = Word;     (* Unsigned Distance  *)

  TT_F2Dot14 = Integer; (* signed fixed float 2.14 used for *)
                        (* unary vectors, with layout :     *)
                        (*                                  *)
                        (*  s : 1  -- sign bit              *)
                        (*  m : 1  -- mantissa bit          *)
                        (*  f : 14 -- unsigned fractional   *)
                        (*                                  *)
                        (*  's:m' is the 2-bit signed int   *)
                        (*  value to which the *positive*   *)
                        (*  fractional part should be       *)
                        (*  added.                          *)
                        (*                                  *)

  TT_F26Dot6 = LongInt; (* 26.6 fixed float, used for pixel coordinates *)

  TT_Pos     = Longint; (* funits or 26.6, depending on context *)

  (******************************************************)
  (*  a simple unit vector type                         *)
  (*                                                    *)
  TT_UnitVector = record

      x : TT_F2Dot14;
      y : TT_F2Dot14;
  end;

  (******************************************************)
  (*  a simple vector type                              *)
  (*                                                    *)
  TT_Vector = record

      x : TT_Pos;
      y : TT_Pos;
  end;

  (******************************************************)
  (*  a simple 2x2 matrix type                          *)
  (*                                                    *)
  TT_Matrix = record

      xx, xy : TT_Fixed;
      yx, yy : TT_Fixed;
  end;

  (******************************************************)
  (*  a glyph's bounding box                            *)
  (*                                                    *)
  TT_BBox = record

    xMin, yMin : TT_Pos;
    xMax, yMax : TT_Pos;
  end;

  (******************************************************)
  (*  the engine's error condition type - 0 always      *)
  (*  means success.                                    *)
  (*                                                    *)
  TT_Error = TT_Int;

  TT_Points_Table = array[0..99] of TT_Vector;
  TT_Points       = ^TT_Points_Table;

  TT_Coordinates  = array[0..99] of TT_Pos;
  TT_PCoordinates = ^TT_Coordinates;

  TT_TouchTable   = array[0..9] of byte;
  TT_PTouchTable  = ^TT_TouchTable;

  TT_ConStarts  = array[0..9] of word;
  TT_PConStarts = ^TT_ConStarts;

  (******************************************************)
  (*  glyph outline description                         *)
  (*                                                    *)
  TT_Outline = record

      n_points   : integer;
      n_contours : integer;

      points     : TT_Points;       (* array of point coordinates *)
      flags      : TT_PTouchTable;  (* array of point flags       *)
      conEnds    : TT_PConStarts;    (* array of contours ends    *)

      owner     : Boolean;          (* this flag is set when the outline *)
                                    (* owns the arrays it uses.          *)

      high_precision : Boolean;
      second_pass    : Boolean;
      dropout_mode   : Byte;
  end;

  (******************************************************)
  (*  glyph metrics structure                           *)
  (*                                                    *)
  TT_Glyph_Metrics = record

      bbox     : TT_BBox;
      bearingX : TT_Pos;
      bearingY : TT_Pos;
      advance  : TT_Pos;
  end;

  (******************************************************)
  (*  big glyph metrics structure                       *)
  (*                                                    *)
  TT_Big_Glyph_Metrics = record

      bbox         : TT_BBox;
      horiBearingX : TT_Pos;
      horiBearingY : TT_Pos;
      horiAdvance  : TT_Pos;
      vertBearingX : TT_Pos;
      vertBearingY : TT_Pos;
      vertAdvance  : TT_Pos;
  end;

  (******************************************************)
  (*  instance metrics. used to return information to   *)
  (*  clients regarding an instance's important state   *)
  (*                                                    *)
  TT_Instance_Metrics = record

      pointsize    : integer;

      x_ppem       : integer;
      y_ppem       : integer;

      x_scale      : TT_Fixed;
      y_scale      : TT_Fixed;

      x_resolution : integer;
      y_resolution : integer;
  end;

const
  TT_Flow_Down = -1;
  TT_Flow_Up   = +1;

type

  (******************************************************)
  (*  a record used to describe a bitmap or pixmap to   *)
  (*  the rasterizer.                                   *)
  (*                                                    *)
  TT_Raster_Map  = record

      Rows   : TT_Int;      (* rows number of the bitmap    *)
      Cols   : TT_Int;      (* columns (bytes) per row      *)
      Width  : TT_Int;      (* pixels per row               *)
      Flow   : TT_Int;      (* bit/pixmap's flow            *)
      Buffer : pointer;      (* bit/pixmap data              *)
      Size   : longint;      (* bit/pixmap data size (bytes) *)
  end;

  (******************************************************)
  (*  The TrueType font header table structure          *)
  (*                                                    *)
  TT_Header = record

      table_version   : TT_Fixed;
      font_revision   : TT_Fixed;

      checksum_adjust : TT_Long;
      magic_number    : TT_Long;

      flags           : TT_UShort;
      units_per_EM    : TT_UShort;

      created         : array[0..1] of TT_Long;
      modified        : array[0..1] of TT_Long;

      xMin, yMin      : TT_FWord;
      xMax, yMax      : TT_FWord;

      mac_style       : TT_UShort;
      lowest_rec_PPEM : TT_UShort;
      font_direction  : TT_Short;

      index_to_loc_format : TT_Short;
      glyph_data_format   : TT_Short;
  end;

  (******************************************************)
  (*  The TrueType horizontal header table structure    *)
  (*                                                    *)
  TT_Horizontal_Header = record

      version   : TT_Fixed;
      ascender  : TT_FWord;
      descender : TT_FWord;
      line_gap  : TT_FWord;

      advance_Width_Max      : TT_UShort;
      min_left_side_bearing  : TT_Short;
      min_right_side_bearing : TT_Short;
      xMax_extent            : TT_Short;
      caret_slope_rise       : TT_Short;
      caret_slope_run        : TT_Short;

      reserved  : array[0..4] of TT_SHort;

      metric_data_format     : TT_Short;
      number_of_HMetrics     : TT_UShort;

      (* the following are not part of the header in the file *)

      short_metrics : Pointer;
      long_metrics  : Pointer;
  end;

  (******************************************************)
  (*  The TrueType vertical header table structure      *)
  (*                                                    *)
  TT_Vertical_Header = record

      version   : TT_Fixed;
      ascender  : TT_FWord;
      descender : TT_FWord;
      line_gap  : TT_FWord;

      advance_Height_Max      : TT_UShort;
      min_top_side_bearing    : TT_Short;
      min_bottom_side_bearing : TT_Short;
      yMax_extent             : TT_Short;
      caret_slope_rise        : TT_Short;
      caret_slope_run         : TT_Short;

      reserved  : array[0..4] of TT_SHort;

      metric_data_format     : TT_Short;
      number_of_VMetrics     : TT_UShort;

      (* the following are not part of the header in the file *)

      short_metrics : Pointer;
      long_metrics  : Pointer;
  end;

  (******************************************************)
  (*  The TrueType OS/2 table structure                 *)
  (*                                                    *)
  TT_OS2 = record
      version             : TT_UShort;   (* $0001 *)
      xAvgCharWidth       : TT_Short;
      usWeightClass       : TT_UShort;
      usWidthClass        : TT_UShort;
      fsType              : TT_Short;
      ySubscriptXSize     : TT_Short;
      ySubscriptYSize     : TT_Short;
      ySubScriptXOffset   : TT_Short;
      ySubscriptYOffset   : TT_Short;
      ySuperscriptXSize   : TT_Short;
      ySuperscriptYSize   : TT_Short;
      ySuperscriptXOffset : TT_Short;
      ySuperscriptYOffset : TT_Short;
      yStrikeoutSize      : TT_Short;
      yStrikeoutPosition  : TT_Short;
      sFamilyClass        : TT_Short;
      panose              : array[0..9] of Byte;
      ulUnicodeRange1     : TT_ULong;   (* bits  0-31  *)
      ulUnicodeRange2     : TT_ULong;   (* bits 32-63  *)
      ulUnicodeRange3     : TT_ULong;   (* bits 64-95  *)
      ulUnicodeRange4     : TT_ULong;   (* bits 96-127 *)
      achVendID           : array[0..3] of Byte;
      fsSelection         : TT_UShort;
      usFirstCharIndex    : TT_UShort;
      usLastCharIndex     : TT_UShort;
      sTypoAscender       : TT_Short;
      sTypoDescender      : TT_Short;
      sTypoLineGap        : TT_Short;
      usWinAscent         : TT_UShort;
      usWinDescent        : TT_UShort;

      (* only version 1 tables *)
      ulCodePageRange1    : TT_ULong;
      ulCodePageRange2    : TT_ULong;
  end;

  (******************************************************)
  (*  The TrueType Postscript table structure           *)
  (*                                                    *)
  TT_Postscript = record

      FormatType         : TT_Fixed;
      italicAngle        : TT_Fixed;
      underlinePosition  : TT_Short;
      underlineThickness : TT_Short;
      isFixedPitch       : TT_ULong;
      minMemType42       : TT_ULong;
      maxMemType42       : TT_ULong;
      minMemType1        : TT_ULong;
      maxMemType1        : TT_ULong;
  end;

  (******************************************************)
  (*  face properties. use to report important face     *)
  (*  data to clients                                   *)
  (*                                                    *)
  TT_Face_Properties = record

      num_glyphs   : integer;
      max_points   : integer;
      max_contours : integer;
      max_faces    : integer;

      header       : ^TT_Header;
      horizontal   : ^TT_Horizontal_Header;
      vertical     : ^TT_Vertical_Header;
      os2          : ^TT_OS2;
      postscript   : ^TT_Postscript;
  end;

  (******************************************************)
  (*  Objects handle types                              *)
  (*                                                    *)
  TT_Stream   = record z : Pointer; end;
  TT_Face     = record z : Pointer; end;
//  TT_Face     = Pointer;
  TT_Instance = record z : Pointer; end;
//  TT_Instance = Pointer;
  TT_Glyph    = record z : Pointer; end;
  TT_CharMap  = record z : Pointer; end;

  TT_Gray_Palette = array[0..4] of byte;

  (******************************************************************)
  (*                                                                *)
  (*                         ERROR CODES                            *)
  (*                                                                *)
  (******************************************************************)

const
  (* ------------------- Success is always 0 ---------------------- *)

  TT_Err_Ok                      =  0;

  (* -------------------------------------------------------------- *)

  TT_Err_Invalid_Face_Handle     = $0001;
  TT_Err_Invalid_Instance_Handle = $0002;
  TT_Err_Invalid_Glyph_Handle    = $0003;
  TT_Err_Invalid_CharMap_Handle  = $0004;
  TT_Err_Invalid_Result_Address  = $0005;
  TT_Err_Invalid_Glyph_Index     = $0006;
  TT_Err_Invalid_Argument        = $0007;
  TT_Err_Could_Not_Open_File     = $0008;
  TT_Err_File_Is_Not_Collection  = $0009;

  TT_Err_Table_Missing           = $000A;
  TT_Err_Invalid_Horiz_Metrics   = $000B;
  TT_Err_Invalid_Vert_Metrics    = $000B;
  TT_Err_Invalid_CharMap_Format  = $000C;

  TT_Err_Invalid_File_Format     = $0010;
  TT_Err_File_Error              = $0011;

  TT_Err_Invalid_Engine          = $0020;
  TT_Err_Too_Many_Extensions     = $0021;
  TT_Err_Extensions_Unsupported  = $0022;
  TT_Err_Invalid_Extension_Id    = $0023;

  TT_Err_No_Vertical_Data        = $0030;

  TT_Err_Max_Profile_Missing     = $0080;
  TT_Err_Header_Table_Missing    = $0081;
  TT_Err_Horiz_Header_Missing    = $0082;
  TT_Err_Locations_Missing       = $0083;
  TT_Err_Name_Table_Missing      = $0084;
  TT_Err_CMap_Table_Missing      = $0085;
  TT_Err_Hmtx_Table_Missing      = $0086;
  TT_Err_OS2_Table_Missing       = $0087;
  TT_Err_Post_Table_Missing      = $0088;

  (* -------------------------------------------------------------- *)

  TT_Err_Out_Of_Memory           = $0100;

  (* -------------------------------------------------------------- *)

  TT_Err_Invalid_File_Offset     = $0200;
  TT_Err_Invalid_File_Read       = $0201;
  TT_Err_Invalid_Frame_Access    = $0202;

  (* -------------------------------------------------------------- *)

  TT_Err_Too_Many_Points         = $0300;
  TT_Err_Too_Many_Contours       = $0301;
  TT_Err_Invalid_Composite       = $0302;
  TT_Err_Too_Many_Ins            = $0303;

  (* -------------------------------------------------------------- *)

  TT_Err_Invalid_Opcode          = $0400;
  TT_Err_Too_Few_Arguments       = $0401;
  TT_Err_Stack_Overflow          = $0402;
  TT_Err_Code_Overflow           = $0403;
  TT_Err_Bad_Argument            = $0404;
  TT_Err_Divide_By_Zero          = $0405;
  TT_Err_Storage_Overflow        = $0406;
  TT_Err_Cvt_Overflow            = $0407;
  TT_Err_Invalid_Reference       = $0408;
  TT_Err_Invalid_Distance        = $0409;
  TT_Err_Interpolate_Twilight    = $040A;
  TT_Err_Debug_Opcode            = $040B;
  TT_Err_ENDF_In_Exec_Stream     = $040C;
  TT_Err_Out_Of_CodeRanges       = $040D;
  TT_Err_Nested_DEFs             = $040E;
  TT_Err_Invalid_CodeRange       = $040F;
  TT_Err_Invalid_Displacement    = $0410;
  TT_Err_Execution_Too_Long      = $0411;
  TT_Err_Too_Many_FuncDefs       = $0412;
  TT_Err_Too_Many_InsDefs        = $0413;

  TT_Err_Nested_Frame_Access     = $0500;
  TT_Err_Invalid_Cache_List      = $0501;
  TT_Err_Could_Not_Find_Context  = $0502;
  TT_Err_UNlisted_Object         = $0503;

  TT_Err_Raster_Pool_Overflow    = $0600;
  TT_Err_Raster_Negative_Height  = $0601;
  TT_Err_Invalid_Value           = $0602;
  TT_Err_Raster_Not_Initialised  = $0603;

  (* -------------------------------------------------------------- *)

  (***********************************************************************)
  (*                                                                     *)
  (*                  Base Library Functions                             *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  Initialise the engine                                        *)
  (*                                                               *)

type
  //  function  TT_Init_FreeType : TT_Error;cdecl;
  TTT_Init_FreeType =function (var Engine:TT_Engine):TT_Error;cdecl;

  (*****************************************************************)
  (*  Finalise the engine - discards all objects                   *)
  (*                                                               *)
//  procedure TT_Done_FreeType;
  TTT_Done_FreeType=function (Engine:TT_Engine):TT_Error;cdecl;

  (*****************************************************************)
  (*  Set the gray-level palette used for font-smoothing           *)
  (*                                                               *)
  (*  it is an array of 5 bytes following this convention :        *)
  (*                                                               *)
  (*    palette[0] := background (white)                           *)
  (*    palette[1] := light                                        *)
  (*    palette[2] := medium                                       *)
  (*    palette[3] := dark                                         *)
  (*    palette[4] := foreground (black)                           *)
  (*                                                               *)
//  function TT_Set_Raster_Palette( palette : TT_Gray_Palette ) : TT_Error;

  (***********************************************************************)
  (*                                                                     *)
  (*                    Face Management functions                        *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  Open a new font file and returns a handle for it in '_face'  *)
  (*                                                               *)
  (*  The file can be a TrueType collection, in which case the     *)
  (*  first embedded font will be loaded.                          *)
  (*                                                               *)
//  function TT_Open_Face( fontname  : string;
//                         var _face : TT_Face ) : TT_Error;
  TTT_Open_Face=function ( engine:TT_Engine;fontname:pchar;var _face : TT_Face ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Open a font file embedded in a collection.                   *)
  (*                                                               *)
//  function TT_Open_Collection( fontname  : string;
//                               faceIndex : integer;
//                               var _face : TT_Face ) : TT_Error;

  (*****************************************************************)
  (*  Return face properties in 'prop'                             *)
  (*                                                               *)
//  function TT_Get_Face_Properties( _face    : TT_Face;
//                                   var prop : TT_Face_Properties ) : TT_Error;
  TTT_Get_Face_Properties=function (_face    : TT_Face;
                                   var prop : TT_Face_Properties ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Set face's generic pointer                                   *)
  (*                                                               *)
//  function TT_Set_Face_Pointer( _face : TT_Face;
//                                data  : Pointer ) : TT_Error;

  (*****************************************************************)
  (*  Get face's generic pointer                                   *)
  (*                                                               *)
//  function TT_Get_Face_Pointer( _face : TT_Face ) : Pointer;

  (*****************************************************************)
  (*  close a given face object. This releases all child objects   *)
  (*  like instances and glyphs                                    *)
  (*                                                               *)
//  function TT_Close_Face( _face : TT_Face ) : TT_Error;
  TTT_Close_Face=function  ( _face : TT_Face ) : TT_Error;cdecl;

  (***********************************************************************)
  (*                                                                     *)
  (*                  Instance management functions                      *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  open a new face instance and return a handle in '_ins'       *)
  (*                                                               *)
//  function TT_New_Instance( _face    : TT_Face;
//                            var _ins : TT_Instance ) : TT_Error;
  TTT_New_Instance=function ( _face    : TT_Face;
                             var _ins : TT_Instance ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  set an instance's device resolutions, expressed in dpi       *)
  (*                                                               *)
//  function TT_Set_Instance_Resolutions( _ins         : TT_Instance;
//                                        x_resolution : Integer;
//                                        y_resolution : Integer ) : TT_Error;
  TTT_Set_Instance_Resolutions=function ( _ins         : TT_Instance;
                                        x_resolution : tt_ushort;
                                        y_resolution : tt_ushort ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  set an instance's point size (assumes width==height)         *)
  (*                                                               *)
//  function TT_Set_Instance_PointSize( _ins      : TT_Instance;
//                                      pointsize : Integer ) : TT_Error;

  (*****************************************************************)
  (*  set an instance's point size (assumes width==height)         *)
  (*                                                               *)
//  function TT_Set_Instance_CharSize( _ins     : TT_Instance;
//                                     charsize : Integer ) : TT_Error;
  TTT_Set_Instance_CharSize=function (_ins     : TT_Instance;
                                     charsize : Integer ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  set an instance's point size (assumes width==height)         *)
  (*                                                               *)
//  function TT_Set_Instance_CharSizes( _ins      : TT_Instance;
//                                      charsizex : Integer;
//                                      charsizey : Integer ) : TT_Error;

  (*****************************************************************)
  (*  set an instance's height and width, expressed in pixels      *)
  (*                                                               *)
//  function TT_Set_Instance_PixelSizes( _ins      : TT_Instance;
//                                       pixelX    : Integer;
//                                       pixelY    : Integer;
//                                       pointsize : Integer ) : TT_Error;

  (*****************************************************************)
  (*  set an instance's height and width, expressed in pixels      *)
  (*                                                               *)

  (*****************************************************************)
  (*  the core truetype engine doesn't provide _direct_ support    *)
  (*  for rotation or stretching. This means that the transforms   *)
  (*  must be applied on the glyph outline by a higher-level       *)
  (*  library or the client application. However, we use two flags *)
  (*  to notice the TrueType hinter that the glyphs will be        *)
  (*  transformed later.                                           *)
  (*                                                               *)
  (*    rotated   : set if the glyphs are to be rotated            *)
  (*    distorted : set if the glyphs are to be distorted          *)
  (*                                                               *)
  (*  an application is any transform that doesn't keep distances  *)
  (*  constants. skewing and stretching are examples of distorsion *)
  (*                                                               *)
//  function TT_Set_Instance_Transforms( _ins      : TT_Instance;
//                                       rotated   : Boolean;
//                                       distorted : Boolean ) : TT_Error;

  (*****************************************************************)
  (*  Return instance metrics in 'm'                               *)
  (*                                                               *)
//  function TT_Get_Instance_Metrics( _ins  : TT_Instance;
//                                    var m : TT_Instance_Metrics ) : TT_Error;
  TTT_Get_Instance_Metrics=function ( _ins  : TT_Instance;
                                    var m : TT_Instance_Metrics ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Set instance generic pointer                                 *)
  (*                                                               *)
//  function TT_Set_Instance_Pointer( _ins : TT_Instance;
//                                    data : Pointer ) : TT_Error;

  (*****************************************************************)
  (*  Get instance generic pointer                                 *)
  (*                                                               *)
//  function TT_Get_Instance_Pointer( _ins : TT_Instance ) : Pointer;

  (*****************************************************************)
  (*  Close an instance                                            *)
  (*                                                               *)
//  function TT_Done_Instance( _ins : TT_Instance ) : TT_Error;
  TTT_Done_Instance=function ( _ins : TT_Instance ) : TT_Error;cdecl;

  (***********************************************************************)
  (*                                                                     *)
  (*                  Glyph management functions                         *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  Create a new glyph container, return a handle in '_glyph'    *)
  (*                                                               *)
//  function TT_New_Glyph( _face      : TT_Face;
//                         var _glyph : TT_Glyph ) : TT_Error;
  TTT_New_Glyph=function ( _face      : TT_Face;
                         var _glyph : TT_Glyph ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Releases a glyph container                                   *)
  (*                                                               *)
//  function TT_Done_Glyph( _glyph : TT_Glyph ) : TT_Error;
  TTT_Done_Glyph=function (_glyph : TT_Glyph ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Load a glyph inside a container                              *)
  (*                                                               *)
//  function  TT_Load_Glyph( _instance   : TT_Instance;
//                           _glyph      : TT_Glyph;
//                           glyph_index : Word;
//                           load_flags  : Integer ) : TT_Error;
  TTT_Load_Glyph=function  (_instance   : TT_Instance;
                           _glyph      : TT_Glyph;
                           glyph_index : Word;
                           load_flags  : Integer ) : TT_Error;cdecl;

const
  TT_Load_Scale_Glyph = 1;  (* ask the loader to scale the glyph  *)
                            (* to the current pointsize/transform *)

  TT_Load_Hint_Glyph  = 2;  (* when scaling is on, ask the loader *)
                            (* to hint the glyph too..            *)

  TT_Load_Debug       = 16;

  TT_Load_Default     = TT_Load_Scale_Glyph or
                        TT_Load_Hint_Glyph;


  (*****************************************************************)
  (*  Get a glyph's outline                                        *)
  (*                                                               *)
//  function TT_Get_Glyph_Outline( _glyph      : TT_Glyph;
//                                 var outline : TT_Outline ) : TT_Error;

  (*****************************************************************)
  (*  Get a glyph's metrics                                        *)
  (*                                                               *)
//  function TT_Get_Glyph_Metrics( _glyph       : TT_Glyph;
//                                 var gmetrics : TT_Glyph_Metrics ) : TT_Error;
type
  TTT_Get_Glyph_Metrics=function ( _glyph       : TT_Glyph;
                                 var gmetrics : TT_Glyph_Metrics ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Get a glyph's big metrics                                    *)
  (*                                                               *)
//  function TT_Get_Glyph_Big_Metrics( _glyph       : TT_Glyph;
//                                     var gmetrics : TT_Big_Glyph_Metrics
//                                   ) : TT_Error;

  (*****************************************************************)
  (*  Render a glyph's bitmap                                      *)
  (*                                                               *)
//  function TT_Get_Glyph_Bitmap( _glyph   : TT_Glyph;
//                                var map  : TT_Raster_Map;
//                                x_offset : TT_F26Dot6;
//                                y_offset : TT_F26Dot6 ) : TT_Error;

  (*****************************************************************)
  (*  Render a glyph's pixmap (i.e. smoothed glyph )               *)
  (*                                                               *)
//  function TT_Get_Glyph_Pixmap( _glyph   : TT_Glyph;
//                                var map  : TT_Raster_Map;
//                                x_offset : TT_F26Dot6;
//                                y_offset : TT_F26Dot6 ) : TT_Error;

  (***********************************************************************)
  (*                                                                     *)
  (*                      Outline functions                              *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  Apply translation to an outline                              *)
  (*                                                               *)
//  function TT_Translate_Outline( var out : TT_Outline;
//                                 x, y    : TT_F26Dot6 ) : TT_Error;

  (*****************************************************************)
  (*  Apply a 2x2 transform to an outline                          *)
  (*                                                               *)
//  function TT_Transform_Outline( var out : TT_Outline;
//                                 var mat : TT_Matrix ) : TT_Error;

  (*****************************************************************)
  (*  Apply a 2x2 transform to a vector                            *)
  (*                                                               *)
//  function TT_Transform_Vector( var x, y : TT_F26Dot6;
//                                var mat  : TT_Matrix ) : TT_Error;

  (*****************************************************************)
  (*  Render an outline into a bitmap                              *)
  (*                                                               *)
//  function TT_Get_Outline_Bitmap( var out : TT_Outline;
//                                  var map : TT_raster_Map ) : TT_Error;

  (*****************************************************************)
  (*  Render an outline into a pixmap                              *)
  (*                                                               *)
//  function TT_Get_Outline_Pixmap( var out : TT_Outline;
//                                  var map : TT_raster_Map ) : TT_Error;


  (*****************************************************************)
  (*  Get an outline's bounding box                                *)
  (*                                                               *)
//  function TT_Get_Outline_BBox( var out  : TT_Outline;
//                                var bbox : TT_Bbox     ) : TT_Error;

  (*****************************************************************)
  (*  Create a new glyph outline                                   *)
  (*                                                               *)
//  function TT_New_Outline( n_points   : integer;
//                           n_contours : integer;
//                           var out    : TT_Outline ) : TT_Error;

  (*****************************************************************)
  (*  Copy a glyph outline into another one                        *)
  (*                                                               *)
//  function TT_Copy_Outline( var source : TT_Outline;
//                            var target : TT_Outline ) : TT_Error;

  (*****************************************************************)
  (*  Clone a given outline. This will create the outline, then    *)
  (*  copy the source in it                                        *)
  (*                                                               *)
//  function TT_Clone_Outline( var source : TT_Outline;
//                             var target : TT_Outline ) : TT_Error;

  (*****************************************************************)
  (*  Discards a glyph outline                                     *)
  (*                                                               *)
//  function TT_Done_Outline( var out : TT_Outline ) : TT_Error;

  (***********************************************************************)
  (*                                                                     *)
  (*                     Character Mapping support                       *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  Get a face's number of character maps                        *)
  (*                                                               *)
  TTT_Get_CharMap_Count=function  (face : TT_Face ) : integer;cdecl;

  (*****************************************************************)
  (*  Get a given char. map's ID in a face                         *)
  (*                                                               *)
//  function  TT_Get_CharMap_ID( face         : TT_Face;
//                               charmapIndex : integer;
 //                              var platform : integer;
  //                             var encoding : integer ) : TT_Error;
  TTT_Get_CharMap_ID=function( face         : TT_Face;
                               charmapIndex : integer;
                              var platform : integer;
                             var encoding : integer ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Get a handle to a given char. map                            *)
  (*                                                               *)
//  function  TT_Get_CharMap( face         : TT_Face;
//                            charmapIndex : integer;
//                            var charMap  : TT_CharMap ) : TT_Error;
  TTT_Get_CharMap=function  ( face         : TT_Face;
                            charmapIndex : integer;
                            var charMap  : TT_CharMap ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Translate from char. code to glyph index                     *)
  (*                                                               *)
//  function  TT_Char_Index( charmap  : TT_CharMap;
//                           charCode : Longint ) : Word;
  TTT_Char_Index=function  ( charmap  : TT_CharMap;
                           charCode : Longint ) : Word;cdecl;

  (***********************************************************************)
  (*                                                                     *)
  (*                        Names Table support                          *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  Return number of name table entries                          *)
  (*                                                               *)
//  function  TT_Get_Name_Count( face : TT_Face ) : integer;
  TTT_Get_Name_Count=function  ( face : TT_Face ) : integer;cdecl;
  (*****************************************************************)
  (*  Return the ID of a given name table entry                    *)
  (*                                                               *)
  TTT_Get_Name_ID=function  (face         : TT_Face;
                            nameIndex    : integer;
                            var platform : integer;
                            var encoding : integer;
                            var language : integer;
                            var nameid   : integer ) : TT_Error;cdecl;

  (*****************************************************************)
  (*  Return a given name table string                             *)
  (*                                                               *)
//  function  TT_Get_Name_String( face      : TT_Face;
//                                nameIndex : integer;
//                                var str   : Pointer;
//                                var len   : integer ) : TT_Error;
  TTT_Get_Name_String=function  ( face      : TT_Face;
                                nameIndex : integer;
                                var str   : Pointer;
                                var len   : integer ) : TT_Error;cdecl;

  (***********************************************************************)
  (*                                                                     *)
  (*                        Font Storage Access                          *)
  (*                                                                     *)
  (***********************************************************************)

  (*****************************************************************)
  (*  Access font data and copies it into user block               *)
  (*                                                               *)
//  function  TT_Get_Font_Data( face       : TT_Face;
//                              tableTag   : Longint;
//                              offset     : Longint;
//                              var buffer;
//                              var length : longint ) : TT_Error;

procedure CheckFreeTypeLoaded;
procedure LoadFreeType;
procedure FreeFreeType;
procedure CheckFreeType(avalue:TT_Error);

var
  FT_Init_FreeType:TFT_Init_FreeType;
  FT_Done_FreeType:TFT_Done_FreeType;
  FT_New_Face:TFT_New_Face;
  FT_Done_Face:TFT_Done_Face;
  FT_Get_Char_Index:TFT_Get_Char_Index;
  FT_Load_Glyph:TFT_Load_Glyph;
  FT_Set_Charmap:TFT_Set_Charmap;
  FT_Select_Charmap:TFT_Select_Charmap;

  TT_Get_Glyph_Metrics:TTT_Get_Glyph_Metrics;
  TT_Get_Name_Count:TTT_Get_Name_Count;
  TT_Get_Name_String:TTT_Get_Name_String;
  TT_Get_Name_ID:TTT_Get_Name_ID;
var
{$IFDEF MSWINDOWS}
 FreeTypeLib:HINST;
{$ENDIF}
{$IFDEF LINUX}
 FreeTypeLib:Pointer;
{$ENDIF}

implementation

procedure CheckFreeTypeLoaded;
begin
{$IFDEF LINUX}
 if Assigned(FreeTypeLib) then
  exit;
{$ENDIF}
{$IFDEF MSWINDOWS}
 if FreeTypeLib>HINSTANCE_ERROR then
  exit;
{$ENDIF}
 LoadFreeType;
end;

procedure LoadFreeType;
{$IFDEF MSWINDOWS}
  function GetProcAddr(ProcName: PChar): Pointer;
  begin
    Result := GetProcAddress(FreeTypeLib, ProcName);
    if not Assigned(Result) then
      RaiseLastOsError;
  end;
{$ENDIF}
{$IFDEF LINUX}
 function GetProcAddr(ProcName:Pchar):Pointer;
 begin
  Result:=dlsym(FreeTypeLib,ProcName);
  if not Assigned(Result) then
   raise Exception.Create(Format('Error loading %s,Error Code %s',[ProcName,dlerror]));
 end;
{$ENDIF}
begin
{$IFDEF LINUX}
 FreeTypeLib:=dlopen(Pchar(C_FREETYPE),RTLD_GLOBAL);
 if FreeTypeLib=nil then
  Raise Exception.Create('Error opening:'+C_FREETYPE);
{$ENDIF}

{$IFDEF MSWINDOWS}
  FreeTypeLib := LoadLibrary(PChar(C_FREETYPE));
  if (FreeTypeLib <= HINSTANCE_ERROR) then
   Raise Exception.Create('Error opening:'+C_FREETYPE);
{$ENDIF}

 // Load function addresses
 FT_Init_FreeType:=GetProcAddr('FT_Init_FreeType');
 FT_Done_FreeType:=GetProcAddr('FT_Done_FreeType');
 FT_New_Face:=GetProcAddr('FT_New_Face');
 FT_Done_Face:=GetProcAddr('FT_Done_Face');
 FT_Get_Char_Index:=GetProcAddr('FT_Get_Char_Index');
 FT_Load_Glyph:=GetProcAddr('FT_Load_Glyph');
 FT_Set_Charmap:=GetProcAddr('FT_Set_Charmap');
 FT_Select_Charmap:=GetProcAddr('FT_Select_Charmap');

{ TT_New_Instance:=GetProcAddr('TT_New_Instance');
 TT_Done_Instance:=GetProcAddr('TT_Done_Instance');
 TT_Set_Instance_CharSize:=GetProcAddr('TT_Set_Instance_CharSize');
 TT_Set_Instance_Resolutions:=GetProcAddr('TT_Set_Instance_Resolutions');
 TT_Get_Face_Properties:=GetProcAddr('TT_Get_Face_Properties');
 TT_New_Glyph:=GetProcAddr('TT_New_Glyph');
 TT_Done_Glyph:=GetProcAddr('TT_Done_Glyph');
 TT_Get_CharMap_Count:=GetProcAddr('TT_Get_CharMap_Count');
 TT_Get_CharMap:=GetProcAddr('TT_Get_CharMap');
 TT_Char_Index:=GetProcAddr('TT_Char_Index');
 TT_Load_Glyph:=GetProcAddr('TT_Load_Glyph');
 TT_Get_Glyph_Metrics:=GetProcAddr('TT_Get_Glyph_Metrics');
 TT_Get_Name_Count:=GetProcAddr('TT_Get_Name_Count');
 TT_Get_Name_String:=GetProcAddr('TT_Get_Name_String');
 TT_Get_Name_ID:=GetProcAddr('TT_Get_Name_ID');
}
end;

procedure FreeFreeType;
begin
{$IFDEF LINUX}
  if IBLibrary > HINSTANCE_ERROR then
  begin
    FreeLibrary(IBLibrary);
    IBLibrary := 0;
  end;
{$ENDIF}
 // Do nothing because the Linux Loader already frees it
end;

procedure CheckFreeType(avalue:FT_Error);
begin
 if avalue<>0 then
  Raise Exception.Create('Freetype 2 library error:'+IntToStr(avalue));
end;

initialization
{$IFDEF MSWINDOWS}
 FreeTypeLib:=HINSTANCE_ERROR;
{$ENDIF}
{$IFDEF LINUX}
 FreeTypeLib:=nil;
{$ENDIF}
finalization
 FreeFreeType;
end.
