unit jpeglib;

interface

uses  ctypes, BaseUnix, jmorecfg;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  Tboolean = boolean;
  Tdouble = cdouble;
  PFILE = Pointer;

{
 * jpeglib.h
 *
 * This file was part of the Independent JPEG Group's software:
 * Copyright (C) 1991-1998, Thomas G. Lane.
 * Modified 2002-2009 by Guido Vollbeding.
 * libjpeg-turbo Modifications:
 * Copyright (C) 2009-2011, 2013-2014, 2016-2017, 2020, D. R. Commander.
 * Copyright (C) 2015, Google, Inc.
 * For conditions of distribution and use, see the accompanying README.ijg
 * file.
 *
 * This file defines the application interface for the JPEG library.
 * Most applications using the library need only include this file,
 * and perhaps jerror.h if they want to know the exact error codes.
  }

{
 * First we include the configuration files that record how this
 * installation of the JPEG library is set up.  jconfig.h can be
 * generated automatically for many systems.  jmorecfg.h contains
 * manual configuration options that most people need not worry about.
  }
  //{$ifndef JCONFIG_INCLUDED        /* in case jinclude.h already did */}
  //{$include "jconfig.h"            /* widely used configuration options */}
  //{$endif}
  //{$include "jmorecfg.h"           /* seldom changed options */}
{ Various constants determining the sizes of things.
 * All of these are specified by the JPEG standard, so don't change them
 * if you want to be compatible.
  }
  { The basic DCT block is 8x8 samples  }

const
  JMSG_STR_PARM_MAX = 80;
  { recommended size of format_message buffer  }
  JMSG_LENGTH_MAX = 200;

type
  Tsize_t = SizeInt;


const
  DCTSIZE = 8;
  { DCTSIZE squared; # of elements in a block  }
  DCTSIZE2 = 64;
  { Quantization tables are numbered 0..3  }
  NUM_QUANT_TBLS = 4;
  { Huffman tables are numbered 0..3  }
  NUM_HUFF_TBLS = 4;
  { Arith-coding tables are numbered 0..15  }
  NUM_ARITH_TBLS = 16;
  { JPEG limit on # of components in one scan  }
  MAX_COMPS_IN_SCAN = 4;
  { JPEG limit on sampling factors  }
  MAX_SAMP_FACTOR = 4;
{ Unfortunately, some bozo at Adobe saw no reason to be bound by the standard;
 * the PostScript DCT filter can emit files with many more than 10 blocks/MCU.
 * If you happen to run across such a file, you can up D_MAX_BLOCKS_IN_MCU
 * to handle it.  We even let you do this from the jconfig.h file.  However,
 * we strongly discourage changing C_MAX_BLOCKS_IN_MCU; just because Adobe
 * sometimes emits noncompliant files doesn't mean you should too.
  }
  { compressor's limit on blocks per MCU  }
  C_MAX_BLOCKS_IN_MCU = 10;
  //{$ifndef D_MAX_BLOCKS_IN_MCU}
  { decompressor's limit on blocks per MCU  }

const
  D_MAX_BLOCKS_IN_MCU = 10;
  ///{$endif}
{ Data structures for images (arrays of samples and of DCT coefficients).
  }
type
  PJSAMPROW = ^TJSAMPROW;
  TJSAMPROW = PJSAMPLE;
  { ptr to one image row of pixel samples.  }

  PJSAMPARRAY = ^TJSAMPARRAY;
  TJSAMPARRAY = PJSAMPROW;
  { ptr to some rows (a 2-D sample array)  }

  PJSAMPIMAGE = ^TJSAMPIMAGE;
  TJSAMPIMAGE = PJSAMPARRAY;
  { a 3-D sample array: top index is color  }

  PJBLOCK = ^TJBLOCK;
  TJBLOCK = array[0..(DCTSIZE2) - 1] of TJCOEF;
  { one block of coefficients  }

  PJBLOCKROW = ^TJBLOCKROW;
  TJBLOCKROW = PJBLOCK;
  { pointer to one row of coefficient blocks  }

  PJBLOCKARRAY = ^TJBLOCKARRAY;
  TJBLOCKARRAY = PJBLOCKROW;
  { a 2-D array of coefficient blocks  }

  PJBLOCKIMAGE = ^TJBLOCKIMAGE;
  TJBLOCKIMAGE = PJBLOCKARRAY;
  { a 3-D array of coefficient blocks  }

  PJCOEFPTR = ^TJCOEFPTR;
  TJCOEFPTR = PJCOEF;
  { useful in a couple of places  }
  { Types for JPEG compression parameters and working tables.  }
  { DCT coefficient quantization tables.  }
{ This array gives the coefficient quantizers in natural array order
   * (not the zigzag order in which they are stored in a JPEG DQT marker).
   * CAUTION: IJG versions prior to v6a kept this array in zigzag order.
    }
  { quantization step for each coefficient  }
{ This field is used only during compression.  It's initialized FALSE when
   * the table is created, and set TRUE when it's been output to the file.
   * You could suppress output of a table by setting this to TRUE.
   * (See jpeg_suppress_tables for an example.)
    }
  { TRUE when table has been output  }

  PJQUANT_TBL = ^TJQUANT_TBL;

  TJQUANT_TBL = record
    quantval: array[0..(DCTSIZE2) - 1] of TUINT16;
    sent_table: Tboolean;
  end;
  { Huffman coding tables.  }
  { These two fields directly represent the contents of a JPEG DHT marker  }
  { bits[k] = # of symbols with codes of  }
  { length k bits; bits[0] is unused  }
  { The symbols, in order of incr code length  }
{ This field is used only during compression.  It's initialized FALSE when
   * the table is created, and set TRUE when it's been output to the file.
   * You could suppress output of a table by setting this to TRUE.
   * (See jpeg_suppress_tables for an example.)
    }
  { TRUE when table has been output  }

  PJHUFF_TBL = ^TJHUFF_TBL;

  TJHUFF_TBL = record
    bits: array[0..16] of TUINT8;
    huffval: array[0..255] of TUINT8;
    sent_table: Tboolean;
  end;
  { Basic info about one component (color channel).  }
  { These values are fixed over the whole image.  }
  { For compression, they must be supplied by parameter setup;  }
  { for decompression, they are read from the SOF marker.  }
  { identifier for this component (0..255)  }
  { its index in SOF or cinfo->comp_info[]  }
  { horizontal sampling factor (1..4)  }
  { vertical sampling factor (1..4)  }
  { quantization table selector (0..3)  }
  { These values may vary between scans.  }
  { For compression, they must be supplied by parameter setup;  }
  { for decompression, they are read from the SOS marker.  }
  { The decompressor output side may not use these variables.  }
  { DC entropy table selector (0..3)  }
  { AC entropy table selector (0..3)  }
  { Remaining fields should be treated as private by applications.  }
  { These values are computed during compression or decompression startup:  }
{ Component's size in DCT blocks.
   * Any dummy blocks added to complete an MCU are not counted; therefore
   * these values do not depend on whether a scan is interleaved or not.
    }
{ Size of a DCT block in samples.  Always DCTSIZE for compression.
   * For decompression this is the size of the output from one DCT block,
   * reflecting any scaling we choose to apply during the IDCT step.
   * Values from 1 to 16 are supported.
   * Note that different components may receive different IDCT scalings.
    }
  //{$if JPEG_LIB_VERSION >= 70}
  //{$else}
  //{$endif}
{ The downsampled dimensions are the component's actual, unpadded number
   * of samples at the main buffer (preprocessing/compression interface), thus
   * downsampled_width = ceil(image_width * Hi/Hmax)
   * and similarly for height.  For decompression, IDCT scaling is included, so
   * downsampled_width = ceil(image_width * Hi/Hmax * DCT_[h_]scaled_size/DCTSIZE)
    }
  { actual width in samples  }
  { actual height in samples  }
{ This flag is used only for decompression.  In cases where some of the
   * components will be ignored (eg grayscale output from YCbCr image),
   * we can skip most computations for the unused components.
    }
  { do we need the value of this component?  }
  { These values are computed before starting a scan of the component.  }
  { The decompressor output side may not use these variables.  }
  { number of blocks per MCU, horizontally  }
  { number of blocks per MCU, vertically  }
  { MCU_width * MCU_height  }
  { MCU width in samples, MCU_width*DCT_[h_]scaled_size  }
  { # of non-dummy blocks across in last MCU  }
  { # of non-dummy blocks down in last MCU  }
{ Saved quantization table for component; NULL if none yet saved.
   * See jdinput.c comments about the need for this information.
   * This field is currently used only for decompression.
    }
  { Private per-component storage for DCT or IDCT subsystem.  }
type
  Pjpeg_component_info = ^Tjpeg_component_info;

  Tjpeg_component_info = record
    component_id: longint;
    component_index: longint;
    h_samp_factor: longint;
    v_samp_factor: longint;
    quant_tbl_no: longint;
    dc_tbl_no: longint;
    ac_tbl_no: longint;
    width_in_blocks: TJDIMENSION;
    height_in_blocks: TJDIMENSION;
    DCT_h_scaled_size: longint;
    DCT_v_scaled_size: longint;
    DCT_scaled_size: longint;
    downsampled_width: TJDIMENSION;
    downsampled_height: TJDIMENSION;
    component_needed: Tboolean;
    MCU_width: longint;
    MCU_height: longint;
    MCU_blocks: longint;
    MCU_sample_width: longint;
    last_col_width: longint;
    last_row_height: longint;
    quant_table: PJQUANT_TBL;
    dct_table: pointer;
  end;
  { The script for encoding a multiple-scan file is an array of these:  }
  { number of components encoded in this scan  }
  { their SOF/comp_info[] indexes  }
  { progressive JPEG spectral selection parms  }
  { progressive JPEG successive approx. parms  }

  Pjpeg_scan_info = ^Tjpeg_scan_info;

  Tjpeg_scan_info = record
    comps_in_scan: longint;
    component_index: array[0..(MAX_COMPS_IN_SCAN) - 1] of longint;
    Ss: longint;
    Se: longint;
    Ah: longint;
    Al: longint;
  end;
  { The decompressor can save APPn and COM markers in a list of these:  }

  Pjpeg_saved_marker_ptr = ^Tjpeg_saved_marker_ptr;
  Tjpeg_saved_marker_ptr = ^Tjpeg_marker_struct;
  { next in list, or NULL  }
  { marker code: JPEG_COM, or JPEG_APP0+n  }
  { # bytes of data in the file  }
  { # bytes of data saved at data[]  }
  { the data contained in the marker  }
  { the marker length word is not counted in data_length or original_length  }
  Pjpeg_marker_struct = ^Tjpeg_marker_struct;

  Tjpeg_marker_struct = record
    Next: Tjpeg_saved_marker_ptr;
    marker: TUINT8;
    original_length: dword;
    data_length: dword;
    Data: PJOCTET;
  end;

  { Known color spaces.  }

const
  JCS_EXTENSIONS = 1;
  JCS_ALPHA_EXTENSIONS = 1;
  { error/unspecified  }
  { monochrome  }
{ red/green/blue as specified by the RGB_RED,
                             RGB_GREEN, RGB_BLUE, and RGB_PIXELSIZE macros  }
  { Y/Cb/Cr (also known as YUV)  }
  { C/M/Y/K  }
  { Y/Cb/Cr/K  }
  { red/green/blue  }
  { red/green/blue/x  }
  { blue/green/red  }
  { blue/green/red/x  }
  { x/blue/green/red  }
  { x/red/green/blue  }
{ When out_color_space it set to JCS_EXT_RGBX, JCS_EXT_BGRX, JCS_EXT_XBGR,
     or JCS_EXT_XRGB during decompression, the X byte is undefined, and in
     order to ensure the best performance, libjpeg-turbo can set that byte to
     whatever value it wishes.  Use the following colorspace constants to
     ensure that the X byte is set to 0xFF, so that it can be interpreted as an
     opaque alpha channel.  }
  { red/green/blue/alpha  }
  { blue/green/red/alpha  }
  { alpha/blue/green/red  }
  { alpha/red/green/blue  }
  { 5-bit red/6-bit green/5-bit blue  }
type
  PJ_COLOR_SPACE = ^TJ_COLOR_SPACE;
  TJ_COLOR_SPACE = longint;

const
  JCS_UNKNOWN = 0;
  JCS_GRAYSCALE = 1;
  JCS_RGB = 2;
  JCS_YCbCr = 3;
  JCS_CMYK = 4;
  JCS_YCCK = 5;
  JCS_EXT_RGB = 6;
  JCS_EXT_RGBX = 7;
  JCS_EXT_BGR = 8;
  JCS_EXT_BGRX = 9;
  JCS_EXT_XBGR = 10;
  JCS_EXT_XRGB = 11;
  JCS_EXT_RGBA = 12;
  JCS_EXT_BGRA = 13;
  JCS_EXT_ABGR = 14;
  JCS_EXT_ARGB = 15;
  JCS_RGB565 = 16;

  { DCT/IDCT algorithm options.  }
  { accurate integer method  }
  { less accurate integer method [legacy feature]  }
  { floating-point method [legacy feature]  }
type
  PJ_DCT_METHOD = ^TJ_DCT_METHOD;
  TJ_DCT_METHOD = longint;

const
  JDCT_ISLOW = 0;
  JDCT_IFAST = 1;
  JDCT_FLOAT = 2;

  //{$ifndef JDCT_DEFAULT            /* may be overridden in jconfig.h */}

const
  JDCT_DEFAULT = JDCT_ISLOW;
  //{$endif}
  //{$ifndef JDCT_FASTEST            /* may be overridden in jconfig.h */}

const
  JDCT_FASTEST = JDCT_IFAST;
  //{$endif}
  { Dithering options for decompression.  }
  { no dithering  }
  { simple ordered dither  }
  { Floyd-Steinberg error diffusion dither  }
type
  PJ_DITHER_MODE = ^TJ_DITHER_MODE;
  TJ_DITHER_MODE = longint;

const
  JDITHER_NONE = 0;
  JDITHER_ORDERED = 1;
  JDITHER_FS = 2;

  { Common fields between JPEG compression and decompression master structs.  }
  {#define jpeg_common_fields \ }
  {  struct jpeg_error_mgr *err;   /* Error handler module */ \ }
  {  struct jpeg_memory_mgr *mem;  /* Memory manager module */ \ }
  {  struct jpeg_progress_mgr *progress; /* Progress monitor, or NULL if none */ \ }
  {  void *client_data;            /* Available for use by application */ \ }
  {  boolean is_decompressor;      /* So common code can tell which is which */ \ }
  {  int global_state              /* For checking call sequence validity */ }
{ Routines that are to be used by both halves of the library are declared
 * to receive a pointer to this structure.  There are no actual instances of
 * jpeg_common_struct, only of jpeg_compress_struct and jpeg_decompress_struct.
  }
  {  jpeg_common_fields;           /* Fields common to both master struct types */ }
  { Error handler module  }{ Memory manager module  }{ Progress monitor, or NULL if none  }{ Available for use by application  }{ So common code can tell which is which  }{ For checking call sequence validity *



  /* Additional fields follow in an actual jpeg_compress_struct or
   * jpeg_decompress_struct.  All three structs must agree on these
   * initial fields!  (This would be a lot cleaner in C++.)
    }
type
  Pj_common_ptr = ^Tj_common_ptr;
  Tj_common_ptr = ^Tjpeg_common_struct;

  Pjpeg_common_struct = ^Tjpeg_common_struct;

  Tjpeg_common_struct = record
    err: ^Tjpeg_error_mgr;
    mem: ^Tjpeg_memory_mgr;
    progress: ^Tjpeg_progress_mgr;
    client_data: pointer;
    is_decompressor: Tboolean;
    global_state: longint;
  end;

  Pjpeg_error_mgr = ^Tjpeg_error_mgr;

  Tjpeg_error_mgr = record
    error_exit: procedure(cinfo: Tj_common_ptr); cdecl;
    emit_message: procedure(cinfo: Tj_common_ptr; msg_level: longint); cdecl;
    output_message: procedure(cinfo: Tj_common_ptr); cdecl;
    format_message: procedure(cinfo: Tj_common_ptr; buffer: PChar); cdecl;
    reset_error_mgr: procedure(cinfo: Tj_common_ptr); cdecl;
    msg_code: longint;
    msg_parm: record
      case longint of
        0: (i: array[0..7] of longint);
        1: (s: array[0..(JMSG_STR_PARM_MAX) - 1] of char);
      end;
    trace_level: longint;
    num_warnings: longint;
    jpeg_message_table: ^PChar;
    last_jpeg_message: longint;
    addon_message_table: ^PChar;
    first_addon_message: longint;
    last_addon_message: longint;
  end;



  Pj_compress_ptr = ^Tj_compress_ptr;
  Tj_compress_ptr = ^Tjpeg_compress_struct;

  Pj_decompress_ptr = ^Tj_decompress_ptr;
  Tj_decompress_ptr = ^Tjpeg_decompress_struct;

  //type
  // --- neu
  Pjvirt_sarray_control = Pointer;
  Pjvirt_barray_control = Pointer;
  // ---

  Pjvirt_sarray_ptr = ^Tjvirt_sarray_ptr;
  Tjvirt_sarray_ptr = Pjvirt_sarray_control;

  Pjvirt_barray_ptr = ^Tjvirt_barray_ptr;
  Tjvirt_barray_ptr = Pjvirt_barray_control;
  { Method pointers  }
{ Limit on memory allocation for this JPEG object.  (Note that this is
   * merely advisory, not a guaranteed maximum; it only affects the space
   * used for virtual-array buffers.)  May be changed by outer application
   * after creating the JPEG object.
    }
  { Maximum allocation request accepted by alloc_large.  }
{ Routine signature for application-supplied marker processing methods.
 * Need not pass marker code since it is stored in cinfo->unread_marker.
  }

  Tjpeg_marker_parser_method = function(cinfo: Tj_decompress_ptr): Tboolean; cdecl;
{ Originally, this macro was used as a way of defining function prototypes
 * for both modern compilers as well as older compilers that did not support
 * prototype parameters.  libjpeg-turbo has never supported these older,
 * non-ANSI compilers, but the macro is still included because there is some
 * software out there that uses it.
  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }


  Pjpeg_memory_mgr = ^Tjpeg_memory_mgr;

  Tjpeg_memory_mgr = record
    alloc_small: function(cinfo: Tj_common_ptr; pool_id: longint; sizeofobject: Tsize_t): pointer; cdecl;
    alloc_large: function(cinfo: Tj_common_ptr; pool_id: longint; sizeofobject: Tsize_t): pointer; cdecl;
    alloc_sarray: function(cinfo: Tj_common_ptr; pool_id: longint; samplesperrow: TJDIMENSION; numrows: TJDIMENSION): TJSAMPARRAY; cdecl;
    alloc_barray: function(cinfo: Tj_common_ptr; pool_id: longint; blocksperrow: TJDIMENSION; numrows: TJDIMENSION): TJBLOCKARRAY; cdecl;
    request_virt_sarray: function(cinfo: Tj_common_ptr; pool_id: longint; pre_zero: Tboolean; samplesperrow: TJDIMENSION; numrows: TJDIMENSION;
      maxaccess: TJDIMENSION): Tjvirt_sarray_ptr; cdecl;
    request_virt_barray: function(cinfo: Tj_common_ptr; pool_id: longint; pre_zero: Tboolean; blocksperrow: TJDIMENSION; numrows: TJDIMENSION;
      maxaccess: TJDIMENSION): Tjvirt_barray_ptr; cdecl;
    realize_virt_arrays: procedure(cinfo: Tj_common_ptr); cdecl;
    access_virt_sarray: function(cinfo: Tj_common_ptr; ptr: Tjvirt_sarray_ptr; start_row: TJDIMENSION; num_rows: TJDIMENSION; writable: Tboolean): TJSAMPARRAY; cdecl;
    access_virt_barray: function(cinfo: Tj_common_ptr; ptr: Tjvirt_barray_ptr; start_row: TJDIMENSION; num_rows: TJDIMENSION; writable: Tboolean): TJBLOCKARRAY; cdecl;
    free_pool: procedure(cinfo: Tj_common_ptr; pool_id: longint); cdecl;
    self_destruct: procedure(cinfo: Tj_common_ptr); cdecl;
    max_memory_to_use: longint;
    max_alloc_chunk: longint;
  end;

  { Progress monitor object  }
  { work units completed in this pass  }
  { total number of work units in this pass  }
  { passes completed so far  }
  { total number of passes expected  }
  Pjpeg_progress_mgr = ^Tjpeg_progress_mgr;

  Tjpeg_progress_mgr = record
    progress_monitor: procedure(cinfo: Tj_common_ptr); cdecl;
    pass_counter: longint;
    pass_limit: longint;
    completed_passes: longint;
    total_passes: longint;
  end;

  { Data destination object for compression  }
  { => next byte to write in buffer  }
  { # of byte spaces remaining in buffer  }
  Pjpeg_destination_mgr = ^Tjpeg_destination_mgr;

  Tjpeg_destination_mgr = record
    next_output_byte: PJOCTET;
    free_in_buffer: Tsize_t;
    init_destination: procedure(cinfo: Tj_compress_ptr); cdecl;
    empty_output_buffer: function(cinfo: Tj_compress_ptr): Tboolean; cdecl;
    term_destination: procedure(cinfo: Tj_compress_ptr); cdecl;
  end;

  { Data source object for decompression  }
  (* Const before type ignored *)
  { => next byte to read from buffer  }
  { # of bytes remaining in buffer  }
  Pjpeg_source_mgr = ^Tjpeg_source_mgr;

  Tjpeg_source_mgr = record
    next_input_byte: PJOCTET;
    bytes_in_buffer: Tsize_t;
    init_source: procedure(cinfo: Tj_decompress_ptr); cdecl;
    fill_input_buffer: function(cinfo: Tj_decompress_ptr): Tboolean; cdecl;
    skip_input_data: procedure(cinfo: Tj_decompress_ptr; num_bytes: longint); cdecl;
    resync_to_restart: function(cinfo: Tj_decompress_ptr; desired: longint): Tboolean; cdecl;
    term_source: procedure(cinfo: Tj_decompress_ptr); cdecl;
  end;

{ Memory manager object.
 * Allocates "small" objects (a few K total), "large" objects (tens of K),
 * and "really big" objects (virtual arrays with backing store if needed).
 * The memory manager does not allow individual objects to be freed; rather,
 * each created object is assigned to a pool, and whole pools can be freed
 * at once.  This is faster and more convenient than remembering exactly what
 * to free, especially where malloc()/free() are not too speedy.
 * NB: alloc routines never return NULL.  They exit to error_exit if not
 * successful.
  }
  { lasts until master record is destroyed  }

  // --- neu
  Pjpeg_comp_master = Pointer;
  Pjpeg_c_main_controller = Pointer;
  Pjpeg_c_prep_controller = Pointer;
  Pjpeg_c_coef_controller = Pointer;
  Pjpeg_marker_writer = Pointer;
  Pjpeg_color_converter = Pointer;
  Pjpeg_downsampler = Pointer;
  Pjpeg_forward_dct = Pointer;
  Pjpeg_entropy_encoder = Pointer;
  // ---


  Pjpeg_compress_struct = ^Tjpeg_compress_struct;

  Tjpeg_compress_struct = record
    err: Pjpeg_error_mgr;
    mem: Pjpeg_memory_mgr;
    progress: Pjpeg_progress_mgr;
    client_data: pointer;
    is_decompressor: Tboolean;
    global_state: longint;
    dest: Pjpeg_destination_mgr;
    image_width: TJDIMENSION;
    image_height: TJDIMENSION;
    input_components: longint;
    in_color_space: TJ_COLOR_SPACE;
    input_gamma: Tdouble;
    scale_num: dword;
    scale_denom: dword;
    jpeg_width: TJDIMENSION;
    jpeg_height: TJDIMENSION;
    data_precision: longint;
    num_components: longint;
    jpeg_color_space: TJ_COLOR_SPACE;
    comp_info: Pjpeg_component_info;
    quant_tbl_ptrs: array[0..(NUM_QUANT_TBLS) - 1] of PJQUANT_TBL;
    q_scale_factor: array[0..(NUM_QUANT_TBLS) - 1] of longint;
    dc_huff_tbl_ptrs: array[0..(NUM_HUFF_TBLS) - 1] of PJHUFF_TBL;
    ac_huff_tbl_ptrs: array[0..(NUM_HUFF_TBLS) - 1] of PJHUFF_TBL;
    arith_dc_L: array[0..(NUM_ARITH_TBLS) - 1] of TUINT8;
    arith_dc_U: array[0..(NUM_ARITH_TBLS) - 1] of TUINT8;
    arith_ac_K: array[0..(NUM_ARITH_TBLS) - 1] of TUINT8;
    num_scans: longint;
    scan_info: Pjpeg_scan_info;
    raw_data_in: Tboolean;
    arith_code: Tboolean;
    optimize_coding: Tboolean;
    CCIR601_sampling: Tboolean;
    do_fancy_downsampling: Tboolean;
    smoothing_factor: longint;
    dct_method: TJ_DCT_METHOD;
    restart_interval: dword;
    restart_in_rows: longint;
    write_JFIF_header: Tboolean;
    JFIF_major_version: TUINT8;
    JFIF_minor_version: TUINT8;
    density_unit: TUINT8;
    X_density: TUINT16;
    Y_density: TUINT16;
    write_Adobe_marker: Tboolean;
    next_scanline: TJDIMENSION;
    progressive_mode: Tboolean;
    max_h_samp_factor: longint;
    max_v_samp_factor: longint;
    min_DCT_h_scaled_size: longint;
    min_DCT_v_scaled_size: longint;
    total_iMCU_rows: TJDIMENSION;
    comps_in_scan: longint;
    cur_comp_info: array[0..(MAX_COMPS_IN_SCAN) - 1] of Pjpeg_component_info;
    MCUs_per_row: TJDIMENSION;
    MCU_rows_in_scan: TJDIMENSION;
    blocks_in_MCU: longint;
    MCU_membership: array[0..(C_MAX_BLOCKS_IN_MCU) - 1] of longint;
    Ss: longint;
    Se: longint;
    Ah: longint;
    Al: longint;
    block_size: longint;
    natural_order: Plongint;
    lim_Se: longint;
    master: Pjpeg_comp_master;
    main: Pjpeg_c_main_controller;
    prep: Pjpeg_c_prep_controller;
    coef: Pjpeg_c_coef_controller;
    marker: Pjpeg_marker_writer;
    cconvert: Pjpeg_color_converter;
    downsample: Pjpeg_downsampler;
    fdct: Pjpeg_forward_dct;
    entropy: Pjpeg_entropy_encoder;
    script_space: Pjpeg_scan_info;
    script_space_size: longint;
  end;

  // --- neu

  Tcoef_bits = array[0..(DCTSIZE2) - 1] of longint;
  Pcoef_bits = ^Tcoef_bits;

  Pjpeg_decomp_master = Pointer;
  Pjpeg_d_main_controller = Pointer;
  Pjpeg_d_coef_controller = Pointer;
  Pjpeg_d_post_controller = Pointer;
  Pjpeg_input_controller = Pointer;
  Pjpeg_marker_reader = Pointer;
  Pjpeg_entropy_decoder = Pointer;
  Pjpeg_inverse_dct = Pointer;
  Pjpeg_upsampler = Pointer;
  Pjpeg_color_deconverter = Pointer;
  Pjpeg_color_quantizer = Pointer;

  // ---


  Pjpeg_decompress_struct = ^Tjpeg_decompress_struct;

  Tjpeg_decompress_struct = record
    err: Pjpeg_error_mgr;
    mem: Pjpeg_memory_mgr;
    progress: Pjpeg_progress_mgr;
    client_data: pointer;
    is_decompressor: Tboolean;
    global_state: longint;
    src: Pjpeg_source_mgr;
    image_width: TJDIMENSION;
    image_height: TJDIMENSION;
    num_components: longint;
    jpeg_color_space: TJ_COLOR_SPACE;
    out_color_space: TJ_COLOR_SPACE;
    scale_num: dword;
    scale_denom: dword;
    output_gamma: Tdouble;
    buffered_image: Tboolean;
    raw_data_out: Tboolean;
    dct_method: TJ_DCT_METHOD;
    do_fancy_upsampling: Tboolean;
    do_block_smoothing: Tboolean;
    quantize_colors: Tboolean;
    dither_mode: TJ_DITHER_MODE;
    two_pass_quantize: Tboolean;
    desired_number_of_colors: longint;
    enable_1pass_quant: Tboolean;
    enable_external_quant: Tboolean;
    enable_2pass_quant: Tboolean;
    output_width: TJDIMENSION;
    output_height: TJDIMENSION;
    out_color_components: longint;
    output_components: longint;
    rec_outbuf_height: longint;
    actual_number_of_colors: longint;
    colormap: TJSAMPARRAY;
    output_scanline: TJDIMENSION;
    input_scan_number: longint;
    input_iMCU_row: TJDIMENSION;
    output_scan_number: longint;
    output_iMCU_row: TJDIMENSION;
    coef_bits: tcoef_bits;
    quant_tbl_ptrs: array[0..(NUM_QUANT_TBLS) - 1] of PJQUANT_TBL;
    dc_huff_tbl_ptrs: array[0..(NUM_HUFF_TBLS) - 1] of PJHUFF_TBL;
    ac_huff_tbl_ptrs: array[0..(NUM_HUFF_TBLS) - 1] of PJHUFF_TBL;
    data_precision: longint;
    comp_info: Pjpeg_component_info;
    is_baseline: Tboolean;
    progressive_mode: Tboolean;
    arith_code: Tboolean;
    arith_dc_L: array[0..(NUM_ARITH_TBLS) - 1] of TUINT8;
    arith_dc_U: array[0..(NUM_ARITH_TBLS) - 1] of TUINT8;
    arith_ac_K: array[0..(NUM_ARITH_TBLS) - 1] of TUINT8;
    restart_interval: dword;
    saw_JFIF_marker: Tboolean;
    JFIF_major_version: TUINT8;
    JFIF_minor_version: TUINT8;
    density_unit: TUINT8;
    X_density: TUINT16;
    Y_density: TUINT16;
    saw_Adobe_marker: Tboolean;
    Adobe_transform: TUINT8;
    CCIR601_sampling: Tboolean;
    marker_list: Tjpeg_saved_marker_ptr;
    max_h_samp_factor: longint;
    max_v_samp_factor: longint;
    min_DCT_h_scaled_size: longint;
    min_DCT_v_scaled_size: longint;
    min_DCT_scaled_size: longint;
    total_iMCU_rows: TJDIMENSION;
    sample_range_limit: PJSAMPLE;
    comps_in_scan: longint;
    cur_comp_info: array[0..(MAX_COMPS_IN_SCAN) - 1] of Pjpeg_component_info;
    MCUs_per_row: TJDIMENSION;
    MCU_rows_in_scan: TJDIMENSION;
    blocks_in_MCU: longint;
    MCU_membership: array[0..(D_MAX_BLOCKS_IN_MCU) - 1] of longint;
    Ss: longint;
    Se: longint;
    Ah: longint;
    Al: longint;
    block_size: longint;
    natural_order: Plongint;
    lim_Se: longint;
    unread_marker: longint;
    master: Pjpeg_decomp_master;
    main: Pjpeg_d_main_controller;
    coef: Pjpeg_d_coef_controller;
    post: Pjpeg_d_post_controller;
    inputctl: Pjpeg_input_controller;
    marker: Pjpeg_marker_reader;
    entropy: Pjpeg_entropy_decoder;
    idct: Pjpeg_inverse_dct;
    upsample: Pjpeg_upsampler;
    cconvert: Pjpeg_color_deconverter;
    cquantize: Pjpeg_color_quantizer;
  end;

{ "Object" declarations for JPEG modules that may be supplied or called
 * directly by the surrounding application.
 * As with all objects in the JPEG library, these structs only define the
 * publicly visible methods and state variables of a module.  Additional
 * private fields may exist after the public ones.
  }
  { Error handler object  }

  { Error exit handler: does not return to caller  }
  { Conditionally emit a trace or warning message  }
  { Routine that actually outputs a trace or error message  }
  { Format a message string for the most recent JPEG error or message  }
  { Reset error state variables at start of a new image  }
{ The message ID code and any parameters are saved here.
   * A message can have one string parameter or up to 8 int parameters.
    }
  { Standard state variables for error facility  }
  { max msg_level that will be displayed  }
{ For recoverable corrupt-data errors, we emit a warning message,
   * but keep going unless emit_message chooses to abort.  emit_message
   * should count warnings in num_warnings.  The surrounding application
   * can check for bad data by seeing if num_warnings is nonzero at the
   * end of processing.
    }
  { number of corrupt-data warnings  }
{ These fields point to the table(s) of error message strings.
   * An application can change the table pointer to switch to a different
   * message list (typically, to change the language in which errors are
   * reported).  Some applications may wish to add additional error codes
   * that will be handled by the JPEG library error mechanism; the second
   * table pointer is used for this purpose.
   *
   * First table includes all errors generated by JPEG library itself.
   * Error code 0 is reserved for a "no such error string" message.
    }
  (* Const before type ignored *)
  (* Const before declarator ignored *)
  { Library errors  }
  { Table contains strings 0..last_jpeg_message  }
{ Second table can be added by application (see cjpeg/djpeg for example).
   * It contains strings numbered first_addon_message..last_addon_message.
    }
  (* Const before type ignored *)
  (* Const before declarator ignored *)
  { Non-library errors  }
  { code for first string in addon table  }
  { code for last string in addon table  }
const
  JPOOL_PERMANENT = 0;
  { lasts until done with image/datastream  }
  JPOOL_IMAGE = 1;
  JPOOL_NUMPOOLS = 2;

function JPP(arglist: longint): longint;

{ Default error-management setup  }
function jpeg_std_error(err: Pjpeg_error_mgr): Pjpeg_error_mgr; cdecl; external;
{ Initialization of JPEG compression objects.
 * jpeg_create_compress() and jpeg_create_decompress() are the exported
 * names that applications should call.  These expand to calls on
 * jpeg_CreateCompress and jpeg_CreateDecompress with additional information
 * passed for version mismatch checking.
 * NB: you must set up the error-manager BEFORE calling jpeg_create_xxx.
  }
{#define jpeg_create_compress(cinfo) \ }
{  jpeg_CreateCompress((cinfo), JPEG_LIB_VERSION, \ }
{                      (size_t)sizeof(struct jpeg_compress_struct)) }
{#define jpeg_create_decompress(cinfo) \ }
{  jpeg_CreateDecompress((cinfo), JPEG_LIB_VERSION, \ }
{                        (size_t)sizeof(struct jpeg_decompress_struct)) }
procedure jpeg_CreateCompress(cinfo: Tj_compress_ptr; version: longint; structsize: Tsize_t); cdecl; external;
procedure jpeg_CreateDecompress(cinfo: Tj_decompress_ptr; version: longint; structsize: Tsize_t); cdecl; external;
{ Destruction of JPEG compression objects  }
procedure jpeg_destroy_compress(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_destroy_decompress(cinfo: Tj_decompress_ptr); cdecl; external;
{ Standard data source and destination managers: stdio streams.  }
{ Caller is responsible for opening the file before and closing after.  }
procedure jpeg_stdio_dest(cinfo: Tj_compress_ptr; outfile: PFILE); cdecl; external;
procedure jpeg_stdio_src(cinfo: Tj_decompress_ptr; infile: PFILE); cdecl; external;
{ Data source and destination managers: memory buffers.  }

procedure jpeg_mem_dest(cinfo: Tj_compress_ptr; outbuffer: PPbyte; outsize: Pdword); cdecl; external;
(* Const before type ignored *)
procedure jpeg_mem_src(cinfo: Tj_decompress_ptr; inbuffer: pbyte; insize: dword); cdecl; external;
{ Default parameter setup for compression  }

procedure jpeg_set_defaults(cinfo: Tj_compress_ptr); cdecl; external;
{ Compression parameter setup aids  }
procedure jpeg_set_colorspace(cinfo: Tj_compress_ptr; colorspace: TJ_COLOR_SPACE); cdecl; external;
procedure jpeg_default_colorspace(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_set_quality(cinfo: Tj_compress_ptr; quality: longint; force_baseline: Tboolean); cdecl; external;
procedure jpeg_set_linear_quality(cinfo: Tj_compress_ptr; scale_factor: longint; force_baseline: Tboolean); cdecl; external;

procedure jpeg_default_qtables(cinfo: Tj_compress_ptr; force_baseline: Tboolean); cdecl; external;
(* Const before type ignored *)

procedure jpeg_add_quant_table(cinfo: Tj_compress_ptr; which_tbl: longint; basic_table: Pdword; scale_factor: longint; force_baseline: Tboolean); cdecl; external;
function jpeg_quality_scaling(quality: longint): longint; cdecl; external;
procedure jpeg_simple_progression(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_suppress_tables(cinfo: Tj_compress_ptr; suppress: Tboolean); cdecl; external;
function jpeg_alloc_quant_table(cinfo: Tj_common_ptr): PJQUANT_TBL; cdecl; external;
function jpeg_alloc_huff_table(cinfo: Tj_common_ptr): PJHUFF_TBL; cdecl; external;
{ Main entry points for compression  }
procedure jpeg_start_compress(cinfo: Tj_compress_ptr; write_all_tables: Tboolean); cdecl; external;
function jpeg_write_scanlines(cinfo: Tj_compress_ptr; scanlines: TJSAMPARRAY; num_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
procedure jpeg_finish_compress(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_calc_jpeg_dimensions(cinfo: Tj_compress_ptr); cdecl; external;

function jpeg_write_raw_data(cinfo: Tj_compress_ptr; Data: TJSAMPIMAGE; num_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
{ Write a special marker.  See libjpeg.txt concerning safe usage.  }
(* Const before type ignored *)
procedure jpeg_write_marker(cinfo: Tj_compress_ptr; marker: longint; dataptr: PJOCTET; datalen: dword); cdecl; external;
{ Same, but piecemeal.  }
procedure jpeg_write_m_header(cinfo: Tj_compress_ptr; marker: longint; datalen: dword); cdecl; external;
procedure jpeg_write_m_byte(cinfo: Tj_compress_ptr; val: longint); cdecl; external;
{ Alternate compression function: just write an abbreviated table file  }
procedure jpeg_write_tables(cinfo: Tj_compress_ptr); cdecl; external;
{ Write ICC profile.  See libjpeg.txt for usage information.  }
(* Const before type ignored *)
procedure jpeg_write_icc_profile(cinfo: Tj_compress_ptr; icc_data_ptr: PJOCTET; icc_data_len: dword); cdecl; external;
{ Decompression startup: read start of JPEG datastream to see what's there  }
function jpeg_read_header(cinfo: Tj_decompress_ptr; require_image: Tboolean): longint; cdecl; external;
{ Return value is one of:  }
{ Suspended due to lack of input data  }
const
  JPEG_SUSPENDED = 0;
  { Found valid image datastream  }
  JPEG_HEADER_OK = 1;
  { Found valid table-specs-only datastream  }
  JPEG_HEADER_TABLES_ONLY = 2;

{ If you pass require_image = TRUE (normal case), you need not check for
 * a TABLES_ONLY return code; an abbreviated file will cause an error exit.
 * JPEG_SUSPENDED is only possible if you use a data source module that can
 * give a suspension return (the stdio source module doesn't).
  }
  { Main entry points for decompression  }

function jpeg_start_decompress(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
function jpeg_read_scanlines(cinfo: Tj_decompress_ptr; scanlines: TJSAMPARRAY; max_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
function jpeg_skip_scanlines(cinfo: Tj_decompress_ptr; num_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
procedure jpeg_crop_scanline(cinfo: Tj_decompress_ptr; xoffset: PJDIMENSION; Width: PJDIMENSION); cdecl; external;
function jpeg_finish_decompress(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
{ Replaces jpeg_read_scanlines when reading raw downsampled data.  }
function jpeg_read_raw_data(cinfo: Tj_decompress_ptr; Data: TJSAMPIMAGE; max_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
{ Additional entry points for buffered-image mode.  }
function jpeg_has_multiple_scans(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
function jpeg_start_output(cinfo: Tj_decompress_ptr; scan_number: longint): Tboolean; cdecl; external;
function jpeg_finish_output(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
function jpeg_input_complete(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
procedure jpeg_new_colormap(cinfo: Tj_decompress_ptr); cdecl; external;
function jpeg_consume_input(cinfo: Tj_decompress_ptr): longint; cdecl; external;
{ Return value is one of:  }
{ #define JPEG_SUSPENDED       0    Suspended due to lack of input data  }
{ Reached start of new scan  }
const
  JPEG_REACHED_SOS = 1;
  { Reached end of image  }
  JPEG_REACHED_EOI = 2;
  { Completed one iMCU row  }
  JPEG_ROW_COMPLETED = 3;
  { Completed last iMCU row of a scan  }
  JPEG_SCAN_COMPLETED = 4;

  { Precalculate output dimensions for current decompression parameters.  }
procedure jpeg_core_output_dimensions(cinfo: Tj_decompress_ptr); cdecl; external;

procedure jpeg_calc_output_dimensions(cinfo: Tj_decompress_ptr); cdecl; external;
{ Control saving of COM and APPn markers into marker_list.  }
procedure jpeg_save_markers(cinfo: Tj_decompress_ptr; marker_code: longint; length_limit: dword); cdecl; external;
{ Install a special processing method for COM or APPn markers.  }
procedure jpeg_set_marker_processor(cinfo: Tj_decompress_ptr; marker_code: longint; routine: Tjpeg_marker_parser_method); cdecl; external;
{ Read or write raw DCT coefficients --- useful for lossless transcoding.  }
function jpeg_read_coefficients(cinfo: Tj_decompress_ptr): Pjvirt_barray_ptr; cdecl; external;
procedure jpeg_write_coefficients(cinfo: Tj_compress_ptr; coef_arrays: Pjvirt_barray_ptr); cdecl; external;
procedure jpeg_copy_critical_parameters(srcinfo: Tj_decompress_ptr; dstinfo: Tj_compress_ptr); cdecl; external;
{ If you choose to abort compression or decompression before completing
 * jpeg_finish_(de)compress, then you need to clean up to release memory,
 * temporary files, etc.  You can just call jpeg_destroy_(de)compress
 * if you're done with the JPEG object, but if you want to clean it up and
 * reuse it, call this:
  }
procedure jpeg_abort_compress(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_abort_decompress(cinfo: Tj_decompress_ptr); cdecl; external;
{ Generic versions of jpeg_abort and jpeg_destroy that work on either
 * flavor of JPEG object.  These may be more convenient in some places.
  }
procedure jpeg_abort(cinfo: Tj_common_ptr); cdecl; external;
procedure jpeg_destroy(cinfo: Tj_common_ptr); cdecl; external;
{ Default restart-marker-resync procedure for use by data source modules  }
function jpeg_resync_to_restart(cinfo: Tj_decompress_ptr; desired: longint): Tboolean; cdecl; external;
{ Read ICC profile.  See libjpeg.txt for usage information.  }
function jpeg_read_icc_profile(cinfo: Tj_decompress_ptr; icc_data_ptr: PPJOCTET; icc_data_len: Pdword): Tboolean; cdecl; external;
{ These marker codes are exported since applications and data source modules
 * are likely to want to use them.
  }
{ RST0 marker code  }
const
  JPEG_RST0 = $D0;
  { EOI marker code  }
  JPEG_EOI = $D9;
  { APP0 marker code  }
  JPEG_APP0 = $E0;
  { COM marker code  }
  JPEG_COM = $FE;
{ If we have a brain-damaged compiler that emits warnings (or worse, errors)
 * for structure definitions that are never filled in, keep it quiet by
 * supplying dummy definitions for the various substructures.
  }
  {$ifdef INCOMPLETE_TYPES_BROKEN}
  {$ifndef JPEG_INTERNALS          /* will be defined in jpegint.h */}
  {$endif}
  { JPEG_INTERNALS  }
  {$endif}
  { INCOMPLETE_TYPES_BROKEN  }
{
 * The JPEG library modules define JPEG_INTERNALS before including this file.
 * The internal structure declarations are read only when that is true.
 * Applications using the library should not include jpegint.h, but may wish
 * to include jerror.h.
  }
  {$ifdef JPEG_INTERNALS}
  {$include "jpegint.h"            /* fetch private declarations */}
  {$include "jerror.h"             /* fetch error codes too */}
  {$endif}
  { JPEGLIB_H  }

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function JPP(arglist: longint): longint;
begin
  JPP := arglist;
end;

end.
