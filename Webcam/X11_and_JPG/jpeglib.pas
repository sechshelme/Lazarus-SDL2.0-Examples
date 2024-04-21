unit jpeglib;

interface

uses  ctypes, BaseUnix, jmorecfg, jconfig;

  {$LinkLib jpeg}

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  Tboolean = cint;
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

const
  JMSG_STR_PARM_MAX = 80;
  JMSG_LENGTH_MAX = 200;

type
  Tsize_t = SizeInt;


const
  DCTSIZE = 8;
  DCTSIZE2 = 64;
  NUM_QUANT_TBLS = 4;
  NUM_HUFF_TBLS = 4;
  NUM_ARITH_TBLS = 16;
  MAX_COMPS_IN_SCAN = 4;
  MAX_SAMP_FACTOR = 4;
  C_MAX_BLOCKS_IN_MCU = 10;

const
  D_MAX_BLOCKS_IN_MCU = 10;

type
  PJSAMPROW = ^TJSAMPROW;
  TJSAMPROW = PJSAMPLE;

  PJSAMPARRAY = ^TJSAMPARRAY;
  TJSAMPARRAY = PJSAMPROW;

  PJSAMPIMAGE = ^TJSAMPIMAGE;
  TJSAMPIMAGE = PJSAMPARRAY;

  PJBLOCK = ^TJBLOCK;
  TJBLOCK = array[0..(DCTSIZE2) - 1] of TJCOEF;

  PJBLOCKROW = ^TJBLOCKROW;
  TJBLOCKROW = PJBLOCK;

  PJBLOCKARRAY = ^TJBLOCKARRAY;
  TJBLOCKARRAY = PJBLOCKROW;

  PJBLOCKIMAGE = ^TJBLOCKIMAGE;
  TJBLOCKIMAGE = PJBLOCKARRAY;

  PJCOEFPTR = ^TJCOEFPTR;
  TJCOEFPTR = PJCOEF;

  PJQUANT_TBL = ^TJQUANT_TBL;

  TJQUANT_TBL = record
    quantval: array[0..(DCTSIZE2) - 1] of TUINT16;
    sent_table: Tboolean;
  end;

  PJHUFF_TBL = ^TJHUFF_TBL;

  TJHUFF_TBL = record
    bits: array[0..16] of TUINT8;
    huffval: array[0..255] of TUINT8;
    sent_table: Tboolean;
  end;

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

  Pjpeg_scan_info = ^Tjpeg_scan_info;

  Tjpeg_scan_info = record
    comps_in_scan: longint;
    component_index: array[0..(MAX_COMPS_IN_SCAN) - 1] of longint;
    Ss: longint;
    Se: longint;
    Ah: longint;
    Al: longint;
  end;

  Pjpeg_saved_marker_ptr = ^Tjpeg_saved_marker_ptr;
  Tjpeg_saved_marker_ptr = ^Tjpeg_marker_struct;
  Pjpeg_marker_struct = ^Tjpeg_marker_struct;

  Tjpeg_marker_struct = record
    Next: Tjpeg_saved_marker_ptr;
    marker: TUINT8;
    original_length: dword;
    data_length: dword;
    Data: PJOCTET;
  end;

const
  JCS_EXTENSIONS = 1;
  JCS_ALPHA_EXTENSIONS = 1;

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

type
  PJ_DCT_METHOD = ^TJ_DCT_METHOD;
  TJ_DCT_METHOD = longint;

const
  JDCT_ISLOW = 0;
  JDCT_IFAST = 1;
  JDCT_FLOAT = 2;

const
  JDCT_DEFAULT = JDCT_ISLOW;

const
  JDCT_FASTEST = JDCT_IFAST;

type
  PJ_DITHER_MODE = ^TJ_DITHER_MODE;
  TJ_DITHER_MODE = longint;

const
  JDITHER_NONE = 0;
  JDITHER_ORDERED = 1;
  JDITHER_FS = 2;

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

  Tjpeg_marker_parser_method = function(cinfo: Tj_decompress_ptr): Tboolean; cdecl;

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

  Pjpeg_progress_mgr = ^Tjpeg_progress_mgr;

  Tjpeg_progress_mgr = record
    progress_monitor: procedure(cinfo: Tj_common_ptr); cdecl;
    pass_counter: longint;
    pass_limit: longint;
    completed_passes: longint;
    total_passes: longint;
  end;

  Pjpeg_destination_mgr = ^Tjpeg_destination_mgr;

  Tjpeg_destination_mgr = record
    next_output_byte: PJOCTET;
    free_in_buffer: Tsize_t;
    init_destination: procedure(cinfo: Tj_compress_ptr); cdecl;
    empty_output_buffer: function(cinfo: Tj_compress_ptr): Tboolean; cdecl;
    term_destination: procedure(cinfo: Tj_compress_ptr); cdecl;
  end;

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
    output_gamma: cdouble;
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
    coef_bits: Pcoef_bits;
    quant_tbl_ptrs: array[0..(NUM_QUANT_TBLS) - 1] of PJQUANT_TBL;
    dc_huff_tbl_ptrs: array[0..(NUM_HUFF_TBLS) - 1] of PJHUFF_TBL;
    ac_huff_tbl_ptrs: array[0..(NUM_HUFF_TBLS) - 1] of PJHUFF_TBL;
    data_precision: longint;
    comp_info: Pjpeg_component_info;
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

const
  JPOOL_PERMANENT = 0;
  JPOOL_IMAGE = 1;
  JPOOL_NUMPOOLS = 2;

function JPP(arglist: longint): longint;

function jpeg_std_error(err: Pjpeg_error_mgr): Pjpeg_error_mgr; cdecl; external;

// --- neu
procedure jpeg_create_compress(cinfo: Tj_compress_ptr);
procedure jpeg_create_decompress(cinfo: Tj_decompress_ptr);
// ---

procedure jpeg_CreateCompress(cinfo: Tj_compress_ptr; version: longint; structsize: Tsize_t); cdecl; external;
procedure jpeg_CreateDecompress(cinfo: Tj_decompress_ptr; version: longint; structsize: Tsize_t); cdecl; external;
procedure jpeg_destroy_compress(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_destroy_decompress(cinfo: Tj_decompress_ptr); cdecl; external;
procedure jpeg_stdio_dest(cinfo: Tj_compress_ptr; outfile: PFILE); cdecl; external;
procedure jpeg_stdio_src(cinfo: Tj_decompress_ptr; infile: PFILE); cdecl; external;

procedure jpeg_mem_dest(cinfo: Tj_compress_ptr; outbuffer: PPbyte; outsize: Pdword); cdecl; external;
procedure jpeg_mem_src(cinfo: Tj_decompress_ptr; inbuffer: pbyte; insize: dword); cdecl; external;

procedure jpeg_set_defaults(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_set_colorspace(cinfo: Tj_compress_ptr; colorspace: TJ_COLOR_SPACE); cdecl; external;
procedure jpeg_default_colorspace(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_set_quality(cinfo: Tj_compress_ptr; quality: longint; force_baseline: Tboolean); cdecl; external;
procedure jpeg_set_linear_quality(cinfo: Tj_compress_ptr; scale_factor: longint; force_baseline: Tboolean); cdecl; external;

procedure jpeg_default_qtables(cinfo: Tj_compress_ptr; force_baseline: Tboolean); cdecl; external;

procedure jpeg_add_quant_table(cinfo: Tj_compress_ptr; which_tbl: longint; basic_table: Pdword; scale_factor: longint; force_baseline: Tboolean); cdecl; external;
function jpeg_quality_scaling(quality: longint): longint; cdecl; external;
procedure jpeg_simple_progression(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_suppress_tables(cinfo: Tj_compress_ptr; suppress: Tboolean); cdecl; external;
function jpeg_alloc_quant_table(cinfo: Tj_common_ptr): PJQUANT_TBL; cdecl; external;
function jpeg_alloc_huff_table(cinfo: Tj_common_ptr): PJHUFF_TBL; cdecl; external;
procedure jpeg_start_compress(cinfo: Tj_compress_ptr; write_all_tables: Tboolean); cdecl; external;
function jpeg_write_scanlines(cinfo: Tj_compress_ptr; scanlines: TJSAMPARRAY; num_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
procedure jpeg_finish_compress(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_calc_jpeg_dimensions(cinfo: Tj_compress_ptr); cdecl; external;

function jpeg_write_raw_data(cinfo: Tj_compress_ptr; Data: TJSAMPIMAGE; num_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
procedure jpeg_write_marker(cinfo: Tj_compress_ptr; marker: longint; dataptr: PJOCTET; datalen: dword); cdecl; external;
procedure jpeg_write_m_header(cinfo: Tj_compress_ptr; marker: longint; datalen: dword); cdecl; external;
procedure jpeg_write_m_byte(cinfo: Tj_compress_ptr; val: longint); cdecl; external;
procedure jpeg_write_tables(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_write_icc_profile(cinfo: Tj_compress_ptr; icc_data_ptr: PJOCTET; icc_data_len: dword); cdecl; external;
function jpeg_read_header(cinfo: Tj_decompress_ptr; require_image: Tboolean): longint; cdecl; external;
const
  JPEG_SUSPENDED = 0;
  JPEG_HEADER_OK = 1;
  JPEG_HEADER_TABLES_ONLY = 2;

function jpeg_start_decompress(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
function jpeg_read_scanlines(cinfo: Tj_decompress_ptr; scanlines: TJSAMPARRAY; max_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
function jpeg_skip_scanlines(cinfo: Tj_decompress_ptr; num_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
procedure jpeg_crop_scanline(cinfo: Tj_decompress_ptr; xoffset: PJDIMENSION; Width: PJDIMENSION); cdecl; external;
function jpeg_finish_decompress(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
function jpeg_read_raw_data(cinfo: Tj_decompress_ptr; Data: TJSAMPIMAGE; max_lines: TJDIMENSION): TJDIMENSION; cdecl; external;
function jpeg_has_multiple_scans(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
function jpeg_start_output(cinfo: Tj_decompress_ptr; scan_number: longint): Tboolean; cdecl; external;
function jpeg_finish_output(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
function jpeg_input_complete(cinfo: Tj_decompress_ptr): Tboolean; cdecl; external;
procedure jpeg_new_colormap(cinfo: Tj_decompress_ptr); cdecl; external;
function jpeg_consume_input(cinfo: Tj_decompress_ptr): longint; cdecl; external;
const
  JPEG_REACHED_SOS = 1;
  JPEG_REACHED_EOI = 2;
  JPEG_ROW_COMPLETED = 3;
  JPEG_SCAN_COMPLETED = 4;

procedure jpeg_core_output_dimensions(cinfo: Tj_decompress_ptr); cdecl; external;

procedure jpeg_calc_output_dimensions(cinfo: Tj_decompress_ptr); cdecl; external;
procedure jpeg_save_markers(cinfo: Tj_decompress_ptr; marker_code: longint; length_limit: dword); cdecl; external;
procedure jpeg_set_marker_processor(cinfo: Tj_decompress_ptr; marker_code: longint; routine: Tjpeg_marker_parser_method); cdecl; external;
function jpeg_read_coefficients(cinfo: Tj_decompress_ptr): Pjvirt_barray_ptr; cdecl; external;
procedure jpeg_write_coefficients(cinfo: Tj_compress_ptr; coef_arrays: Pjvirt_barray_ptr); cdecl; external;
procedure jpeg_copy_critical_parameters(srcinfo: Tj_decompress_ptr; dstinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_abort_compress(cinfo: Tj_compress_ptr); cdecl; external;
procedure jpeg_abort_decompress(cinfo: Tj_decompress_ptr); cdecl; external;
procedure jpeg_abort(cinfo: Tj_common_ptr); cdecl; external;
procedure jpeg_destroy(cinfo: Tj_common_ptr); cdecl; external;
function jpeg_resync_to_restart(cinfo: Tj_decompress_ptr; desired: longint): Tboolean; cdecl; external;
function jpeg_read_icc_profile(cinfo: Tj_decompress_ptr; icc_data_ptr: PPJOCTET; icc_data_len: Pdword): Tboolean; cdecl; external;
const
  JPEG_RST0 = $D0;
  JPEG_EOI = $D9;
  JPEG_APP0 = $E0;
  JPEG_COM = $FE;

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function JPP(arglist: longint): longint;
begin
  JPP := arglist;
end;

procedure jpeg_create_compress(cinfo: Tj_compress_ptr);
begin
  jpeg_CreateCompress(cinfo, JPEG_LIB_VERSION, SizeOf(Tjpeg_compress_struct));
end;

procedure jpeg_create_decompress(cinfo: Tj_decompress_ptr);
begin
  jpeg_CreateDecompress(cinfo, JPEG_LIB_VERSION, SizeOf(Tjpeg_decompress_struct));
end;

end.
