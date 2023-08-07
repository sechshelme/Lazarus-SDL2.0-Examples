unit v4l2_driver;

interface

uses
  BaseUnix;

  {$L v4l2_driver.o}
  {$LinkLib c}

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  BUF_NUM = 4;

var
  IMAGE_WIDTH: longint; cvar;external;
  IMAGE_HEIGHT: longint; cvar;external;

  // ---- von /usr/include/linux/videodev2.h
const
  //  #define VIDIOC_QBUF    _IOWR('V', 15, struct v4l2_buffer)
  //  #define VIDIOC_EXPBUF    _IOWR('V', 16, struct v4l2_exportbuffer)
  //  #define VIDIOC_DQBUF    _IOWR('V', 17, struct v4l2_buffer)


  VIDIOC_DQBUF = 3227014673;
  VIDIOC_QBUF = 3227014671;
  V4L2_PIX_FMT_YUYV = 1448695129;


const
  V4L2_BUF_TYPE_VIDEO_CAPTURE = 1;
  V4L2_BUF_TYPE_VIDEO_OUTPUT = 2;
  V4L2_BUF_TYPE_VIDEO_OVERLAY = 3;
  V4L2_BUF_TYPE_VBI_CAPTURE = 4;
  V4L2_BUF_TYPE_VBI_OUTPUT = 5;
  V4L2_BUF_TYPE_SLICED_VBI_CAPTURE = 6;
  V4L2_BUF_TYPE_SLICED_VBI_OUTPUT = 7;
  V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY = 8;
  V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE = 9;
  V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE = 10;
  V4L2_BUF_TYPE_SDR_CAPTURE = 11;
  V4L2_BUF_TYPE_SDR_OUTPUT = 12;
  V4L2_BUF_TYPE_META_CAPTURE = 13;
  V4L2_BUF_TYPE_META_OUTPUT = 14;
  //* Deprecated; do not use */
  V4L2_BUF_TYPE_PRIVATE = $80;

const
  V4L2_MEMORY_MMAP = 1;
  V4L2_MEMORY_USERPTR = 2;
  V4L2_MEMORY_OVERLAY = 3;
  V4L2_MEMORY_DMABUF = 4;

type
  Pv4l2_plane = ^Tv4l2_plane;

  Tv4l2_plane = record
    bytesused: uint32;
    length: uint32;
    m: record
      mem_offset: uint32;
      userptr: culong;
      fd: uint32;
      end;
    data_offset: uint32;
    reserved: array [0..10] of uint32;
  end;

  Tv4l2_timecode = record
    _type: uint32;
    flags: uint32;
    frames: uint8;
    seconds: uint8;
    minutes: uint8;
    hours: uint8;
    userbits: array[0..3] of uint8;
  end;

  Tv4l2_buffer = record
    index: uint32;
    _type: uint32;
    bytesused: uint32;
    flags: uint32;
    field: uint32;
    timestamp: TTimeVal;
    timecode: Tv4l2_timecode;
    sequence: uint32;

    // memory location
    memory: uint32;
    m: record
      offset: uint32;
      userptr: culong;
      planes: pv4l2_plane;
      fd: int32;
      end;
    length: uint32;
    reserved2: uint32;
    u: record
      request_fd: int32;
      reserved: uint32;
      end;
  end;

  // -------------------------------------------


type
  Pv4l2_ubuffer = ^Tv4l2_ubuffer;

  Tv4l2_ubuffer = record
    start: pointer;
    length: dword;
  end;

var
  v4l2_ubuffers: Pv4l2_ubuffer; cvar;external;

  { functions  }

function v4l2_open(device: PChar): longint; cdecl; external;
function v4l2_close(fd: longint): longint; cdecl; external;
function v4l2_querycap(fd: longint; device: PChar): longint; cdecl; external;
function v4l2_sfmt(fd: longint; pfmt: uint32): longint; cdecl; external;
function v4l2_gfmt(fd: longint): longint; cdecl; external;
function v4l2_mmap(fd: longint): longint; cdecl; external;
function v4l2_munmap: longint; cdecl; external;
function v4l2_sfps(fd: longint; fps: longint): longint; cdecl; external;
function v4l2_streamon(fd: longint): longint; cdecl; external;
function v4l2_streamoff(fd: longint): longint; cdecl; external;

implementation

end.
