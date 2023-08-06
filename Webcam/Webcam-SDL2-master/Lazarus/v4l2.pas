unit v4l2;

{$mode ObjFPC}{$H+}

interface

uses
  ctypes, BaseUnix, Classes, SysUtils, videodev2;

const
  VIDIOC_QUERYCAP = 2154321408;
  VIDIOC_ENUM_FMT = 3225441794;


//  //* Values for 'capabilities' field */
//  V4L2_CAP_VIDEO_CAPTURE = $00000001; // Is a video capture device
//  V4L2_CAP_VIDEO_OUTPUT = $00000002; // Is a video output device
//  V4L2_CAP_VIDEO_OVERLAY = $00000004; // Can do video overlay
//  V4L2_CAP_VBI_CAPTURE = $00000010; // Is a raw VBI capture device
//  V4L2_CAP_VBI_OUTPUT = $00000020; // Is a raw VBI output device
//  V4L2_CAP_SLICED_VBI_CAPTURE = $00000040; // Is a sliced VBI capture device
//  V4L2_CAP_SLICED_VBI_OUTPUT = $00000080; // Is a sliced VBI output device
//  V4L2_CAP_RDS_CAPTURE = $00000100; // RDS data capture
//  V4L2_CAP_VIDEO_OUTPUT_OVERLAY = $00000200; // Can do video output overlay
//  V4L2_CAP_HW_FREQ_SEEK = $00000400; // Can do hardware frequency seek
//  V4L2_CAP_RDS_OUTPUT = $00000800; // Is an RDS encoder
//
//  // Is a video capture device that supports multiplanar formats
//  V4L2_CAP_VIDEO_CAPTURE_MPLANE = $00001000;
//  // Is a video output device that supports multiplanar formats
//  V4L2_CAP_VIDEO_OUTPUT_MPLANE = $00002000;
//  // Is a video mem-to-mem device that supports multiplanar formats
//  V4L2_CAP_VIDEO_M2M_MPLANE = $00004000;
//  // Is a video mem-to-mem device
//  V4L2_CAP_VIDEO_M2M = $00008000;
//
//  V4L2_CAP_TUNER = $00010000; // has a tuner
//  V4L2_CAP_AUDIO = $00020000; // has audio support
//  V4L2_CAP_RADIO = $00040000; // is a radio device
//  V4L2_CAP_MODULATOR = $00080000; // has a modulator
//
//  V4L2_CAP_SDR_CAPTURE = $00100000; // Is a SDR capture device
//  V4L2_CAP_EXT_PIX_FORMAT = $00200000; // Supports the extended pixel format
//  V4L2_CAP_SDR_OUTPUT = $00400000; // Is a SDR output device
//  V4L2_CAP_META_CAPTURE = $00800000; // Is a metadata capture device
//
//  V4L2_CAP_READWRITE = $01000000; // read/write systemcalls
//  V4L2_CAP_ASYNCIO = $02000000; // async I/O
//  V4L2_CAP_STREAMING = $04000000; // streaming I/O ioctls
//  V4L2_CAP_META_OUTPUT = $08000000; // Is a metadata output device
//
//  V4L2_CAP_TOUCH = $10000000; // Is a touch device
//
//  V4L2_CAP_IO_MC = $20000000; // Is input/output controlled by the media controller
//
//  V4L2_CAP_DEVICE_CAPS = $80000000; // sets device capabilities field
//
//
//  V4L2_BUF_TYPE_VIDEO_CAPTURE = 1;
//  V4L2_BUF_TYPE_VIDEO_OUTPUT = 2;
//  V4L2_BUF_TYPE_VIDEO_OVERLAY = 3;
//  V4L2_BUF_TYPE_VBI_CAPTURE = 4;
//  V4L2_BUF_TYPE_VBI_OUTPUT = 5;
//  V4L2_BUF_TYPE_SLICED_VBI_CAPTURE = 6;
//  V4L2_BUF_TYPE_SLICED_VBI_OUTPUT = 7;
//  V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY = 8;
//  V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE = 9;
//  V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE = 10;
//  V4L2_BUF_TYPE_SDR_CAPTURE = 11;
//  V4L2_BUF_TYPE_SDR_OUTPUT = 12;
//  V4L2_BUF_TYPE_META_CAPTURE = 13;
//  V4L2_BUF_TYPE_META_OUTPUT = 14;
//  // Deprecated; do not use
//  V4L2_BUF_TYPE_PRIVATE = $80;
//
//type
//  Tv4l2_capability = record
//    driver: array[0..15] of char;
//    card: array[0..31] of char;
//    bus_info: array[0..31] of char;
//    version: uint32;
//    capabilities: uint32;
//    device_caps: uint32;
//    reserved: array [0..2] of uint32;
//  end;
//
//  Tv4l2_fmtdesc = record
//    index: uint32;             // Format number
//    _type: uint32;              // enum v4l2_buf_type
//    flags: uint32;
//    description: array[0..31] of char;   // Description string
//    pixelformat: uint32;       // Format fourcc
//    mbus_code: uint32;    // Media bus code
//    reserved: array[0..2] of uint32;
//  end;
//
//  Tv4l2_format = record
//    _type: cuint32;
//    fmt: record
//      case byte of
//        //struct v4l2_pix_format    pix;     /* V4L2_BUF_TYPE_VIDEO_CAPTURE */
//        //struct v4l2_pix_format_mplane  pix_mp;  /* V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE */
//        //struct v4l2_window    win;     /* V4L2_BUF_TYPE_VIDEO_OVERLAY */
//        //struct v4l2_vbi_format    vbi;     /* V4L2_BUF_TYPE_VBI_CAPTURE */
//        //struct v4l2_sliced_vbi_format  sliced;  /* V4L2_BUF_TYPE_SLICED_VBI_CAPTURE */
//        //struct v4l2_sdr_format    sdr;     /* V4L2_BUF_TYPE_SDR_CAPTURE */
//        //struct v4l2_meta_format    meta;    /* V4L2_BUF_TYPE_META_CAPTURE */
//        0: (raw_data: array[0..199] of char);                   // user-defined */
//      end;
//  end;
//
//
  { Tv4l2 }

type
  Tv4l2 = class(TObject)
  private
    fDevice: string;
    fHandle: cint;
  public
    constructor Create(const device: string);
    function QueryCap: cint;
    function SFmt(pfmt: uint32): cint;

    function getHandler: cint;
  end;

implementation

{ Tv4l2 }

constructor Tv4l2.Create(const device: string);
var
  st: stat;
begin
  fDevice := device;
  if FpStat(device, st) = -1 then begin
    WriteLn('Kann Device nicht öffnen !');
    Halt(1);
  end;
  if not fpS_ISCHR(st.st_mode) then begin
    WriteLn('Fehler');
    Halt(1);
  end else begin
    WriteLn('Konnte Device öffnen');
  end;
  fHandle := FpOpen(device, O_RDWR or O_NONBLOCK, 0);
  if fHandle = -1 then begin
    WriteLn('Konnte Device nicht öffnen');
    Halt(1);
  end;
end;

function Tv4l2.QueryCap: cint;
var
  cap: Tv4l2_capability;
  fmdesc: Tv4l2_fmtdesc;
begin
  if FpIOCtl(fHandle, VIDIOC_QUERYCAP, @cap) = -1 then begin
    WriteLn('Kann QueryCap nicht öffnen');
    Result := -1;
  end else begin
    WriteLn('driver:       ', cap.driver);
    WriteLn('card:         ', cap.card);
    WriteLn('bus_info:     ', cap.bus_info);
    WriteLn('version:      ', cap.version);
    WriteLn('capabilities: ', cap.capabilities);
  end;

  if cap.capabilities and V4L2_CAP_VIDEO_CAPTURE = V4L2_CAP_VIDEO_CAPTURE then begin
    WriteLn('Device ', fDevice, ': supports capture.');
  end;
  if cap.capabilities and V4L2_CAP_STREAMING = V4L2_CAP_STREAMING then begin
    WriteLn('Device ', fDevice, ': supports streaming.');
  end;

  fmdesc.index := 0;
  fmdesc._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  WriteLn(#27'[31mSupport format: '#10#27'[0m');

  while FpIOCtl(fHandle, VIDIOC_ENUM_FMT, @fmdesc) <> -1 do begin
    WriteLn(#27'[31m       ', fmdesc.index + 1, '.', fmdesc.description, #27'[0m');
    Inc(fmdesc.index);
  end;

  Result := 0;
end;

function Tv4l2.SFmt(pfmt: uint32): cint;
var
  fmt: Tv4l2_format;
begin

  Result := 0;
end;

function Tv4l2.getHandler: cint;
begin
  Result := fHandle;
end;

end.

