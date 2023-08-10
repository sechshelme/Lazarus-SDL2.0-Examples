unit v4l2;

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

uses
  BaseUnix, Classes, SysUtils, videodev2;

  // v4l2-ctl --list-formats-ext
  // ffmpeg -f v4l2 -list_formats all -i /dev/video0


const
//  IMAGE_WIDTH = 640;
//  IMAGE_HEIGHT = 480;
//  IMAGE_WIDTH = 1920;
//  IMAGE_HEIGHT = 1080;
  //  IMAGE_WIDTH = 320;
  //  IMAGE_HEIGHT = 200;
//  IMAGE_WIDTH = 1280;
//  IMAGE_HEIGHT = 720;

  BUF_NUM = 4;

type
  Tv4l2_ubuffer = record
    start: pointer;
    length: dword;
  end;

type
  Tv4l2 = class(TObject)
  private
    fDevice: string;
    fHandle: cint;
    v4l2_ubuffers: array of Tv4l2_ubuffer;
  public
    IMAGE_WIDTH,    IMAGE_HEIGHT :cint;
    constructor Create(const device: string);
    destructor Destroy; override;
    function QueryCap: cint;
    function SetFormat(pfmt: uint32): cint;
    function GetFormat: cint;
    function SetFPS(fps: cint): cint;
    function MemoryMap: cint;
    function MemoryUnMap: cint;
    function StreamOn: cint;
    function StreamOff: cint;

    function GetVideoBuffer: Pointer;
  end;

implementation

constructor Tv4l2.Create(const device: string);
var
  st: stat;
begin
  IMAGE_WIDTH:=640;
  IMAGE_HEIGHT:=480;

  inherited Create;
  v4l2_ubuffers := nil;

  fDevice := device;
  FillChar(st, SizeOf(st), 0);
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

destructor Tv4l2.Destroy;
begin
  FpClose(fHandle);
  inherited Destroy;
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

  while fpIOCtl(fHandle, VIDIOC_ENUM_FMT, @fmdesc) <> -1 do begin
    WriteLn(#27'[31m       ', fmdesc.index + 1, '.', fmdesc.description, #27'[0m');
    Inc(fmdesc.index);
  end;

  Result := 0;
end;

function Tv4l2.SetFormat(pfmt: uint32): cint;
var
  fmt: Tv4l2_format;
begin
  //v4l2_sfmt(fHandle,pfmt);
  //  exit;

  //WriteLn(SizeOf(fmt));
  //WriteLn(SizeOf(fmt.fmt.pix));
  FillChar(fmt, SizeOf(fmt), $00);
  //FillChar(fmt.fmt.pix, SizeOf(fmt.fmt.pix), $00);

  fmt._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fmt.fmt.pix.pixelformat := pfmt;
  fmt.fmt.pix.Height := IMAGE_HEIGHT;
  fmt.fmt.pix.Width := IMAGE_WIDTH;
  fmt.fmt.pix.field := V4L2_FIELD_INTERLACED;

  if FpIOCtl(fHandle, VIDIOC_S_FMT, @fmt) = -1 then begin
    Result := -1;
    WriteLn('Fehler: SetFormat()');
    Exit;
  end;

  Result := 0;
end;

function Tv4l2.GetFormat: cint;
var
  fmt: Tv4l2_format;
begin
   fmt._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if FpIOCtl(fHandle, VIDIOC_G_FMT, @fmt) = -1 then begin
    Result := -1;
    WriteLn('Fehler: GetFormat()');
    //    Exit;
  end;
  WriteLn(#27'[33mpix.pixelformatth: ',
    char(fmt.fmt.pix.pixelformat and $FF),
    char(fmt.fmt.pix.pixelformat shr 8 and $FF),
    char(fmt.fmt.pix.pixelformat shr 16 and $FF),
    char(fmt.fmt.pix.pixelformat shr 24 and $FF), #27'[0m');

  WriteLn('pix.width:     ', fmt.fmt.pix.Width);
  WriteLn('pix.height:    ', fmt.fmt.pix.Height);
  WriteLn('pix.field:     ', fmt.fmt.pix.field);
  WriteLn('pix.sizeimage: ', fmt.fmt.pix.sizeimage);

  Result := 0;
end;

function Tv4l2.SetFPS(fps: cint): cint;
var
  sfps: Tv4l2_streamparm;
begin
  FillChar(sfps, SizeOf(setfps), 0);
  sfps._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  sfps.parm.capture.timeperframe.numerator := 1;
  sfps.parm.capture.timeperframe.denominator := fps;
  if fpIOCtl(fHandle, VIDIOC_S_PARM, @sfps) = -1 then begin
    Result := -1;
    WriteLn('Fehler: SetFPS()');
  end;
  Result := 0;
end;

// https://www.freepascal.org/docs-html/rtl/baseunix/fpmmap.html

function Tv4l2.MemoryMap: cint;
var
  req: Tv4l2_requestbuffers;
  buf: Tv4l2_buffer;
  i: integer;
begin
  req.Count := BUF_NUM;
  req._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory := V4L2_MEMORY_MMAP;
  if FpIOCtl(fHandle, VIDIOC_REQBUFS, @req) = -1 then begin
    Result := -1;
    WriteLn('Fehler: mmap()');
  end;

  SetLength(v4l2_ubuffers, req.Count);
  for i := 0 to req.Count - 1 do begin
    buf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory := V4L2_MEMORY_MMAP;
    buf.index := i;
    if FpIOCtl(fHandle, VIDIOC_QUERYBUF, @buf) = -1 then begin
      Result := -1;
      WriteLn('Fehler: mmap()');
    end;

    v4l2_ubuffers[i].length := buf.length;
    v4l2_ubuffers[i].start := Fpmmap(nil, buf.length, PROT_READ or PROT_WRITE, MAP_SHARED, fHandle, buf.m.offset);

     WriteLn('buffer offset:', buf.m.offset, '   length:', buf.length);

    if v4l2_ubuffers[i].start = MAP_FAILED then begin
      WriteLn('buffer map error ', i);
      Result := -1;
    end;

  end;

  Result := 0;
end;

function Tv4l2.MemoryUnMap: cint;
var
  i: integer;
begin
  for i := 0 to BUF_NUM - 1 do begin
    if Fpmunmap(v4l2_ubuffers[i].start, v4l2_ubuffers[i].length) = -1 then begin
      WriteLn('buffer unmap error ', i);
      Result := -1;
    end;
  end;
end;

function Tv4l2.StreamOn: cint;
var
  buf: Tv4l2_buffer;
  typ: Tv4l2_buf_type;
  i: integer;
begin
  buf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buf.memory := V4L2_MEMORY_MMAP;
  for i := 0 to BUF_NUM - 1 do begin
    buf.index := i;
    if FpIOCtl(fHandle, VIDIOC_QBUF, @buf) = -1 then begin
      Result := -1;
      WriteLn('Fehler: StreamOn 1()');
      Exit;
    end;

    typ := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if FpIOCtl(fHandle, VIDIOC_STREAMON, @typ) = -1 then begin
      Result := -1;
      WriteLn('Fehler: StreamOn 2()');
      Exit;
    end;
  end;

  Result := 0;
end;

function Tv4l2.StreamOff: cint;
var
  typ: Tv4l2_buf_type;
begin
  typ := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  if FpIOCtl(fHandle, VIDIOC_STREAMOFF, @typ) = -1 then begin
    Result := -1;
    WriteLn('Fehler: StreamOff()');
    Exit;
  end;
end;

function Tv4l2.GetVideoBuffer: Pointer;
var
  fds: TFDSet;
  tv: TTimeVal = (tv_sec: 1; tv_usec: 0);
var
  buf: Tv4l2_buffer;
begin
  fpFD_ZERO(fds);
  fpFD_SET(fHandle, fds);
  fpSelect(fHandle + 1, @fds, nil, nil, @tv);

  buf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buf.memory := V4L2_MEMORY_MMAP;
  FpIOCtl(fHandle, VIDIOC_DQBUF, @buf);

  buf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buf.memory := V4L2_MEMORY_MMAP;
  FpIOCtl(fHandle, VIDIOC_QBUF, @buf);

  Result := v4l2_ubuffers[buf.index].start;
end;

end.
