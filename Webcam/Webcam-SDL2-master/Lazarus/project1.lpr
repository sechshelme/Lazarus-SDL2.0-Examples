program project1;

uses
  BaseUnix,
  unixtype,
  SDL2,
  ctypes,
  v4l2_driver;

  // https://github.com/chendotjs/Webcam-SDL2

const
  device = '/dev/video0';
  IMAGE_WIDTH = 640;
  IMAGE_HEIGHT = 480;


type
  TFramehandler = procedure(pframe: Pointer; len: cint);

  TStreamHandle = record
    fd: cint;
    framehandler: TFramehandler;
  end;

var
  video_fildes: longint;
  sH: TStreamHandle;

  sdlTexture: PSDL_Texture;
  sdlRenderer: PSDL_Renderer;
  sdlRect: TSDL_Rect;
  sdlScreen: PSDL_Window;
  quit: boolean = False;
  fds: TFDSet;

  tv: TTimeVal = (tv_sec: 1; tv_usec: 0);
  buf:Tv4l2_buffer;

  procedure frame_handler(pframe: Pointer; len: cint);
  begin
    SDL_UpdateTexture(sdlTexture, @sdlRect, pframe, IMAGE_WIDTH * 2);
    SDL_RenderClear(sdlRenderer);
    SDL_RenderCopy(sdlRenderer, sdlTexture, nil, @sdlRect);
    SDL_RenderPresent(sdlRenderer);
  end;

begin
  video_fildes := v4l2_open(device);
  if video_fildes = -1 then begin
    WriteLn('Kann Gerät "', device, '" nicht öffnen');
    Halt(1);
  end;

  if v4l2_querycap(video_fildes, nil) = -1 then begin
    WriteLn('Fehler: "v4l2_querycap"');
    Halt(1);
  end;

  if v4l2_sfmt(video_fildes, 1448695129) = -1 then begin     // ??????
    WriteLn('v4l2_sfmt');
    Halt(1);
  end;

  if v4l2_gfmt(video_fildes) = -1 then begin
    WriteLn('v4l2_gfmt');
    Halt(1);
  end;

  if v4l2_sfmt(video_fildes, 30) = -1 then begin
    WriteLn('v4l2_sfmt()');
    Halt(1);
  end;

  if v4l2_mmap(video_fildes) = -1 then begin
    WriteLn('v4l2_mmap()');
    Halt(1);
  end;

  if v4l2_streamon(video_fildes) = -1 then begin
    WriteLn('v4l2_streamon()');
    Halt(1);
  end;

  sH.fd := video_fildes;
  sH.framehandler := @frame_handler;

  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);

  sdlScreen := SDL_CreateWindow('webcam', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_SHOWN);
  if sdlScreen = nil then begin
    WriteLn('Kann SDL nicht öffnen');
  end;

  sdlRenderer := SDL_CreateRenderer(sdlScreen, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if sdlRenderer = nil then begin
    Write('Kann Renderer nicht öffnen');
  end;

  sdlTexture := SDL_CreateTexture(sdlRenderer, SDL_PIXELFORMAT_YUY2, SDL_TEXTUREACCESS_STREAMING, 640, 480);

  sdlRect.w := 640;
  sdlRect.h := 480;

//  #define VIDIOC_QBUF		_IOWR('V', 15, struct v4l2_buffer)
//  #define VIDIOC_EXPBUF		_IOWR('V', 16, struct v4l2_exportbuffer)
//  #define VIDIOC_DQBUF		_IOWR('V', 17, struct v4l2_buffer)


  repeat
    fpFD_ZERO(fds);
    fpFD_SET(video_fildes, fds);

    fpSelect(video_fildes + 1, @fds, nil, nil, @tv);

    buf._type:=V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory:=V4L2_MEMORY_MMAP;
    FpIOCtl(video_fildes, VIDIOC_DQBUF, @buf);

    buf._type:=V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory:=V4L2_MEMORY_MMAP;
    FpIOCtl(video_fildes, VIDIOC_QBUF, @buf);

    //////////////77


  until quit;




  WriteLn('io.');
end.
