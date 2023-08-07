program project1;

uses
  BaseUnix,
  unixtype,
  SDL2,
  ctypes,
  videodev2,

  v4l2_driver;

  // https://github.com/chendotjs/Webcam-SDL2

  function ioctl(fd: cint; request: culong): cint; cdecl; varargs; external;

const
  VIDIOC_QUERYCAP = 2154321408;
  VIDIOC_ENUM_FMT = 3225441794;
  VIDIOC_S_FMT = 3234878981;
  VIDIOC_G_FMT = 3234878980;
  VIDIOC_S_PARM = 3234616854;
  VIDIOC_REQBUFS = 3222558216;
  VIDIOC_QUERYBUF = 3227014665;

  VIDIOC_DQBUF = 3227014673;
  VIDIOC_QBUF = 3227014671;
  V4L2_PIX_FMT_YUYV = 1448695129;



const
  device = '/dev/video0';


var
  video_fildes: longint;

  sdlTexture: PSDL_Texture;
  sdlRenderer: PSDL_Renderer;
  sdlRect: TSDL_Rect;
  sdlScreen: PSDL_Window;
  e: TSDL_Event;

  quit: boolean = False;
  fds: TFDSet;

  tv: TTimeVal = (tv_sec: 1; tv_usec: 0);
  buf: Tv4l2_buffer;

  function SetFormat(fHandle: cint; pfmt: uint32): cint;
  var
    fmt: Tv4l2_format;
  begin
    WriteLn(SizeOf(fmt));
    WriteLn(SizeOf(fmt.fmt.pix));
    //  FillChar(fmt, SizeOf(fmt), $00);
    //  FillChar(fmt.fmt.pix, SizeOf(fmt.fmt.pix), $00);

    fmt._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    fmt.fmt.pix.pixelformat := pfmt;
    fmt.fmt.pix.Height := IMAGE_HEIGHT;
    fmt.fmt.pix.Width := IMAGE_WIDTH;
    fmt.fmt.pix.field := V4L2_FIELD_INTERLACED;

    if IOCtl(fHandle, VIDIOC_S_FMT, @fmt) = -1 then begin
      Result := -1;
      WriteLn('Fehler: SetFormat()');
      Exit;
    end;

    Result := 0;
  end;

  function GetFormat(fHandle: cint): cint;
  var
    fmt: Tv4l2_format;
  begin
    FillChar(fmt, SizeOf(fmt), $00);
    fmt.fmt.pix.Height := 122;
    if IOCtl(fHandle, VIDIOC_G_FMT, @fmt) = -1 then begin
      Result := -1;
      WriteLn('Fehler: GetFormat()');
      //  Exit;
    end;
    WriteLn(#27'[33mpix.pixelformatth: ',
      char(fmt.fmt.pix.pixelformat and $FF),
      char(fmt.fmt.pix.pixelformat shr 8 and $FF),
      char(fmt.fmt.pix.pixelformat shr 16 and $FF),
      char(fmt.fmt.pix.pixelformat shr 24 and $FF), #27'[0m');

    WriteLn('pix.width:    ', fmt.fmt.pix.Width);
    WriteLn('pix.height:   ', fmt.fmt.pix.Height);
    WriteLn('pix.field:    ', fmt.fmt.pix.field);

    Result := 0;
  end;

begin
  video_fildes := v4l2_open(device);
  v4l2_querycap(video_fildes, device);

  v4l2_sfmt(video_fildes, V4L2_PIX_FMT_YUYV);
  SetFormat(video_fildes, V4L2_PIX_FMT_YUYV);

  WriteLn(#10#27'[0m--- C ---');
  v4l2_gfmt(video_fildes);
  WriteLn(#10#27'[0m--- Pascal ---');
  GetFormat(video_fildes);

  v4l2_sfps(video_fildes, 30);
  v4l2_mmap(video_fildes);
  v4l2_streamon(video_fildes);

  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER);

  sdlScreen := SDL_CreateWindow('webcam', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, IMAGE_WIDTH, IMAGE_HEIGHT, SDL_WINDOW_SHOWN);
  if sdlScreen = nil then begin
    WriteLn('Kann SDL nicht öffnen');
  end;

  sdlRenderer := SDL_CreateRenderer(sdlScreen, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if sdlRenderer = nil then begin
    Write('Kann Renderer nicht öffnen');
  end;

  sdlTexture := SDL_CreateTexture(sdlRenderer, SDL_PIXELFORMAT_YUY2, SDL_TEXTUREACCESS_STREAMING, IMAGE_WIDTH, IMAGE_HEIGHT);

  sdlRect.w := IMAGE_WIDTH;
  sdlRect.h := IMAGE_HEIGHT;

  repeat
    fpFD_ZERO(fds);
    fpFD_SET(video_fildes, fds);

    fpSelect(video_fildes + 1, @fds, nil, nil, @tv);

    buf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory := V4L2_MEMORY_MMAP;
    FpIOCtl(video_fildes, VIDIOC_DQBUF, @buf);

    buf._type := V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory := V4L2_MEMORY_MMAP;
    FpIOCtl(video_fildes, VIDIOC_QBUF, @buf);

    while SDL_PollEvent(@e) <> 0 do begin
      case e.type_ of
        SDL_KEYDOWN: begin
          case e.key.keysym.sym of
            SDLK_ESCAPE: begin
              quit := True;
            end;
          end;
        end;
        SDL_QUITEV: begin
          quit := True;
        end;
      end;
    end;


    SDL_UpdateTexture(sdlTexture, @sdlRect, v4l2_ubuffers[buf.index].start, IMAGE_WIDTH * 2);
    //  SDL_UpdateYUVTexture
    SDL_RenderClear(sdlRenderer);
    SDL_RenderCopy(sdlRenderer, sdlTexture, nil, @sdlRect);
    SDL_RenderPresent(sdlRenderer);
  until quit;

  v4l2_streamoff(video_fildes);
  v4l2_munmap;
  v4l2_close(video_fildes);

  SDL_Quit();

  //    My_v4l2.Free;

  WriteLn('ende.');
end.
