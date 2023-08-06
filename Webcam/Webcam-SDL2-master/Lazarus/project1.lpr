program project1;

uses
  BaseUnix,
  unixtype,
  SDL2,
  ctypes,
  v4l2_driver, v4l2;

  // https://github.com/chendotjs/Webcam-SDL2

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

  My_v4l2:Tv4l2;

begin
  My_v4l2:=Tv4l2.Create(device);


//  video_fildes := v4l2_open(device);
  video_fildes:=My_v4l2.getHandler;

  //if video_fildes = -1 then begin
  //  WriteLn('Kann Gerät "', device, '" nicht öffnen');
  //  Halt(1);
  //end;
  //

  My_v4l2.QueryCap;
  //if v4l2_querycap(video_fildes, nil) = -1 then begin
  //  WriteLn('Fehler: "v4l2_querycap"');
  //  Halt(1);
  //end;

  v4l2_sfmt(video_fildes, V4L2_PIX_FMT_YUYV)  ;
My_v4l2.SetFormat(V4L2_PIX_FMT_YUYV);
  //if v4l2_sfmt(video_fildes, V4L2_PIX_FMT_YUYV) = -1 then begin
  //  WriteLn('v4l2_sfmt');
  //  Halt(1);
  //end;

  WriteLn('--------------xxxxxxxxx---------------------------------------------');

  My_v4l2.GetFormat;
  //if v4l2_gfmt(video_fildes) = -1 then begin
  //  WriteLn('v4l2_gfmt');
  //  Halt(1);
  //end;

  My_v4l2.SetFPS(30);
  //if v4l2_sfps(video_fildes, 30) = -1 then begin
  //  WriteLn('v4l2_sfmt()');
  //  Halt(1);
  //end;

  My_v4l2.MemoryMap;
  v4l2_mmap(video_fildes);
  //if v4l2_mmap(video_fildes) = -1 then begin

  //  WriteLn('v4l2_mmap()');
  //  Halt(1);
  //end;

  if v4l2_streamon(video_fildes) = -1 then begin
    WriteLn('v4l2_streamon()');
    Halt(1);
  end;

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

    My_v4l2.Free;

  WriteLn('ende.');
end.
