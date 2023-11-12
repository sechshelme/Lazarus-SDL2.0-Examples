program project1;

uses
  BaseUnix,
  unixtype,
  SDL2,
  ctypes,
  videodev2,
  v4l2_driver;

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

begin
  video_fildes := v4l2_open(device);
  v4l2_querycap(video_fildes, device);

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

  WriteLn('ende.');
end.
