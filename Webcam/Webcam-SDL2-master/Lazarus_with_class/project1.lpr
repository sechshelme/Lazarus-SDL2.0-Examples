program project1;

uses
  BaseUnix,
  unixtype,
  SDL2,
  ctypes,
  videodev2,

  v4l2;

  // https://github.com/chendotjs/Webcam-SDL2

const
  device = '/dev/video0';


var
  sdlTexture: PSDL_Texture;
  sdlRenderer: PSDL_Renderer;
  sdlRect: TSDL_Rect;
  sdlScreen: PSDL_Window;
  e: TSDL_Event;

  quit: boolean = False;
  My_v4l2: Tv4l2;


begin
  My_v4l2 := Tv4l2.Create(device);

  My_v4l2.QueryCap;

  My_v4l2.SetFormat(V4L2_PIX_FMT_YUYV);
  My_v4l2.GetFormat;

  My_v4l2.SetFPS(30);

  My_v4l2.MemoryMap;
  My_v4l2.StreamOn;


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
  if sdlTexture = nil then begin
    Write('Kann Textur nicht öffnen');
  end;

  sdlRect.w := IMAGE_WIDTH;
  sdlRect.h := IMAGE_HEIGHT;

  repeat
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

    SDL_UpdateTexture(sdlTexture, @sdlRect, My_v4l2.GetVideoBuffer, IMAGE_WIDTH * 2);
    SDL_RenderClear(sdlRenderer);
    SDL_RenderCopy(sdlRenderer, sdlTexture, nil, @sdlRect);
    SDL_RenderPresent(sdlRenderer);
  until quit;

  My_v4l2.StreamOff;
  My_v4l2.MemoryUnMap;
  My_v4l2.Free;

  SDL_Quit();
end.
