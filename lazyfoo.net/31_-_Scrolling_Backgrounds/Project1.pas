program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes,
  LTexture,
  LTimer,
  LDot;

const
  Screen_Widht = 640;
  Screen_Height = 480;
  Screen_FPS = 240;
  Screen_Tick_Per_Frame = 1000 div Screen_FPS;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;
  //  camera: TSDL_Rect = (x: 0; y: 0; w: Screen_Widht; h: Screen_Height);
  scrollingOffset: integer;

  fpsTimer: TLTimer;
  frameTicks: uint32;

  gBGTexture: TLTexture;
  Dot: TLDot;
  i: integer;
  w: TSDL_Rect;

  function init: boolean;
  var
    sucess: boolean = True;
    imgFlags: cint32 = 0;
  begin
    sucess := True;
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
      sucess := False;
    end else begin
      gWindow := SDL_CreateWindow('SDL Tuorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, Screen_Widht, Screen_Height, SDL_WINDOW_SHOWN);
      if gWindow = nil then begin
        WriteLn('Window could not be created! SDL_Error: ', SDL_GetError);
        sucess := False;
      end else begin
        gRenderer := SDL_CreateRenderer(gWindow, -1, SDL_RENDERER_ACCELERATED);
        if gRenderer = nil then begin
          WriteLn('Renderer could not be created! SDL Error: ', SDL_GetError);
          sucess := False;
        end else begin
          SDL_SetRenderDrawColor(gRenderer, $FF, $FF, $FF, $FF);

          if (IMG_Init(imgFlags) and imgFlags) <> 0 then begin
            WriteLn('SDL_image could not initialize! SDL_image Error: ', IMG_GetError);
            sucess := False;
          end;

        end;
      end;
    end;
    Result := sucess;

    fpsTimer := TLTimer.Create;
    Dot := TLDot.Create(gRenderer, Screen_Widht, Screen_Height);
    gBGTexture := TLTexture.Create(gRenderer);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    if not gBGTexture.LoadFromFile('bg.png') then begin
      WriteLn('Failed to load background texture!');
      sucess := False;
    end;

    dot.LoadTextures('foo.png',$00,$FF,$FF);

    Result := sucess;
  end;

  procedure Close;
  begin
    fpsTimer.Free;
    Dot.Free;

    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
    gWindow := nil;
    gRenderer := nil;

    IMG_Quit;
    SDL_Quit;
  end;

begin
  if not init then begin
    WriteLn('Failed to initialize');
  end else begin
    if not loadMedia then begin
      WriteLn('Failed to load media');
    end else begin
      //      fpsTimer.start;
      while not quit do begin
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
          Dot.HandleEvent(e);
        end;

        Dot.move;

        Dec(scrollingOffset);
        if scrollingOffset < -gBGTexture.Widht then begin
          scrollingOffset := 0;
        end;

        SDL_SetRenderDrawColor(gRenderer, $00, $9F, $00, $FF);
        SDL_RenderClear(gRenderer);

        gBGTexture.Render(scrollingOffset, 0);
        gBGTexture.Render(scrollingOffset + gBGTexture.Widht, 0);

        Dot.render;

        SDL_RenderPresent(gRenderer);

        frameTicks := fpsTimer.getTicks;
        if frameTicks < Screen_Tick_Per_Frame then begin
          SDL_Delay(Screen_Tick_Per_Frame - frameTicks);
        end;
      end;
    end;
  end;
  Close;
end.
