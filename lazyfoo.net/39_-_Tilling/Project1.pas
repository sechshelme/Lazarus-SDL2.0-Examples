program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

uses
  sdl2,
  sdl2_image,
  ctypes,
  LTexture,
  LTimer,
  LDot,
  LWall;

const
  Screen_Widht = 640;
  Screen_Height = 480;
  Screen_FPS = 480;
  Screen_Tick_Per_Frame = 1000 div Screen_FPS;

  Level_Width = 1280;
  Level_Height = 960;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;
  camera: TSDL_Rect = (x: 0; y: 0; w: Screen_Widht; h: Screen_Height);

  fpsTimer: TLTimer;
  frameTicks: uint32;

  gBGTexture: TLTexture;
  Dot: TLDot;
  //  wall: TSDL_Rects = nil;
  wall: TLWall = nil;
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
    Dot := TLDot.Create(gRenderer, Level_Width, Level_Height, Screen_Widht, Screen_Height);
    gBGTexture := TLTexture.Create(gRenderer);

    wall := TLWall.Create(gRenderer);

    wall.Add(150, 40, 40, 400);
    wall.Add(300, 40, 40, 400);
    wall.Add(450, 40, 40, 400);

    wall.Add(790, 520, 40, 400);
    wall.Add(940, 520, 40, 400);
    wall.Add(1150, 520, 40, 400);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    if not gBGTexture.LoadFromFile('bg.png') then begin
      WriteLn('Failed to load background texture!');
      sucess := False;
    end;

    Result := sucess;
  end;

  procedure Close;
  begin
    fpsTimer.Free;
    Dot.Free;
    wall.Free;

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

        Dot.move(wall.Rect);
        Dot.CameraMove(camera);

        SDL_SetRenderDrawColor(gRenderer, $00, $9F, $00, $FF);
        SDL_RenderClear(gRenderer);

        gBGTexture.Render(0, 0, @camera);

        wall.CameraMove(camera);
        wall.renderer;

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
