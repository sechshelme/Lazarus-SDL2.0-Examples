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
  wall: TSDL_Rects = nil;
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
    Dot := TLDot.Create(gRenderer, Level_Width, Level_Height);
    gBGTexture := TLTexture.Create(gRenderer);

    wall.Add(300, 40, 40, 400);
    wall.Add(150, 40, 40, 400);
    wall.Add(450, 40, 40, 400);

    wall.Add(400, 520, 40, 400);
    wall.Add(250, 520, 40, 400);
    wall.Add(550, 520, 40, 400);
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

        Dot.move(wall);

        camera.x := (Dot.PosXY.x + Dot.PosXY.w div 2) - Screen_Widht div 2;
        camera.y := (Dot.PosXY.y + Dot.PosXY.h div 2) - Screen_Height div 2;

        if camera.x < 0 then begin
          camera.x := 0;
        end;
        if camera.y < 0 then begin
          camera.y := 0;
        end;
        if camera.x > Level_Width - camera.w then begin
          camera.x := Level_Width - camera.w;
        end;
        if camera.y > Level_Height - camera.h then begin
          camera.y := Level_Height - camera.h;
        end;

        SDL_SetRenderDrawColor(gRenderer, $00, $9F, $00, $FF);
        SDL_RenderClear(gRenderer);

        gBGTexture.Render(0, 0, @camera);

        SDL_SetRenderDrawColor(gRenderer, $80, $40, $00, $FF);
        for i := 0 to Length(wall) - 1 do begin
          w := wall[i];
          w.x := w.x - camera.x;
          w.y := w.y - camera.y;
          SDL_RenderFillRect(gRenderer, @w);
        end;

        Dot.render(camera.x, camera.y);

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
