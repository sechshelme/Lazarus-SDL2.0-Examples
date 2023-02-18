program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal
{$modeswitch typehelpers}

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

  //  {$MODESWITCH ADVANCEDRECORDS}
type
  TSDL_Rects = array of TSDL_Rect;

  { TSDL_RectsHelper }

  TSDL_RectsHelper = type Helper for  TSDL_Rects
  public
    procedure Add(x, y, w, h: integer);
  end;

  { TSDL_RectsHelper }

  procedure TSDL_RectsHelper.Add(x, y, w, h: integer);
  var
    l: SizeInt;
  begin
    l := Length(self);
    SetLength(Self, l + 1);
    Self[l].x := x;
    Self[l].y := y;
    Self[l].w := w;
    Self[l].h := h;
  end;

var
  gWindow: PSDL_Window;
  gRenderer: PSDL_Renderer;

  quit: boolean = False;
  e: TSDL_Event;

  capTimer, fpsTimer: TLTimer;
  myDot: TLDot;

  frameTicks: uint32;
  wall: TSDL_Rects = nil;

  i: integer;

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
    capTimer := TLTimer.Create;
    myDot := TLDot.Create(gRenderer, Screen_Widht, Screen_Height);
    myDot.IsParticle := True;

    Randomize;
    for i := 0 to 7 do begin
      if i mod 2 = 1 then  begin
        wall.Add(random(Screen_Widht - 100) + 20, random(100), random(50) + 1, random(Screen_Height - 100) + 20);
      end else begin
        wall.Add(random(100), random(Screen_Height - 100) + 20, random(Screen_Widht - 100) + 20, random(50) + 1);
      end;
    end;
    //wall.Add(300, 40, 40, 400);
    //wall.Add(150, 40, 40, 400);
    //wall.Add(450, 40, 40, 400);
  end;

  function loadMedia: boolean;
  var
    sucess: boolean = True;
  begin
    Result := sucess;
  end;

  procedure Close;
  begin
    fpsTimer.Free;
    capTimer.Free;
    myDot.Free;

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
      fpsTimer.start;
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
          myDot.HandleEvent(e);
        end;

        myDot.move(wall);

        SDL_SetRenderDrawColor(gRenderer, $00, $80, $00, $FF);
        SDL_RenderClear(gRenderer);

        SDL_SetRenderDrawColor(gRenderer, $80, $40, $00, $FF);

        for i := 0 to Length(wall) - 1 do begin
          SDL_RenderFillRect(gRenderer, @wall[i]);
        end;

        myDot.render;

        SDL_RenderPresent(gRenderer);

        frameTicks := capTimer.getTicks;
        if frameTicks < Screen_Tick_Per_Frame then begin
          SDL_Delay(Screen_Tick_Per_Frame - frameTicks);
        end;
      end;
    end;
  end;
  Close;
end.
