program Project1;

// https://wiki.libsdl.org/SDL3/CategoryRender
// https://wiki.libsdl.org/SDL3/SDL_Renderer
// https://wiki.libsdl.org/SDL3/SDL_RenderGeometry

uses
  SDL2;

var
  sdlWindow: PSDL_Window;
  sdlRenderer: PSDL_Renderer;

  e: TSDL_Event;
  quit: boolean = False;
  col: single = 0;
  c: integer;
const
  vert: array of TSDL_Vertex = (
    (position: (x: 400; y: 150); color: (r: $FF; g: $00; b: $00; a: $FF); tex_coord: (x: 0; y: 0)),
    (position: (x: 200; y: 450); color: (r: $00; g: $FF; b: $00; a: $FF); tex_coord: (x: 0; y: 0)),
    (position: (x: 600; y: 450); color: (r: $00; g: $00; b: $FF; a: $FF); tex_coord: (x: 0; y: 0)));

  procedure main;
  begin

    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      Halt;
    end;

    sdlWindow := SDL_CreateWindow('Window', 50, 50, 800, 600, SDL_WINDOW_SHOWN);
    if sdlWindow = nil then begin
      Halt;
    end;

    sdlRenderer := SDL_CreateRenderer(sdlWindow, 0, SDL_RENDERER_ACCELERATED);

    while not quit do begin
      col += 0.001;
      c := Trunc((sin(col) + 1) * 128);
      SDL_SetRenderDrawColor(sdlRenderer, $80, c, $80, $FF);
      SDL_RenderClear(sdlRenderer);

      SDL_RenderGeometry(sdlRenderer, nil, PSDL_Vertex(vert), Length(vert), nil, 0);

      SDL_RenderPresent(sdlRenderer);

      while SDL_PollEvent(@e) <> 0 do begin
        case e.type_ of
          SDL_QUITEV: begin
            quit := True;
          end;
          SDL_KEYDOWN: begin
            case e.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
        end;
      end;
    end;

    SDL_DestroyRenderer(sdlRenderer);
    SDL_DestroyWindow(sdlWindow);

    SDL_Quit;
  end;

begin
  main;
end.
