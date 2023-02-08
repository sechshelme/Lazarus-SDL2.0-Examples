
program cardioid3;

uses
  SysUtils, SDL2, Cairo;

procedure CairoDraw(const ATexture: pSDL_Texture; const AWidth, AHeight: integer);
var
  LSurface: pcairo_surface_t;
  LContext: pcairo_t;
  LPixels: pointer;
  LPitch: integer;
const
  R = 0.15;
  X = -R;
  Y = 0;
  D = 0.3;
  R2 = 1 / 250;
var
  a, xx, yy, rr: double;
begin
  SDL_LockTexture(ATexture, nil, @LPixels, @LPitch);

  LSurface := cairo_image_surface_create_for_data(LPixels, CAIRO_FORMAT_ARGB32, AWidth, AHeight, LPitch);

  LContext := cairo_create(LSurface);

  cairo_set_source_rgb(LContext, 1.0, 1.0, 1.0);
  cairo_paint(LContext);
  cairo_scale(LContext, AWidth, AHeight);
  cairo_translate(LContext, 1 / 2, 1 / 2);
  cairo_set_line_width(LContext, 1 / 500);
  cairo_set_source_rgba(LContext, 1, 0, 0, 0.3);

{ Dessin d'une cardioïde par la méthode de l'enveloppe : https://mathimages.swarthmore.edu/index.php/Cardioid }

  a := 0;
  while a < 2 * PI - PI / 72 do
  begin
    xx := R * Cos(a);
    yy := R * Sin(a);
    rr := Sqrt((xx - X) * (xx - X) + (yy - Y) * (yy - Y));
    cairo_arc(LContext, xx, yy, rr, 0, 2 * PI);
    cairo_stroke(LContext);
    a := a + PI / 36;
  end;

  cairo_destroy(LContext);
  cairo_surface_destroy(LSurface);

  SDL_UnlockTexture(ATexture);
end;

const
  SURFACE_WIDTH  = 480;
  SURFACE_HEIGHT = 480;

var
  LWindow: pSDL_Window;
  LRenderer: pSDL_Renderer;
  LTexture: pSDL_Texture;
  LEvent: pSDL_Event;

  LWindowWidth, LWindowHeight: integer;
  LRendererWidth, LRendererHeight: integer;
  LLoop: boolean = TRUE;

begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
    Halt;

  LWindow := SDL_CreateWindow(
    'Exemple SDL2 Cairo',
    SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED,
    SURFACE_WIDTH,
    SURFACE_HEIGHT,
    SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE or SDL_WINDOW_ALLOW_HIGHDPI
  );
  if LWindow = nil then
    Halt;

  SDL_GetWindowSize(LWindow, @LWindowWidth, @LWindowHeight);
  //WriteLn(Format('LWindowWidth=%d LWindowHeight=%d', [LWindowWidth, LWindowHeight]));

  LRenderer := SDL_CreateRenderer(LWindow, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
  if LRenderer = nil then
    Halt;

  SDL_GetRendererOutputSize(LRenderer, @LRendererWidth, @LRendererHeight);
  //WriteLn(Format('LRendererWidth=%d LRendererHeight=%d', [LRendererWidth, LRendererHeight]));

  SDL_SetRenderDrawColor(LRenderer, 0, 0, 0, 0);
  SDL_RenderClear(LRenderer);

  LTexture := SDL_CreateTexture(LRenderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, LRendererWidth, LRendererHeight);

  New(LEvent);

  while LLoop do
  begin
    while SDL_PollEvent(LEvent) = 1 do
      case LEvent^.type_ of
        SDL_KEYDOWN:
          case LEvent^.key.keysym.sym of
            SDLK_ESCAPE, SDLK_q: LLoop := FALSE;
          end;
        SDL_QUITEV:
          LLoop := FALSE;
        SDL_WINDOWEVENT:
          begin
            case LEvent^.window.event of
              SDL_WINDOWEVENT_SHOWN: WriteLn('W. shown');
              SDL_WINDOWEVENT_MOVED: WriteLn('W. moved');
              SDL_WINDOWEVENT_MINIMIZED: WriteLn('W. minimized');
              SDL_WINDOWEVENT_MAXIMIZED: WriteLn('W. maximized');
              SDL_WINDOWEVENT_SIZE_CHANGED,
              SDL_WINDOWEVENT_RESIZED:
              begin
                WriteLn('W. resized');
                SDL_DestroyTexture(LTexture);
                SDL_GetRendererOutputSize(LRenderer, @LRendererWidth, @LRendererHeight);
                LTexture := SDL_CreateTexture(LRenderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, LRendererWidth, LRendererHeight);
              end;
            end;
          end;
      end;

//    SDL_SetRenderDrawColor(LRenderer, 0, 0, 0, 255);
    SDL_SetRenderDrawColor(LRenderer, $FF, $FF, $FF, 255);
    SDL_RenderClear(LRenderer);

    CairoDraw(LTexture, LRendererWidth, LRendererHeight);

    SDL_RenderCopy(LRenderer, LTexture, nil, nil);
    SDL_RenderPresent(LRenderer);
//    SDL_Delay(100);
  end;

  Dispose(LEvent);

  SDL_DestroyTexture(LTexture);
  SDL_DestroyRenderer(LRenderer);
  SDL_DestroyWindow(LWindow);
  SDL_Quit;
end.
