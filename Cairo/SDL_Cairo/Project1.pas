program Project1;

uses
  Cairo,
  SDL2;

// Ist bei Lazarus nicht dabei !
  procedure cairo_surface_set_device_scale(cr: Pcairo_surface_t; x, y: double); cdecl; external LIB_CAIRO;

var
  window: PSDL_Window;
  sd_surface: PSDL_Surface;
  quit: boolean = False;
  e: TSDL_Event;
  renderer: PSDL_Renderer;
  texture: PSDL_Texture;

  window_height, window_width, renderer_height, renderer_width, cairo_x_multiplier, cairo_y_multiplier: integer;

  cr_surface: Pcairo_surface_t;
  cr: Pcairo_t;
  xc: double = 320;
  yc: double = 240;
  radius: double = 200;
  angele1: double = 45 * (pi / 180);
  angele2: double = 180 * (pi / 180);

begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
    WriteLn('SDL could not initialize! SDL_Error: ', SDL_GetError);
  end else begin
    window := SDL_CreateWindow('An SDL2 window', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI);
    if window = nil then begin
      WriteLn('Window could not be created! SDL_Error: ', SDL_GetError);
    end else begin
      renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
      SDL_GetWindowSize(window, @window_width, @window_height);
      WriteLn('Window: Width: ', window_width, ' Height: ', window_height);

      SDL_GetRendererOutputSize(renderer, @renderer_width, @renderer_height);
      WriteLn('Renderer: Width: ', window_width, ' Height: ', window_height);

      cairo_x_multiplier := renderer_width div window_width;
      cairo_y_multiplier := renderer_height div window_height;

      sd_surface := SDL_CreateRGBSurface(0, renderer_width, renderer_height, 32, $FF0000, $00FF00, $0000FF, 0);

      WriteLn('sdl_surface->w=', sd_surface^.w);
      WriteLn('sdl_surface->h=', sd_surface^.h);
      WriteLn('sdl_surface->pitch=', sd_surface^.pitch);
      WriteLn('sdl_surface->format=', sd_surface^.format^.format);

      cr_surface := cairo_image_surface_create_for_data(sd_surface^.pixels, CAIRO_FORMAT_RGB24, sd_surface^.w, sd_surface^.h, sd_surface^.pitch);

      cairo_surface_set_device_scale(cr_surface, cairo_x_multiplier, cairo_y_multiplier);

      cr := cairo_create(cr_surface);

      SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
      SDL_RenderClear(renderer);

      //            SDL_FillRect(sd_surface, nil, SDL_MapRGB(sd_surface^.format, 255, 255, 255));

      cairo_set_source_rgba(cr, 1, 1, 1, 1.0);
      cairo_rectangle(cr, 0, 0, 640, 480);
      cairo_fill(cr);

      cairo_set_source_rgba(cr, 0, 0, 0, 1.0);
      cairo_set_line_width(cr, 10.0);
      cairo_arc(cr, xc, yc, radius, angele1, angele2);
      cairo_stroke(cr);

      cairo_set_source_rgba(cr, 1, 0.2, 0.2, 0.6);
      cairo_set_line_width(cr, 6.0);

      cairo_arc(cr, xc, yc, 10.0, 0, 2 * pi);
      cairo_fill(cr);

      cairo_arc(cr, xc, yc, radius, angele1, angele1);
      cairo_line_to(cr, xc, yc);
      cairo_arc(cr, xc, yc, radius, angele2, angele2);
      cairo_line_to(cr, xc, yc);
      cairo_stroke(cr);

      texture := SDL_CreateTextureFromSurface(renderer, sd_surface);
      SDL_FreeSurface(sd_surface);

      SDL_RenderCopy(renderer, texture, nil, nil);
      SDL_RenderPresent(renderer);

      while not quit do begin
        while SDL_PollEvent(@e) <> 0 do begin
          case e.type_ of
            SDL_QUITEV: begin
              quit := True;
            end;
          end;
        end;
      end;
    end;
  end;
  SDL_DestroyWindow(window);
  SDL_Quit();
end.
