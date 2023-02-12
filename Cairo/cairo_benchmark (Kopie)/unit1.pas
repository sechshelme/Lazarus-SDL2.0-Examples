unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Cairo, xlib, x, keysym,
  CairoXlib, glib2, gdk2, gtk2, sdl2, Classes, SysUtils, ctypes,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DrawCairo;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnBitmap: TButton;
    Button1: TButton;
    ButtonX11: TButton;
    ButtonSDL: TButton;
    GTKButton: TButton;
    procedure BtnBitmapClick(Sender: TObject);
    procedure BtnGTKClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonSDLClick(Sender: TObject);
    procedure ButtonX11Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  gtk_init(@argc, @argv);
end;

// --- GTK

function on_Exposs_Event(widget: PGtkWidget; event: PGdkEventExpose; Data: gpointer): gboolean;
var
  cr: Pcairo_t;
begin
  cr := gdk_cairo_create(widget^.window);
  Draw(cr, 'GTK');
  cairo_destroy(cr);
  Result := False;
end;

procedure TForm1.BtnGTKClick(Sender: TObject);
var
  window: PGtkWidget;
begin
  //  gtk_init(@argc, @argv);
  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title(GTK_WINDOW(Window), 'GTK-Fenster');
  gtk_window_set_default_size(GTK_WINDOW(Window), 640, 480);

  g_signal_connect(window, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
  g_signal_connect(window, 'expose-event', G_CALLBACK(@on_Exposs_Event), nil);

  gtk_widget_set_app_paintable(window, gTRUE);
  gtk_widget_show_all(window);

  gtk_main;
end;

// --- SDL ohne Textur

procedure TForm1.Button1Click(Sender: TObject);
var
  renderer_width: integer = 640;
  renderer_height: integer = 480;

  window: PSDL_Window;
  renderer: PSDL_Renderer;
  sd_surface: PSDL_Surface;
  cr_surface: Pcairo_surface_t;
  cr: Pcairo_t;
  //  texture: PSDL_Texture;
  quit: boolean = False;
  e: TSDL_Event;
begin
  SDL_Init(SDL_INIT_VIDEO);
  window := SDL_CreateWindow('An SDL2 window', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI);
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);


  //  sd_surface := SDL_CreateRGBSurface(0, renderer_width, renderer_height, 32, $FF0000, $00FF00, $0000FF, 0);
  sd_surface := SDL_GetWindowSurface(window);


  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  cr_surface := cairo_image_surface_create_for_data(sd_surface^.pixels, CAIRO_FORMAT_RGB24, sd_surface^.w, sd_surface^.h, sd_surface^.pitch);
  cr := cairo_create(cr_surface);
  Draw(cr, 'SDL_no_tex');

  //  texture := SDL_CreateTextureFromSurface(renderer, sd_surface);

  SDL_FreeSurface(sd_surface);
  //  SDL_RenderCopy(renderer, texture, nil, nil);
  SDL_RenderPresent(renderer);

  while not quit do begin
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
    SDL_UpdateWindowSurface(window);
  end;

  //  SDL_DestroyTexture(texture);
  cairo_destroy(cr);
  cairo_surface_destroy(cr_surface);

  SDL_DestroyWindow(window);
  SDL_Quit();
end;

// --- SDL

procedure TForm1.ButtonSDLClick(Sender: TObject);
const
  TexturSize = 256;
type
  PBuffer = ^TBuffer;
  TBuffer = array[0..TexturSize - 1, 0..TexturSize - 1] of uint32;
var
  renderer_width: integer = 640;
  renderer_height: integer = 480;

  window: PSDL_Window;
  renderer: PSDL_Renderer;
  sd_surface: PSDL_Surface;
  cr_surface: Pcairo_surface_t;
  cr: Pcairo_t;
  texture: PSDL_Texture;
  quit: boolean = False;
  e: TSDL_Event;
  p: Pointer;
  pitch: integer;

  buffer: PBuffer;
begin

  SDL_Init(SDL_INIT_VIDEO);
  window := SDL_CreateWindow('An SDL2 window', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI);
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);


  sd_surface := SDL_CreateRGBSurface(0, renderer_width, renderer_height, 32, $FF0000, $00FF00, $0000FF, 0);
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  //  texture := SDL_CreateTextureFromSurface(renderer, sd_surface);
  //  SDL_LockTexture(texture, nil, @Pointer( buffer), @pitch);
  //  WriteLn(pitch);
  //  cr_surface := cairo_image_surface_create_for_data(@buffer^[0,0], CAIRO_FORMAT_RGB24, 640, 480, pitch);



  cr_surface := cairo_image_surface_create_for_data(sd_surface^.pixels, CAIRO_FORMAT_RGB24, sd_surface^.w, sd_surface^.h, sd_surface^.pitch);
  cr := cairo_create(cr_surface);
  Draw(cr, 'SDL');
  //  SDL_UnlockTexture(texture);

  texture := SDL_CreateTextureFromSurface(renderer, sd_surface);

  SDL_FreeSurface(sd_surface);

  while not quit do begin
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
    SDL_RenderCopy(renderer, texture, nil, nil);
    SDL_RenderPresent(renderer);
  end;

  cairo_destroy(cr);
  cairo_surface_destroy(cr_surface);

  SDL_DestroyTexture(texture);
  SDL_DestroyWindow(window);
  SDL_Quit();
end;

// --- X11

procedure TForm1.ButtonX11Click(Sender: TObject);
var
  win: TWindow;
  dis: PDisplay;
  scr: cint;
  Event: TXEvent;

  cr_surface: Pcairo_surface_t;
  cr: Pcairo_t;
  wm_delete_window: TAtom;
  quit: boolean = False;
begin
  dis := XOpenDisplay(nil);
  scr := DefaultScreen(dis);
  win := XCreateSimpleWindow(dis, RootWindow(dis, scr), 10, 10, 640, 480, 0, $FFFFFF, $000000);
  XSelectInput(dis, win, KeyPressMask or ButtonPressMask);
  XStoreName(dis, win, 'X11');
  XMapWindow(dis, win);

  wm_delete_window := XInternAtom(dis, 'WM_DELETE_WINDOW', False);
  XSetWMProtocols(dis, win, @wm_delete_window, 1);

  cr_surface := cairo_xlib_surface_create(dis, win, DefaultVisual(dis, scr), 640, 480);
  cairo_xlib_surface_set_size(cr_surface, 640, 480);
  cr := cairo_create(cr_surface);
  Draw(cr, 'X11');

  while not quit do begin
    XNextEvent(dis, @Event);
    case Event._type of
      x.KeyPress: begin
        // Beendet das Programm bei [ESC]
        if XLookupKeysym(@Event.xkey, 0) = XK_Escape then begin
          Break;
        end;
      end;
      x.ClientMessage: begin
        if (Event.xclient.Data.l[0] = wm_delete_window) then begin
          WriteLn('[X] wurde gedrückt');
          quit := True;
        end;
      end;
    end;
  end;

  cairo_destroy(cr);
  cairo_surface_destroy(cr_surface);

  XDestroyWindow(dis, win);
  XCloseDisplay(dis);
end;

// --- TBitmap

procedure TForm1.BtnBitmapClick(Sender: TObject);
var
  cr: Pcairo_t;
  bit: TBitmap;
  cr_surface: Pcairo_surface_t;
begin
  bit := TBitmap.Create;
  bit.Width := 640;
  bit.Height := 480;

  //  cr_surface := cairo_image_surface_create_for_data(sd_surface^.pixels, CAIRO_FORMAT_RGB24, sd_surface^.w, sd_surface^.h, sd_surface^.pitch);
  cr_surface := cairo_image_surface_create_for_data(bit.RawImage.Data, CAIRO_FORMAT_RGB24, bit.Width, bit.Height, 640 * 4);
  cr := cairo_create(cr_surface);
  Draw(cr, 'TBitmap');
  cairo_destroy(cr);
  cairo_surface_destroy(cr_surface);
  canvas.Draw(10, 10, bit);

  bit.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  //  canvas.Draw(10, 10, bit);
  //  canvas.Draw(710, 10, bit);
end;

end.
