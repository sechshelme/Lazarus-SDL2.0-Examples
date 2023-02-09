unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Cairo,
  glib2,
  gdk2,
  gtk2,
  sdl2,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DrawCairo;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnBitmap: TButton;
    ButtonSDL: TButton;
    GTKButton: TButton;
    procedure BtnBitmapClick(Sender: TObject);
    procedure BtnGTKClick(Sender: TObject);
    procedure ButtonSDLClick(Sender: TObject);
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

// --- SDL

procedure TForm1.ButtonSDLClick(Sender: TObject);
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
begin
  SDL_Init(SDL_INIT_VIDEO);
  window := SDL_CreateWindow('An SDL2 window', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_SHOWN or SDL_WINDOW_ALLOW_HIGHDPI);
  renderer := SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);


  sd_surface := SDL_CreateRGBSurface(0, renderer_width, renderer_height, 32, $FF0000, $00FF00, $0000FF, 0);
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  cr_surface := cairo_image_surface_create_for_data(sd_surface^.pixels, CAIRO_FORMAT_RGB24, sd_surface^.w, sd_surface^.h, sd_surface^.pitch);
  cr := cairo_create(cr_surface);
  Draw(cr, 'SDL');

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

  SDL_DestroyTexture(texture);
  SDL_DestroyWindow(window);
  SDL_Quit();
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
