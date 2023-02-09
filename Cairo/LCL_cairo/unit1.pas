unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Cairo,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    bit: TBitmap;
    cr_surface: Pcairo_surface_t;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  cr: Pcairo_t;
  xc: double = 320;
  yc: double = 240;
  radius: double = 200;
  angele1: double = 45 * (pi / 180);
  angele2: double = 180 * (pi / 180);

begin
  bit := TBitmap.Create;
  bit.Width := 640;
  bit.Height := 480;

  //  cr_surface := cairo_image_surface_create_for_data(sd_surface^.pixels, CAIRO_FORMAT_RGB24, sd_surface^.w, sd_surface^.h, sd_surface^.pitch);
  cr_surface := cairo_image_surface_create_for_data(bit.RawImage.Data, CAIRO_FORMAT_RGB24, bit.Width, bit.Height, 640*4);

  cr := cairo_create(cr_surface);

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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bit.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  canvas.Draw(10, 10, bit);
  canvas.Draw(710, 10, bit);
end;

end.
