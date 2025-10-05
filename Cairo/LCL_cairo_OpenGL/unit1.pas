unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Cairo, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GL,
  OpenGLContext;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    cr: Pcairo_t;
    cr_surface: Pcairo_surface_t;

    buffer: array of byte;
    procedure DrawCairo(angele1, angele2: double);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.DrawCairo(angele1, angele2: double);
var
  xc: double;
  yc: double;
  radius: double;
begin

  cr_surface := cairo_image_surface_create_for_data(pbyte(buffer), CAIRO_FORMAT_RGB24, OpenGLControl1.Width, OpenGLControl1.Height, OpenGLControl1.Width * 4);
  cr := cairo_create(cr_surface);

  xc := OpenGLControl1.Width / 2;
  yc := OpenGLControl1.Height / 2;
  radius := OpenGLControl1.Height / 3;

  cairo_translate(cr, xc, yc);

  cairo_set_source_rgba(cr, 1, 1, 0, 1.0);
  cairo_rectangle(cr, -xc, -yc, OpenGLControl1.Width, OpenGLControl1.Height);
  cairo_fill(cr);

  cairo_set_source_rgba(cr, 0., 0, 0, 1.0);
  cairo_set_line_width(cr, radius / 15);
  cairo_arc(cr, 0, 0, radius, angele1, angele2);
  cairo_stroke(cr);

  cairo_set_source_rgba(cr, 0.2, 0.2, 1.0, 0.6);
  cairo_set_line_width(cr, radius / 25);

  cairo_arc(cr, 0, 0, 10.0, 0, 2 * pi);
  cairo_fill(cr);

  cairo_arc(cr, 0, 0, radius, angele1, angele1);
  cairo_line_to(cr, 0, 0);
  cairo_arc(cr, 0, 0, radius, angele2, angele2);
  cairo_line_to(cr, 0, 0);
  cairo_stroke(cr);

  glDrawPixels(OpenGLControl1.Width, OpenGLControl1.Height, GL_RGBA, GL_UNSIGNED_BYTE, Pointer(buffer));
  OpenGLControl1.SwapBuffers;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Enabled := False;
  OpenGLControl1.Align := alClient;
  OpenGLControl1.AutoResizeViewport := False;
  OpenGLControl1.MakeCurrent();
  glClearColor(1, 1, 1, 0);

  Timer1.Interval := 10;
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  cairo_destroy(cr);
  cairo_surface_destroy(cr_surface);
end;

procedure TForm1.OpenGLControl1Resize(Sender: TObject);
begin
  SetLength(buffer, OpenGLControl1.Width * OpenGLControl1.Height * 4);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  an: double = 0.0;
begin
  an += 0.03;
  if an > 2 * Pi then begin
    an -= 2 * Pi;
  end;
  DrawCairo(an, an * 2);
end;

end.
