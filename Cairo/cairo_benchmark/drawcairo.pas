unit DrawCairo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Cairo;

procedure Draw(cr: Pcairo_t; Name: string);

implementation

procedure Draw(cr: Pcairo_t; Name: string);
var
  xc: double = 320;
  yc: double = 240;
  radius: double = 200;
  angele1: double = 45 * (pi / 180);
  angele2: double = 180 * (pi / 180);
  startTime: TDateTime;
  i: integer;
begin
  startTime := now;

  for i := 1 to 10000 do begin
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

  WriteLn(Name, ': ', (now - startTime) * 1000: 10: 5);
end;

end.
