unit LWall;

interface

uses
  sdl2;

type
  TSDL_Rects = array of TSDL_Rect;

  { TLWall }

  TLWall = class(TObject)
  private
    FRenderer: PSDL_Renderer;
    Fcamera: TSDL_Rect;
  public
    Rect: TSDL_Rects;

    constructor Create(ARenderer: PSDL_Renderer);
    procedure renderer;
    procedure Add(x, y, w, h: integer);
    procedure CameraMove(var camera: TSDL_Rect);
  end;

implementation

constructor TLWall.Create(ARenderer: PSDL_Renderer);
begin
  inherited Create;
  FRenderer := ARenderer;
end;

procedure TLWall.renderer;
var
  i: integer;
  w: TSDL_Rect;
begin
  SDL_SetRenderDrawColor(FRenderer, $80, $40, $00, $FF);
  for i := 0 to Length(Rect) - 1 do begin
    w := Rect[i];
    w.x := w.x - Fcamera.x;
    w.y := w.y - Fcamera.y;
    SDL_RenderFillRect(FRenderer, @w);
  end;
end;

procedure TLWall.Add(x, y, w, h: integer);
var
  l: SizeInt;
begin
  l := Length(Rect);
  SetLength(Rect, l + 1);
  Rect[l].x := x;
  Rect[l].y := y;
  Rect[l].w := w;
  Rect[l].h := h;
end;

procedure TLWall.CameraMove(var camera: TSDL_Rect);
begin
  Fcamera := camera;
end;

end.
