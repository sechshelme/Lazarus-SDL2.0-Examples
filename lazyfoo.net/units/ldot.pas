unit LDot;

{$modeswitch typehelpers}

interface

uses
  sdl2,
  LTexture, LParticle;

type
  TSDL_Rects = array of TSDL_Rect;

  TSDL_RectsHelper = type Helper for  TSDL_Rects
  public
    procedure Add(x, y, w, h: integer);
  end;

  { TLDot }

  TLDot = class(TObject)
  private
    FIsParticle: boolean;
    widht, Height: integer;
    mVel: TSDL_Point;
    FPosXY: TSDL_Rect;

    gDotTexture: TLTexture;
    FRenderer: PSDL_Renderer;
    particles: array of TLParticle;
    function checkCollision(var a: TSDL_Rect; b: TSDL_Rects): boolean;
    procedure renderParticles;
    procedure SetIsParticle(AValue: boolean);
  public
  const
    DOT_VEL = 1;
    TOTAL_PARTICLES = 20;
    property IsParticle: boolean read FIsParticle write SetIsParticle;
    property PosXY: TSDL_Rect read FPosXY;

    constructor Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer);
    procedure LoadTextures(pfad: string; r: byte = $FF; g: byte = $FF; b: byte = $FF);
    destructor Destroy; override;
    procedure HandleEvent(var e: TSDL_Event);
    procedure move(const wall: TSDL_Rects = nil);
    procedure render(camX: integer = 0; camY: integer = 0);
  end;

implementation

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

{ TLDot }

constructor TLDot.Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer);
begin
  FPosXY.X := 0;
  FPosXY.Y := 0;
  FPosXY.w := 20;
  FPosXY.h := 20;
  mVel.X := 0;
  mVel.Y := 0;
  widht := Awidht;
  Height := Aheigth;

  FRenderer := ARenderer;
  FIsParticle := False;

  gDotTexture := TLTexture.Create(FRenderer);
  LoadTextures(BMPPath + 'dot.bmp');
end;

procedure TLDot.LoadTextures(pfad: string; r: byte; g: byte; b: byte);
begin
  if not gDotTexture.LoadFromFile(pfad, r, g, b) then begin
    WriteLn('Failed to load dottexture!');
  end else begin
    FPosXY.w := gDotTexture.Widht;
    FPosXY.h := gDotTexture.Height;
  end;
end;

destructor TLDot.Destroy;
begin
  gDotTexture.Free;
  if FIsParticle then begin
    SetIsParticle(False);
  end;
  inherited Destroy;
end;

procedure TLDot.SetIsParticle(AValue: boolean);
var
  i: integer;
begin
  if FIsParticle <> AValue then begin
    FIsParticle := AValue;
    if FIsParticle then begin
      SetLength(particles, TOTAL_PARTICLES);
      for i := 0 to Length(particles) - 1 do begin
        particles[i] := TLParticle.Create(FRenderer, FPosXY.X, FPosXY.Y);
      end;
    end else begin
      for i := 0 to Length(particles) - 1 do begin
        particles[i].Free;
      end;
      SetLength(particles, 0);
    end;
  end;
end;

procedure TLDot.HandleEvent(var e: TSDL_Event);
begin
  case e.type_ of
    SDL_KEYDOWN: begin
      if e.key.repeat_ = 0 then begin
        case e.key.keysym.sym of
          SDLK_UP: begin
            Dec(mVel.Y, DOT_VEL);
          end;
          SDLK_DOWN: begin
            Inc(mVel.Y, DOT_VEL);
          end;
          SDLK_LEFT: begin
            Dec(mVel.X, DOT_VEL);
          end;
          SDLK_RIGHT: begin
            Inc(mVel.X, DOT_VEL);
          end;
          SDLK_p: begin
            IsParticle := not IsParticle;
          end;
        end;
      end;
    end;
    SDL_KEYUP: begin
      if e.key.repeat_ = 0 then begin
        case e.key.keysym.sym of
          SDLK_UP: begin
            Inc(mVel.Y, DOT_VEL);
          end;
          SDLK_DOWN: begin
            Dec(mVel.Y, DOT_VEL);
          end;
          SDLK_LEFT: begin
            Inc(mVel.X, DOT_VEL);
          end;
          SDLK_RIGHT: begin
            Dec(mVel.X, DOT_VEL);
          end;
        end;
      end;
    end;
  end;
end;

function TLDot.checkCollision(var a: TSDL_Rect; b: TSDL_Rects): boolean;
var
  leftA, leftB, rightA, rightB, topA, topB, bottomA, bottomB, i: integer;
begin
  leftA := A.x;
  rightA := A.x + A.w;
  topA := A.y;
  bottomA := A.y + A.h;

  Result := False;
  for i := 0 to Length(b) - 1 do begin
    leftB := B[i].x;
    rightB := B[i].x + B[i].w;
    topB := B[i].y;
    bottomB := B[i].y + B[i].h;

    if (bottomA > topB) and (topA < bottomB) and (rightA > leftB) and (leftA < rightB) then begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TLDot.renderParticles;
var
  i: integer;
begin
  for i := 0 to Length(particles) - 1 do begin
    if particles[i].isDead then begin
      particles[i].reset(FPosXY.X, FPosXY.Y);
    end;
  end;
  for i := 0 to Length(particles) - 1 do begin
    particles[i].render;
  end;
end;

procedure TLDot.move(const wall: TSDL_Rects);
var
  oldX, oldY: integer;
begin
  oldX := FPosXY.x;
  oldY := FPosXY.y;

  Inc(FPosXY.x, mVel.X);
  if (FPosXY.x < 0) or (FPosXY.x + FPosXY.w > widht) or checkCollision(FPosXY, wall) then begin
    FPosXY.x := oldX;
  end;

  Inc(FPosXY.y, mVel.Y);
  if (FPosXY.y < 0) or (FPosXY.y + FPosXY.h > Height) or checkCollision(FPosXY, wall) then begin
    FPosXY.y := oldY;
  end;
end;

procedure TLDot.render(camX: integer; camY: integer);
begin
  gDotTexture.Render(FPosXY.X - camX, FPosXY.Y - camY);
  if FIsParticle then  begin
    renderParticles;
  end;
end;

end.
