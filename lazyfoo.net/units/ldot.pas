unit LDot;

interface

uses
  sdl2,
  LTexture, LParticle;

type

  TSDL_Rects = array of TSDL_Rect;

  { TLDot }

  TLDot = class(TObject)
  private
    widht, Height: integer;
    mVel: TSDL_Point;
    mPos: TSDL_Rect;
    misParticle: boolean;

    gDotTexture: TLTexture;
    FRenderer: PSDL_Renderer;
    particles: array of TLParticle;
    procedure renderParticles;
  public
  const
    DOT_WIDTH = 20;
    DOT_HEIGHT = 20;
    DOT_VEL = 1;
    TOTAL_PARTICLES = 20;
    constructor Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer; AisParticle: boolean = False);
    destructor Destroy; override;
    procedure HandleEvent(var e: TSDL_Event);
    procedure move(const wall: TSDL_Rects = nil);
    procedure render;
  end;

implementation

function checkCollision(var a: TSDL_Rect; b: TSDL_Rects): boolean;
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

{ TLDot }

constructor TLDot.Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer; AisParticle: boolean);
var
  i: integer;
begin
  mPos.X := 0;
  mPos.Y := 0;
  mPos.w := DOT_WIDTH;
  mPos.h := DOT_HEIGHT;
  mVel.X := 0;
  mVel.Y := 0;
  widht := Awidht;
  Height := Aheigth;

  FRenderer := ARenderer;
  misParticle := AisParticle;

  gDotTexture := TLTexture.Create(FRenderer);
  if not gDotTexture.LoadFromFile(BMPPath + 'dot.bmp', $FF, $FF, $FF) then begin
    WriteLn('Failed to load dottexture!');
  end;

  if misParticle then begin
    SetLength(particles, TOTAL_PARTICLES);
    for i := 0 to Length(particles) - 1 do begin
      particles[i] := TLParticle.Create(FRenderer, mPos.X, mPos.Y);
    end;
  end;
end;

destructor TLDot.Destroy;
var
  i: integer;
begin
  gDotTexture.Free;
  if misParticle then begin
    for i := 0 to Length(particles) - 1 do begin
      particles[i].Free;
    end;
  end;
  inherited Destroy;
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

procedure TLDot.renderParticles;
var
  i: integer;
begin
  for i := 0 to Length(particles) - 1 do begin
    if particles[i].isDead then begin
      particles[i].reset(mPos.X, mPos.Y);
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
  oldX := mPos.x;
  oldY := mPos.y;

  Inc(mPos.x, mVel.X);

  if (mPos.x < 0) or (mPos.x + DOT_WIDTH > widht) or checkCollision(mPos, wall) then begin
    mPos.x := oldX;
  end;

  Inc(mPos.y, mVel.Y);
  if (mPos.y < 0) or (mPos.y + DOT_HEIGHT > Height) or checkCollision(mPos, wall) then begin
    mPos.y := oldY;
  end;
end;

procedure TLDot.render;
begin
  gDotTexture.Render(mPos.X, mPos.Y);
  if misParticle then  begin
    renderParticles;
  end;
end;

end.
