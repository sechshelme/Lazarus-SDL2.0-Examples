unit LDot;

{$modeswitch typehelpers}

interface

uses
  sdl2,
  LTexture, LParticle,LWall;

type

  { TLDot }

  TLDot = class(TObject)
  private
    FIsParticle: boolean;
    FLevelWidth, FLevelHeight: integer;
    FScreenWidth, FScreenHeight: integer;
    mVel: TSDL_Point;
    Fcamera: TSDL_Rect;
    FPosXY: TSDL_Rect;

    gDotTexture: TLTexture;
    FRenderer: PSDL_Renderer;
    particles: array of TLParticle;
    function checkCollision(var a: TSDL_Rect; b: TSDL_Rects): boolean;
    procedure renderParticles;
    procedure SetIsParticle(AValue: boolean);
    procedure SetLevelHeight(AValue: integer);
    procedure SetLevelWidth(AValue: integer);
  public
    const
      DOT_VEL = 1;
      TOTAL_PARTICLES = 20;
    property LevelWidth: integer read FLevelWidth write SetLevelWidth;
    property LevelHeight: integer read FLevelHeight write SetLevelHeight;
    property IsParticle: boolean read FIsParticle write SetIsParticle;
    property PosXY: TSDL_Rect read FPosXY;
    constructor Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer; AScreenWidht: integer = 0; AScreenHeight: integer = 0);
    procedure LoadTextures(pfad: string; r: byte = $FF; g: byte = $FF; b: byte = $FF);
    destructor Destroy; override;
    procedure HandleEvent(var e: TSDL_Event);
    procedure move(const wall: TSDL_Rects = nil);
    procedure CameraMove(var camera: TSDL_Rect);
    procedure render;
  end;

implementation

{ TLDot }

constructor TLDot.Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer; AScreenWidht: integer; AScreenHeight: integer);
begin
  FPosXY.X := 0;
  FPosXY.Y := 0;
  FPosXY.w := 20;
  FPosXY.h := 20;
  mVel.X := 0;
  mVel.Y := 0;
  FLevelWidth := Awidht;
  FLevelHeight := Aheigth;

  if AScreenWidht = 0 then begin
    FScreenWidth := Awidht;
  end else begin
    FScreenWidth := AScreenWidht;
  end;
  if AScreenHeight = 0 then begin
    FScreenHeight := Aheigth;
  end else begin
    FScreenHeight := AScreenHeight;
  end;

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

procedure TLDot.SetLevelHeight(AValue: integer);
begin
  if FLevelHeight = AValue then begin
    Exit;
  end;
  FLevelHeight := AValue;
end;

procedure TLDot.SetLevelWidth(AValue: integer);
begin
  if FLevelWidth = AValue then begin
    Exit;
  end;
  FLevelWidth := AValue;
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
  if (FPosXY.x < 0) or (FPosXY.x + FPosXY.w > FLevelWidth) or checkCollision(FPosXY, wall) then begin
    FPosXY.x := oldX;
  end;

  Inc(FPosXY.y, mVel.Y);
  if (FPosXY.y < 0) or (FPosXY.y + FPosXY.h > FLevelHeight) or checkCollision(FPosXY, wall) then begin
    FPosXY.y := oldY;
  end;
end;

procedure TLDot.CameraMove(var camera: TSDL_Rect);
begin
  camera.x := (PosXY.x + PosXY.w div 2) - FScreenWidth div 2;
  camera.y := (PosXY.y + PosXY.h div 2) - FScreenHeight div 2;

  if camera.x < 0 then begin
    camera.x := 0;
  end;
  if camera.y < 0 then begin
    camera.y := 0;
  end;
  if camera.x > LevelWidth - camera.w then begin
    camera.x := LevelWidth - camera.w;
  end;
  if camera.y > LevelHeight - camera.h then begin
    camera.y := LevelHeight - camera.h;
  end;

  Fcamera:=camera;
end;

procedure TLDot.render;
begin
  gDotTexture.Render(FPosXY.X - Fcamera.x, FPosXY.Y - Fcamera.y);
  if FIsParticle then  begin
    renderParticles;
  end;
end;

end.
