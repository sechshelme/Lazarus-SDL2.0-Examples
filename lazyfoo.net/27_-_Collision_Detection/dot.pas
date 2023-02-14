unit dot;

interface

uses
  sdl2,
  sdl2_image, LTexture;

type

  { Tdot }

  Tdot = class(TObject)
  private
    widht, Height, mPosX, mPosY, mVelX, mVelY: integer;
    mCollider: TSDL_Rect;
    gDotTexture: TLTexture;
  public
  const
    DOT_WIDTH = 20;
    DOT_HEIGHT = 20;
    DOT_VEL = 10;
    constructor Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer);
    destructor Destroy; override;
    procedure HandleEvent(var e: TSDL_Event);
    procedure move(wall: TSDL_Rect);
    procedure render;
  end;

function checkCollision(var a, b: TSDL_Rect): boolean;

implementation

function checkCollision(var a, b: TSDL_Rect): boolean;
var
  leftA, leftB, rightA, rightB, topA, topB, bottomA, bottomB: integer;
begin
  leftA := A.x;
  rightA := A.x + A.w;
  topA := A.y;
  bottomA := A.y + A.h;

  leftB := B.x;
  rightB := B.x + B.w;
  topB := B.y;
  bottomB := B.y + B.h;

  Result := True;
  if bottomA <= topB then begin
    Result := False;
  end;
  if topA >= bottomB then begin
    Result := False;
  end;
  if rightA <= leftB then begin
    Result := False;
  end;
  if leftA >= rightB then begin
    Result := False;
  end;
end;

{ Tdot }

constructor Tdot.Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer);
begin
  mPosX := 0;
  mPosY := 0;
  mVelX := 0;
  mVelY := 0;
  widht := Awidht;
  Height := Aheigth;
  mCollider.w := DOT_WIDTH;
  mCollider.h := DOT_HEIGHT;

  gDotTexture := TLTexture.Create(ARenderer);
  gDotTexture.LoadFromFile('dot.bmp', $FF, $FF, $FF);
  if gDotTexture = nil then begin
    WriteLn('Failed to load dot texture!');
  end;
end;

destructor Tdot.Destroy;
begin
  gDotTexture.Free;
  inherited Destroy;
end;

procedure Tdot.HandleEvent(var e: TSDL_Event);
begin
  case e.type_ of
    SDL_KEYDOWN: begin
      if e.key.repeat_ = 0 then begin
        case e.key.keysym.sym of
          SDLK_UP: begin
            Dec(mVelY, DOT_VEL);
          end;
          SDLK_DOWN: begin
            Inc(mVelY, DOT_VEL);
          end;
          SDLK_LEFT: begin
            Dec(mVelX, DOT_VEL);
          end;
          SDLK_RIGHT: begin
            Inc(mVelX, DOT_VEL);
          end;
        end;
      end;
    end;
    SDL_KEYUP: begin
      if e.key.repeat_ = 0 then begin
        case e.key.keysym.sym of
          SDLK_UP: begin
            Inc(mVelY, DOT_VEL);
          end;
          SDLK_DOWN: begin
            Dec(mVelY, DOT_VEL);
          end;
          SDLK_LEFT: begin
            Inc(mVelX, DOT_VEL);
          end;
          SDLK_RIGHT: begin
            Dec(mVelX, DOT_VEL);
          end;
        end;
      end;
    end;
  end;
end;

procedure Tdot.move(wall: TSDL_Rect);
var
  oldX, oldY: integer;
begin
  oldX := mPosX;
  oldY := mPosY;
  Inc(mPosX, mVelX);
  mCollider.x := mPosX;
  if (mPosX < 0) or (mPosX + DOT_WIDTH > widht) or checkCollision(mCollider, wall) then begin
    mPosX := oldX;
    mCollider.x := mPosX;
  end;

  Inc(mPosY, mVelY);
  mCollider.y := mPosY;
  if (mPosY < 0) or (mPosY + DOT_HEIGHT > Height) or checkCollision(mCollider, wall) then begin
    mPosY := oldY;
    mCollider.y := mPosY;
  end;
end;

procedure Tdot.render;
begin
  gDotTexture.Render(mPosX, mPosY);
end;

end.
