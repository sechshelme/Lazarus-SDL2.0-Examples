unit dot;

interface

uses
  sdl2,
  sdl2_image, LTexture;

type

  { Tdot }

  Tdot = class(TObject)
  private
    widht, Height, mVelX, mVelY: integer;
    mPos: TSDL_Rect;
  public
  const
    DOT_WIDTH = 20;
    DOT_HEIGHT = 20;
    DOT_VEL = 10;
    constructor Create(Awidht, Aheigth: integer);
    procedure HandleEvent(var e: TSDL_Event);
    procedure move(wall: array of TSDL_Rect);
    procedure render(tex: TLTexture);
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

constructor Tdot.Create(Awidht, Aheigth: integer);
begin
  mPos.x := 0;
  mPos.y := 0;
  mVelX := 0;
  mVelY := 0;
  widht := Awidht;
  Height := Aheigth;
  mPos.w := DOT_WIDTH;
  mPos.h := DOT_HEIGHT;
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

procedure Tdot.move(wall: array of TSDL_Rect);
var
  oldX, oldY, i: integer;
begin
  oldX := mPos.x;
  oldY := mPos.y;

  Inc(mPos.x, mVelX);

  if (mPos.x < 0) or (mPos.x + DOT_WIDTH > widht) then begin
    mPos.x := oldX;
  end;
  for i := 0 to Length(wall) - 1 do begin
    if checkCollision(mPos, wall[i]) then begin
      mPos.x := oldX;
    end;
  end;

  Inc(mPos.y, mVelY);
  if (mPos.y < 0) or (mPos.y + DOT_HEIGHT > Height) then begin
    mPos.y := oldY;
  end;
  for i := 0 to Length(wall) - 1 do begin
    if checkCollision(mPos, wall[i]) then begin
      mPos.y := oldY;
    end;
  end;

end;

procedure Tdot.render(tex: TLTexture);
begin
  tex.Render(mPos.x, mPos.y);
end;

end.
