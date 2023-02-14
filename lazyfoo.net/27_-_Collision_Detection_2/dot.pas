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
    gDotTexture: TLTexture;
  public
  const
    DOT_WIDTH = 20;
    DOT_HEIGHT = 20;
    DOT_VEL = 1;
    constructor Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer);
    destructor Destroy; override;
    procedure HandleEvent(var e: TSDL_Event);
    procedure move(const wall: array of TSDL_Rect);
    procedure render;
  end;

function checkCollision(var a: TSDL_Rect; b: array of TSDL_Rect): boolean;

implementation

function checkCollision(var a: TSDL_Rect; b: array of TSDL_Rect): boolean;
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

{ Tdot }

constructor Tdot.Create(ARenderer: PSDL_Renderer; Awidht, Aheigth: integer);
begin
  mPos.x := 0;
  mPos.y := 0;
  mVelX := 0;
  mVelY := 0;
  widht := Awidht;
  Height := Aheigth;
  mPos.w := DOT_WIDTH;
  mPos.h := DOT_HEIGHT;

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

procedure Tdot.move(const wall: array of TSDL_Rect);
var
  oldX, oldY: integer;
begin
  oldX := mPos.x;
  oldY := mPos.y;

  Inc(mPos.x, mVelX);

  if (mPos.x < 0) or (mPos.x + DOT_WIDTH > widht) or checkCollision(mPos, wall) then begin
    mPos.x := oldX;
  end;

  Inc(mPos.y, mVelY);
  if (mPos.y < 0) or (mPos.y + DOT_HEIGHT > Height) or checkCollision(mPos, wall) then begin
    mPos.y := oldY;
  end;
end;

procedure Tdot.render;
begin
  gDotTexture.Render(mPos.x, mPos.y);
end;

end.
