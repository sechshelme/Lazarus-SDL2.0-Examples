unit LMouse;

interface

uses
  sdl2,
  sdl2_image,
  ctypes, LTexture;

const
  BUTTON_WIDTH = 300;
  BUTTON_HEIGHT = 200;

type
  TLButton = class(TObject)
  private
    mPosition: TSDL_Point;
    mCurrentSprite: (BUTTON_SPRITE_MOUSE_OUT, BUTTON_SPRITE_MOUSE_OVER_MOTION, BUTTON_SPRITE_MOUSE_DOWN, BUTTON_SPRITE_MOUSE_UP, BUTTON_SPRITE_TOTAL);
    gSpritesChlips: array[0..3] of TSDL_Rect;
    FButtonSpriteSheetTexture:TLTexture;
  public
    constructor Create(AButtonSpriteSheetTexture:TLTexture);
    procedure SetPosition(Ax, Ay: integer);
    procedure HandleEvent(e: PSDL_Event);
    procedure Renderer;
  end;

implementation

constructor TLButton.Create(AButtonSpriteSheetTexture: TLTexture);
var
  i: integer;
begin
  FButtonSpriteSheetTexture:=AButtonSpriteSheetTexture;
  mPosition.x := 0;
  mPosition.y := 0;
  mCurrentSprite := BUTTON_SPRITE_MOUSE_OUT;
  for i := 0 to Length(gSpritesChlips) - 1 do begin
    gSpritesChlips[i].x := 0;
    gSpritesChlips[i].y := i * 200;
    gSpritesChlips[i].w := BUTTON_WIDTH;
    gSpritesChlips[i].h := BUTTON_HEIGHT;
  end;
end;

procedure TLButton.SetPosition(Ax, Ay: integer);
begin
  mPosition.x := Ax;
  mPosition.y := Ay;
end;

procedure TLButton.HandleEvent(e: PSDL_Event);
var
  x, y: integer;
  inside: boolean;
begin
  case e^.type_ of
    SDL_MOUSEMOTION,
    SDL_MOUSEBUTTONDOWN,
    SDL_MOUSEBUTTONUP: begin
      SDL_GetMouseState(@x, @y);
      inside := True;
      if x < mPosition.x then begin
        inside := False;
      end else if x > mPosition.x + BUTTON_WIDTH then begin
        inside := False;
      end else if y < mPosition.y then begin
        inside := False;
      end else if y > mPosition.y + BUTTON_HEIGHT then begin
        inside := False;
      end;
      if not inside then begin
        mCurrentSprite := BUTTON_SPRITE_MOUSE_OUT;
      end else begin
        case e^.type_ of
          SDL_MOUSEMOTION: begin
            mCurrentSprite := BUTTON_SPRITE_MOUSE_OVER_MOTION;
          end;
          SDL_MOUSEBUTTONDOWN: begin
            mCurrentSprite := BUTTON_SPRITE_MOUSE_DOWN;
          end;
          SDL_MOUSEBUTTONUP: begin
            mCurrentSprite := BUTTON_SPRITE_MOUSE_UP;
          end;
        end;
      end;
    end;
  end;
end;

procedure TLButton.Renderer;
begin
  FButtonSpriteSheetTexture.Render(mPosition.x, mPosition.y, @gSpritesChlips[shortint(mCurrentSprite)]);
//  FButtonSpriteSheetTexture.Render(mPosition.x, mPosition.y, @gSpritesChlips[mCurrentSprite]);
end;

end.
