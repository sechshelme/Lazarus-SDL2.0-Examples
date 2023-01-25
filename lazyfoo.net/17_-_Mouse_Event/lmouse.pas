unit LMouse;

interface

uses
  sdl2,
  sdl2_image,
  ctypes, LTexture;

const
  BUTTON_WIDTH = 300;
  BUTTON_HEIGHT = 200;
  TOTAL_BUTTONS = 4;


    BUTTON_SPRITE_MOUSE_OUT=0;
    BUTTON_SPRITE_MOUSE_OVER_MOTION=1;
    BUTTON_SPRITE_MOUSE_DOWN=2;
    BUTTON_SPRITE_MOUSE_UP=3;
    BUTTON_SPRITE_TOTAL=4;
type
//  TButtonSprite = (
//    BUTTON_SPRITE_MOUSE_OUT,
//    BUTTON_SPRITE_MOUSE_OVER_MOTION,
//    BUTTON_SPRITE_MOUSE_DOWN,
//    BUTTON_SPRITE_MOUSE_UP,
//    BUTTON_SPRITE_TOTAL);

  TLButton = class(TObject)
  private
    mPosition: TSDL_Point;
//    mCurrentSprite: TButtonSprite;
      mCurrentSprite: Integer;
  public
    constructor Create;
    procedure SetPosition(Ax, Ay: integer);
    procedure HandleEvent(e: PSDL_Event);
    procedure Renderer;
  end;

var
  gSpritesChlips: array[0..TOTAL_BUTTONS - 1] of TSDL_Rect;
  gButtonSpriteSheetTexture: TLTexture;

implementation

{ TLButton }

constructor TLButton.Create;
begin
  mPosition.x := 0;
  mPosition.y := 0;
  mCurrentSprite := BUTTON_SPRITE_MOUSE_OUT;
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
//  gButtonSpriteSheetTexture.Render(mPosition.x, mPosition.y, @gSpritesChlips[shortint(mCurrentSprite)]);
  gButtonSpriteSheetTexture.Render(mPosition.x, mPosition.y, @gSpritesChlips[mCurrentSprite]);
end;

end.
