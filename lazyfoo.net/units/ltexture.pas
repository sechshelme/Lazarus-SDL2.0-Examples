unit LTexture;

interface

uses
  sdl2,
  sdl2_image,
  sdl2_ttf,
  ctypes;

type

  { TLTexture }

  TLTexture = class(TObject)
  private
    FHeight: integer;
    FWidht: integer;
    mTexture: PSDL_Texture;
    FRenderer: PSDL_Renderer;
  public
    property Widht: integer read FWidht;
    property Height: integer read FHeight;
    constructor Create(ARenderer: PSDL_Renderer);
    destructor Destroy; override;
    function LoadFromFile(path: string): boolean;
    function LoadFromRenderedText(gFont: PTTF_Font; textureText: string; textColor: TSDL_Color): boolean;
    procedure FreeTexture;
    procedure SetColor(red, green, blue: byte);
    procedure SetBlendMode(blending: TSDL_BlendMode);
    procedure SetAlpha(alpha: byte);
    //    procedure Render(x, y: integer; clip: PSDL_Rect = nil;angele:Double=0.0;center:PSDL_Point=nil;flip:SDL_RendererFlip=SDL_FLIP_NONE);
    procedure Render(x, y: integer; clip: PSDL_Rect = nil; angle: double = 0.0; center: PSDL_Point = nil; flip: cint = SDL_FLIP_NONE);
  end;

implementation

{ TLTexture }

constructor TLTexture.Create(ARenderer: PSDL_Renderer);
begin
  FWidht := 0;
  FHeight := 0;
  mTexture := nil;
  FRenderer := ARenderer;
end;

destructor TLTexture.Destroy;
begin
  FreeTexture;
  inherited Destroy;
end;

function TLTexture.LoadFromFile(path: string): boolean;
var
  loadedSurface: PSDL_Surface;
  newTexture: PSDL_Texture;
begin
  loadedSurface := IMG_Load(PChar(path));
  if loadedSurface = nil then  begin
    WriteLn('Unable to load image ' + path + '! SDL_image Error: ', IMG_GetError());
  end else begin
    SDL_SetColorKey(loadedSurface, SDL_TRUE, SDL_MapRGB(loadedSurface^.format, $00, $FF, $FF));
    //    SDL_SetColorKey(loadedSurface, 1, SDL_MapRGB(loadedSurface^.format, $00, $FF, $FF));
    newTexture := SDL_CreateTextureFromSurface(FRenderer, loadedSurface);
    if newTexture = nil then begin
      WriteLn('Unable to create texturefrom ', path, ' SDL Error: ', SDL_GetError);
    end else begin
      FWidht := loadedSurface^.w;
      FHeight := loadedSurface^.h;
    end;
    SDL_FreeSurface(loadedSurface);
  end;

  mTexture := newTexture;
  Result := mTexture <> nil;
end;

function TLTexture.LoadFromRenderedText(gFont: PTTF_Font; textureText: string; textColor: TSDL_Color): boolean;
var
  textSurface: PSDL_Surface;
begin
  FreeTexture;
  textSurface := TTF_RenderText_Solid(gFont, PChar(textureText), textColor);
  if textSurface = nil then begin
    WriteLn('Unable to create texture from rendered text! SDL Error: ', SDL_GetError);
  end else begin
    mTexture := SDL_CreateTextureFromSurface(FRenderer, textSurface);
    if mTexture = nil then begin
      WriteLn('Unable to create texture from rendered text! SDL Error: ', SDL_GetError);
    end else begin
      FWidht := textSurface^.w;
      FHeight := textSurface^.h;
    end;
    SDL_FreeSurface(textSurface);
  end;
  Result := mTexture <> nil;
end;

procedure TLTexture.FreeTexture;
begin
  if mTexture <> nil then begin
    SDL_DestroyTexture(mTexture);
    mTexture := nil;
    FWidht := 0;
    FHeight := 0;
  end;
end;

procedure TLTexture.SetColor(red, green, blue: byte);
begin
  SDL_SetTextureColorMod(mTexture, red, green, blue);
end;

procedure TLTexture.SetBlendMode(blending: TSDL_BlendMode);
begin
  SDL_SetTextureBlendMode(mTexture, blending);
end;

procedure TLTexture.SetAlpha(alpha: byte);
begin
  SDL_SetTextureAlphaMod(mTexture, alpha);
end;

procedure TLTexture.Render(x, y: integer; clip: PSDL_Rect; angle: double; center: PSDL_Point; flip: cint);
var
  renderQuad: TSDL_Rect;
begin
  renderQuad.x := x;
  renderQuad.y := y;
  if clip <> nil then begin
    renderQuad.w := clip^.w;
    renderQuad.h := clip^.h;
  end else begin
    renderQuad.w := FWidht;
    renderQuad.h := FHeight;
  end;
  SDL_RenderCopyEx(FRenderer, mTexture, clip, @renderQuad, angle, center, flip);
end;

end.
