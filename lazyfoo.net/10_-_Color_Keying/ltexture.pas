unit LTexture;

interface

uses
  sdl2,
  sdl2_image,
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
    procedure render(x, y: integer);
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
  if mTexture <> nil then begin
    SDL_DestroyTexture(mTexture);
  end;
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
    //      SDL_SetColorKey(loadedSurface, SDL_TRUE, SDL_MapRGB(loadedSurface^.format, $00, $FF, $FF));
    SDL_SetColorKey(loadedSurface, 1, SDL_MapRGB(loadedSurface^.format, $00, $FF, $FF));
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

procedure TLTexture.render(x, y: integer);
var renderQuad :TSDL_Rect;
begin
  renderQuad.x:=x;
  renderQuad.y:=y;
  renderQuad.w:=FWidht;
  renderQuad.h:=FHeight;
  SDL_RenderCopy(FRenderer,mTexture,nil,@renderQuad);
end;

end.
