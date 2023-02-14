unit Particle;

interface

uses
  sdl2,
  sdl2_image, LTexture;

type

  { TParticle }

  TParticle = class(TObject)
  private
    mPosX, mPosY, mFrame: integer;
    mTexture, gRedTexture, gGreenTexture, gBlueTexture, gShimmerTexture: TLTexture;
  public
    constructor Create(ARenderer: PSDL_Renderer; x, y: integer);
    destructor Destroy; override;
    procedure render;
    function isDead: boolean;
  end;

implementation

{ TParticle }

constructor TParticle.Create(ARenderer: PSDL_Renderer; x, y: integer);
begin
  gRedTexture := TLTexture.Create(ARenderer);
  if not gRedTexture.LoadFromFile('red.bmp') then begin
    WriteLn('Failed to load red texture!');
  end;
  gRedTexture.SetAlpha(192);
  gGreenTexture := TLTexture.Create(ARenderer);
  if not gGreenTexture.LoadFromFile('green.bmp') then begin
    WriteLn('Failed to load green texture!');
  end;
  gGreenTexture.SetAlpha(192);
  gBlueTexture := TLTexture.Create(ARenderer);
  if not gBlueTexture.LoadFromFile('blue.bmp') then begin
    WriteLn('Failed to load blue texture!');
  end;
  gBlueTexture.SetAlpha(192);
  gShimmerTexture := TLTexture.Create(ARenderer);
  if not gShimmerTexture.LoadFromFile('shimmer.bmp') then begin
    WriteLn('Failed to load shimmer texture!');
  end;
  gShimmerTexture.SetAlpha(192);

  mPosX := x - 5 + Random(25);
  mPosY := y - 5 + Random(25);
  mFrame := Random(5);

  case Random(3) of
    0: begin
      mTexture := gRedTexture;
    end;
    1: begin
      mTexture := gGreenTexture;
    end;
    2: begin
      mTexture := gBlueTexture;
    end;
  end;
end;

destructor TParticle.Destroy;
begin
  gRedTexture.Free;
  gGreenTexture.Free;
  gBlueTexture.Free;
  gShimmerTexture.Free;
  inherited Destroy;
end;

procedure TParticle.render;
begin
  mTexture.Render(mPosX, mPosY);
  if mFrame mod 2 = 0 then begin
    gShimmerTexture.Render(mPosX, mPosY);
  end;
  Inc(mFrame);
end;

function TParticle.isDead: boolean;
begin
  Result := mFrame > 10;
end;

end.
