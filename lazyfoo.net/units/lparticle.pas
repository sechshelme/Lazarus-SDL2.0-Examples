unit LParticle;

interface

uses
  sdl2,
  LTexture;

const
  BMPPath = '../units/';

type

  { TLParticle }

  TLParticle = class(TObject)
  private
    mPos: TSDL_Point;
    mFrame: integer;
    mTexture, gRedTexture, gGreenTexture, gBlueTexture, gShimmerTexture: TLTexture;
  public
    constructor Create(ARenderer: PSDL_Renderer; x, y: integer);
    destructor Destroy; override;
    procedure reset(x, y: integer);
    procedure render;
    function isDead: boolean;
  end;

implementation

const
  Part_Step = 50;

{ TLParticle }

constructor TLParticle.Create(ARenderer: PSDL_Renderer; x, y: integer);
begin
  gRedTexture := TLTexture.Create(ARenderer);
  if not gRedTexture.LoadFromFile(BMPPath + 'red.bmp') then begin
    WriteLn('Failed to load red texture!');
  end;
  gRedTexture.SetAlpha(192);
  gGreenTexture := TLTexture.Create(ARenderer);
  if not gGreenTexture.LoadFromFile(BMPPath + 'green.bmp') then begin
    WriteLn('Failed to load green texture!');
  end;
  gGreenTexture.SetAlpha(192);
  gBlueTexture := TLTexture.Create(ARenderer);
  if not gBlueTexture.LoadFromFile(BMPPath + 'blue.bmp') then begin
    WriteLn('Failed to load blue texture!');
  end;
  gBlueTexture.SetAlpha(192);
  gShimmerTexture := TLTexture.Create(ARenderer);
  if not gShimmerTexture.LoadFromFile(BMPPath + 'shimmer.bmp') then begin
    WriteLn('Failed to load shimmer texture!');
  end;
  gShimmerTexture.SetAlpha(192);

  reset(x, y);
end;

destructor TLParticle.Destroy;
begin
  gRedTexture.Free;
  gGreenTexture.Free;
  gBlueTexture.Free;
  gShimmerTexture.Free;
  inherited Destroy;
end;

procedure TLParticle.reset(x, y: integer);
begin
  mPos.X := x - 5 + Random(25);
  mPos.Y := y - 5 + Random(25);
  mFrame := Random(Part_Step);

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

procedure TLParticle.render;
begin
  mTexture.Render(mPos.X, mPos.Y);
  if mFrame mod 2 = 0 then begin
    gShimmerTexture.Render(mPos.X, mPos.Y);
  end;
  Inc(mFrame);
end;

function TLParticle.isDead: boolean;
begin
  Result := mFrame > Part_Step shl 1;
end;

end.
