unit LTimer;

interface

uses
  sdl2;

type

  { TLTimer }

  TLTimer = class(TObject)
  public
    constructor Create;
    procedure start;
    procedure stop;
    procedure pause;
    procedure unpause;
    function getTicks: uint32;
    function isStarted: boolean;
    function isPausedd: boolean;
  private
    mStartTicks, mPausedTicks: uint32;
    mPaused, mStarted: boolean;
  end;


implementation

{ TLTimer }

constructor TLTimer.Create;
begin
  mStartTicks := 0;
  mPausedTicks := 0;
  mPaused := False;
  mStarted := False;
end;

procedure TLTimer.start;
begin
  mStarted := True;
  mPaused := False;
  mStartTicks := SDL_GetTicks;;
  mPausedTicks := 0;
end;

procedure TLTimer.stop;
begin
  mStarted := False;
  mPaused := False;
  mStartTicks := 0;
  mPausedTicks := 0;
end;

procedure TLTimer.pause;
begin
  if mStarted and not mPaused then begin
    mPaused := True;
    mPausedTicks := SDL_GetTicks - mStartTicks;
    mStartTicks := 0;
  end;
end;

procedure TLTimer.unpause;
begin
  if mStarted and mPaused then begin
    mPaused := False;
    mStartTicks := SDL_GetTicks - mPausedTicks;
    mPausedTicks := 0;
  end;
end;

function TLTimer.getTicks: uint32;
var
  time: uint32;
begin
  if mStarted then begin
    if mPaused then begin
      time := mPausedTicks;
    end else begin
      time := SDL_GetTicks - mStartTicks;
    end;
  end;
  Result := time;
end;

function TLTimer.isStarted: boolean;
begin
  Result := mStarted;
end;

function TLTimer.isPausedd: boolean;
begin
  Result := mStarted and mPaused;
end;

end.
