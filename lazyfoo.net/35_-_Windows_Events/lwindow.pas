unit LWindow;

interface

uses
  sdl2,
  sdl2_image,
  sdl2_ttf,
  ctypes;

type

  { TLWindow }

  TLWindow = class(TObject)
  private
    FHeight: integer;
    FWidth: integer;
    mWindow: PSDL_Window;
    mMouseFocus, mKeybordFocus, mFullScreeen, mMinimized: boolean;
  public
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    constructor Create;
    destructor Destroy;
    function Init(AWidth, AHeight: integer): boolean;
    function createRenderer: PSDL_Renderer;
    procedure handleEvent(e: TSDL_Event; ARenderer: PSDL_Renderer);
    function hasMouseFocus: boolean;
    function hasKeybordFocus: boolean;
    function isMinimized: boolean;
  end;

implementation

{ TLWindow }

constructor TLWindow.Create;
begin
  mWindow := nil;
end;

destructor TLWindow.Destroy;
begin
  if mWindow <> nil then begin
    SDL_DestroyWindow(mWindow);
  end;
  mMouseFocus := False;
  mKeybordFocus := False;
  FWidth := 0;
  FHeight := 0;
end;

function TLWindow.Init(AWidth, AHeight: integer): boolean;
begin
  mWindow := SDL_CreateWindow('SDL Tutorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, AWidth, AHeight, SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE);
  if mWindow <> nil then begin
    mMouseFocus := True;
    mKeybordFocus := True;
    FWidth := AWidth;
    FHeight := AHeight;
  end;
  Result := mWindow <> nil;
end;

function TLWindow.createRenderer: PSDL_Renderer;
begin
  Result := SDL_CreateRenderer(mWindow, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
end;

procedure TLWindow.handleEvent(e: TSDL_Event; ARenderer: PSDL_Renderer);
var
  updateCaption: boolean;
  s: string;
begin
  case e.type_ of
    SDL_WINDOWEVENT: begin
      updateCaption := False;
      case e.window.event of
        SDL_WINDOWEVENT_SIZE_CHANGED: begin
          FWidth := e.window.data1;
          FHeight := e.window.data2;
          SDL_RenderPresent(ARenderer);
        end;
        SDL_WINDOWEVENT_EXPOSED: begin
          SDL_RenderPresent(ARenderer);
        end;
        SDL_WINDOWEVENT_ENTER: begin
          mMouseFocus := True;
          updateCaption := True;
        end;
        SDL_WINDOWEVENT_LEAVE: begin
          mMouseFocus := False;
          updateCaption := True;
        end;
        SDL_WINDOWEVENT_FOCUS_GAINED: begin
          mKeybordFocus := True;
          updateCaption := True;
        end;
        SDL_WINDOWEVENT_FOCUS_LOST: begin
          mKeybordFocus := False;
          updateCaption := True;
        end;
        SDL_WINDOWEVENT_MINIMIZED: begin
          mMinimized := True;
        end;
        SDL_WINDOWEVENT_MAXIMIZED: begin
          mMinimized := False;
        end;
        SDL_WINDOWEVENT_RESTORED: begin
          mMinimized := False;
        end;
      end;
      if updateCaption then begin
        WriteStr(s, 'SDL Tutorial - MouseFocus:', mMouseFocus, 'KeyboardFocus:', mKeybordFocus);
        SDL_SetWindowTitle(mWindow, PChar(s));
      end;
    end;
    SDL_KEYDOWN: begin
      case e.key.keysym.sym of
        SDLK_RETURN: begin
          if mFullScreeen then  begin
            SDL_SetWindowFullscreen(mWindow, SDL_FALSE);
            mFullScreeen := False;
          end else begin
            SDL_SetWindowFullscreen(mWindow, SDL_TRUE);
            mFullScreeen := True;
            mMinimized := False;
          end;
        end;
      end;
    end;
  end;
end;

function TLWindow.hasMouseFocus: boolean;
begin
  Result := mMouseFocus;
end;

function TLWindow.hasKeybordFocus: boolean;
begin
  Result := mKeybordFocus;
end;

function TLWindow.isMinimized: boolean;
begin
  Result := mMinimized;
end;

end.
