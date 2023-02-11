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
    FKeyboardFocus: Boolean;
    FMouseFocus: Boolean;
    FShown: Boolean;
    FWidth: integer;
    FRenderer: PSDL_Renderer;
    mWindow: PSDL_Window;
    mWindowID: integer;
    mFullScreeen, FMinimized: boolean;
  public
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Renderer: PSDL_Renderer read FRenderer;
    property MousesFocus:Boolean read FMouseFocus;
    property KeyboardFocus:Boolean read FKeyboardFocus;
    property Minimized:Boolean read FMinimized;
    property Shown:Boolean read FShown;
    constructor Create(AWidth, AHeight: integer);
    destructor Destroy; override;
    procedure handleEvent(e: TSDL_Event);
    procedure focus;
  end;

implementation

{ TLWindow }

constructor TLWindow.Create(AWidth, AHeight: integer);
begin
  mWindow := nil;
  FMouseFocus := False;
  FKeyboardFocus := False;
  mFullScreeen := False;
  FShown := False;
  mWindowID := -1;

  mWindow := SDL_CreateWindow('SDL Tutorial', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, AWidth, AHeight, SDL_WINDOW_SHOWN or SDL_WINDOW_RESIZABLE);
  if mWindow <> nil then begin
    FMouseFocus := True;
    FKeyboardFocus := True;
    FWidth := AWidth;
    FHeight := AHeight;

    FRenderer := SDL_CreateRenderer(mWindow, -1, SDL_RENDERER_ACCELERATED or SDL_RENDERER_PRESENTVSYNC);
    if FRenderer = nil then begin
      WriteLn('Renderer could not be created! SDL Error: ', SDL_GetError);
      SDL_DestroyWindow(mWindow);
      mWindow := nil;
    end else begin
      SDL_SetRenderDrawColor(FRenderer, $FF, $FF, $FF, $FF);
      mWindowID := SDL_GetWindowID(mWindow);
      FShown := True;
    end;

  end else begin
    WriteLn('Window could not be created! SDL Error: ', SDL_GetError);
  end;
 //Result := (mWindow <> nil) and (mRenderer <> nil);
end;

destructor TLWindow.Destroy;
begin
  if mWindow <> nil then begin
    SDL_DestroyWindow(mWindow);
  end;
  FMouseFocus := False;
  FKeyboardFocus := False;
  FWidth := 0;
  FHeight := 0;
end;

procedure TLWindow.handleEvent(e: TSDL_Event);
var
  updateCaption: boolean;
  s: string;
begin
  case e.type_ of
    SDL_WINDOWEVENT: begin
      if e.window.windowID = mWindowID then begin
        updateCaption := False;
        case e.window.event of
          SDL_WINDOWEVENT_SHOWN: begin
            FShown := True;
          end;
          SDL_WINDOWEVENT_HIDDEN: begin
            FShown := False;
          end;
          SDL_WINDOWEVENT_SIZE_CHANGED: begin
            FWidth := e.window.data1;
            FHeight := e.window.data2;
            SDL_RenderPresent(FRenderer);
          end;
          SDL_WINDOWEVENT_EXPOSED: begin
            SDL_RenderPresent(FRenderer);
          end;
          SDL_WINDOWEVENT_ENTER: begin
            FMouseFocus := True;
            updateCaption := True;
          end;
          SDL_WINDOWEVENT_LEAVE: begin
            FMouseFocus := False;
            updateCaption := True;
          end;
          SDL_WINDOWEVENT_FOCUS_GAINED: begin
            FKeyboardFocus := True;
            updateCaption := True;
          end;
          SDL_WINDOWEVENT_FOCUS_LOST: begin
            FKeyboardFocus := False;
            updateCaption := True;
          end;
          SDL_WINDOWEVENT_MINIMIZED: begin
            FMinimized := True;
          end;
          SDL_WINDOWEVENT_MAXIMIZED: begin
            FMinimized := False;
          end;
          SDL_WINDOWEVENT_RESTORED: begin
            FMinimized := False;
          end;
          SDL_WINDOWEVENT_CLOSE: begin
            SDL_HideWindow(mWindow);
          end;
        end;
        if updateCaption then begin
          WriteStr(s, 'SDL Tutorial - ID: ', mWindowID, ' - MouseFocus:', FMouseFocus, ' - KeyboardFocus:', FKeyboardFocus);
          SDL_SetWindowTitle(mWindow, PChar(s));
        end;
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
            FMinimized := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TLWindow.focus;
begin
  if not FShown then begin
    SDL_ShowWindow(mWindow);
  end;
  SDL_RaiseWindow(mWindow);
end;

end.
