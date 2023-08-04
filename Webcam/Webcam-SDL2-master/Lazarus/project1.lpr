program project1;

uses
  SDL2,
  ctypes,
  v4l2_driver;

  // https://github.com/chendotjs/Webcam-SDL2

const
  device = '/dev/video0';
  IMAGE_WIDTH = 640;
  IMAGE_HEIGHT = 480;


type
  TFramehandler = procedure(pframe: Pointer; len: cint);

  TStreamHandle = record
    fd: cint;
    framehandler: TFramehandler;
  end;

var
  video_fildes: longint;
  sH: TStreamHandle;

var
  sdlTexture: PSDL_Texture;
  sdlRenderer: PSDL_Renderer;
  sdlRect: TSDL_Rect;
  sdlScreen: PSDL_Window;
  quit:Boolean=False;

  procedure frame_handler(pframe: Pointer; len: cint);
  begin
    SDL_UpdateTexture(sdlTexture, @sdlRect, pframe, IMAGE_WIDTH * 2);
    SDL_RenderClear(sdlRenderer);
    SDL_RenderCopy(sdlRenderer, sdlTexture, nil, @sdlRect);
    SDL_RenderPresent(sdlRenderer);
  end;

begin
  video_fildes := v4l2_open(device);
  if video_fildes = -1 then begin
    WriteLn('Kann Gerät "', device, '" nicht öffnen');
    Halt(1);
  end;

  if v4l2_querycap(video_fildes, nil) = -1 then begin
    WriteLn('Fehler: "v4l2_querycap"');
    Halt(1);
  end;

  if v4l2_sfmt(video_fildes, 1448695129) = -1 then begin     // ??????
    WriteLn('v4l2_sfmt');
    Halt(1);
  end;

  if v4l2_gfmt(video_fildes) = -1 then begin
    WriteLn('v4l2_gfmt');
    Halt(1);
  end;

  if v4l2_sfmt(video_fildes,30) = -1 then begin
    WriteLn('v4l2_sfmt()');
    Halt(1);
  end;

  if v4l2_mmap(video_fildes) = -1 then begin
    WriteLn('v4l2_mmap()');
    Halt(1);
  end;

  if v4l2_streamon(video_fildes) = -1 then begin
    WriteLn('v4l2_streamon()');
    Halt(1);
  end;


  sH.fd := video_fildes;
  sH.framehandler := @frame_handler;


  sdlScreen:=SDL_CreateWindow('webcam', SDL_WINDOWPOS_UNDEFINED,SDL_WINDOWPOS_UNDEFINED,640,480,SDL_WINDOW_SHOWN);
  if sdlScreen=nil then WriteLn('Kann SDL nicht öffnen');

  repeat until quit;








  WriteLn('io.');
end.
