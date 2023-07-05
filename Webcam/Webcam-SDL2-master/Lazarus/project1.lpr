program project1;

uses
  SDL2,
  ctypes,
  v4l2_driver;

  // https://github.com/chendotjs/Webcam-SDL2

const
  device = '/dev/video0';
  IMAGE_WIDTH=640;
  IMAGE_HEIGHT=480;


  type
    TFramehandler=procedure(pframe:Pointer;len:cint);

    TStreamHandle=record fd:cint;framehandler:TFramehandler; end;

var
  video_fildes: longint;
  sH:TStreamHandle;

var
  sdlTexture: PSDL_Texture;
  sdlRenderer: PSDL_Renderer;
  sdlRect: TSDL_Rect;

procedure frame_handler(pframe: Pointer; len: cint);
begin
     SDL_UpdateTexture(sdlTexture, @sdlRect,pframe,IMAGE_WIDTH*2 );
     SDL_RenderClear(sdlRenderer);
     SDL_RenderCopy(sdlRenderer,sdlTexture,nil,@sdlRect);
     SDL_RenderPresent(sdlRenderer);
end;

begin
  video_fildes := v4l2_open(device);
  if video_fildes = -1 then begin
    WriteLn('Kann Gerät "', device, '" nicht öffnen');
  end;
if   v4l2_querycap(video_fildes, nil)=-1 then WriteLn('Fehler: "v4l2_querycap"');
sH.fd:=video_fildes;
sH.framehandler:=@frame_handler;
end.
