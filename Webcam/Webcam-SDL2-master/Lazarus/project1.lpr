program project1;

uses
  v4l2_driver;

  // https://github.com/chendotjs/Webcam-SDL2

const
  device = '/dev/video0';
var
  video_files: longint;
begin
  video_files := v4l2_open(device);
  if video_files = -1 then begin
    WriteLn('Kann Gerät "', device, '" nicht öffnen');
  end;
end.
