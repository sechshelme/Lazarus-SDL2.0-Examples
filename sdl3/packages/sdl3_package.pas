{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SDL3_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  SDL3, SDL3_blendmode, SDL3_init, SDL3_messagebox, SDL3_pixels, 
  SDL3_properties, SDL3_rect, SDL3_rwops, SDL3_stdinc, SDL3_surface, 
  SDL3_timer, SDL3_version, SDL3_video, SDL3_quit, SDL3_events, SDL3_keyboard, 
  SDL3_keycode, SDL3_mouse, SDL3_joystick, SDL3_camera, SDL3_mutex, 
  SDL3_assert, SDL3_error, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SDL3_package', @Register);
end.
