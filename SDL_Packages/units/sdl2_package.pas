{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SDL2_Package;

{$warn 5023 off : no warning about unused units}
interface

uses
  sdl2, sdl2_gfx, sdl2_image, sdl2_mixer, sdl2_net, sdl2_ttf, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SDL2_Package', @Register);
end.
