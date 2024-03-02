{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SDL3;

{$warn 5023 off : no warning about unused units}
interface

uses
  SDL_Video, SDL_stdinc, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SDL3', @Register);
end.
