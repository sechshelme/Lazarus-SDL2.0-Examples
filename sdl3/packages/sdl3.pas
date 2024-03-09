unit SDL3;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$DEFINE read_interface}
//{$include SDL3_includes.inc}
{$UNDEF read_interface}



implementation

{$DEFINE read_implementation}
//{$include SDL3_includes.inc}
{$UNDEF read_implementation}

end.

(*
Ohne Abhängigkeit:

unit SDL_guid;
unit SDL_scancode;
unit SDL_mutex;
unit SDL_touch;

unit SDL3_properties; SDL3_stdinc;

unit SDL_sensor;      SDL3_properties;


unit SDL_pen;         SDL3_stdinc, SDL_guid;
unit SDL_audio;       SDL3_stdinc, SDL3_rwops;
unit SDL_camera;      SDL3_properties, SDL3_surface;


unit SDL_keycode;     SDL_scancode;
unit SDL_keyboard;    SDL3_stdinc,SDL3_rect,  SDL_scancode, SDL_keycode, SDL3_video;
unit SDL_mouse;       SDL3_stdinc, SDL3_video, SDL3_surface;
unit SDL_joystick;    SDL_guid, SDL3_stdinc, , SDL_mutex;


*)
