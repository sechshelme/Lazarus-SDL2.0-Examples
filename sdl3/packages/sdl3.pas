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

unit SDL3_stdinc;
unit SDL3_guid;
unit SDL3_scancode;
unit SDL3_mutex;
unit SDL3_touch;
unit SDL3_blendmode;
unit SDL3_init;
unit SDL3_assert;

unit SDL3_timer;       SDL3_stdinc;
unit SDL3_pixels;      SDL3_stdinc;
unit SDL3_properties;  SDL3_stdinc;
unit SDL3_rect;        SDL3_stdinc;
unit SDL3_rwops;       SDL3_stdinc;
unit SDL3_version;     SDL3_stdinc;

unit SDL3_sensor;      SDL3_properties;

unit SDL3_pen;         SDL3_stdinc, SDL_guid;
unit SDL3_audio;       SDL3_stdinc, SDL3_rwops;

unit SDL3_surface;     SDL3_pixels, SDL3_stdinc, SDL3_rect, SDL3_properties, SDL3_rwops, SDL3_blendmode;
unit SDL3_video;       SDL3_stdinc, SDL3_rect, SDL3_surface;
unit SDL3_camera;      SDL3_properties, SDL3_surface, SDL3_pixels;
unit SDL3_messagebox;  SDL3_video;

unit SDL3_keycode;     SDL_scancode;
unit SDL3_keyboard;    SDL3_stdinc,SDL3_rect,  SDL_scancode, SDL_keycode, SDL3_video;
unit SDL3_mouse;       SDL3_stdinc, SDL3_video, SDL3_surface;
unit SDL3_joystick;    SDL_guid, SDL3_stdinc, SDL_mutex;

unit SDL3_events;      SDL3_stdinc, SDL3_video, SDL3_keyboard, SDL3_mouse, SDL3_joystick, SDL3_audio, SDL3_camera, SDL3_touch, SDL3_pen, SDL3_sensor;
unit SDL3_quit;        SDL3_stdinc, SDL3_events;
unit SDL3_main;        SDL3_events;

*)