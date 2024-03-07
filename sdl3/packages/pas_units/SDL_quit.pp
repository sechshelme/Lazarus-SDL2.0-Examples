unit SDL_quit;

interface

uses
  SDL3_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


function SDL_QuitRequested:TSDL_bool;

{ There are no functions directly affecting the quit event  }
(* error 
#define SDL_QuitRequested() (SDL_PumpEvents(), (SDL_PeepEvents(NULL,0,SDL_PEEKEVENT,SDL_EVENT_QUIT,SDL_EVENT_QUIT) > 0))
in define line 55 *)

implementation

function SDL_QuitRequested: TSDL_bool;
begin
//  SDL_PumpEvents();
//  result :=SDL_PeepEvents(NULL,0,SDL_PEEKEVENT,SDL_EVENT_QUIT,SDL_EVENT_QUIT) > 0;
end;


end.
