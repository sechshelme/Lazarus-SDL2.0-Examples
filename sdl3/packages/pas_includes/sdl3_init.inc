{$DEFINE read_interface}

type
  PSDL_InitFlags = ^TSDL_InitFlags;
  TSDL_InitFlags =  Longint;
  Const
    SDL_INIT_TIMER = $00000001;
    SDL_INIT_AUDIO = $00000010;
    SDL_INIT_VIDEO = $00000020;
    SDL_INIT_JOYSTICK = $00000200;
    SDL_INIT_HAPTIC = $00001000;
    SDL_INIT_GAMEPAD = $00002000;
    SDL_INIT_EVENTS = $00004000;
    SDL_INIT_SENSOR = $00008000;
    SDL_INIT_CAMERA = $00010000;

function SDL_Init(flags:Uint32):longint;cdecl;external;
function SDL_InitSubSystem(flags:Uint32):longint;cdecl;external;
procedure SDL_QuitSubSystem(flags:Uint32);cdecl;external;
function SDL_WasInit(flags:Uint32):Uint32;cdecl;external;
procedure SDL_Quit;cdecl;external;

{$UNDEF read_interface}


{$DEFINE read_implementation}

{$UNDEF read_implementation}
