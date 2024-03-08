unit SDL_joystick;

interface

uses
  SDL_guid, SDL3_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_joystick_h_}
//{$define SDL_joystick_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_guid.h>}
//{$include <SDL3/SDL_mutex.h>}
//{$include <SDL3/SDL_properties.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 *  \file SDL_joystick.h
 *
 *  In order to use these functions, SDL_Init() must have been called
 *  with the ::SDL_INIT_JOYSTICK flag.  This causes SDL to scan the system
 *  for joysticks, and load appropriate drivers.
 *
 *  If you would like to receive joystick updates while the application
 *  is in the background, you should set the following hint before calling
 *  SDL_Init(): SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
  }
{*
 * The joystick structure used to identify an SDL joystick
  }
{$ifdef SDL_THREAD_SAFETY_ANALYSIS}
  var
    SDL_joystick_lock : PSDL_Mutex;cvar;external;
{$endif}
type
  PSDL_Joystick = ^TSDL_Joystick;
  TSDL_Joystick = record
      {undefined structure}
    end;

{ A structure that encodes the stable unique id for a joystick device  }

  PSDL_JoystickGUID = ^TSDL_JoystickGUID;
  TSDL_JoystickGUID = TSDL_GUID;
{*
 * This is a unique ID for a joystick for the time it is connected to the system,
 * and is never reused for the lifetime of the application. If the joystick is
 * disconnected and reconnected, it will get a new ID.
 *
 * The ID value starts at 1 and increments from there. The value 0 is an invalid ID.
  }

  PSDL_JoystickID = ^TSDL_JoystickID;
  TSDL_JoystickID = Uint32;

  PSDL_JoystickType = ^TSDL_JoystickType;
  TSDL_JoystickType =  Longint;
  Const
    SDL_JOYSTICK_TYPE_UNKNOWN = 0;
    SDL_JOYSTICK_TYPE_GAMEPAD = 1;
    SDL_JOYSTICK_TYPE_WHEEL = 2;
    SDL_JOYSTICK_TYPE_ARCADE_STICK = 3;
    SDL_JOYSTICK_TYPE_FLIGHT_STICK = 4;
    SDL_JOYSTICK_TYPE_DANCE_PAD = 5;
    SDL_JOYSTICK_TYPE_GUITAR = 6;
    SDL_JOYSTICK_TYPE_DRUM_KIT = 7;
    SDL_JOYSTICK_TYPE_ARCADE_PAD = 8;
    SDL_JOYSTICK_TYPE_THROTTLE = 9;

{ <= 5%  }
{ <= 20%  }
{ <= 70%  }
{ <= 100%  }
type
  PSDL_JoystickPowerLevel = ^TSDL_JoystickPowerLevel;
  TSDL_JoystickPowerLevel =  Longint;
  Const
    SDL_JOYSTICK_POWER_UNKNOWN = -(1);
    SDL_JOYSTICK_POWER_EMPTY = (-(1))+1;
    SDL_JOYSTICK_POWER_LOW = (-(1))+2;
    SDL_JOYSTICK_POWER_MEDIUM = (-(1))+3;
    SDL_JOYSTICK_POWER_FULL = (-(1))+4;
    SDL_JOYSTICK_POWER_WIRED = (-(1))+5;
    SDL_JOYSTICK_POWER_MAX = (-(1))+6;

  SDL_JOYSTICK_AXIS_MAX = 32767;  
  SDL_JOYSTICK_AXIS_MIN = -(32768);  
{ Set max recognized G-force from accelerometer
   See src/joystick/uikit/SDL_sysjoystick.m for notes on why this is needed
  }
  SDL_IPHONE_MAX_GFORCE = 5.0;  
{ Function prototypes  }
{*
 * Locking for atomic access to the joystick API
 *
 * The SDL joystick functions are thread-safe, however you can lock the
 * joysticks while processing to guarantee that the joystick list won't change
 * and joystick and gamepad events will not be delivered.
 *
 * \since This function is available since SDL 3.0.0.
  }

procedure SDL_LockJoysticks;cdecl;external;
{SDL_ACQUIRE(SDL_joystick_lock); }
{*
 * Unlocking for atomic access to the joystick API
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_UnlockJoysticks;cdecl;external;
{/ SDL_RELEASE(SDL_joystick_lock); }
{*
 * Get a list of currently connected joysticks.
 *
 * \param count a pointer filled in with the number of joysticks returned
 * \returns a 0 terminated array of joystick instance IDs which should be
 *          freed with SDL_free(), or NULL on error; call SDL_GetError() for
 *          more details.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_OpenJoystick
  }
function SDL_GetJoysticks(count:Plongint):PSDL_JoystickID;cdecl;external;
{*
 * Get the implementation dependent name of a joystick.
 *
 * This can be called before any joysticks are opened.
 *
 * \param instance_id the joystick instance ID
 * \returns the name of the selected joystick. If no name can be found, this
 *          function returns NULL; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickName
 * \sa SDL_OpenJoystick
  }
(* Const before type ignored *)
function SDL_GetJoystickInstanceName(instance_id:TSDL_JoystickID):Pchar;cdecl;external;
{*
 * Get the implementation dependent path of a joystick.
 *
 * This can be called before any joysticks are opened.
 *
 * \param instance_id the joystick instance ID
 * \returns the path of the selected joystick. If no path can be found, this
 *          function returns NULL; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickPath
 * \sa SDL_OpenJoystick
  }
(* Const before type ignored *)
function SDL_GetJoystickInstancePath(instance_id:TSDL_JoystickID):Pchar;cdecl;external;
{*
 * Get the player index of a joystick.
 *
 * This can be called before any joysticks are opened.
 *
 * \param instance_id the joystick instance ID
 * \returns the player index of a joystick, or -1 if it's not available
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickPlayerIndex
 * \sa SDL_OpenJoystick
  }
function SDL_GetJoystickInstancePlayerIndex(instance_id:TSDL_JoystickID):longint;cdecl;external;
{*
 * Get the implementation-dependent GUID of a joystick.
 *
 * This can be called before any joysticks are opened.
 *
 * \param instance_id the joystick instance ID
 * \returns the GUID of the selected joystick. If called on an invalid index,
 *          this function returns a zero GUID
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickGUID
 * \sa SDL_GetJoystickGUIDString
  }
function SDL_GetJoystickInstanceGUID(instance_id:TSDL_JoystickID):TSDL_JoystickGUID;cdecl;external;
{*
 * Get the USB vendor ID of a joystick, if available.
 *
 * This can be called before any joysticks are opened. If the vendor ID isn't
 * available this function returns 0.
 *
 * \param instance_id the joystick instance ID
 * \returns the USB vendor ID of the selected joystick. If called on an
 *          invalid index, this function returns zero
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickInstanceVendor(instance_id:TSDL_JoystickID):Uint16;cdecl;external;
{*
 * Get the USB product ID of a joystick, if available.
 *
 * This can be called before any joysticks are opened. If the product ID isn't
 * available this function returns 0.
 *
 * \param instance_id the joystick instance ID
 * \returns the USB product ID of the selected joystick. If called on an
 *          invalid index, this function returns zero
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickInstanceProduct(instance_id:TSDL_JoystickID):Uint16;cdecl;external;
{*
 * Get the product version of a joystick, if available.
 *
 * This can be called before any joysticks are opened. If the product version
 * isn't available this function returns 0.
 *
 * \param instance_id the joystick instance ID
 * \returns the product version of the selected joystick. If called on an
 *          invalid index, this function returns zero
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickInstanceProductVersion(instance_id:TSDL_JoystickID):Uint16;cdecl;external;
{*
 * Get the type of a joystick, if available.
 *
 * This can be called before any joysticks are opened.
 *
 * \param instance_id the joystick instance ID
 * \returns the SDL_JoystickType of the selected joystick. If called on an
 *          invalid index, this function returns `SDL_JOYSTICK_TYPE_UNKNOWN`
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickInstanceType(instance_id:TSDL_JoystickID):TSDL_JoystickType;cdecl;external;
{*
 * Open a joystick for use.
 *
 * The joystick subsystem must be initialized before a joystick can be opened
 * for use.
 *
 * \param instance_id the joystick instance ID
 * \returns a joystick identifier or NULL if an error occurred; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseJoystick
  }
function SDL_OpenJoystick(instance_id:TSDL_JoystickID):PSDL_Joystick;cdecl;external;
{*
 * Get the SDL_Joystick associated with an instance ID, if it has been opened.
 *
 * \param instance_id the instance ID to get the SDL_Joystick for
 * \returns an SDL_Joystick on success or NULL on failure or if it hasn't been
 *          opened yet; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickFromInstanceID(instance_id:TSDL_JoystickID):PSDL_Joystick;cdecl;external;
{*
 * Get the SDL_Joystick associated with a player index.
 *
 * \param player_index the player index to get the SDL_Joystick for
 * \returns an SDL_Joystick on success or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickFromPlayerIndex(player_index:longint):PSDL_Joystick;cdecl;external;
{*
 * Attach a new virtual joystick.
 *
 * \param type type of joystick
 * \param naxes number of axes
 * \param nbuttons number of buttons
 * \param nhats number of hats
 * \returns the joystick instance ID, or 0 if an error occurred; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_AttachVirtualJoystick(_type:TSDL_JoystickType; naxes:longint; nbuttons:longint; nhats:longint):TSDL_JoystickID;cdecl;external;
{*
 * The structure that defines an extended virtual joystick description
 *
 * The caller must zero the structure and then initialize the version with `SDL_VIRTUAL_JOYSTICK_DESC_VERSION` before passing it to SDL_AttachVirtualJoystickEx()
 *  All other elements of this structure are optional and can be left 0.
 *
 * \sa SDL_AttachVirtualJoystickEx
  }
{*< `SDL_VIRTUAL_JOYSTICK_DESC_VERSION`  }
{*< `SDL_JoystickType`  }
{*< the number of axes on this joystick  }
{*< the number of buttons on this joystick  }
{*< the number of hats on this joystick  }
{*< the USB vendor ID of this joystick  }
{*< the USB product ID of this joystick  }
{*< unused  }
{*< A mask of which buttons are valid for this controller
                             e.g. (1 << SDL_GAMEPAD_BUTTON_SOUTH)  }
{*< A mask of which axes are valid for this controller
                             e.g. (1 << SDL_GAMEPAD_AXIS_LEFTX)  }
(* Const before type ignored *)
{*< the name of the joystick  }
{*< User data pointer passed to callbacks  }
{*< Called when the joystick state should be updated  }
{*< Called when the player index is set  }
{*< Implements SDL_RumbleJoystick()  }
{*< Implements SDL_RumbleJoystickTriggers()  }
{*< Implements SDL_SetJoystickLED()  }
(* Const before type ignored *)
{*< Implements SDL_SendJoystickEffect()  }
type
  PSDL_VirtualJoystickDesc = ^TSDL_VirtualJoystickDesc;
  TSDL_VirtualJoystickDesc = record
      version : Uint16;
      _type : Uint16;
      naxes : Uint16;
      nbuttons : Uint16;
      nhats : Uint16;
      vendor_id : Uint16;
      product_id : Uint16;
      padding : Uint16;
      button_mask : Uint32;
      axis_mask : Uint32;
      name : Pchar;
      userdata : pointer;
      Update : procedure (userdata:pointer);cdecl;
      SetPlayerIndex : procedure (userdata:pointer; player_index:longint);cdecl;
      Rumble : function (userdata:pointer; low_frequency_rumble:Uint16; high_frequency_rumble:Uint16):longint;cdecl;
      RumbleTriggers : function (userdata:pointer; left_rumble:Uint16; right_rumble:Uint16):longint;cdecl;
      SetLED : function (userdata:pointer; red:Uint8; green:Uint8; blue:Uint8):longint;cdecl;
      SendEffect : function (userdata:pointer; data:pointer; size:longint):longint;cdecl;
    end;
{*
 * The current version of the SDL_VirtualJoystickDesc structure
  }

const
  SDL_VIRTUAL_JOYSTICK_DESC_VERSION = 1;  
{*
 * Attach a new virtual joystick with extended properties.
 *
 * \param desc Joystick description
 * \returns the joystick instance ID, or 0 if an error occurred; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)

function SDL_AttachVirtualJoystickEx(desc:PSDL_VirtualJoystickDesc):TSDL_JoystickID;cdecl;external;
{*
 * Detach a virtual joystick.
 *
 * \param instance_id the joystick instance ID, previously returned from
 *                    SDL_AttachVirtualJoystick()
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_DetachVirtualJoystick(instance_id:TSDL_JoystickID):longint;cdecl;external;
{*
 * Query whether or not a joystick is virtual.
 *
 * \param instance_id the joystick instance ID
 * \returns SDL_TRUE if the joystick is virtual, SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_IsJoystickVirtual(instance_id:TSDL_JoystickID):TSDL_bool;cdecl;external;
{*
 * Set values on an opened, virtual-joystick's axis.
 *
 * Please note that values set here will not be applied until the next call to
 * SDL_UpdateJoysticks, which can either be called directly, or can be called
 * indirectly through various other SDL APIs, including, but not limited to
 * the following: SDL_PollEvent, SDL_PumpEvents, SDL_WaitEventTimeout,
 * SDL_WaitEvent.
 *
 * Note that when sending trigger axes, you should scale the value to the full
 * range of Sint16. For example, a trigger at rest would have the value of
 * `SDL_JOYSTICK_AXIS_MIN`.
 *
 * \param joystick the virtual joystick on which to set state.
 * \param axis the specific axis on the virtual joystick to set.
 * \param value the new value for the specified axis.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetJoystickVirtualAxis(joystick:PSDL_Joystick; axis:longint; value:TSint16):longint;cdecl;external;
{*
 * Set values on an opened, virtual-joystick's button.
 *
 * Please note that values set here will not be applied until the next call to
 * SDL_UpdateJoysticks, which can either be called directly, or can be called
 * indirectly through various other SDL APIs, including, but not limited to
 * the following: SDL_PollEvent, SDL_PumpEvents, SDL_WaitEventTimeout,
 * SDL_WaitEvent.
 *
 * \param joystick the virtual joystick on which to set state.
 * \param button the specific button on the virtual joystick to set.
 * \param value the new value for the specified button.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetJoystickVirtualButton(joystick:PSDL_Joystick; button:longint; value:TUint8):longint;cdecl;external;
{*
 * Set values on an opened, virtual-joystick's hat.
 *
 * Please note that values set here will not be applied until the next call to
 * SDL_UpdateJoysticks, which can either be called directly, or can be called
 * indirectly through various other SDL APIs, including, but not limited to
 * the following: SDL_PollEvent, SDL_PumpEvents, SDL_WaitEventTimeout,
 * SDL_WaitEvent.
 *
 * \param joystick the virtual joystick on which to set state.
 * \param hat the specific hat on the virtual joystick to set.
 * \param value the new value for the specified hat.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetJoystickVirtualHat(joystick:PSDL_Joystick; hat:longint; value:TUint8):longint;cdecl;external;
{*
 * Get the properties associated with a joystick.
 *
 * The following read-only properties are provided by SDL:
 *
 * - `SDL_PROP_JOYSTICK_CAP_MONO_LED_BOOLEAN`: true if this joystick has an
 *   LED that has adjustable brightness
 * - `SDL_PROP_JOYSTICK_CAP_RGB_LED_BOOLEAN`: true if this joystick has an LED
 *   that has adjustable color
 * - `SDL_PROP_JOYSTICK_CAP_PLAYER_LED_BOOLEAN`: true if this joystick has a
 *   player LED
 * - `SDL_PROP_JOYSTICK_CAP_RUMBLE_BOOLEAN`: true if this joystick has
 *   left/right rumble
 * - `SDL_PROP_JOYSTICK_CAP_TRIGGER_RUMBLE_BOOLEAN`: true if this joystick has
 *   simple trigger rumble
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetProperty
 * \sa SDL_SetProperty
  }
function SDL_GetJoystickProperties(joystick:PSDL_Joystick):TSDL_PropertiesID;cdecl;external;
const
  SDL_PROP_JOYSTICK_CAP_MONO_LED_BOOLEAN = 'SDL.joystick.cap.mono_led';  
  SDL_PROP_JOYSTICK_CAP_RGB_LED_BOOLEAN = 'SDL.joystick.cap.rgb_led';  
  SDL_PROP_JOYSTICK_CAP_PLAYER_LED_BOOLEAN = 'SDL.joystick.cap.player_led';  
  SDL_PROP_JOYSTICK_CAP_RUMBLE_BOOLEAN = 'SDL.joystick.cap.rumble';  
  SDL_PROP_JOYSTICK_CAP_TRIGGER_RUMBLE_BOOLEAN = 'SDL.joystick.cap.trigger_rumble';  
{*
 * Get the implementation dependent name of a joystick.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the name of the selected joystick. If no name can be found, this
 *          function returns NULL; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickInstanceName
 * \sa SDL_OpenJoystick
  }
(* Const before type ignored *)

function SDL_GetJoystickName(joystick:PSDL_Joystick):Pchar;cdecl;external;
{*
 * Get the implementation dependent path of a joystick.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the path of the selected joystick. If no path can be found, this
 *          function returns NULL; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickInstancePath
  }
(* Const before type ignored *)
function SDL_GetJoystickPath(joystick:PSDL_Joystick):Pchar;cdecl;external;
{*
 * Get the player index of an opened joystick.
 *
 * For XInput controllers this returns the XInput user index. Many joysticks
 * will not be able to supply this information.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the player index, or -1 if it's not available.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickPlayerIndex(joystick:PSDL_Joystick):longint;cdecl;external;
{*
 * Set the player index of an opened joystick.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \param player_index Player index to assign to this joystick, or -1 to clear
 *                     the player index and turn off player LEDs.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetJoystickPlayerIndex(joystick:PSDL_Joystick; player_index:longint):longint;cdecl;external;
{*
 * Get the implementation-dependent GUID for the joystick.
 *
 * This function requires an open joystick.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the GUID of the given joystick. If called on an invalid index,
 *          this function returns a zero GUID; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickInstanceGUID
 * \sa SDL_GetJoystickGUIDString
  }
function SDL_GetJoystickGUID(joystick:PSDL_Joystick):TSDL_JoystickGUID;cdecl;external;
{*
 * Get the USB vendor ID of an opened joystick, if available.
 *
 * If the vendor ID isn't available this function returns 0.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the USB vendor ID of the selected joystick, or 0 if unavailable.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickVendor(joystick:PSDL_Joystick):TUint16;cdecl;external;
{*
 * Get the USB product ID of an opened joystick, if available.
 *
 * If the product ID isn't available this function returns 0.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the USB product ID of the selected joystick, or 0 if unavailable.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickProduct(joystick:PSDL_Joystick):TUint16;cdecl;external;
{*
 * Get the product version of an opened joystick, if available.
 *
 * If the product version isn't available this function returns 0.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the product version of the selected joystick, or 0 if unavailable.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickProductVersion(joystick:PSDL_Joystick):TUint16;cdecl;external;
{*
 * Get the firmware version of an opened joystick, if available.
 *
 * If the firmware version isn't available this function returns 0.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the firmware version of the selected joystick, or 0 if
 *          unavailable.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickFirmwareVersion(joystick:PSDL_Joystick):TUint16;cdecl;external;
{*
 * Get the serial number of an opened joystick, if available.
 *
 * Returns the serial number of the joystick, or NULL if it is not available.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the serial number of the selected joystick, or NULL if
 *          unavailable.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
function SDL_GetJoystickSerial(joystick:PSDL_Joystick):Pchar;cdecl;external;
{*
 * Get the type of an opened joystick.
 *
 * \param joystick the SDL_Joystick obtained from SDL_OpenJoystick()
 * \returns the SDL_JoystickType of the selected joystick.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickType(joystick:PSDL_Joystick):TSDL_JoystickType;cdecl;external;
{*
 * Get an ASCII string representation for a given SDL_JoystickGUID.
 *
 * You should supply at least 33 bytes for pszGUID.
 *
 * \param guid the SDL_JoystickGUID you wish to convert to string
 * \param pszGUID buffer in which to write the ASCII string
 * \param cbGUID the size of pszGUID
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickInstanceGUID
 * \sa SDL_GetJoystickGUID
 * \sa SDL_GetJoystickGUIDFromString
  }
function SDL_GetJoystickGUIDString(guid:TSDL_JoystickGUID; pszGUID:Pchar; cbGUID:longint):longint;cdecl;external;
{*
 * Convert a GUID string into a SDL_JoystickGUID structure.
 *
 * Performs no error checking. If this function is given a string containing
 * an invalid GUID, the function will silently succeed, but the GUID generated
 * will not be useful.
 *
 * \param pchGUID string containing an ASCII representation of a GUID
 * \returns a SDL_JoystickGUID structure.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickGUIDString
  }
(* Const before type ignored *)
function SDL_GetJoystickGUIDFromString(pchGUID:Pchar):TSDL_JoystickGUID;cdecl;external;
{*
 * Get the device information encoded in a SDL_JoystickGUID structure
 *
 * \param guid the SDL_JoystickGUID you wish to get info about
 * \param vendor A pointer filled in with the device VID, or 0 if not
 *               available
 * \param product A pointer filled in with the device PID, or 0 if not
 *                available
 * \param version A pointer filled in with the device version, or 0 if not
 *                available
 * \param crc16 A pointer filled in with a CRC used to distinguish different
 *              products with the same VID/PID, or 0 if not available
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickInstanceGUID
  }
procedure SDL_GetJoystickGUIDInfo(guid:TSDL_JoystickGUID; vendor:PUint16; product:PUint16; version:PUint16; crc16:PUint16);cdecl;external;
{*
 * Get the status of a specified joystick.
 *
 * \param joystick the joystick to query
 * \returns SDL_TRUE if the joystick has been opened, SDL_FALSE if it has not;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CloseJoystick
 * \sa SDL_OpenJoystick
  }
function SDL_JoystickConnected(joystick:PSDL_Joystick):TSDL_bool;cdecl;external;
{*
 * Get the instance ID of an opened joystick.
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \returns the instance ID of the specified joystick on success or 0 on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_OpenJoystick
  }
function SDL_GetJoystickInstanceID(joystick:PSDL_Joystick):TSDL_JoystickID;cdecl;external;
{*
 * Get the number of general axis controls on a joystick.
 *
 * Often, the directional pad on a game controller will either look like 4
 * separate buttons or a POV hat, and not axes, but all of this is up to the
 * device and platform.
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \returns the number of axis controls/number of axes on success or a
 *          negative error code on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickAxis
 * \sa SDL_OpenJoystick
  }
function SDL_GetNumJoystickAxes(joystick:PSDL_Joystick):longint;cdecl;external;
{*
 * Get the number of POV hats on a joystick.
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \returns the number of POV hats on success or a negative error code on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickHat
 * \sa SDL_OpenJoystick
  }
function SDL_GetNumJoystickHats(joystick:PSDL_Joystick):longint;cdecl;external;
{*
 * Get the number of buttons on a joystick.
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \returns the number of buttons on success or a negative error code on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetJoystickButton
 * \sa SDL_OpenJoystick
  }
function SDL_GetNumJoystickButtons(joystick:PSDL_Joystick):longint;cdecl;external;
{*
 * Set the state of joystick event processing.
 *
 * If joystick events are disabled, you must call SDL_UpdateJoysticks()
 * yourself and check the state of the joystick when you want joystick
 * information.
 *
 * \param enabled whether to process joystick events or not
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_JoystickEventsEnabled
  }
procedure SDL_SetJoystickEventsEnabled(enabled:TSDL_bool);cdecl;external;
{*
 * Query the state of joystick event processing.
 *
 * If joystick events are disabled, you must call SDL_UpdateJoysticks()
 * yourself and check the state of the joystick when you want joystick
 * information.
 *
 * \returns SDL_TRUE if joystick events are being processed, SDL_FALSE
 *          otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetJoystickEventsEnabled
  }
function SDL_JoystickEventsEnabled:TSDL_bool;cdecl;external;
{*
 * Update the current state of the open joysticks.
 *
 * This is called automatically by the event loop if any joystick events are
 * enabled.
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_UpdateJoysticks;cdecl;external;
{*
 * Get the current state of an axis control on a joystick.
 *
 * SDL makes no promises about what part of the joystick any given axis refers
 * to. Your game should have some sort of configuration UI to let users
 * specify what each axis should be bound to. Alternately, SDL's higher-level
 * Game Controller API makes a great effort to apply order to this lower-level
 * interface, so you know that a specific axis is the "left thumb stick," etc.
 *
 * The value returned by SDL_GetJoystickAxis() is a signed integer (-32768 to
 * 32767) representing the current position of the axis. It may be necessary
 * to impose certain tolerances on these values to account for jitter.
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \param axis the axis to query; the axis indices start at index 0
 * \returns a 16-bit signed integer representing the current position of the
 *          axis or 0 on failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetNumJoystickAxes
  }
function SDL_GetJoystickAxis(joystick:PSDL_Joystick; axis:longint):TSint16;cdecl;external;
{*
 * Get the initial state of an axis control on a joystick.
 *
 * The state is a value ranging from -32768 to 32767.
 *
 * The axis indices start at index 0.
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \param axis the axis to query; the axis indices start at index 0
 * \param state Upon return, the initial value is supplied here.
 * \returns SDL_TRUE if this axis has any initial value, or SDL_FALSE if not.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickAxisInitialState(joystick:PSDL_Joystick; axis:longint; state:PSint16):TSDL_bool;cdecl;external;
{*
 *  \name Hat positions
  }
{ @  }
const
  SDL_HAT_CENTERED = $00;  
  SDL_HAT_UP = $01;  
  SDL_HAT_RIGHT = $02;  
  SDL_HAT_DOWN = $04;  
  SDL_HAT_LEFT = $08;  
  SDL_HAT_RIGHTUP = SDL_HAT_RIGHT or SDL_HAT_UP;  
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;  
  SDL_HAT_LEFTUP = SDL_HAT_LEFT or SDL_HAT_UP;  
  SDL_HAT_LEFTDOWN = SDL_HAT_LEFT or SDL_HAT_DOWN;  
{ @  }
{*
 * Get the current state of a POV hat on a joystick.
 *
 * The returned value will be one of the following positions:
 *
 * - `SDL_HAT_CENTERED`
 * - `SDL_HAT_UP`
 * - `SDL_HAT_RIGHT`
 * - `SDL_HAT_DOWN`
 * - `SDL_HAT_LEFT`
 * - `SDL_HAT_RIGHTUP`
 * - `SDL_HAT_RIGHTDOWN`
 * - `SDL_HAT_LEFTUP`
 * - `SDL_HAT_LEFTDOWN`
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \param hat the hat index to get the state from; indices start at index 0
 * \returns the current hat position.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetNumJoystickHats
  }

function SDL_GetJoystickHat(joystick:PSDL_Joystick; hat:longint):TUint8;cdecl;external;
{*
 * Get the current state of a button on a joystick.
 *
 * \param joystick an SDL_Joystick structure containing joystick information
 * \param button the button index to get the state from; indices start at
 *               index 0
 * \returns 1 if the specified button is pressed, 0 otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetNumJoystickButtons
  }
function SDL_GetJoystickButton(joystick:PSDL_Joystick; button:longint):TUint8;cdecl;external;
{*
 * Start a rumble effect.
 *
 * Each call to this function cancels any previous rumble effect, and calling
 * it with 0 intensity stops any rumbling.
 *
 * This function requires you to process SDL events or call
 * SDL_UpdateJoysticks() to update rumble state.
 *
 * \param joystick The joystick to vibrate
 * \param low_frequency_rumble The intensity of the low frequency (left)
 *                             rumble motor, from 0 to 0xFFFF
 * \param high_frequency_rumble The intensity of the high frequency (right)
 *                              rumble motor, from 0 to 0xFFFF
 * \param duration_ms The duration of the rumble effect, in milliseconds
 * \returns 0, or -1 if rumble isn't supported on this joystick
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_RumbleJoystick(joystick:PSDL_Joystick; low_frequency_rumble:TUint16; high_frequency_rumble:TUint16; duration_ms:TUint32):longint;cdecl;external;
{*
 * Start a rumble effect in the joystick's triggers
 *
 * Each call to this function cancels any previous trigger rumble effect, and
 * calling it with 0 intensity stops any rumbling.
 *
 * Note that this is rumbling of the _triggers_ and not the game controller as
 * a whole. This is currently only supported on Xbox One controllers. If you
 * want the (more common) whole-controller rumble, use SDL_RumbleJoystick()
 * instead.
 *
 * This function requires you to process SDL events or call
 * SDL_UpdateJoysticks() to update rumble state.
 *
 * \param joystick The joystick to vibrate
 * \param left_rumble The intensity of the left trigger rumble motor, from 0
 *                    to 0xFFFF
 * \param right_rumble The intensity of the right trigger rumble motor, from 0
 *                     to 0xFFFF
 * \param duration_ms The duration of the rumble effect, in milliseconds
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_RumbleJoystickTriggers(joystick:PSDL_Joystick; left_rumble:TUint16; right_rumble:TUint16; duration_ms:TUint32):longint;cdecl;external;
{*
 * Update a joystick's LED color.
 *
 * An example of a joystick LED is the light on the back of a PlayStation 4's
 * DualShock 4 controller.
 *
 * For joysticks with a single color LED, the maximum of the RGB values will
 * be used as the LED brightness.
 *
 * \param joystick The joystick to update
 * \param red The intensity of the red LED
 * \param green The intensity of the green LED
 * \param blue The intensity of the blue LED
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetJoystickLED(joystick:PSDL_Joystick; red:TUint8; green:TUint8; blue:TUint8):longint;cdecl;external;
{*
 * Send a joystick specific effect packet
 *
 * \param joystick The joystick to affect
 * \param data The data to send to the joystick
 * \param size The size of the data to send to the joystick
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
function SDL_SendJoystickEffect(joystick:PSDL_Joystick; data:pointer; size:longint):longint;cdecl;external;
{*
 * Close a joystick previously opened with SDL_OpenJoystick().
 *
 * \param joystick The joystick device to close
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_OpenJoystick
  }
procedure SDL_CloseJoystick(joystick:PSDL_Joystick);cdecl;external;
{*
 * Get the battery level of a joystick as SDL_JoystickPowerLevel.
 *
 * \param joystick the SDL_Joystick to query
 * \returns the current battery level as SDL_JoystickPowerLevel on success or
 *          `SDL_JOYSTICK_POWER_UNKNOWN` if it is unknown
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetJoystickPowerLevel(joystick:PSDL_Joystick):TSDL_JoystickPowerLevel;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_joystick_h_  }

implementation


end.
