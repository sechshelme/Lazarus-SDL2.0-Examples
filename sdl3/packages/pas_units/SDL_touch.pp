unit SDL_touch;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_touch_h_}
//{$define SDL_touch_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_video.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
type
  PSDL_TouchID = ^TSDL_TouchID;
  TSDL_TouchID = Uint64;

  PSDL_FingerID = ^TSDL_FingerID;
  TSDL_FingerID = Uint64;
{ touch screen with window-relative coordinates  }
{ trackpad with absolute device coordinates  }
{ trackpad with screen cursor-relative coordinates  }

  PSDL_TouchDeviceType = ^TSDL_TouchDeviceType;
  TSDL_TouchDeviceType =  Longint;
  Const
    SDL_TOUCH_DEVICE_INVALID = -(1);
    SDL_TOUCH_DEVICE_DIRECT = (-(1))+1;
    SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE = (-(1))+2;
    SDL_TOUCH_DEVICE_INDIRECT_RELATIVE = (-(1))+3;

type
  PSDL_Finger = ^TSDL_Finger;
  TSDL_Finger = record
      id : TSDL_FingerID;
      x : single;
      y : single;
      pressure : single;
    end;
{ Used as the device ID for mouse events simulated with touch input  }

{ was #define dname def_expr }
function SDL_TOUCH_MOUSEID : Uint32;

{ Used as the SDL_TouchID for touch events simulated with mouse input  }
{ was #define dname def_expr }
function SDL_MOUSE_TOUCHID : Uint64;

{*
 * Get a list of registered touch devices.
 *
 * On some platforms SDL first sees the touch device if it was actually used.
 * Therefore the returned list might be empty, although devices are available.
 * After using all devices at least once the number will be correct.
 *
 * This was fixed for Android in SDL 2.0.1.
 *
 * \param count a pointer filled in with the number of devices returned, can
 *              be NULL.
 * \returns a 0 terminated array of touch device IDs which should be freed
 *          with SDL_free(), or NULL on error; call SDL_GetError() for more
 *          details.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTouchDevices(count:Plongint):PSDL_TouchID;cdecl;external;
{*
 * Get the touch device name as reported from the driver.
 *
 * You do not own the returned string, do not modify or free it.
 *
 * \param touchID the touch device instance ID.
 * \returns touch device name, or NULL on error; call SDL_GetError() for more
 *          details.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
function SDL_GetTouchDeviceName(touchID:TSDL_TouchID):Pchar;cdecl;external;
{*
 * Get the type of the given touch device.
 *
 * \param touchID the ID of a touch device
 * \returns touch device type
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTouchDeviceType(touchID:TSDL_TouchID):TSDL_TouchDeviceType;cdecl;external;
{*
 * Get the number of active fingers for a given touch device.
 *
 * \param touchID the ID of a touch device
 * \returns the number of active fingers for a given touch device on success
 *          or a negative error code on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetTouchFinger
  }
function SDL_GetNumTouchFingers(touchID:TSDL_TouchID):longint;cdecl;external;
{*
 * Get the finger object for specified touch device ID and finger index.
 *
 * The returned resource is owned by SDL and should not be deallocated.
 *
 * \param touchID the ID of the requested touch device
 * \param index the index of the requested finger
 * \returns a pointer to the SDL_Finger object or NULL if no object at the
 *          given ID and index could be found.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTouchFinger(touchID:TSDL_TouchID; index:longint):PSDL_Finger;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$e7ndif}
{ SDL_touch_h_  }

implementation

{ was #define dname def_expr }
function SDL_TOUCH_MOUSEID : Uint32;
  begin
    SDL_TOUCH_MOUSEID:=Uint32(-(1));
  end;

{ was #define dname def_expr }
function SDL_MOUSE_TOUCHID : Uint64;
  begin
    SDL_MOUSE_TOUCHID:=Uint64(-(1));
  end;


end.
