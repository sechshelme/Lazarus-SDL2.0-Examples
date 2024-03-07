unit SDL3_events;

interface

uses
  SDL3_video, SDL_keyboard;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_events_h_}
//{$define SDL_events_h_}
//{$include <SDL3/SDL_audio.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_gamepad.h>}
//{$include <SDL3/SDL_joystick.h>}
//{$include <SDL3/SDL_keyboard.h>}
//{$include <SDL3/SDL_mouse.h>}
//{$include <SDL3/SDL_pen.h>}
//{$include <SDL3/SDL_quit.h>}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_touch.h>}
//{$include <SDL3/SDL_video.h>}
//{$include <SDL3/SDL_camera.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ General keyboard/mouse state definitions  }

const
  SDL_RELEASED = 0;  
  SDL_PRESSED = 1;  
{*
 * The types of events that can be delivered.
  }
{*< Unused (do not remove)  }
{ Application events  }
{*< User-requested quit  }
{ These application events have special meaning on iOS, see README-ios.md for details  }
{*< The application is being terminated by the OS
                                     Called on iOS in applicationWillTerminate()
                                     Called on Android in onDestroy()
                                 }
{*< The application is low on memory, free memory if possible.
                                     Called on iOS in applicationDidReceiveMemoryWarning()
                                     Called on Android in onLowMemory()
                                 }
{*< The application is about to enter the background
                                     Called on iOS in applicationWillResignActive()
                                     Called on Android in onPause()
                                 }
{*< The application did enter the background and may not get CPU for some time
                                     Called on iOS in applicationDidEnterBackground()
                                     Called on Android in onPause()
                                 }
{*< The application is about to enter the foreground
                                     Called on iOS in applicationWillEnterForeground()
                                     Called on Android in onResume()
                                 }
{*< The application is now interactive
                                     Called on iOS in applicationDidBecomeActive()
                                     Called on Android in onResume()
                                 }
{*< The user's locale preferences have changed.  }
{*< The system theme changed  }
{ Display events  }
{ 0x150 was SDL_DISPLAYEVENT, reserve the number for sdl2-compat  }
{*< Display orientation has changed to data1  }
{*< Display has been added to the system  }
{*< Display has been removed from the system  }
{*< Display has changed position  }
{*< Display has changed content scale  }
{*< Display HDR properties have changed  }
{ Window events  }
{ 0x200 was SDL_WINDOWEVENT, reserve the number for sdl2-compat  }
{ 0x201 was SDL_EVENT_SYSWM, reserve the number for sdl2-compat  }
{*< Window has been shown  }
{*< Window has been hidden  }
{*< Window has been exposed and should be redrawn  }
{*< Window has been moved to data1, data2  }
{*< Window has been resized to data1xdata2  }
{*< The pixel size of the window has changed to data1xdata2  }
{*< Window has been minimized  }
{*< Window has been maximized  }
{*< Window has been restored to normal size and position  }
{*< Window has gained mouse focus  }
{*< Window has lost mouse focus  }
{*< Window has gained keyboard focus  }
{*< Window has lost keyboard focus  }
{*< The window manager requests that the window be closed  }
{*< Window is being offered a focus (should SetWindowInputFocus() on itself or a subwindow, or ignore)  }
{*< Window had a hit test that wasn't SDL_HITTEST_NORMAL  }
{*< The ICC profile of the window's display has changed  }
{*< Window has been moved to display data1  }
{*< Window display scale has been changed  }
{*< The window has been occluded  }
{*< The window has entered fullscreen mode  }
{*< The window has left fullscreen mode  }
{*< The window with the associated ID is being or has been destroyed. If this message is being handled
                                             in an event watcher, the window handle is still valid and can still be used to retrieve any userdata
                                             associated with the window. Otherwise, the handle has already been destroyed and all resources
                                             associated with it are invalid  }
{*< Window has gained focus of the pressure-sensitive pen with ID "data1"  }
{*< Window has lost focus of the pressure-sensitive pen with ID "data1"  }
{ Keyboard events  }
{*< Key pressed  }
{*< Key released  }
{*< Keyboard text editing (composition)  }
{*< Keyboard text input  }
{*< Keymap changed due to a system event such as an
                                            input language or keyboard layout change.  }
{ Mouse events  }
{*< Mouse moved  }
{*< Mouse button pressed  }
{*< Mouse button released  }
{*< Mouse wheel motion  }
{ Joystick events  }
{*< Joystick axis motion  }
{*< Joystick hat position change  }
{*< Joystick button pressed  }
{*< Joystick button released  }
{*< A new joystick has been inserted into the system  }
{*< An opened joystick has been removed  }
{*< Joystick battery level change  }
{*< Joystick update is complete  }
{ Gamepad events  }
{*< Gamepad axis motion  }
{*< Gamepad button pressed  }
{*< Gamepad button released  }
{*< A new gamepad has been inserted into the system  }
{*< An opened gamepad has been removed  }
{*< The gamepad mapping was updated  }
{*< Gamepad touchpad was touched  }
{*< Gamepad touchpad finger was moved  }
{*< Gamepad touchpad finger was lifted  }
{*< Gamepad sensor was updated  }
{*< Gamepad update is complete  }
{*< Gamepad Steam handle has changed  }
{ Touch events  }
{ 0x800, 0x801, and 0x802 were the Gesture events from SDL2. Do not reuse these values! sdl2-compat needs them!  }
{ Clipboard events  }
{*< The clipboard or primary selection changed  }
{ Drag and drop events  }
{*< The system requests a file open  }
{*< text/plain drag-and-drop event  }
{*< A new set of drops is beginning (NULL filename)  }
{*< Current set of drops is now complete (NULL filename)  }
{*< Position while moving over the window  }
{ Audio hotplug events  }
{*< A new audio device is available  }
{*< An audio device has been removed.  }
{*< An audio device's format has been changed by the system.  }
{ Sensor events  }
{*< A sensor was updated  }
{ Pressure-sensitive pen events  }
{*< Pressure-sensitive pen touched drawing surface  }
{*< Pressure-sensitive pen stopped touching drawing surface  }
{*< Pressure-sensitive pen moved, or angle/pressure changed  }
{*< Pressure-sensitive pen button pressed  }
{*< Pressure-sensitive pen button released  }
{ Camera hotplug events  }
{*< A new camera device is available  }
{*< A camera device has been removed.  }
{*< A camera device has been approved for use by the user.  }
{*< A camera device has been denied for use by the user.  }
{ Render events  }
{*< The render targets have been reset and their contents need to be updated  }
{*< The device has been reset and all textures need to be recreated  }
{ Internal events  }
{*< Signals the end of an event poll cycle  }
{* Events ::SDL_EVENT_USER through ::SDL_EVENT_LAST are for your use,
     *  and should be allocated with SDL_RegisterEvents()
      }
{*
     *  This last event is only for bounding internal arrays
      }
type
  PSDL_EventType = ^TSDL_EventType;
  TSDL_EventType =  Longint;
  Const
    SDL_EVENT_FIRST = 0;
    SDL_EVENT_QUIT = $100;
    SDL_EVENT_TERMINATING = 257;
    SDL_EVENT_LOW_MEMORY = 258;
    SDL_EVENT_WILL_ENTER_BACKGROUND = 259;
    SDL_EVENT_DID_ENTER_BACKGROUND = 260;
    SDL_EVENT_WILL_ENTER_FOREGROUND = 261;
    SDL_EVENT_DID_ENTER_FOREGROUND = 262;
    SDL_EVENT_LOCALE_CHANGED = 263;
    SDL_EVENT_SYSTEM_THEME_CHANGED = 264;
    SDL_EVENT_DISPLAY_ORIENTATION = $151;
    SDL_EVENT_DISPLAY_ADDED = 338;
    SDL_EVENT_DISPLAY_REMOVED = 339;
    SDL_EVENT_DISPLAY_MOVED = 340;
    SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED = 341;
    SDL_EVENT_DISPLAY_HDR_STATE_CHANGED = 342;
    SDL_EVENT_DISPLAY_FIRST = SDL_EVENT_DISPLAY_ORIENTATION;
    SDL_EVENT_DISPLAY_LAST = SDL_EVENT_DISPLAY_HDR_STATE_CHANGED;
    SDL_EVENT_WINDOW_SHOWN = $202;
    SDL_EVENT_WINDOW_HIDDEN = 515;
    SDL_EVENT_WINDOW_EXPOSED = 516;
    SDL_EVENT_WINDOW_MOVED = 517;
    SDL_EVENT_WINDOW_RESIZED = 518;
    SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED = 519;
    SDL_EVENT_WINDOW_MINIMIZED = 520;
    SDL_EVENT_WINDOW_MAXIMIZED = 521;
    SDL_EVENT_WINDOW_RESTORED = 522;
    SDL_EVENT_WINDOW_MOUSE_ENTER = 523;
    SDL_EVENT_WINDOW_MOUSE_LEAVE = 524;
    SDL_EVENT_WINDOW_FOCUS_GAINED = 525;
    SDL_EVENT_WINDOW_FOCUS_LOST = 526;
    SDL_EVENT_WINDOW_CLOSE_REQUESTED = 527;
    SDL_EVENT_WINDOW_TAKE_FOCUS = 528;
    SDL_EVENT_WINDOW_HIT_TEST = 529;
    SDL_EVENT_WINDOW_ICCPROF_CHANGED = 530;
    SDL_EVENT_WINDOW_DISPLAY_CHANGED = 531;
    SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED = 532;
    SDL_EVENT_WINDOW_OCCLUDED = 533;
    SDL_EVENT_WINDOW_ENTER_FULLSCREEN = 534;
    SDL_EVENT_WINDOW_LEAVE_FULLSCREEN = 535;
    SDL_EVENT_WINDOW_DESTROYED = 536;
    SDL_EVENT_WINDOW_PEN_ENTER = 537;
    SDL_EVENT_WINDOW_PEN_LEAVE = 538;
    SDL_EVENT_WINDOW_FIRST = SDL_EVENT_WINDOW_SHOWN;
    SDL_EVENT_WINDOW_LAST = SDL_EVENT_WINDOW_PEN_LEAVE;
    SDL_EVENT_KEY_DOWN = $300;
    SDL_EVENT_KEY_UP = 769;
    SDL_EVENT_TEXT_EDITING = 770;
    SDL_EVENT_TEXT_INPUT = 771;
    SDL_EVENT_KEYMAP_CHANGED = 772;
    SDL_EVENT_MOUSE_MOTION = $400;
    SDL_EVENT_MOUSE_BUTTON_DOWN = 1025;
    SDL_EVENT_MOUSE_BUTTON_UP = 1026;
    SDL_EVENT_MOUSE_WHEEL = 1027;
    SDL_EVENT_JOYSTICK_AXIS_MOTION = $600;
    SDL_EVENT_JOYSTICK_HAT_MOTION = $602;
    SDL_EVENT_JOYSTICK_BUTTON_DOWN = 1539;
    SDL_EVENT_JOYSTICK_BUTTON_UP = 1540;
    SDL_EVENT_JOYSTICK_ADDED = 1541;
    SDL_EVENT_JOYSTICK_REMOVED = 1542;
    SDL_EVENT_JOYSTICK_BATTERY_UPDATED = 1543;
    SDL_EVENT_JOYSTICK_UPDATE_COMPLETE = 1544;
    SDL_EVENT_GAMEPAD_AXIS_MOTION = $650;
    SDL_EVENT_GAMEPAD_BUTTON_DOWN = 1617;
    SDL_EVENT_GAMEPAD_BUTTON_UP = 1618;
    SDL_EVENT_GAMEPAD_ADDED = 1619;
    SDL_EVENT_GAMEPAD_REMOVED = 1620;
    SDL_EVENT_GAMEPAD_REMAPPED = 1621;
    SDL_EVENT_GAMEPAD_TOUCHPAD_DOWN = 1622;
    SDL_EVENT_GAMEPAD_TOUCHPAD_MOTION = 1623;
    SDL_EVENT_GAMEPAD_TOUCHPAD_UP = 1624;
    SDL_EVENT_GAMEPAD_SENSOR_UPDATE = 1625;
    SDL_EVENT_GAMEPAD_UPDATE_COMPLETE = 1626;
    SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED = 1627;
    SDL_EVENT_FINGER_DOWN = $700;
    SDL_EVENT_FINGER_UP = 1793;
    SDL_EVENT_FINGER_MOTION = 1794;
    SDL_EVENT_CLIPBOARD_UPDATE = $900;
    SDL_EVENT_DROP_FILE = $1000;
    SDL_EVENT_DROP_TEXT = 4097;
    SDL_EVENT_DROP_BEGIN = 4098;
    SDL_EVENT_DROP_COMPLETE = 4099;
    SDL_EVENT_DROP_POSITION = 4100;
    SDL_EVENT_AUDIO_DEVICE_ADDED = $1100;
    SDL_EVENT_AUDIO_DEVICE_REMOVED = 4353;
    SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED = 4354;
    SDL_EVENT_SENSOR_UPDATE = $1200;
    SDL_EVENT_PEN_DOWN = $1300;
    SDL_EVENT_PEN_UP = 4865;
    SDL_EVENT_PEN_MOTION = 4866;
    SDL_EVENT_PEN_BUTTON_DOWN = 4867;
    SDL_EVENT_PEN_BUTTON_UP = 4868;
    SDL_EVENT_CAMERA_DEVICE_ADDED = $1400;
    SDL_EVENT_CAMERA_DEVICE_REMOVED = 5121;
    SDL_EVENT_CAMERA_DEVICE_APPROVED = 5122;
    SDL_EVENT_CAMERA_DEVICE_DENIED = 5123;
    SDL_EVENT_RENDER_TARGETS_RESET = $2000;
    SDL_EVENT_RENDER_DEVICE_RESET = 8193;
    SDL_EVENT_POLL_SENTINEL = $7F00;
    SDL_EVENT_USER = $8000;
    SDL_EVENT_LAST = $FFFF;

{*
 *  Fields shared by every event
  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
type
  PSDL_CommonEvent = ^TSDL_CommonEvent;
  TSDL_CommonEvent = record
      _type : Uint32;
      reserved : Uint32;
      timestamp : Uint64;
    end;
{*
 *  Display state change event data (event.display.*)
  }
{*< ::SDL_DISPLAYEVENT_*  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The associated display  }
{*< event dependent data  }

  PSDL_DisplayEvent = ^TSDL_DisplayEvent;
  TSDL_DisplayEvent = record
      _type : Uint32;
      reserved : Uint32;
      timestamp : Uint64;
      displayID : TSDL_DisplayID;
      data1 : int32;
    end;
{*
 *  Window state change event data (event.window.*)
  }
{*< ::SDL_WINDOWEVENT_*  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The associated window  }
{*< event dependent data  }
{*< event dependent data  }

  PSDL_WindowEvent = ^TSDL_WindowEvent;
  TSDL_WindowEvent = record
      _type : Uint32;
      reserved : Uint32;
      timestamp : Uint64;
      windowID : TSDL_WindowID;
      data1 : int32;
      data2 : int32;
    end;
{*
 *  Keyboard button event structure (event.key.*)
  }
{*< ::SDL_EVENT_KEY_DOWN or ::SDL_EVENT_KEY_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with keyboard focus, if any  }
{*< ::SDL_PRESSED or ::SDL_RELEASED  }
{*< Non-zero if this is a key repeat  }
{*< The key that was pressed or released  }

  PSDL_KeyboardEvent = ^TSDL_KeyboardEvent;
  TSDL_KeyboardEvent = record
      _type : Uint32;
      reserved : Uint32;
      timestamp : Uint64;
      windowID : TSDL_WindowID;
      state : Uint8;
      _repeat : Uint8;
      padding2 : Uint8;
      padding3 : Uint8;
      keysym : TSDL_Keysym;
    end;

const
  SDL_TEXTEDITINGEVENT_TEXT_SIZE = 64;  
{*
 *  Keyboard text editing event structure (event.edit.*)
 *
 *  The `text` is owned by SDL and should be copied if the application
 *  wants to hold onto it beyond the scope of handling this event.
  }
{*< ::SDL_EVENT_TEXT_EDITING  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with keyboard focus, if any  }
{*< The editing text  }
{*< The start cursor of selected editing text  }
{*< The length of selected editing text  }
type
  PSDL_TextEditingEvent = ^TSDL_TextEditingEvent;
  TSDL_TextEditingEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      text : Pchar;
      start : TSint32;
      length : TSint32;
    end;

const
  SDL_TEXTINPUTEVENT_TEXT_SIZE = 64;  
{*
 *  Keyboard text input event structure (event.text.*)
 *
 *  The `text` is owned by SDL and should be copied if the application
 *  wants to hold onto it beyond the scope of handling this event.
  }
{*< ::SDL_EVENT_TEXT_INPUT  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with keyboard focus, if any  }
{*< The input text  }
type
  PSDL_TextInputEvent = ^TSDL_TextInputEvent;
  TSDL_TextInputEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      text : Pchar;
    end;
{*
 *  Mouse motion event structure (event.motion.*)
  }
{*< ::SDL_EVENT_MOUSE_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The mouse instance id, SDL_TOUCH_MOUSEID, or SDL_PEN_MOUSEID  }
{*< The current button state  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< The relative motion in the X direction  }
{*< The relative motion in the Y direction  }

  PSDL_MouseMotionEvent = ^TSDL_MouseMotionEvent;
  TSDL_MouseMotionEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_MouseID;
      state : TUint32;
      x : single;
      y : single;
      xrel : single;
      yrel : single;
    end;
{*
 *  Mouse button event structure (event.button.*)
  }
{*< ::SDL_EVENT_MOUSE_BUTTON_DOWN or ::SDL_EVENT_MOUSE_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The mouse instance id, SDL_TOUCH_MOUSEID, or SDL_PEN_MOUSEID  }
{*< The mouse button index  }
{*< ::SDL_PRESSED or ::SDL_RELEASED  }
{*< 1 for single-click, 2 for double-click, etc.  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }

  PSDL_MouseButtonEvent = ^TSDL_MouseButtonEvent;
  TSDL_MouseButtonEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_MouseID;
      button : TUint8;
      state : TUint8;
      clicks : TUint8;
      padding : TUint8;
      x : single;
      y : single;
    end;
{*
 *  Mouse wheel event structure (event.wheel.*)
  }
{*< ::SDL_EVENT_MOUSE_WHEEL  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The mouse instance id, SDL_TOUCH_MOUSEID, or SDL_PEN_MOUSEID  }
{*< The amount scrolled horizontally, positive to the right and negative to the left  }
{*< The amount scrolled vertically, positive away from the user and negative toward the user  }
{*< Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }

  PSDL_MouseWheelEvent = ^TSDL_MouseWheelEvent;
  TSDL_MouseWheelEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_MouseID;
      x : single;
      y : single;
      direction : TUint32;
      mouse_x : single;
      mouse_y : single;
    end;
{*
 *  Joystick axis motion event structure (event.jaxis.*)
  }
{*< ::SDL_EVENT_JOYSTICK_AXIS_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick axis index  }
{*< The axis value (range: -32768 to 32767)  }

  PSDL_JoyAxisEvent = ^TSDL_JoyAxisEvent;
  TSDL_JoyAxisEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      axis : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
      value : TSint16;
      padding4 : TUint16;
    end;
{*
 *  Joystick hat position change event structure (event.jhat.*)
  }
{*< ::SDL_EVENT_JOYSTICK_HAT_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick hat index  }
{*< The hat position value.
                         *   \sa ::SDL_HAT_LEFTUP ::SDL_HAT_UP ::SDL_HAT_RIGHTUP
                         *   \sa ::SDL_HAT_LEFT ::SDL_HAT_CENTERED ::SDL_HAT_RIGHT
                         *   \sa ::SDL_HAT_LEFTDOWN ::SDL_HAT_DOWN ::SDL_HAT_RIGHTDOWN
                         *
                         *   Note that zero means the POV is centered.
                          }

  PSDL_JoyHatEvent = ^TSDL_JoyHatEvent;
  TSDL_JoyHatEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      hat : TUint8;
      value : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 *  Joystick button event structure (event.jbutton.*)
  }
{*< ::SDL_EVENT_JOYSTICK_BUTTON_DOWN or ::SDL_EVENT_JOYSTICK_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick button index  }
{*< ::SDL_PRESSED or ::SDL_RELEASED  }

  PSDL_JoyButtonEvent = ^TSDL_JoyButtonEvent;
  TSDL_JoyButtonEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      button : TUint8;
      state : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 *  Joystick device event structure (event.jdevice.*)
  }
{*< ::SDL_EVENT_JOYSTICK_ADDED or ::SDL_EVENT_JOYSTICK_REMOVED or ::SDL_EVENT_JOYSTICK_UPDATE_COMPLETE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }

  PSDL_JoyDeviceEvent = ^TSDL_JoyDeviceEvent;
  TSDL_JoyDeviceEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
    end;
{*
 *  Joysick battery level change event structure (event.jbattery.*)
  }
{*< ::SDL_EVENT_JOYSTICK_BATTERY_UPDATED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick battery level  }

  PSDL_JoyBatteryEvent = ^TSDL_JoyBatteryEvent;
  TSDL_JoyBatteryEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      level : TSDL_JoystickPowerLevel;
    end;
{*
 *  Gamepad axis motion event structure (event.gaxis.*)
  }
{*< ::SDL_EVENT_GAMEPAD_AXIS_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The gamepad axis (SDL_GamepadAxis)  }
{*< The axis value (range: -32768 to 32767)  }

  PSDL_GamepadAxisEvent = ^TSDL_GamepadAxisEvent;
  TSDL_GamepadAxisEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      axis : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
      value : TSint16;
      padding4 : TUint16;
    end;
{*
 *  Gamepad button event structure (event.gbutton.*)
  }
{*< ::SDL_EVENT_GAMEPAD_BUTTON_DOWN or ::SDL_EVENT_GAMEPAD_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The gamepad button (SDL_GamepadButton)  }
{*< ::SDL_PRESSED or ::SDL_RELEASED  }

  PSDL_GamepadButtonEvent = ^TSDL_GamepadButtonEvent;
  TSDL_GamepadButtonEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      button : TUint8;
      state : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 *  Gamepad device event structure (event.gdevice.*)
  }
{*< ::SDL_EVENT_GAMEPAD_ADDED, ::SDL_EVENT_GAMEPAD_REMOVED, or ::SDL_EVENT_GAMEPAD_REMAPPED, ::SDL_EVENT_GAMEPAD_UPDATE_COMPLETE or ::SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }

  PSDL_GamepadDeviceEvent = ^TSDL_GamepadDeviceEvent;
  TSDL_GamepadDeviceEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
    end;
{*
 *  Gamepad touchpad event structure (event.gtouchpad.*)
  }
{*< ::SDL_EVENT_GAMEPAD_TOUCHPAD_DOWN or ::SDL_EVENT_GAMEPAD_TOUCHPAD_MOTION or ::SDL_EVENT_GAMEPAD_TOUCHPAD_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The index of the touchpad  }
{*< The index of the finger on the touchpad  }
{*< Normalized in the range 0...1 with 0 being on the left  }
{*< Normalized in the range 0...1 with 0 being at the top  }
{*< Normalized in the range 0...1  }

  PSDL_GamepadTouchpadEvent = ^TSDL_GamepadTouchpadEvent;
  TSDL_GamepadTouchpadEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      touchpad : TSint32;
      finger : TSint32;
      x : single;
      y : single;
      pressure : single;
    end;
{*
 *  Gamepad sensor event structure (event.gsensor.*)
  }
{*< ::SDL_EVENT_GAMEPAD_SENSOR_UPDATE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The type of the sensor, one of the values of ::SDL_SensorType  }
{*< Up to 3 values from the sensor, as defined in SDL_sensor.h  }
{*< The timestamp of the sensor reading in nanoseconds, not necessarily synchronized with the system clock  }

  PSDL_GamepadSensorEvent = ^TSDL_GamepadSensorEvent;
  TSDL_GamepadSensorEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      sensor : TSint32;
      data : array[0..2] of single;
      sensor_timestamp : TUint64;
    end;
{*
 *  Audio device event structure (event.adevice.*)
  }
{*< ::SDL_EVENT_AUDIO_DEVICE_ADDED, or ::SDL_EVENT_AUDIO_DEVICE_REMOVED, or ::SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< SDL_AudioDeviceID for the device being added or removed or changing  }
{*< zero if an output device, non-zero if a capture device.  }

  PSDL_AudioDeviceEvent = ^TSDL_AudioDeviceEvent;
  TSDL_AudioDeviceEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_AudioDeviceID;
      iscapture : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 *  Camera device event structure (event.cdevice.*)
  }
{*< ::SDL_EVENT_CAMERA_DEVICE_ADDED, ::SDL_EVENT_CAMERA_DEVICE_REMOVED, ::SDL_EVENT_CAMERA_DEVICE_APPROVED, ::SDL_EVENT_CAMERA_DEVICE_DENIED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< SDL_CameraDeviceID for the device being added or removed or changing  }

  PSDL_CameraDeviceEvent = ^TSDL_CameraDeviceEvent;
  TSDL_CameraDeviceEvent = record
      _type : TUint32;
      timestamp : TUint64;
      which : TSDL_CameraDeviceID;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 *  Touch finger event structure (event.tfinger.*)
  }
{*< ::SDL_EVENT_FINGER_MOTION or ::SDL_EVENT_FINGER_DOWN or ::SDL_EVENT_FINGER_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The touch device id  }
{*< Normalized in the range 0...1  }
{*< Normalized in the range 0...1  }
{*< Normalized in the range -1...1  }
{*< Normalized in the range -1...1  }
{*< Normalized in the range 0...1  }
{*< The window underneath the finger, if any  }

  PSDL_TouchFingerEvent = ^TSDL_TouchFingerEvent;
  TSDL_TouchFingerEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      touchID : TSDL_TouchID;
      fingerID : TSDL_FingerID;
      x : single;
      y : single;
      dx : single;
      dy : single;
      pressure : single;
      windowID : TSDL_WindowID;
    end;

const
  SDL_DROPEVENT_DATA_SIZE = 64;  
{*
 *  Pressure-sensitive pen touched or stopped touching surface (event.ptip.*)
  }
{*< ::SDL_EVENT_PEN_DOWN or ::SDL_EVENT_PEN_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with pen focus, if any  }
{*< The pen instance id  }
{*< ::SDL_PEN_TIP_INK when using a regular pen tip, or ::SDL_PEN_TIP_ERASER if the pen is being used as an eraser (e.g., flipped to use the eraser tip)   }
{*< ::SDL_PRESSED on ::SDL_EVENT_PEN_DOWN and ::SDL_RELEASED on ::SDL_EVENT_PEN_UP  }
{*< Pen button masks (where SDL_BUTTON(1) is the first button, SDL_BUTTON(2) is the second button etc.),
			   ::SDL_PEN_DOWN_MASK is set if the pen is touching the surface, and
			   ::SDL_PEN_ERASER_MASK is set if the pen is (used as) an eraser.  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< Pen axes such as pressure and tilt (ordered as per ::SDL_PenAxis)  }
type
  PSDL_PenTipEvent = ^TSDL_PenTipEvent;
  TSDL_PenTipEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TUint32;
      which : TSDL_PenID;
      tip : TUint8;
      state : TUint8;
      pen_state : TUint16;
      x : single;
      y : single;
      axes : array[0..(SDL_PEN_NUM_AXES)-1] of single;
    end;
{*
 *  Pressure-sensitive pen motion / pressure / angle event structure (event.pmotion.*)
  }
{*< ::SDL_EVENT_PEN_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with pen focus, if any  }
{*< The pen instance id  }
{*< Pen button masks (where SDL_BUTTON(1) is the first button, SDL_BUTTON(2) is the second button etc.),
			   ::SDL_PEN_DOWN_MASK is set if the pen is touching the surface, and
			   ::SDL_PEN_ERASER_MASK is set if the pen is (used as) an eraser.  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< Pen axes such as pressure and tilt (ordered as per ::SDL_PenAxis)  }

  PSDL_PenMotionEvent = ^TSDL_PenMotionEvent;
  TSDL_PenMotionEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TUint32;
      which : TSDL_PenID;
      padding1 : TUint8;
      padding2 : TUint8;
      pen_state : TUint16;
      x : single;
      y : single;
      axes : array[0..(SDL_PEN_NUM_AXES)-1] of single;
    end;
{*
 *  Pressure-sensitive pen button event structure (event.pbutton.*)
  }
{*< ::SDL_EVENT_PEN_BUTTON_DOWN or ::SDL_EVENT_PEN_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with pen focus, if any  }
{*< The pen instance id  }
{*< The pen button index (1 represents the pen tip for compatibility with mouse events)  }
{*< ::SDL_PRESSED or ::SDL_RELEASED  }
{*< Pen button masks (where SDL_BUTTON(1) is the first button, SDL_BUTTON(2) is the second button etc.),
			   ::SDL_PEN_DOWN_MASK is set if the pen is touching the surface, and
			   ::SDL_PEN_ERASER_MASK is set if the pen is (used as) an eraser.  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< Pen axes such as pressure and tilt (ordered as per ::SDL_PenAxis)  }

  PSDL_PenButtonEvent = ^TSDL_PenButtonEvent;
  TSDL_PenButtonEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TUint32;
      which : TSDL_PenID;
      button : TUint8;
      state : TUint8;
      pen_state : TUint16;
      x : single;
      y : single;
      axes : array[0..(SDL_PEN_NUM_AXES)-1] of single;
    end;
{*
 *  An event used to drop text or request a file open by the system (event.drop.*)
 *
 *  The `data` is owned by SDL and should be copied if the application
 *  wants to hold onto it beyond the scope of handling this event.
  }
{*< ::SDL_EVENT_DROP_BEGIN or ::SDL_EVENT_DROP_FILE or ::SDL_EVENT_DROP_TEXT or ::SDL_EVENT_DROP_COMPLETE or ::SDL_EVENT_DROP_POSITION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window that was dropped on, if any  }
{*< X coordinate, relative to window (not on begin)  }
{*< Y coordinate, relative to window (not on begin)  }
{*< The source app that sent this drop event, or NULL if that isn't available  }
{*< The text for SDL_EVENT_DROP_TEXT and the file name for SDL_EVENT_DROP_FILE, NULL for other events  }

  PSDL_DropEvent = ^TSDL_DropEvent;
  TSDL_DropEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      x : single;
      y : single;
      source : Pchar;
      data : Pchar;
    end;
{*
 * An event triggered when the clipboard contents have changed (event.clipboard.*)
  }
{*< ::SDL_EVENT_CLIPBOARD_UPDATE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }

  PSDL_ClipboardEvent = ^TSDL_ClipboardEvent;
  TSDL_ClipboardEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
    end;
{*
 *  Sensor event structure (event.sensor.*)
  }
{*< ::SDL_EVENT_SENSOR_UPDATE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The instance ID of the sensor  }
{*< Up to 6 values from the sensor - additional values can be queried using SDL_GetSensorData()  }
{*< The timestamp of the sensor reading in nanoseconds, not necessarily synchronized with the system clock  }

  PSDL_SensorEvent = ^TSDL_SensorEvent;
  TSDL_SensorEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_SensorID;
      data : array[0..5] of single;
      sensor_timestamp : TUint64;
    end;
{*
 *  The "quit requested" event
  }
{*< ::SDL_EVENT_QUIT  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }

  PSDL_QuitEvent = ^TSDL_QuitEvent;
  TSDL_QuitEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
    end;
{*
 *  A user-defined event type (event.user.*)
  }
{*< ::SDL_EVENT_USER through ::SDL_EVENT_LAST-1  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The associated window if any  }
{*< User defined event code  }
{*< User defined data pointer  }
{*< User defined data pointer  }

  PSDL_UserEvent = ^TSDL_UserEvent;
  TSDL_UserEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      code : TSint32;
      data1 : pointer;
      data2 : pointer;
    end;
{*
 *  General event structure
  }
{*< Event type, shared with all events  }
{*< Common event data  }
{*< Display event data  }
{*< Window event data  }
{*< Keyboard event data  }
{*< Text editing event data  }
{*< Text input event data  }
{*< Mouse motion event data  }
{*< Mouse button event data  }
{*< Mouse wheel event data  }
{*< Joystick axis event data  }
{*< Joystick hat event data  }
{*< Joystick button event data  }
{*< Joystick device change event data  }
{*< Joystick battery event data  }
{*< Gamepad axis event data  }
{*< Gamepad button event data  }
{*< Gamepad device event data  }
{*< Gamepad touchpad event data  }
{*< Gamepad sensor event data  }
{*< Audio device event data  }
{*< Camera device event data  }
{*< Sensor event data  }
{*< Quit request event data  }
{*< Custom event data  }
{*< Touch finger event data  }
{*< Pen tip touching or leaving drawing surface  }
{*< Pen change in position, pressure, or angle  }
{*< Pen button press  }
{*< Drag and drop event data  }
{*< Clipboard event data  }
{ This is necessary for ABI compatibility between Visual C++ and GCC.
       Visual C++ will respect the push pack pragma and use 52 bytes (size of
       SDL_TextEditingEvent, the largest structure for 32-bit and 64-bit
       architectures) for this union, and GCC will use the alignment of the
       largest datatype within the union, which is 8 bytes on 64-bit
       architectures.

       So... we'll add padding to force the size to be the same for both.

       On architectures where pointers are 16 bytes, this needs rounding up to
       the next multiple of 16, 64, and on architectures where pointers are
       even larger the size of SDL_UserEvent will dominate as being 3 pointers.
     }

  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
      case longint of
        0 : ( _type : TUint32 );
        1 : ( common : TSDL_CommonEvent );
        2 : ( display : TSDL_DisplayEvent );
        3 : ( window : TSDL_WindowEvent );
        4 : ( key : TSDL_KeyboardEvent );
        5 : ( edit : TSDL_TextEditingEvent );
        6 : ( text : TSDL_TextInputEvent );
        7 : ( motion : TSDL_MouseMotionEvent );
        8 : ( button : TSDL_MouseButtonEvent );
        9 : ( wheel : TSDL_MouseWheelEvent );
        10 : ( jaxis : TSDL_JoyAxisEvent );
        11 : ( jhat : TSDL_JoyHatEvent );
        12 : ( jbutton : TSDL_JoyButtonEvent );
        13 : ( jdevice : TSDL_JoyDeviceEvent );
        14 : ( jbattery : TSDL_JoyBatteryEvent );
        15 : ( gaxis : TSDL_GamepadAxisEvent );
        16 : ( gbutton : TSDL_GamepadButtonEvent );
        17 : ( gdevice : TSDL_GamepadDeviceEvent );
        18 : ( gtouchpad : TSDL_GamepadTouchpadEvent );
        19 : ( gsensor : TSDL_GamepadSensorEvent );
        20 : ( adevice : TSDL_AudioDeviceEvent );
        21 : ( cdevice : TSDL_CameraDeviceEvent );
        22 : ( sensor : TSDL_SensorEvent );
        23 : ( quit : TSDL_QuitEvent );
        24 : ( user : TSDL_UserEvent );
        25 : ( tfinger : TSDL_TouchFingerEvent );
        26 : ( ptip : TSDL_PenTipEvent );
        27 : ( pmotion : TSDL_PenMotionEvent );
        28 : ( pbutton : TSDL_PenButtonEvent );
        29 : ( drop : TSDL_DropEvent );
        30 : ( clipboard : TSDL_ClipboardEvent );
        31 : ( padding : array[0..127] of TUint8 );
      end;
{ Make sure we haven't broken binary compatibility  }
{///SDL_COMPILE_TIME_ASSERT(SDL_Event, sizeof(SDL_Event) == sizeof(((SDL_Event *)NULL)->padding)); }
{ Function prototypes  }
{*
 * Pump the event loop, gathering events from the input devices.
 *
 * This function updates the event queue and internal input device state.
 *
 * **WARNING**: This should only be run in the thread that initialized the
 * video subsystem, and for extra safety, you should consider only doing those
 * things on the main thread in any case.
 *
 * SDL_PumpEvents() gathers all the pending input information from devices and
 * places it in the event queue. Without calls to SDL_PumpEvents() no events
 * would ever be placed on the queue. Often the need for calls to
 * SDL_PumpEvents() is hidden from the user since SDL_PollEvent() and
 * SDL_WaitEvent() implicitly call SDL_PumpEvents(). However, if you are not
 * polling or waiting for events (e.g. you are filtering them), then you must
 * call SDL_PumpEvents() to force an event queue update.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_WaitEvent
  }

procedure SDL_PumpEvents;cdecl;external;
{ @  }
type
  PSDL_eventaction = ^TSDL_eventaction;
  TSDL_eventaction =  Longint;
  Const
    SDL_ADDEVENT = 0;
    SDL_PEEKEVENT = 1;
    SDL_GETEVENT = 2;
;
{*
 * Check the event queue for messages and optionally return them.
 *
 * `action` may be any of the following:
 *
 * - `SDL_ADDEVENT`: up to `numevents` events will be added to the back of the
 *   event queue.
 * - `SDL_PEEKEVENT`: `numevents` events at the front of the event queue,
 *   within the specified minimum and maximum type, will be returned to the
 *   caller and will _not_ be removed from the queue.
 * - `SDL_GETEVENT`: up to `numevents` events at the front of the event queue,
 *   within the specified minimum and maximum type, will be returned to the
 *   caller and will be removed from the queue.
 *
 * You may have to call SDL_PumpEvents() before calling this function.
 * Otherwise, the events may not be ready to be filtered when you call
 * SDL_PeepEvents().
 *
 * This function is thread-safe.
 *
 * \param events destination buffer for the retrieved events
 * \param numevents if action is SDL_ADDEVENT, the number of events to add
 *                  back to the event queue; if action is SDL_PEEKEVENT or
 *                  SDL_GETEVENT, the maximum number of events to retrieve
 * \param action action to take; see [[#action|Remarks]] for details
 * \param minType minimum value of the event type to be considered;
 *                SDL_EVENT_FIRST is a safe choice
 * \param maxType maximum value of the event type to be considered;
 *                SDL_EVENT_LAST is a safe choice
 * \returns the number of events actually stored or a negative error code on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_PumpEvents
 * \sa SDL_PushEvent
  }

function SDL_PeepEvents(events:PSDL_Event; numevents:longint; action:TSDL_eventaction; minType:TUint32; maxType:TUint32):longint;cdecl;external;
{ @  }
{*
 * Check for the existence of a certain event type in the event queue.
 *
 * If you need to check for a range of event types, use SDL_HasEvents()
 * instead.
 *
 * \param type the type of event to be queried; see SDL_EventType for details
 * \returns SDL_TRUE if events matching `type` are present, or SDL_FALSE if
 *          events matching `type` are not present.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasEvents
  }
function SDL_HasEvent(_type:TUint32):TSDL_bool;cdecl;external;
{*
 * Check for the existence of certain event types in the event queue.
 *
 * If you need to check for a single event type, use SDL_HasEvent() instead.
 *
 * \param minType the low end of event type to be queried, inclusive; see
 *                SDL_EventType for details
 * \param maxType the high end of event type to be queried, inclusive; see
 *                SDL_EventType for details
 * \returns SDL_TRUE if events with type >= `minType` and <= `maxType` are
 *          present, or SDL_FALSE if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasEvents
  }
function SDL_HasEvents(minType:TUint32; maxType:TUint32):TSDL_bool;cdecl;external;
{*
 * Clear events of a specific type from the event queue.
 *
 * This will unconditionally remove any events from the queue that match
 * `type`. If you need to remove a range of event types, use SDL_FlushEvents()
 * instead.
 *
 * It's also normal to just ignore events you don't care about in your event
 * loop without calling this function.
 *
 * This function only affects currently queued events. If you want to make
 * sure that all pending OS events are flushed, you can call SDL_PumpEvents()
 * on the main thread immediately before the flush call.
 *
 * If you have user events with custom data that needs to be freed, you should
 * use SDL_PeepEvents() to remove and clean up those events before calling
 * this function.
 *
 * \param type the type of event to be cleared; see SDL_EventType for details
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_FlushEvents
  }
procedure SDL_FlushEvent(_type:TUint32);cdecl;external;
{*
 * Clear events of a range of types from the event queue.
 *
 * This will unconditionally remove any events from the queue that are in the
 * range of `minType` to `maxType`, inclusive. If you need to remove a single
 * event type, use SDL_FlushEvent() instead.
 *
 * It's also normal to just ignore events you don't care about in your event
 * loop without calling this function.
 *
 * This function only affects currently queued events. If you want to make
 * sure that all pending OS events are flushed, you can call SDL_PumpEvents()
 * on the main thread immediately before the flush call.
 *
 * \param minType the low end of event type to be cleared, inclusive; see
 *                SDL_EventType for details
 * \param maxType the high end of event type to be cleared, inclusive; see
 *                SDL_EventType for details
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_FlushEvent
  }
procedure SDL_FlushEvents(minType:TUint32; maxType:TUint32);cdecl;external;
{*
 * Poll for currently pending events.
 *
 * If `event` is not NULL, the next event is removed from the queue and stored
 * in the SDL_Event structure pointed to by `event`. The 1 returned refers to
 * this event, immediately stored in the SDL Event structure -- not an event
 * to follow.
 *
 * If `event` is NULL, it simply returns 1 if there is an event in the queue,
 * but will not remove it from the queue.
 *
 * As this function may implicitly call SDL_PumpEvents(), you can only call
 * this function in the thread that set the video mode.
 *
 * SDL_PollEvent() is the favored way of receiving system events since it can
 * be done from the main loop and does not suspend the main loop while waiting
 * on an event to be posted.
 *
 * The common practice is to fully process the event queue once every frame,
 * usually as a first step before updating the game's state:
 *
 * ```c
 * while (game_is_still_running) 
 *     SDL_Event event;
 *     while (SDL_PollEvent(&event))   // poll until all events are handled!
 *         // decide what to do with this event.
 *     
 *
 *     // update game state, draw the current frame
 * 
 * ```
 *
 * \param event the SDL_Event structure to be filled with the next event from
 *              the queue, or NULL
 * \returns SDL_TRUE if this got an event or SDL_FALSE if there are none
 *          available.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PushEvent
 * \sa SDL_WaitEvent
 * \sa SDL_WaitEventTimeout
  }
function SDL_PollEvent(event:PSDL_Event):TSDL_bool;cdecl;external;
{*
 * Wait indefinitely for the next available event.
 *
 * If `event` is not NULL, the next event is removed from the queue and stored
 * in the SDL_Event structure pointed to by `event`.
 *
 * As this function may implicitly call SDL_PumpEvents(), you can only call
 * this function in the thread that initialized the video subsystem.
 *
 * \param event the SDL_Event structure to be filled in with the next event
 *              from the queue, or NULL
 * \returns SDL_TRUE on success or SDL_FALSE if there was an error while
 *          waiting for events; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_PushEvent
 * \sa SDL_WaitEventTimeout
  }
function SDL_WaitEvent(event:PSDL_Event):TSDL_bool;cdecl;external;
{*
 * Wait until the specified timeout (in milliseconds) for the next available
 * event.
 *
 * If `event` is not NULL, the next event is removed from the queue and stored
 * in the SDL_Event structure pointed to by `event`.
 *
 * As this function may implicitly call SDL_PumpEvents(), you can only call
 * this function in the thread that initialized the video subsystem.
 *
 * The timeout is not guaranteed, the actual wait time could be longer due to
 * system scheduling.
 *
 * \param event the SDL_Event structure to be filled in with the next event
 *              from the queue, or NULL
 * \param timeoutMS the maximum number of milliseconds to wait for the next
 *                  available event
 * \returns SDL_TRUE if this got an event or SDL_FALSE if the timeout elapsed
 *          without any events available.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_PushEvent
 * \sa SDL_WaitEvent
  }
function SDL_WaitEventTimeout(event:PSDL_Event; timeoutMS:TSint32):TSDL_bool;cdecl;external;
{*
 * Add an event to the event queue.
 *
 * The event queue can actually be used as a two way communication channel.
 * Not only can events be read from the queue, but the user can also push
 * their own events onto it. `event` is a pointer to the event structure you
 * wish to push onto the queue. The event is copied into the queue, and the
 * caller may dispose of the memory pointed to after SDL_PushEvent() returns.
 *
 * Note: Pushing device input events onto the queue doesn't modify the state
 * of the device within SDL.
 *
 * This function is thread-safe, and can be called from other threads safely.
 *
 * Note: Events pushed onto the queue with SDL_PushEvent() get passed through
 * the event filter but events added with SDL_PeepEvents() do not.
 *
 * For pushing application-specific events, please use SDL_RegisterEvents() to
 * get an event type that does not conflict with other code that also wants
 * its own custom event types.
 *
 * \param event the SDL_Event to be added to the queue
 * \returns 1 on success, 0 if the event was filtered, or a negative error
 *          code on failure; call SDL_GetError() for more information. A
 *          common reason for error is the event queue being full.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PeepEvents
 * \sa SDL_PollEvent
 * \sa SDL_RegisterEvents
  }
function SDL_PushEvent(event:PSDL_Event):longint;cdecl;external;
{*
 * A function pointer used for callbacks that watch the event queue.
 *
 * \param userdata what was passed as `userdata` to SDL_SetEventFilter()
 *        or SDL_AddEventWatch, etc
 * \param event the event that triggered the callback
 * \returns 1 to permit event to be added to the queue, and 0 to disallow
 *          it. When used with SDL_AddEventWatch, the return value is ignored.
 *
 * \sa SDL_SetEventFilter
 * \sa SDL_AddEventWatch
  }
type

  TSDL_EventFilter = function (userdata:pointer; event:PSDL_Event):longint;cdecl;
{*
 * Set up a filter to process all events before they change internal state and
 * are posted to the internal event queue.
 *
 * If the filter function returns 1 when called, then the event will be added
 * to the internal queue. If it returns 0, then the event will be dropped from
 * the queue, but the internal state will still be updated. This allows
 * selective filtering of dynamically arriving events.
 *
 * **WARNING**: Be very careful of what you do in the event filter function,
 * as it may run in a different thread!
 *
 * On platforms that support it, if the quit event is generated by an
 * interrupt signal (e.g. pressing Ctrl-C), it will be delivered to the
 * application at the next event poll.
 *
 * There is one caveat when dealing with the ::SDL_QuitEvent event type. The
 * event filter is only called when the window manager desires to close the
 * application window. If the event filter returns 1, then the window will be
 * closed, otherwise the window will remain open if possible.
 *
 * Note: Disabled events never make it to the event filter function; see
 * SDL_SetEventEnabled().
 *
 * Note: If you just want to inspect events without filtering, you should use
 * SDL_AddEventWatch() instead.
 *
 * Note: Events pushed onto the queue with SDL_PushEvent() get passed through
 * the event filter, but events pushed onto the queue with SDL_PeepEvents() do
 * not.
 *
 * \param filter An SDL_EventFilter function to call when an event happens
 * \param userdata a pointer that is passed to `filter`
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_AddEventWatch
 * \sa SDL_SetEventEnabled
 * \sa SDL_GetEventFilter
 * \sa SDL_PeepEvents
 * \sa SDL_PushEvent
  }

procedure SDL_SetEventFilter(filter:TSDL_EventFilter; userdata:pointer);cdecl;external;
{*
 * Query the current event filter.
 *
 * This function can be used to "chain" filters, by saving the existing filter
 * before replacing it with a function that will call that saved filter.
 *
 * \param filter the current callback function will be stored here
 * \param userdata the pointer that is passed to the current event filter will
 *                 be stored here
 * \returns SDL_TRUE on success or SDL_FALSE if there is no event filter set.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetEventFilter
  }
function SDL_GetEventFilter(filter:PSDL_EventFilter; userdata:Ppointer):TSDL_bool;cdecl;external;
{*
 * Add a callback to be triggered when an event is added to the event queue.
 *
 * `filter` will be called when an event happens, and its return value is
 * ignored.
 *
 * **WARNING**: Be very careful of what you do in the event filter function,
 * as it may run in a different thread!
 *
 * If the quit event is generated by a signal (e.g. SIGINT), it will bypass
 * the internal queue and be delivered to the watch callback immediately, and
 * arrive at the next event poll.
 *
 * Note: the callback is called for events posted by the user through
 * SDL_PushEvent(), but not for disabled events, nor for events by a filter
 * callback set with SDL_SetEventFilter(), nor for events posted by the user
 * through SDL_PeepEvents().
 *
 * \param filter an SDL_EventFilter function to call when an event happens.
 * \param userdata a pointer that is passed to `filter`
 * \returns 0 on success, or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DelEventWatch
 * \sa SDL_SetEventFilter
  }
function SDL_AddEventWatch(filter:TSDL_EventFilter; userdata:pointer):longint;cdecl;external;
{*
 * Remove an event watch callback added with SDL_AddEventWatch().
 *
 * This function takes the same input as SDL_AddEventWatch() to identify and
 * delete the corresponding callback.
 *
 * \param filter the function originally passed to SDL_AddEventWatch()
 * \param userdata the pointer originally passed to SDL_AddEventWatch()
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_AddEventWatch
  }
procedure SDL_DelEventWatch(filter:TSDL_EventFilter; userdata:pointer);cdecl;external;
{*
 * Run a specific filter function on the current event queue, removing any
 * events for which the filter returns 0.
 *
 * See SDL_SetEventFilter() for more information. Unlike SDL_SetEventFilter(),
 * this function does not change the filter permanently, it only uses the
 * supplied filter until this function returns.
 *
 * \param filter the SDL_EventFilter function to call when an event happens
 * \param userdata a pointer that is passed to `filter`
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEventFilter
 * \sa SDL_SetEventFilter
  }
procedure SDL_FilterEvents(filter:TSDL_EventFilter; userdata:pointer);cdecl;external;
{*
 * Set the state of processing events by type.
 *
 * \param type the type of event; see SDL_EventType for details
 * \param enabled whether to process the event or not
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_IsEventEnabled
  }
procedure SDL_SetEventEnabled(_type:TUint32; enabled:TSDL_bool);cdecl;external;
{*
 * Query the state of processing events by type.
 *
 * \param type the type of event; see SDL_EventType for details
 * \returns SDL_TRUE if the event is being processed, SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetEventEnabled
  }
function SDL_EventEnabled(_type:TUint32):TSDL_bool;cdecl;external;
{*
 * Allocate a set of user-defined events, and return the beginning event
 * number for that set of events.
 *
 * Calling this function with `numevents` <= 0 is an error and will return
 * (Uint32)-1.
 *
 * Note, (Uint32)-1 means the maximum unsigned 32-bit integer value (or
 * 0xFFFFFFFF), but is clearer to write.
 *
 * \param numevents the number of events to be allocated
 * \returns the beginning event number, or (Uint32)-1 if there are not enough
 *          user-defined events left.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PushEvent
  }
function SDL_RegisterEvents(numevents:longint):TUint32;cdecl;external;
{*
 * Allocate dynamic memory for an SDL event
 *
 * You can use this to allocate memory for user events that will be
 * automatically freed after the event is processed.
 *
 * \param size the amount of memory to allocate
 * \returns a pointer to the memory allocated or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_AllocateEventMemory(size:Tsize_t):pointer;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_events_h_  }

implementation


end.
