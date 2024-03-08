unit SDL_keyboard;

interface

uses
SDL3_stdinc,SDL3_rect,  SDL_scancode, SDL_keycode, SDL3_video;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_keyboard_h_}
//{$define SDL_keyboard_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_keycode.h>}
//{$include <SDL3/SDL_video.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 *  The SDL keysym structure, used in key events.
 *
 *  \note  If you are looking for translated character input, see the ::SDL_EVENT_TEXT_INPUT event.
  }
{*< SDL physical key code - see ::SDL_Scancode for details  }
{*< SDL virtual key code - see ::SDL_Keycode for details  }
{*< current key modifiers  }
type
  PSDL_Keysym = ^TSDL_Keysym;
  TSDL_Keysym = record
      scancode : TSDL_Scancode;
      sym : TSDL_Keycode;
      mod_ : Uint16;
      unused : Uint32;
    end;
{ Function prototypes  }
{*
 * Query the window which currently has keyboard focus.
 *
 * \returns the window with keyboard focus.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetKeyboardFocus:PSDL_Window;cdecl;external;
{*
 * Get a snapshot of the current state of the keyboard.
 *
 * The pointer returned is a pointer to an internal SDL array. It will be
 * valid for the whole lifetime of the application and should not be freed by
 * the caller.
 *
 * A array element with a value of 1 means that the key is pressed and a value
 * of 0 means that it is not. Indexes into this array are obtained by using
 * SDL_Scancode values.
 *
 * Use SDL_PumpEvents() to update the state array.
 *
 * This function gives you the current state after all events have been
 * processed, so if a key or button has been pressed and released before you
 * process events, then the pressed state will never show up in the
 * SDL_GetKeyboardState() calls.
 *
 * Note: This function doesn't take into account whether shift has been
 * pressed or not.
 *
 * \param numkeys if non-NULL, receives the length of the returned array
 * \returns a pointer to an array of key states.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PumpEvents
 * \sa SDL_ResetKeyboard
  }
(* Const before type ignored *)
function SDL_GetKeyboardState(numkeys:Plongint):PUint8;cdecl;external;
{*
 * Clear the state of the keyboard
 *
 * This function will generate key up events for all pressed keys.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyboardState
  }
procedure SDL_ResetKeyboard;cdecl;external;
{*
 * Get the current key modifier state for the keyboard.
 *
 * \returns an OR'd combination of the modifier keys for the keyboard. See
 *          SDL_Keymod for details.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyboardState
 * \sa SDL_SetModState
  }
function SDL_GetModState:TSDL_Keymod;cdecl;external;
{*
 * Set the current key modifier state for the keyboard.
 *
 * The inverse of SDL_GetModState(), SDL_SetModState() allows you to impose
 * modifier key states on your application. Simply pass your desired modifier
 * states into `modstate`. This value may be a bitwise, OR'd combination of
 * SDL_Keymod values.
 *
 * This does not change the keyboard state, only the key modifier flags that
 * SDL reports.
 *
 * \param modstate the desired SDL_Keymod for the keyboard
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetModState
  }
procedure SDL_SetModState(modstate:TSDL_Keymod);cdecl;external;
{*
 * Get the key code corresponding to the given scancode according to the
 * current keyboard layout.
 *
 * See SDL_Keycode for details.
 *
 * \param scancode the desired SDL_Scancode to query
 * \returns the SDL_Keycode that corresponds to the given SDL_Scancode.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyName
 * \sa SDL_GetScancodeFromKey
  }
function SDL_GetKeyFromScancode(scancode:TSDL_Scancode):TSDL_Keycode;cdecl;external;
{*
 * Get the scancode corresponding to the given key code according to the
 * current keyboard layout.
 *
 * See SDL_Scancode for details.
 *
 * \param key the desired SDL_Keycode to query
 * \returns the SDL_Scancode that corresponds to the given SDL_Keycode.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyFromScancode
 * \sa SDL_GetScancodeName
  }
function SDL_GetScancodeFromKey(key:TSDL_Keycode):TSDL_Scancode;cdecl;external;
{*
 * Get a human-readable name for a scancode.
 *
 * See SDL_Scancode for details.
 *
 * **Warning**: The returned name is by design not stable across platforms,
 * e.g. the name for `SDL_SCANCODE_LGUI` is "Left GUI" under Linux but "Left
 * Windows" under Microsoft Windows, and some scancodes like
 * `SDL_SCANCODE_NONUSBACKSLASH` don't have any name at all. There are even
 * scancodes that share names, e.g. `SDL_SCANCODE_RETURN` and
 * `SDL_SCANCODE_RETURN2` (both called "Return"). This function is therefore
 * unsuitable for creating a stable cross-platform two-way mapping between
 * strings and scancodes.
 *
 * \param scancode the desired SDL_Scancode to query
 * \returns a pointer to the name for the scancode. If the scancode doesn't
 *          have a name this function returns an empty string ("").
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetScancodeFromKey
 * \sa SDL_GetScancodeFromName
  }
(* Const before type ignored *)
function SDL_GetScancodeName(scancode:TSDL_Scancode):Pchar;cdecl;external;
{*
 * Get a scancode from a human-readable name.
 *
 * \param name the human-readable scancode name
 * \returns the SDL_Scancode, or `SDL_SCANCODE_UNKNOWN` if the name wasn't
 *          recognized; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyFromName
 * \sa SDL_GetScancodeFromKey
 * \sa SDL_GetScancodeName
  }
(* Const before type ignored *)
function SDL_GetScancodeFromName(name:Pchar):TSDL_Scancode;cdecl;external;
{*
 * Get a human-readable name for a key.
 *
 * See SDL_Scancode and SDL_Keycode for details.
 *
 * \param key the desired SDL_Keycode to query
 * \returns a pointer to a UTF-8 string that stays valid at least until the
 *          next call to this function. If you need it around any longer, you
 *          must copy it. If the key doesn't have a name, this function
 *          returns an empty string ("").
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyFromName
 * \sa SDL_GetKeyFromScancode
 * \sa SDL_GetScancodeFromKey
  }
(* Const before type ignored *)
function SDL_GetKeyName(key:TSDL_Keycode):Pchar;cdecl;external;
{*
 * Get a key code from a human-readable name.
 *
 * \param name the human-readable key name
 * \returns key code, or `SDLK_UNKNOWN` if the name wasn't recognized; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyFromScancode
 * \sa SDL_GetKeyName
 * \sa SDL_GetScancodeFromName
  }
(* Const before type ignored *)
function SDL_GetKeyFromName(name:Pchar):TSDL_Keycode;cdecl;external;
{*
 * Start accepting Unicode text input events.
 *
 * This function will start accepting Unicode text input events in the focused
 * SDL window, and start emitting SDL_TextInputEvent (SDL_EVENT_TEXT_INPUT)
 * and SDL_TextEditingEvent (SDL_EVENT_TEXT_EDITING) events. Please use this
 * function in pair with SDL_StopTextInput().
 *
 * Text input events are received by default.
 *
 * On some platforms using this function activates the screen keyboard.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetTextInputRect
 * \sa SDL_StopTextInput
  }
procedure SDL_StartTextInput;cdecl;external;
{*
 * Check whether or not Unicode text input events are enabled.
 *
 * \returns SDL_TRUE if text input events are enabled else SDL_FALSE.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StartTextInput
  }
function SDL_TextInputActive:TSDL_bool;cdecl;external;
{*
 * Stop receiving any text input events.
 *
 * Text input events are received by default.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StartTextInput
  }
procedure SDL_StopTextInput;cdecl;external;
{*
 * Dismiss the composition window/IME without disabling the subsystem.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StartTextInput
 * \sa SDL_StopTextInput
  }
procedure SDL_ClearComposition;cdecl;external;
{*
 * Returns if an IME Composite or Candidate window is currently shown.
 *
 * \returns SDL_TRUE if shown, else SDL_FALSE
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_TextInputShown:TSDL_bool;cdecl;external;
{*
 * Set the rectangle used to type Unicode text inputs.
 *
 * Native input methods will place a window with word suggestions near it,
 * without covering the text being inputted.
 *
 * To start text input in a given location, this function is intended to be
 * called before SDL_StartTextInput, although some platforms support moving
 * the rectangle even while text input (and a composition) is active.
 *
 * Note: If you want to use the system native IME window, try setting hint
 * **SDL_HINT_IME_SHOW_UI** to **1**, otherwise this function won't give you
 * any feedback.
 *
 * \param rect the SDL_Rect structure representing the rectangle to receive
 *             text (ignored if NULL)
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StartTextInput
  }
(* Const before type ignored *)
function SDL_SetTextInputRect(rect:PSDL_Rect):longint;cdecl;external;
{*
 * Check whether the platform has screen keyboard support.
 *
 * \returns SDL_TRUE if the platform has some screen keyboard support or
 *          SDL_FALSE if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_StartTextInput
 * \sa SDL_ScreenKeyboardShown
  }
function SDL_HasScreenKeyboardSupport:TSDL_bool;cdecl;external;
{*
 * Check whether the screen keyboard is shown for given window.
 *
 * \param window the window for which screen keyboard should be queried
 * \returns SDL_TRUE if screen keyboard is shown or SDL_FALSE if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasScreenKeyboardSupport
  }
function SDL_ScreenKeyboardShown(window:PSDL_Window):TSDL_bool;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_keyboard_h_  }

implementation


end.
