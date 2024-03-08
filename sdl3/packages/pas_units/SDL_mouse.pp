unit SDL_mouse;

interface

uses
 SDL3_stdinc, SDL3_video, SDL3_surface;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_mouse_h_}
//{$define SDL_mouse_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_video.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
type
  PSDL_MouseID = ^TSDL_MouseID;
  TSDL_MouseID = Uint32;
{*< Implementation dependent  }
{*
 * Cursor types for SDL_CreateSystemCursor().
  }
{*< Arrow  }
{*< I-beam  }
{*< Wait  }
{*< Crosshair  }
{*< Small wait cursor (or Wait if not available)  }
{*< Double arrow pointing northwest and southeast  }
{*< Double arrow pointing northeast and southwest  }
{*< Double arrow pointing west and east  }
{*< Double arrow pointing north and south  }
{*< Four pointed arrow pointing north, south, east, and west  }
{*< Slashed circle or crossbones  }
{*< Hand  }
{*< Window resize top-left (or SIZENWSE)  }
{*< Window resize top (or SIZENS)  }
{*< Window resize top-right (or SIZENESW)  }
{*< Window resize right (or SIZEWE)  }
{*< Window resize bottom-right (or SIZENWSE)  }
{*< Window resize bottom (or SIZENS)  }
{*< Window resize bottom-left (or SIZENESW)  }
{*< Window resize left (or SIZEWE)  }

  PSDL_Cursor = Pointer;

  PSDL_SystemCursor = ^TSDL_SystemCursor;
  TSDL_SystemCursor =  Longint;
  Const
    SDL_SYSTEM_CURSOR_ARROW = 0;
    SDL_SYSTEM_CURSOR_IBEAM = 1;
    SDL_SYSTEM_CURSOR_WAIT = 2;
    SDL_SYSTEM_CURSOR_CROSSHAIR = 3;
    SDL_SYSTEM_CURSOR_WAITARROW = 4;
    SDL_SYSTEM_CURSOR_SIZENWSE = 5;
    SDL_SYSTEM_CURSOR_SIZENESW = 6;
    SDL_SYSTEM_CURSOR_SIZEWE = 7;
    SDL_SYSTEM_CURSOR_SIZENS = 8;
    SDL_SYSTEM_CURSOR_SIZEALL = 9;
    SDL_SYSTEM_CURSOR_NO = 10;
    SDL_SYSTEM_CURSOR_HAND = 11;
    SDL_SYSTEM_CURSOR_WINDOW_TOPLEFT = 12;
    SDL_SYSTEM_CURSOR_WINDOW_TOP = 13;
    SDL_SYSTEM_CURSOR_WINDOW_TOPRIGHT = 14;
    SDL_SYSTEM_CURSOR_WINDOW_RIGHT = 15;
    SDL_SYSTEM_CURSOR_WINDOW_BOTTOMRIGHT = 16;
    SDL_SYSTEM_CURSOR_WINDOW_BOTTOM = 17;
    SDL_SYSTEM_CURSOR_WINDOW_BOTTOMLEFT = 18;
    SDL_SYSTEM_CURSOR_WINDOW_LEFT = 19;
    SDL_NUM_SYSTEM_CURSORS = 20;

{*
 * Scroll direction types for the Scroll event
  }
{*< The scroll direction is normal  }
{*< The scroll direction is flipped / natural  }
type
  PSDL_MouseWheelDirection = ^TSDL_MouseWheelDirection;
  TSDL_MouseWheelDirection =  Longint;
  Const
    SDL_MOUSEWHEEL_NORMAL = 0;
    SDL_MOUSEWHEEL_FLIPPED = 1;

{ Function prototypes  }
{*
 * Get the window which currently has mouse focus.
 *
 * \returns the window with mouse focus.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetMouseFocus:PSDL_Window;cdecl;external;
{*
 * Retrieve the current state of the mouse.
 *
 * The current button state is returned as a button bitmask, which can be
 * tested using the `SDL_BUTTON(X)` macros (where `X` is generally 1 for the
 * left, 2 for middle, 3 for the right button), and `x` and `y` are set to the
 * mouse cursor position relative to the focus window. You can pass NULL for
 * either `x` or `y`.
 *
 * \param x the x coordinate of the mouse cursor position relative to the
 *          focus window
 * \param y the y coordinate of the mouse cursor position relative to the
 *          focus window
 * \returns a 32-bit button bitmask of the current button state.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetGlobalMouseState
 * \sa SDL_GetRelativeMouseState
 * \sa SDL_PumpEvents
  }
function SDL_GetMouseState(x:Psingle; y:Psingle):Uint32;cdecl;external;
{*
 * Get the current state of the mouse in relation to the desktop.
 *
 * This works similarly to SDL_GetMouseState(), but the coordinates will be
 * reported relative to the top-left of the desktop. This can be useful if you
 * need to track the mouse outside of a specific window and SDL_CaptureMouse()
 * doesn't fit your needs. For example, it could be useful if you need to
 * track the mouse while dragging a window, where coordinates relative to a
 * window might not be in sync at all times.
 *
 * Note: SDL_GetMouseState() returns the mouse position as SDL understands it
 * from the last pump of the event queue. This function, however, queries the
 * OS for the current mouse position, and as such, might be a slightly less
 * efficient function. Unless you know what you're doing and have a good
 * reason to use this function, you probably want SDL_GetMouseState() instead.
 *
 * \param x filled in with the current X coord relative to the desktop; can be
 *          NULL
 * \param y filled in with the current Y coord relative to the desktop; can be
 *          NULL
 * \returns the current button state as a bitmask which can be tested using
 *          the SDL_BUTTON(X) macros.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CaptureMouse
  }
function SDL_GetGlobalMouseState(x:Psingle; y:Psingle):Uint32;cdecl;external;
{*
 * Retrieve the relative state of the mouse.
 *
 * The current button state is returned as a button bitmask, which can be
 * tested using the `SDL_BUTTON(X)` macros (where `X` is generally 1 for the
 * left, 2 for middle, 3 for the right button), and `x` and `y` are set to the
 * mouse deltas since the last call to SDL_GetRelativeMouseState() or since
 * event initialization. You can pass NULL for either `x` or `y`.
 *
 * \param x a pointer filled with the last recorded x coordinate of the mouse
 * \param y a pointer filled with the last recorded y coordinate of the mouse
 * \returns a 32-bit button bitmask of the relative button state.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetMouseState
  }
function SDL_GetRelativeMouseState(x:Psingle; y:Psingle):Uint32;cdecl;external;
{*
 * Move the mouse cursor to the given position within the window.
 *
 * This function generates a mouse motion event if relative mode is not
 * enabled. If relative mode is enabled, you can force mouse events for the
 * warp by setting the SDL_HINT_MOUSE_RELATIVE_WARP_MOTION hint.
 *
 * Note that this function will appear to succeed, but not actually move the
 * mouse when used over Microsoft Remote Desktop.
 *
 * \param window the window to move the mouse into, or NULL for the current
 *               mouse focus
 * \param x the x coordinate within the window
 * \param y the y coordinate within the window
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_WarpMouseGlobal
  }
procedure SDL_WarpMouseInWindow(window:PSDL_Window; x:single; y:single);cdecl;external;
{*
 * Move the mouse to the given position in global screen space.
 *
 * This function generates a mouse motion event.
 *
 * A failure of this function usually means that it is unsupported by a
 * platform.
 *
 * Note that this function will appear to succeed, but not actually move the
 * mouse when used over Microsoft Remote Desktop.
 *
 * \param x the x coordinate
 * \param y the y coordinate
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_WarpMouseInWindow
  }
function SDL_WarpMouseGlobal(x:single; y:single):longint;cdecl;external;
{*
 * Set relative mouse mode.
 *
 * While the mouse is in relative mode, the cursor is hidden, the mouse
 * position is constrained to the window, and SDL will report continuous
 * relative mouse motion even if the mouse is at the edge of the window.
 *
 * This function will flush any pending mouse motion.
 *
 * \param enabled SDL_TRUE to enable relative mode, SDL_FALSE to disable.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetRelativeMouseMode
  }
function SDL_SetRelativeMouseMode(enabled:TSDL_bool):longint;cdecl;external;
{*
 * Capture the mouse and to track input outside an SDL window.
 *
 * Capturing enables your app to obtain mouse events globally, instead of just
 * within your window. Not all video targets support this function. When
 * capturing is enabled, the current window will get all mouse events, but
 * unlike relative mode, no change is made to the cursor and it is not
 * restrained to your window.
 *
 * This function may also deny mouse input to other windows--both those in
 * your application and others on the system--so you should use this function
 * sparingly, and in small bursts. For example, you might want to track the
 * mouse while the user is dragging something, until the user releases a mouse
 * button. It is not recommended that you capture the mouse for long periods
 * of time, such as the entire time your app is running. For that, you should
 * probably use SDL_SetRelativeMouseMode() or SDL_SetWindowGrab(), depending
 * on your goals.
 *
 * While captured, mouse events still report coordinates relative to the
 * current (foreground) window, but those coordinates may be outside the
 * bounds of the window (including negative values). Capturing is only allowed
 * for the foreground window. If the window loses focus while capturing, the
 * capture will be disabled automatically.
 *
 * While capturing is enabled, the current window will have the
 * `SDL_WINDOW_MOUSE_CAPTURE` flag set.
 *
 * Please note that as of SDL 2.0.22, SDL will attempt to "auto capture" the
 * mouse while the user is pressing a button; this is to try and make mouse
 * behavior more consistent between platforms, and deal with the common case
 * of a user dragging the mouse outside of the window. This means that if you
 * are calling SDL_CaptureMouse() only to deal with this situation, you no
 * longer have to (although it is safe to do so). If this causes problems for
 * your app, you can disable auto capture by setting the
 * `SDL_HINT_MOUSE_AUTO_CAPTURE` hint to zero.
 *
 * \param enabled SDL_TRUE to enable capturing, SDL_FALSE to disable.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetGlobalMouseState
  }
function SDL_CaptureMouse(enabled:TSDL_bool):longint;cdecl;external;
{*
 * Query whether relative mouse mode is enabled.
 *
 * \returns SDL_TRUE if relative mode is enabled or SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetRelativeMouseMode
  }
function SDL_GetRelativeMouseMode:TSDL_bool;cdecl;external;
{*
 * Create a cursor using the specified bitmap data and mask (in MSB format).
 *
 * `mask` has to be in MSB (Most Significant Bit) format.
 *
 * The cursor width (`w`) must be a multiple of 8 bits.
 *
 * The cursor is created in black and white according to the following:
 *
 * - data=0, mask=1: white
 * - data=1, mask=1: black
 * - data=0, mask=0: transparent
 * - data=1, mask=0: inverted color if possible, black if not.
 *
 * Cursors created with this function must be freed with SDL_DestroyCursor().
 *
 * If you want to have a color cursor, or create your cursor from an
 * SDL_Surface, you should use SDL_CreateColorCursor(). Alternately, you can
 * hide the cursor and draw your own as part of your game's rendering, but it
 * will be bound to the framerate.
 *
 * Also, since SDL 2.0.0, SDL_CreateSystemCursor() is available, which
 * provides twelve readily available system cursors to pick from.
 *
 * \param data the color value for each pixel of the cursor
 * \param mask the mask value for each pixel of the cursor
 * \param w the width of the cursor
 * \param h the height of the cursor
 * \param hot_x the X-axis location of the upper left corner of the cursor
 *              relative to the actual mouse position
 * \param hot_y the Y-axis location of the upper left corner of the cursor
 *              relative to the actual mouse position
 * \returns a new cursor with the specified parameters on success or NULL on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DestroyCursor
 * \sa SDL_SetCursor
  }
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_CreateCursor(data:PUint8; mask:PUint8; w:longint; h:longint; hot_x:longint; 
           hot_y:longint):PSDL_Cursor;cdecl;external;
{*
 * Create a color cursor.
 *
 * \param surface an SDL_Surface structure representing the cursor image
 * \param hot_x the x position of the cursor hot spot
 * \param hot_y the y position of the cursor hot spot
 * \returns the new cursor on success or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateCursor
 * \sa SDL_DestroyCursor
  }
function SDL_CreateColorCursor(surface:PSDL_Surface; hot_x:longint; hot_y:longint):PSDL_Cursor;cdecl;external;
{*
 * Create a system cursor.
 *
 * \param id an SDL_SystemCursor enum value
 * \returns a cursor on success or NULL on failure; call SDL_GetError() for
 *          more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DestroyCursor
  }
function SDL_CreateSystemCursor(id:TSDL_SystemCursor):PSDL_Cursor;cdecl;external;
{*
 * Set the active cursor.
 *
 * This function sets the currently active cursor to the specified one. If the
 * cursor is currently visible, the change will be immediately represented on
 * the display. SDL_SetCursor(NULL) can be used to force cursor redraw, if
 * this is desired for any reason.
 *
 * \param cursor a cursor to make active
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateCursor
 * \sa SDL_GetCursor
  }
function SDL_SetCursor(cursor:PSDL_Cursor):longint;cdecl;external;
{*
 * Get the active cursor.
 *
 * This function returns a pointer to the current cursor which is owned by the
 * library. It is not necessary to free the cursor with SDL_DestroyCursor().
 *
 * \returns the active cursor or NULL if there is no mouse.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetCursor
  }
function SDL_GetCursor:PSDL_Cursor;cdecl;external;
{*
 * Get the default cursor.
 *
 * You do not have to call SDL_DestroyCursor() on the return value, but it is
 * safe to do so.
 *
 * \returns the default cursor on success or NULL on failure.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateSystemCursor
  }
function SDL_GetDefaultCursor:PSDL_Cursor;cdecl;external;
{*
 * Free a previously-created cursor.
 *
 * Use this function to free cursor resources created with SDL_CreateCursor(),
 * SDL_CreateColorCursor() or SDL_CreateSystemCursor().
 *
 * \param cursor the cursor to free
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateColorCursor
 * \sa SDL_CreateCursor
 * \sa SDL_CreateSystemCursor
  }
procedure SDL_DestroyCursor(cursor:PSDL_Cursor);cdecl;external;
{*
 * Show the cursor.
 *
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CursorVisible
 * \sa SDL_HideCursor
  }
function SDL_ShowCursor:longint;cdecl;external;
{*
 * Hide the cursor.
 *
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CursorVisible
 * \sa SDL_ShowCursor
  }
function SDL_HideCursor:longint;cdecl;external;
{*
 * Return whether the cursor is currently being shown.
 *
 * \returns `SDL_TRUE` if the cursor is being shown, or `SDL_FALSE` if the
 *          cursor is hidden.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HideCursor
 * \sa SDL_ShowCursor
  }
function SDL_CursorVisible:TSDL_bool;cdecl;external;
{*
 * Used as a mask when testing buttons in buttonstate.
 *
 * - Button 1:  Left mouse button
 * - Button 2:  Middle mouse button
 * - Button 3:  Right mouse button
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_BUTTON(X : longint) : longint;

const
  SDL_BUTTON_LEFT = 1;  
  SDL_BUTTON_MIDDLE = 2;  
  SDL_BUTTON_RIGHT = 3;  
  SDL_BUTTON_X1 = 4;  
  SDL_BUTTON_X2 = 5;  

{ was #define dname def_expr }
function SDL_BUTTON_LMASK : longint; { return type might be wrong }

{ was #define dname def_expr }
function SDL_BUTTON_MMASK : longint; { return type might be wrong }

{ was #define dname def_expr }
function SDL_BUTTON_RMASK : longint; { return type might be wrong }

{ was #define dname def_expr }
function SDL_BUTTON_X1MASK : longint; { return type might be wrong }

{ was #define dname def_expr }
function SDL_BUTTON_X2MASK : longint; { return type might be wrong }

{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_mouse_h_  }

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_BUTTON(X : longint) : longint;
begin
  SDL_BUTTON:=1 shl ((X)-1);
end;

{ was #define dname def_expr }
function SDL_BUTTON_LMASK : longint; { return type might be wrong }
  begin
    SDL_BUTTON_LMASK:=SDL_BUTTON(SDL_BUTTON_LEFT);
  end;

{ was #define dname def_expr }
function SDL_BUTTON_MMASK : longint; { return type might be wrong }
  begin
    SDL_BUTTON_MMASK:=SDL_BUTTON(SDL_BUTTON_MIDDLE);
  end;

{ was #define dname def_expr }
function SDL_BUTTON_RMASK : longint; { return type might be wrong }
  begin
    SDL_BUTTON_RMASK:=SDL_BUTTON(SDL_BUTTON_RIGHT);
  end;

{ was #define dname def_expr }
function SDL_BUTTON_X1MASK : longint; { return type might be wrong }
  begin
    SDL_BUTTON_X1MASK:=SDL_BUTTON(SDL_BUTTON_X1);
  end;

{ was #define dname def_expr }
function SDL_BUTTON_X2MASK : longint; { return type might be wrong }
  begin
    SDL_BUTTON_X2MASK:=SDL_BUTTON(SDL_BUTTON_X2);
  end;


end.
