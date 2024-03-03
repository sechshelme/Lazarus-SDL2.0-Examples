unit SDL_Video;

interface

uses
  SDL_stdinc, SDL_rect, SDL_surface;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{
  Simple DirectMedia Layer
  Copyright (C) 1997-2024 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
 }
{*
 *  \file SDL_video.h
 *
 *  Header file for SDL video functions.
  }
//{$ifndef SDL_video_h_}
//{$define SDL_video_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_pixels.h>}
//{$include <SDL3/SDL_properties.h>}
//{$include <SDL3/SDL_rect.h>}
//{$include <SDL3/SDL_surface.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
type
  PSDL_DisplayID = ^TSDL_DisplayID;
  TSDL_DisplayID = TUint32;

  PSDL_WindowID = ^TSDL_WindowID;
  TSDL_WindowID = TUint32;
{*
 *  Global video properties
 *
 *  - `SDL_PROP_GLOBAL_VIDEO_WAYLAND_WL_DISPLAY_POINTER`: the pointer to
 *    the global `wl_display` object used by the Wayland video backend. Can be
 *    set before the video subsystem is initialized to import an external
 *    `wl_display` object from an application or toolkit for use in SDL, or
 *    read after initialization to export the `wl_display` used by the
 *    Wayland video backend. Setting this property after the video subsystem
 *    has been initialized has no effect, and reading it when the video
 *    subsystem is uninitialized will either return the user provided value,
 *    if one was set prior to initialization, or NULL. See
 *    docs/README-wayland.md for more information.
  }

const
  SDL_PROP_GLOBAL_VIDEO_WAYLAND_WL_DISPLAY_POINTER = 'video.wayland.wl_display';  
{*
 *  System theme
  }
{*< Unknown system theme  }
{*< Light colored system theme  }
{*< Dark colored system theme  }
type
  PSDL_SystemTheme = ^TSDL_SystemTheme;
  TSDL_SystemTheme =  Longint;
  Const
    SDL_SYSTEM_THEME_UNKNOWN = 0;
    SDL_SYSTEM_THEME_LIGHT = 1;
    SDL_SYSTEM_THEME_DARK = 2;

{*
 *  The structure that defines a display mode
 *
 *  \sa SDL_GetFullscreenDisplayModes()
 *  \sa SDL_GetDesktopDisplayMode()
 *  \sa SDL_GetCurrentDisplayMode()
 *  \sa SDL_SetWindowFullscreenMode()
 *  \sa SDL_GetWindowFullscreenMode()
  }
{*< the display this mode is associated with  }
{*< pixel format  }
{*< width  }
{*< height  }
{*< scale converting size to pixels (e.g. a 1920x1080 mode with 2.0 scale would have 3840x2160 pixels)  }
{*< refresh rate (or zero for unspecified)  }
{*< driver-specific data, initialize to 0  }
type
  PPSDL_DisplayMode = ^PSDL_DisplayMode;
  PSDL_DisplayMode = ^TSDL_DisplayMode;
  TSDL_DisplayMode = record
      displayID : TSDL_DisplayID;
      format : TUint32;
      w : longint;
      h : longint;
      pixel_density : single;
      refresh_rate : single;
      driverdata : pointer;
    end;
{*
 *  Display orientation
  }
{*< The display orientation can't be determined  }
{*< The display is in landscape mode, with the right side up, relative to portrait mode  }
{*< The display is in landscape mode, with the left side up, relative to portrait mode  }
{*< The display is in portrait mode  }
{*< The display is in portrait mode, upside down  }

  PSDL_DisplayOrientation = ^TSDL_DisplayOrientation;
  TSDL_DisplayOrientation =  Longint;
  Const
    SDL_ORIENTATION_UNKNOWN = 0;
    SDL_ORIENTATION_LANDSCAPE = 1;
    SDL_ORIENTATION_LANDSCAPE_FLIPPED = 2;
    SDL_ORIENTATION_PORTRAIT = 3;
    SDL_ORIENTATION_PORTRAIT_FLIPPED = 4;

{*
 *  The type used to identify a window
 *
 *  \sa SDL_CreateWindow
 *  \sa SDL_CreateWindowWithProperties
 *  \sa SDL_DestroyWindow
 *  \sa SDL_FlashWindow
 *  \sa SDL_GetWindowFlags
 *  \sa SDL_GetWindowGrab
 *  \sa SDL_GetWindowKeyboardGrab
 *  \sa SDL_GetWindowMouseGrab
 *  \sa SDL_GetWindowPosition
 *  \sa SDL_GetWindowSize
 *  \sa SDL_GetWindowTitle
 *  \sa SDL_HideWindow
 *  \sa SDL_MaximizeWindow
 *  \sa SDL_MinimizeWindow
 *  \sa SDL_RaiseWindow
 *  \sa SDL_RestoreWindow
 *  \sa SDL_SetWindowFullscreen
 *  \sa SDL_SetWindowGrab
 *  \sa SDL_SetWindowKeyboardGrab
 *  \sa SDL_SetWindowMouseGrab
 *  \sa SDL_SetWindowIcon
 *  \sa SDL_SetWindowPosition
 *  \sa SDL_SetWindowSize
 *  \sa SDL_SetWindowBordered
 *  \sa SDL_SetWindowResizable
 *  \sa SDL_SetWindowTitle
 *  \sa SDL_ShowWindow
 *  \sa SDL_ShowWindowSystemMenu
  }
type
  TSDL_Window = Pointer;
  PSDL_Window = ^TSDL_Window;

{*
 *  The flags on a window
 *
 *  \sa SDL_GetWindowFlags
  }
{*< window is in fullscreen mode  }

const
  SDL_WINDOW_FULLSCREEN = $00000001;  
{*< window usable with OpenGL context  }
  SDL_WINDOW_OPENGL = $00000002;  
{*< window is occluded  }
  SDL_WINDOW_OCCLUDED = $00000004;  
{*< window is neither mapped onto the desktop nor shown in the taskbar/dock/window list; SDL_ShowWindow() is required for it to become visible  }
  SDL_WINDOW_HIDDEN = $00000008;  
{*< no window decoration  }
  SDL_WINDOW_BORDERLESS = $00000010;  
{*< window can be resized  }
  SDL_WINDOW_RESIZABLE = $00000020;  
{*< window is minimized  }
  SDL_WINDOW_MINIMIZED = $00000040;  
{*< window is maximized  }
  SDL_WINDOW_MAXIMIZED = $00000080;  
{*< window has grabbed mouse input  }
  SDL_WINDOW_MOUSE_GRABBED = $00000100;  
{*< window has input focus  }
  SDL_WINDOW_INPUT_FOCUS = $00000200;  
{*< window has mouse focus  }
  SDL_WINDOW_MOUSE_FOCUS = $00000400;  
{*< window not created by SDL  }
  SDL_WINDOW_EXTERNAL = $00000800;  
{*< window uses high pixel density back buffer if possible  }
  SDL_WINDOW_HIGH_PIXEL_DENSITY = $00002000;  
{*< window has mouse captured (unrelated to MOUSE_GRABBED)  }
  SDL_WINDOW_MOUSE_CAPTURE = $00004000;  
{*< window should always be above others  }
  SDL_WINDOW_ALWAYS_ON_TOP = $00008000;  
{*< window should be treated as a utility window, not showing in the task bar and window list  }
  SDL_WINDOW_UTILITY = $00020000;  
{*< window should be treated as a tooltip  }
  SDL_WINDOW_TOOLTIP = $00040000;  
{*< window should be treated as a popup menu  }
  SDL_WINDOW_POPUP_MENU = $00080000;  
{*< window has grabbed keyboard input  }
  SDL_WINDOW_KEYBOARD_GRABBED = $00100000;  
{*< window usable for Vulkan surface  }
  SDL_WINDOW_VULKAN = $10000000;  
{*< window usable for Metal view  }
  SDL_WINDOW_METAL = $20000000;  
{*< window with transparent buffer  }
  SDL_WINDOW_TRANSPARENT = $40000000;  
{*< window should not be focusable  }
  SDL_WINDOW_NOT_FOCUSABLE = $80000000;  
{*
 *  Used to indicate that you don't care what the window position is.
  }
  SDL_WINDOWPOS_UNDEFINED_MASK = $1FFF0000;  
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X : longint) : longint;

{ was #define dname def_expr }
function SDL_WINDOWPOS_UNDEFINED : longint; { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_WINDOWPOS_ISUNDEFINED(X : longint) : longint;

{*
 *  Used to indicate that the window position should be centered.
  }
const
  SDL_WINDOWPOS_CENTERED_MASK = $2FFF0000;  
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_WINDOWPOS_CENTERED_DISPLAY(X : longint) : longint;

{ was #define dname def_expr }
function SDL_WINDOWPOS_CENTERED : longint; { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_WINDOWPOS_ISCENTERED(X : longint) : longint;

{*
 *  Window flash operation
  }
{*< Cancel any window flash state  }
{*< Flash the window briefly to get attention  }
{*< Flash the window until it gets focus  }
type
  PSDL_FlashOperation = ^TSDL_FlashOperation;
  TSDL_FlashOperation =  Longint;
  Const
    SDL_FLASH_CANCEL = 0;
    SDL_FLASH_BRIEFLY = 1;
    SDL_FLASH_UNTIL_FOCUSED = 2;

{*
 *  An opaque handle to an OpenGL context.
  }
type
  PSDL_GLContext = ^TSDL_GLContext;
  TSDL_GLContext = pointer;
{*
 *  Opaque EGL types.
  }

  PSDL_EGLDisplay = ^TSDL_EGLDisplay;
  TSDL_EGLDisplay = pointer;

  PSDL_EGLConfig = ^TSDL_EGLConfig;
  TSDL_EGLConfig = pointer;

  PSDL_EGLSurface = ^TSDL_EGLSurface;
  TSDL_EGLSurface = pointer;

  PSDL_EGLAttrib = ^TSDL_EGLAttrib;
  TSDL_EGLAttrib = Tintptr_t;

  PSDL_EGLint = ^TSDL_EGLint;
  TSDL_EGLint = longint;
{*
 *  EGL attribute initialization callback types.
  }

  PSDL_EGLAttribArrayCallback = ^TSDL_EGLAttribArrayCallback;
  TSDL_EGLAttribArrayCallback = function :PSDL_EGLAttrib;cdecl;

  PSDL_EGLIntArrayCallback = ^TSDL_EGLIntArrayCallback;
  TSDL_EGLIntArrayCallback = function :PSDL_EGLint;cdecl;
{*
 *  OpenGL configuration attributes
  }

  PSDL_GLattr = ^TSDL_GLattr;
  TSDL_GLattr =  Longint;
  Const
    SDL_GL_RED_SIZE = 0;
    SDL_GL_GREEN_SIZE = 1;
    SDL_GL_BLUE_SIZE = 2;
    SDL_GL_ALPHA_SIZE = 3;
    SDL_GL_BUFFER_SIZE = 4;
    SDL_GL_DOUBLEBUFFER = 5;
    SDL_GL_DEPTH_SIZE = 6;
    SDL_GL_STENCIL_SIZE = 7;
    SDL_GL_ACCUM_RED_SIZE = 8;
    SDL_GL_ACCUM_GREEN_SIZE = 9;
    SDL_GL_ACCUM_BLUE_SIZE = 10;
    SDL_GL_ACCUM_ALPHA_SIZE = 11;
    SDL_GL_STEREO = 12;
    SDL_GL_MULTISAMPLEBUFFERS = 13;
    SDL_GL_MULTISAMPLESAMPLES = 14;
    SDL_GL_ACCELERATED_VISUAL = 15;
    SDL_GL_RETAINED_BACKING = 16;
    SDL_GL_CONTEXT_MAJOR_VERSION = 17;
    SDL_GL_CONTEXT_MINOR_VERSION = 18;
    SDL_GL_CONTEXT_FLAGS = 19;
    SDL_GL_CONTEXT_PROFILE_MASK = 20;
    SDL_GL_SHARE_WITH_CURRENT_CONTEXT = 21;
    SDL_GL_FRAMEBUFFER_SRGB_CAPABLE = 22;
    SDL_GL_CONTEXT_RELEASE_BEHAVIOR = 23;
    SDL_GL_CONTEXT_RESET_NOTIFICATION = 24;
    SDL_GL_CONTEXT_NO_ERROR = 25;
    SDL_GL_FLOATBUFFERS = 26;
    SDL_GL_EGL_PLATFORM = 27;

{*< GLX_CONTEXT_ES2_PROFILE_BIT_EXT  }
type
  PSDL_GLprofile = ^TSDL_GLprofile;
  TSDL_GLprofile =  Longint;
  Const
    SDL_GL_CONTEXT_PROFILE_CORE = $0001;
    SDL_GL_CONTEXT_PROFILE_COMPATIBILITY = $0002;
    SDL_GL_CONTEXT_PROFILE_ES = $0004;

type
  PSDL_GLcontextFlag = ^TSDL_GLcontextFlag;
  TSDL_GLcontextFlag =  Longint;
  Const
    SDL_GL_CONTEXT_DEBUG_FLAG = $0001;
    SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = $0002;
    SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG = $0004;
    SDL_GL_CONTEXT_RESET_ISOLATION_FLAG = $0008;

type
  PSDL_GLcontextReleaseFlag = ^TSDL_GLcontextReleaseFlag;
  TSDL_GLcontextReleaseFlag =  Longint;
  Const
    SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE = $0000;
    SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH = $0001;

type
  PSDL_GLContextResetNotification = ^TSDL_GLContextResetNotification;
  TSDL_GLContextResetNotification =  Longint;
  Const
    SDL_GL_CONTEXT_RESET_NO_NOTIFICATION = $0000;
    SDL_GL_CONTEXT_RESET_LOSE_CONTEXT = $0001;

{ Function prototypes  }
{*
 * Get the number of video drivers compiled into SDL.
 *
 * \returns a number >= 1 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetVideoDriver
  }

function SDL_GetNumVideoDrivers:longint;cdecl;external;
{*
 * Get the name of a built in video driver.
 *
 * The video drivers are presented in the order in which they are normally
 * checked during initialization.
 *
 * \param index the index of a video driver
 * \returns the name of the video driver with the given **index**.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetNumVideoDrivers
  }
(* Const before type ignored *)
function SDL_GetVideoDriver(index:longint):Pchar;cdecl;external;
{*
 * Get the name of the currently initialized video driver.
 *
 * \returns the name of the current video driver or NULL if no driver has been
 *          initialized.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetNumVideoDrivers
 * \sa SDL_GetVideoDriver
  }
(* Const before type ignored *)
function SDL_GetCurrentVideoDriver:Pchar;cdecl;external;
{*
 * Get the current system theme
 *
 * \returns the current system theme, light, dark, or unknown
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSystemTheme:TSDL_SystemTheme;cdecl;external;
{*
 * Get a list of currently connected displays.
 *
 * \param count a pointer filled in with the number of displays returned
 * \returns a 0 terminated array of display instance IDs which should be freed
 *          with SDL_free(), or NULL on error; call SDL_GetError() for more
 *          details.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetDisplays(count:Plongint):PSDL_DisplayID;cdecl;external;
{*
 * Return the primary display.
 *
 * \returns the instance ID of the primary display on success or 0 on failure;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplays
  }
function SDL_GetPrimaryDisplay:TSDL_DisplayID;cdecl;external;
{*
 * Get the properties associated with a display.
 *
 * The following read-only properties are provided by SDL:
 *
 * - `SDL_PROP_DISPLAY_HDR_ENABLED_BOOLEAN`: true if the display has HDR
 *   headroom above the SDR white point. This property can change dynamically
 *   when SDL_EVENT_DISPLAY_HDR_STATE_CHANGED is sent.
 * - `SDL_PROP_DISPLAY_SDR_WHITE_POINT_FLOAT`: the value of SDR white in the
 *   SDL_COLORSPACE_SRGB_LINEAR colorspace. On Windows this corresponds to the
 *   SDR white level in scRGB colorspace, and on Apple platforms this is
 *   always 1.0 for EDR content. This property can change dynamically when
 *   SDL_EVENT_DISPLAY_HDR_STATE_CHANGED is sent.
 * - `SDL_PROP_DISPLAY_HDR_HEADROOM_FLOAT`: the additional high dynamic range
 *   that can be displayed, in terms of the SDR white point. When HDR is not
 *   enabled, this will be 1.0. This property can change dynamically when
 *   SDL_EVENT_DISPLAY_HDR_STATE_CHANGED is sent.
 *
 * \param displayID the instance ID of the display to query
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetProperty
 * \sa SDL_SetProperty
  }
function SDL_GetDisplayProperties(displayID:TSDL_DisplayID):TSDL_PropertiesID;cdecl;external;
const
  SDL_PROP_DISPLAY_HDR_ENABLED_BOOLEAN = 'SDL.display.HDR_enabled';  
  SDL_PROP_DISPLAY_SDR_WHITE_POINT_FLOAT = 'SDL.display.SDR_white_point';  
  SDL_PROP_DISPLAY_HDR_HEADROOM_FLOAT = 'SDL.display.HDR_headroom';  
{*
 * Get the name of a display in UTF-8 encoding.
 *
 * \param displayID the instance ID of the display to query
 * \returns the name of a display or NULL on failure; call SDL_GetError() for
 *          more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplays
  }
(* Const before type ignored *)

function SDL_GetDisplayName(displayID:TSDL_DisplayID):Pchar;cdecl;external;
{*
 * Get the desktop area represented by a display.
 *
 * The primary display is always located at (0,0).
 *
 * \param displayID the instance ID of the display to query
 * \param rect the SDL_Rect structure filled in with the display bounds
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplayUsableBounds
 * \sa SDL_GetDisplays
  }
function SDL_GetDisplayBounds(displayID:TSDL_DisplayID; rect:PSDL_Rect):longint;cdecl;external;
{*
 * Get the usable desktop area represented by a display, in screen
 * coordinates.
 *
 * This is the same area as SDL_GetDisplayBounds() reports, but with portions
 * reserved by the system removed. For example, on Apple's macOS, this
 * subtracts the area occupied by the menu bar and dock.
 *
 * Setting a window to be fullscreen generally bypasses these unusable areas,
 * so these are good guidelines for the maximum space available to a
 * non-fullscreen window.
 *
 * \param displayID the instance ID of the display to query
 * \param rect the SDL_Rect structure filled in with the display bounds
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplayBounds
 * \sa SDL_GetDisplays
  }
function SDL_GetDisplayUsableBounds(displayID:TSDL_DisplayID; rect:PSDL_Rect):longint;cdecl;external;
{*
 * Get the orientation of a display when it is unrotated.
 *
 * \param displayID the instance ID of the display to query
 * \returns The SDL_DisplayOrientation enum value of the display, or
 *          `SDL_ORIENTATION_UNKNOWN` if it isn't available.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplays
  }
function SDL_GetNaturalDisplayOrientation(displayID:TSDL_DisplayID):TSDL_DisplayOrientation;cdecl;external;
{*
 * Get the orientation of a display.
 *
 * \param displayID the instance ID of the display to query
 * \returns The SDL_DisplayOrientation enum value of the display, or
 *          `SDL_ORIENTATION_UNKNOWN` if it isn't available.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplays
  }
function SDL_GetCurrentDisplayOrientation(displayID:TSDL_DisplayID):TSDL_DisplayOrientation;cdecl;external;
{*
 * Get the content scale of a display.
 *
 * The content scale is the expected scale for content based on the DPI
 * settings of the display. For example, a 4K display might have a 2.0 (200%)
 * display scale, which means that the user expects UI elements to be twice as
 * big on this display, to aid in readability.
 *
 * \param displayID the instance ID of the display to query
 * \returns The content scale of the display, or 0.0f on error; call
 *          SDL_GetError() for more details.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplays
  }
function SDL_GetDisplayContentScale(displayID:TSDL_DisplayID):single;cdecl;external;
{*
 * Get a list of fullscreen display modes available on a display.
 *
 * The display modes are sorted in this priority:
 *
 * - w -> largest to smallest
 * - h -> largest to smallest
 * - bits per pixel -> more colors to fewer colors
 * - packed pixel layout -> largest to smallest
 * - refresh rate -> highest to lowest
 * - pixel density -> lowest to highest
 *
 * \param displayID the instance ID of the display to query
 * \param count a pointer filled in with the number of displays returned
 * \returns a NULL terminated array of display mode pointers which should be
 *          freed with SDL_free(), or NULL on error; call SDL_GetError() for
 *          more details.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplays
  }
(* Const before type ignored *)
function SDL_GetFullscreenDisplayModes(displayID:TSDL_DisplayID; count:Plongint):PPSDL_DisplayMode;cdecl;external;
{*
 * Get the closest match to the requested display mode.
 *
 * The available display modes are scanned and `closest` is filled in with the
 * closest mode matching the requested mode and returned. The mode format and
 * refresh rate default to the desktop mode if they are set to 0. The modes
 * are scanned with size being first priority, format being second priority,
 * and finally checking the refresh rate. If all the available modes are too
 * small, then NULL is returned.
 *
 * \param displayID the instance ID of the display to query
 * \param w the width in pixels of the desired display mode
 * \param h the height in pixels of the desired display mode
 * \param refresh_rate the refresh rate of the desired display mode, or 0.0f
 *                     for the desktop refresh rate
 * \param include_high_density_modes Boolean to include high density modes in
 *                                   the search
 * \returns a pointer to the closest display mode equal to or larger than the
 *          desired mode, or NULL on error; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplays
 * \sa SDL_GetFullscreenDisplayModes
  }
(* Const before type ignored *)
function SDL_GetClosestFullscreenDisplayMode(displayID:TSDL_DisplayID; w:longint; h:longint; refresh_rate:single; include_high_density_modes:TSDL_bool):PSDL_DisplayMode;cdecl;external;
{*
 * Get information about the desktop's display mode.
 *
 * There's a difference between this function and SDL_GetCurrentDisplayMode()
 * when SDL runs fullscreen and has changed the resolution. In that case this
 * function will return the previous native display mode, and not the current
 * display mode.
 *
 * \param displayID the instance ID of the display to query
 * \returns a pointer to the desktop display mode or NULL on error; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetCurrentDisplayMode
 * \sa SDL_GetDisplays
  }
(* Const before type ignored *)
function SDL_GetDesktopDisplayMode(displayID:TSDL_DisplayID):PSDL_DisplayMode;cdecl;external;
{*
 * Get information about the current display mode.
 *
 * There's a difference between this function and SDL_GetDesktopDisplayMode()
 * when SDL runs fullscreen and has changed the resolution. In that case this
 * function will return the current display mode, and not the previous native
 * display mode.
 *
 * \param displayID the instance ID of the display to query
 * \returns a pointer to the desktop display mode or NULL on error; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDesktopDisplayMode
 * \sa SDL_GetDisplays
  }
(* Const before type ignored *)
function SDL_GetCurrentDisplayMode(displayID:TSDL_DisplayID):PSDL_DisplayMode;cdecl;external;
{*
 * Get the display containing a point.
 *
 * \param point the point to query
 * \returns the instance ID of the display containing the point or 0 on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplayBounds
 * \sa SDL_GetDisplays
  }
(* Const before type ignored *)
function SDL_GetDisplayForPoint(point:PSDL_Point):TSDL_DisplayID;cdecl;external;
{*
 * Get the display primarily containing a rect.
 *
 * \param rect the rect to query
 * \returns the instance ID of the display entirely containing the rect or
 *          closest to the center of the rect on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplayBounds
 * \sa SDL_GetDisplays
  }
(* Const before type ignored *)
function SDL_GetDisplayForRect(rect:PSDL_Rect):TSDL_DisplayID;cdecl;external;
{*
 * Get the display associated with a window.
 *
 * \param window the window to query
 * \returns the instance ID of the display containing the center of the window
 *          on success or 0 on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetDisplayBounds
 * \sa SDL_GetDisplays
  }
function SDL_GetDisplayForWindow(window:PSDL_Window):TSDL_DisplayID;cdecl;external;
{*
 * Get the pixel density of a window.
 *
 * This is a ratio of pixel size to window size. For example, if the window is
 * 1920x1080 and it has a high density back buffer of 3840x2160 pixels, it
 * would have a pixel density of 2.0.
 *
 * \param window the window to query
 * \returns the pixel density or 0.0f on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowDisplayScale
  }
function SDL_GetWindowPixelDensity(window:PSDL_Window):single;cdecl;external;
{*
 * Get the content display scale relative to a window's pixel size.
 *
 * This is a combination of the window pixel density and the display content
 * scale, and is the expected scale for displaying content in this window. For
 * example, if a 3840x2160 window had a display scale of 2.0, the user expects
 * the content to take twice as many pixels and be the same physical size as
 * if it were being displayed in a 1920x1080 window with a display scale of
 * 1.0.
 *
 * Conceptually this value corresponds to the scale display setting, and is
 * updated when that setting is changed, or the window moves to a display with
 * a different scale setting.
 *
 * \param window the window to query
 * \returns the display scale, or 0.0f on failure; call SDL_GetError() for
 *          more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetWindowDisplayScale(window:PSDL_Window):single;cdecl;external;
{*
 * Set the display mode to use when a window is visible and fullscreen.
 *
 * This only affects the display mode used when the window is fullscreen. To
 * change the window size when the window is not fullscreen, use
 * SDL_SetWindowSize().
 *
 * If the window is currently in the fullscreen state, this request is
 * asynchronous on some windowing systems and the new mode dimensions may not
 * be applied immediately upon the return of this function. If an immediate
 * change is required, call SDL_SyncWindow() to block until the changes have
 * taken effect.
 *
 * When the new mode takes effect, an SDL_EVENT_WINDOW_RESIZED and/or an
 * SDL_EVENT_WINDOOW_PIXEL_SIZE_CHANGED event will be emitted with the new
 * mode dimensions.
 *
 * \param window the window to affect
 * \param mode a pointer to the display mode to use, which can be NULL for
 *             borderless fullscreen desktop mode, or one of the fullscreen
 *             modes returned by SDL_GetFullscreenDisplayModes() to set an
 *             exclusive fullscreen mode.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowFullscreenMode
 * \sa SDL_SetWindowFullscreen
 * \sa SDL_SyncWindow
  }
(* Const before type ignored *)
function SDL_SetWindowFullscreenMode(window:PSDL_Window; mode:PSDL_DisplayMode):longint;cdecl;external;
{*
 * Query the display mode to use when a window is visible at fullscreen.
 *
 * \param window the window to query
 * \returns a pointer to the exclusive fullscreen mode to use or NULL for
 *          borderless fullscreen desktop mode
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowFullscreenMode
 * \sa SDL_SetWindowFullscreen
  }
(* Const before type ignored *)
function SDL_GetWindowFullscreenMode(window:PSDL_Window):PSDL_DisplayMode;cdecl;external;
{*
 * Get the raw ICC profile data for the screen the window is currently on.
 *
 * Data returned should be freed with SDL_free.
 *
 * \param window the window to query
 * \param size the size of the ICC profile
 * \returns the raw ICC profile data on success or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetWindowICCProfile(window:PSDL_Window; size:Psize_t):pointer;cdecl;external;
{*
 * Get the pixel format associated with the window.
 *
 * \param window the window to query
 * \returns the pixel format of the window on success or
 *          SDL_PIXELFORMAT_UNKNOWN on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetWindowPixelFormat(window:PSDL_Window):TUint32;cdecl;external;
{*
 * Create a window with the specified dimensions and flags.
 *
 * `flags` may be any of the following OR'd together:
 *
 * - `SDL_WINDOW_FULLSCREEN`: fullscreen window at desktop resolution
 * - `SDL_WINDOW_OPENGL`: window usable with an OpenGL context
 * - `SDL_WINDOW_VULKAN`: window usable with a Vulkan instance
 * - `SDL_WINDOW_METAL`: window usable with a Metal instance
 * - `SDL_WINDOW_HIDDEN`: window is not visible
 * - `SDL_WINDOW_BORDERLESS`: no window decoration
 * - `SDL_WINDOW_RESIZABLE`: window can be resized
 * - `SDL_WINDOW_MINIMIZED`: window is minimized
 * - `SDL_WINDOW_MAXIMIZED`: window is maximized
 * - `SDL_WINDOW_MOUSE_GRABBED`: window has grabbed mouse focus
 *
 * The SDL_Window is implicitly shown if SDL_WINDOW_HIDDEN is not set.
 *
 * On Apple's macOS, you **must** set the NSHighResolutionCapable Info.plist
 * property to YES, otherwise you will not receive a High-DPI OpenGL canvas.
 *
 * The window pixel size may differ from its window coordinate size if the
 * window is on a high pixel density display. Use SDL_GetWindowSize() to query
 * the client area's size in window coordinates, and
 * SDL_GetWindowSizeInPixels() or SDL_GetRenderOutputSize() to query the
 * drawable size in pixels. Note that the drawable size can vary after the
 * window is created and should be queried again if you get an
 * SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED event.
 *
 * If the window is created with any of the SDL_WINDOW_OPENGL or
 * SDL_WINDOW_VULKAN flags, then the corresponding LoadLibrary function
 * (SDL_GL_LoadLibrary or SDL_Vulkan_LoadLibrary) is called and the
 * corresponding UnloadLibrary function is called by SDL_DestroyWindow().
 *
 * If SDL_WINDOW_VULKAN is specified and there isn't a working Vulkan driver,
 * SDL_CreateWindow() will fail because SDL_Vulkan_LoadLibrary() will fail.
 *
 * If SDL_WINDOW_METAL is specified on an OS that does not support Metal,
 * SDL_CreateWindow() will fail.
 *
 * On non-Apple devices, SDL requires you to either not link to the Vulkan
 * loader or link to a dynamic library version. This limitation may be removed
 * in a future version of SDL.
 *
 * \param title the title of the window, in UTF-8 encoding
 * \param w the width of the window
 * \param h the height of the window
 * \param flags 0, or one or more SDL_WindowFlags OR'd together
 * \returns the window that was created or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreatePopupWindow
 * \sa SDL_CreateWindowWithProperties
 * \sa SDL_DestroyWindow
  }
(* Const before type ignored *)
function SDL_CreateWindow(title:Pchar; w:longint; h:longint; flags:TUint32):PSDL_Window;cdecl;external;
{*
 * Create a child popup window of the specified parent window.
 *
 * 'flags' **must** contain exactly one of the following: -
 * 'SDL_WINDOW_TOOLTIP': The popup window is a tooltip and will not pass any
 * input events. - 'SDL_WINDOW_POPUP_MENU': The popup window is a popup menu.
 * The topmost popup menu will implicitly gain the keyboard focus.
 *
 * The following flags are not relevant to popup window creation and will be
 * ignored:
 *
 * - 'SDL_WINDOW_MINIMIZED'
 * - 'SDL_WINDOW_MAXIMIZED'
 * - 'SDL_WINDOW_FULLSCREEN'
 * - 'SDL_WINDOW_BORDERLESS'
 *
 * The parent parameter **must** be non-null and a valid window. The parent of
 * a popup window can be either a regular, toplevel window, or another popup
 * window.
 *
 * Popup windows cannot be minimized, maximized, made fullscreen, raised,
 * flash, be made a modal window, be the parent of a modal window, or grab the
 * mouse and/or keyboard. Attempts to do so will fail.
 *
 * Popup windows implicitly do not have a border/decorations and do not appear
 * on the taskbar/dock or in lists of windows such as alt-tab menus.
 *
 * If a parent window is hidden, any child popup windows will be recursively
 * hidden as well. Child popup windows not explicitly hidden will be restored
 * when the parent is shown.
 *
 * If the parent window is destroyed, any child popup windows will be
 * recursively destroyed as well.
 *
 * \param parent the parent of the window, must not be NULL
 * \param offset_x the x position of the popup window relative to the origin
 *                 of the parent
 * \param offset_y the y position of the popup window relative to the origin
 *                 of the parent window
 * \param w the width of the window
 * \param h the height of the window
 * \param flags SDL_WINDOW_TOOLTIP or SDL_WINDOW_POPUP MENU, and zero or more
 *              additional SDL_WindowFlags OR'd together.
 * \returns the window that was created or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateWindow
 * \sa SDL_CreateWindowWithProperties
 * \sa SDL_DestroyWindow
 * \sa SDL_GetWindowParent
  }
function SDL_CreatePopupWindow(parent:PSDL_Window; offset_x:longint; offset_y:longint; w:longint; h:longint; 
           flags:TUint32):PSDL_Window;cdecl;external;
{*
 * Create a window with the specified properties.
 *
 * These are the supported properties:
 *
 * - `SDL_PROP_WINDOW_CREATE_ALWAYS_ON_TOP_BOOLEAN`: true if the window should
 *   be always on top
 * - `SDL_PROP_WINDOW_CREATE_BORDERLESS_BOOLEAN`: true if the window has no
 *   window decoration
 * - `SDL_PROP_WINDOW_CREATE_EXTERNAL_GRAPHICS_CONTEXT_BOOLEAN`: true if the
 *   window will be used with an externally managed graphics context.
 * - `SDL_PROP_WINDOW_CREATE_FOCUSABLE_BOOLEAN`: true if the window should
 *   accept keyboard input (defaults true)
 * - `SDL_PROP_WINDOW_CREATE_FULLSCREEN_BOOLEAN`: true if the window should
 *   start in fullscreen mode at desktop resolution
 * - `SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER`: the height of the window
 * - `SDL_PROP_WINDOW_CREATE_HIDDEN_BOOLEAN`: true if the window should start
 *   hidden
 * - `SDL_PROP_WINDOW_CREATE_HIGH_PIXEL_DENSITY_BOOLEAN`: true if the window
 *   uses a high pixel density buffer if possible
 * - `SDL_PROP_WINDOW_CREATE_MAXIMIZED_BOOLEAN`: true if the window should
 *   start maximized
 * - `SDL_PROP_WINDOW_CREATE_MENU_BOOLEAN`: true if the window is a popup menu
 * - `SDL_PROP_WINDOW_CREATE_METAL_BOOLEAN`: true if the window will be used
 *   with Metal rendering
 * - `SDL_PROP_WINDOW_CREATE_MINIMIZED_BOOLEAN`: true if the window should
 *   start minimized
 * - `SDL_PROP_WINDOW_CREATE_MOUSE_GRABBED_BOOLEAN`: true if the window starts
 *   with grabbed mouse focus
 * - `SDL_PROP_WINDOW_CREATE_OPENGL_BOOLEAN`: true if the window will be used
 *   with OpenGL rendering
 * - `SDL_PROP_WINDOW_CREATE_PARENT_POINTER`: an SDL_Window that will be the
 *   parent of this window, required for windows with the "toolip" and "menu"
 *   properties
 * - `SDL_PROP_WINDOW_CREATE_RESIZABLE_BOOLEAN`: true if the window should be
 *   resizable
 * - `SDL_PROP_WINDOW_CREATE_TITLE_STRING`: the title of the window, in UTF-8
 *   encoding
 * - `SDL_PROP_WINDOW_CREATE_TRANSPARENT_BOOLEAN`: true if the window show
 *   transparent in the areas with alpha of 0
 * - `SDL_PROP_WINDOW_CREATE_TOOLTIP_BOOLEAN`: true if the window is a tooltip
 * - `SDL_PROP_WINDOW_CREATE_UTILITY_BOOLEAN`: true if the window is a utility
 *   window, not showing in the task bar and window list
 * - `SDL_PROP_WINDOW_CREATE_VULKAN_BOOLEAN`: true if the window will be used
 *   with Vulkan rendering
 * - `SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER`: the width of the window
 * - `SDL_PROP_WINDOW_CREATE_X_NUMBER`: the x position of the window, or
 *   `SDL_WINDOWPOS_CENTERED`, defaults to `SDL_WINDOWPOS_UNDEFINED`. This is
 *   relative to the parent for windows with the "parent" property set.
 * - `SDL_PROP_WINDOW_CREATE_Y_NUMBER`: the y position of the window, or
 *   `SDL_WINDOWPOS_CENTERED`, defaults to `SDL_WINDOWPOS_UNDEFINED`. This is
 *   relative to the parent for windows with the "parent" property set.
 *
 * These are additional supported properties on macOS:
 *
 * - `SDL_PROP_WINDOW_CREATE_COCOA_WINDOW_POINTER`: the
 *   `(__unsafe_unretained)` NSWindow associated with the window, if you want
 *   to wrap an existing window.
 * - `SDL_PROP_WINDOW_CREATE_COCOA_VIEW_POINTER`: the `(__unsafe_unretained)`
 *   NSView associated with the window, defaults to `[window contentView]`
 *
 * These are additional supported properties on Wayland:
 *
 * - `SDL_PROP_WINDOW_CREATE_WAYLAND_SCALE_TO_DISPLAY_BOOLEAN` - true if the
 *   window should use forced scaling designed to produce 1:1 pixel mapping if
 *   not flagged as being DPI-aware. This is intended to allow legacy
 *   applications to be displayed without desktop scaling being applied, and
 *   has issues with certain display configurations, as this forces the window
 *   to behave in a way that Wayland desktops were not designed to
 *   accommodate. Potential issues include, but are not limited to: rounding
 *   errors can result when odd window sizes/scales are used, the window may
 *   be unusably small, the window may jump in visible size at times, the
 *   window may appear to be larger than the desktop space, and possible loss
 *   of cursor precision can occur. New applications should be designed with
 *   proper DPI awareness and handling instead of enabling this.
 * - `SDL_PROP_WINDOW_CREATE_WAYLAND_SURFACE_ROLE_CUSTOM_BOOLEAN` - true if
 *   the application wants to use the Wayland surface for a custom role and
 *   does not want it attached to an XDG toplevel window. See
 *   docs/README-wayland.md for more information on using custom surfaces.
 * - `SDL_PROP_WINDOW_CREATE_WAYLAND_CREATE_EGL_WINDOW_BOOLEAN - true if the
 *   application wants an associated `wl_egl_window` object to be created,
 *   even if the window does not have the OpenGL property or flag set.
 * - `SDL_PROP_WINDOW_CREATE_WAYLAND_WL_SURFACE_POINTER` - the wl_surface
 *   associated with the window, if you want to wrap an existing window. See
 *   docs/README-wayland.md for more information.
 *
 * These are additional supported properties on Windows:
 *
 * - `SDL_PROP_WINDOW_CREATE_WIN32_HWND_POINTER`: the HWND associated with the
 *   window, if you want to wrap an existing window.
 * - `SDL_PROP_WINDOW_CREATE_WIN32_PIXEL_FORMAT_HWND_POINTER`: optional,
 *   another window to share pixel format with, useful for OpenGL windows
 *
 * These are additional supported properties with X11:
 *
 * - `SDL_PROP_WINDOW_CREATE_X11_WINDOW_NUMBER`: the X11 Window associated
 *   with the window, if you want to wrap an existing window.
 *
 * The window is implicitly shown if the "hidden" property is not set.
 *
 * Windows with the "tooltip" and "menu" properties are popup windows and have
 * the behaviors and guidelines outlined in `SDL_CreatePopupWindow()`.
 *
 * \param props the properties to use
 * \returns the window that was created or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateWindow
 * \sa SDL_DestroyWindow
  }
function SDL_CreateWindowWithProperties(props:TSDL_PropertiesID):PSDL_Window;cdecl;external;
const
  SDL_PROP_WINDOW_CREATE_ALWAYS_ON_TOP_BOOLEAN = 'always_on_top';  
  SDL_PROP_WINDOW_CREATE_BORDERLESS_BOOLEAN = 'borderless';  
  SDL_PROP_WINDOW_CREATE_FOCUSABLE_BOOLEAN = 'focusable';  
  SDL_PROP_WINDOW_CREATE_EXTERNAL_GRAPHICS_CONTEXT_BOOLEAN = 'external_graphics_context';  
  SDL_PROP_WINDOW_CREATE_FULLSCREEN_BOOLEAN = 'fullscreen';  
  SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER = 'height';  
  SDL_PROP_WINDOW_CREATE_HIDDEN_BOOLEAN = 'hidden';  
  SDL_PROP_WINDOW_CREATE_HIGH_PIXEL_DENSITY_BOOLEAN = 'high_pixel_density';  
  SDL_PROP_WINDOW_CREATE_MAXIMIZED_BOOLEAN = 'maximized';  
  SDL_PROP_WINDOW_CREATE_MENU_BOOLEAN = 'menu';  
  SDL_PROP_WINDOW_CREATE_METAL_BOOLEAN = 'metal';  
  SDL_PROP_WINDOW_CREATE_MINIMIZED_BOOLEAN = 'minimized';  
  SDL_PROP_WINDOW_CREATE_MOUSE_GRABBED_BOOLEAN = 'mouse_grabbed';  
  SDL_PROP_WINDOW_CREATE_OPENGL_BOOLEAN = 'opengl';  
  SDL_PROP_WINDOW_CREATE_PARENT_POINTER = 'parent';  
  SDL_PROP_WINDOW_CREATE_RESIZABLE_BOOLEAN = 'resizable';  
  SDL_PROP_WINDOW_CREATE_TITLE_STRING = 'title';  
  SDL_PROP_WINDOW_CREATE_TRANSPARENT_BOOLEAN = 'transparent';  
  SDL_PROP_WINDOW_CREATE_TOOLTIP_BOOLEAN = 'tooltip';  
  SDL_PROP_WINDOW_CREATE_UTILITY_BOOLEAN = 'utility';  
  SDL_PROP_WINDOW_CREATE_VULKAN_BOOLEAN = 'vulkan';  
  SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER = 'width';  
  SDL_PROP_WINDOW_CREATE_X_NUMBER = 'x';  
  SDL_PROP_WINDOW_CREATE_Y_NUMBER = 'y';  
  SDL_PROP_WINDOW_CREATE_COCOA_WINDOW_POINTER = 'cocoa.window';  
  SDL_PROP_WINDOW_CREATE_COCOA_VIEW_POINTER = 'cocoa.view';  
  SDL_PROP_WINDOW_CREATE_WAYLAND_SCALE_TO_DISPLAY_BOOLEAN = 'wayland.scale_to_display';  
  SDL_PROP_WINDOW_CREATE_WAYLAND_SURFACE_ROLE_CUSTOM_BOOLEAN = 'wayland.surface_role_custom';  
  SDL_PROP_WINDOW_CREATE_WAYLAND_CREATE_EGL_WINDOW_BOOLEAN = 'wayland.create_egl_window';  
  SDL_PROP_WINDOW_CREATE_WAYLAND_WL_SURFACE_POINTER = 'wayland.wl_surface';  
  SDL_PROP_WINDOW_CREATE_WIN32_HWND_POINTER = 'win32.hwnd';  
  SDL_PROP_WINDOW_CREATE_WIN32_PIXEL_FORMAT_HWND_POINTER = 'win32.pixel_format_hwnd';  
  SDL_PROP_WINDOW_CREATE_X11_WINDOW_NUMBER = 'x11.window';  
{*
 * Get the numeric ID of a window.
 *
 * The numeric ID is what SDL_WindowEvent references, and is necessary to map
 * these events to specific SDL_Window objects.
 *
 * \param window the window to query
 * \returns the ID of the window on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowFromID
  }

function SDL_GetWindowID(window:PSDL_Window):TSDL_WindowID;cdecl;external;
{*
 * Get a window from a stored ID.
 *
 * The numeric ID is what SDL_WindowEvent references, and is necessary to map
 * these events to specific SDL_Window objects.
 *
 * \param id the ID of the window
 * \returns the window associated with `id` or NULL if it doesn't exist; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowID
  }
function SDL_GetWindowFromID(id:TSDL_WindowID):PSDL_Window;cdecl;external;
{*
 * Get parent of a window.
 *
 * \param window the window to query
 * \returns the parent of the window on success or NULL if the window has no
 *          parent.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreatePopupWindow
  }
function SDL_GetWindowParent(window:PSDL_Window):PSDL_Window;cdecl;external;
{*
 * Get the properties associated with a window.
 *
 * The following read-only properties are provided by SDL:
 *
 * - `SDL_PROP_WINDOW_SHAPE_POINTER`: the surface associated with a shaped
 *   window
 *
 * On Android:
 *
 * - `SDL_PROP_WINDOW_ANDROID_WINDOW_POINTER`: the ANativeWindow associated
 *   with the window
 * - `SDL_PROP_WINDOW_ANDROID_SURFACE_POINTER`: the EGLSurface associated with
 *   the window
 *
 * On iOS:
 *
 * - `SDL_PROP_WINDOW_UIKIT_WINDOW_POINTER`: the `(__unsafe_unretained)`
 *   UIWindow associated with the window
 * - `SDL_PROP_WINDOW_UIKIT_METAL_VIEW_TAG_NUMBER`: the NSInteger tag
 *   assocated with metal views on the window
 *
 * On KMS/DRM:
 *
 * - `SDL_PROP_WINDOW_KMSDRM_DEVICE_INDEX_NUMBER`: the device index associated
 *   with the window (e.g. the X in /dev/dri/cardX)
 * - `SDL_PROP_WINDOW_KMSDRM_DRM_FD_NUMBER`: the DRM FD associated with the
 *   window
 * - `SDL_PROP_WINDOW_KMSDRM_GBM_DEVICE_POINTER`: the GBM device associated
 *   with the window
 *
 * On macOS:
 *
 * - `SDL_PROP_WINDOW_COCOA_WINDOW_POINTER`: the `(__unsafe_unretained)`
 *   NSWindow associated with the window
 * - `SDL_PROP_WINDOW_COCOA_METAL_VIEW_TAG_NUMBER`: the NSInteger tag
 *   assocated with metal views on the window
 *
 * On Vivante:
 *
 * - `SDL_PROP_WINDOW_VIVANTE_DISPLAY_POINTER`: the EGLNativeDisplayType
 *   associated with the window
 * - `SDL_PROP_WINDOW_VIVANTE_WINDOW_POINTER`: the EGLNativeWindowType
 *   associated with the window
 * - `SDL_PROP_WINDOW_VIVANTE_SURFACE_POINTER`: the EGLSurface associated with
 *   the window
 *
 * On UWP:
 *
 * - `SDL_PROP_WINDOW_WINRT_WINDOW_POINTER`: the IInspectable CoreWindow
 *   associated with the window
 *
 * On Windows:
 *
 * - `SDL_PROP_WINDOW_WIN32_HWND_POINTER`: the HWND associated with the window
 * - `SDL_PROP_WINDOW_WIN32_HDC_POINTER`: the HDC associated with the window
 * - `SDL_PROP_WINDOW_WIN32_INSTANCE_POINTER`: the HINSTANCE associated with
 *   the window
 *
 * On Wayland:
 *
 * Note: The `xdg_*` window objects do not internally persist across window
 * show/hide calls. They will be null if the window is hidden and must be
 * queried each time it is shown.
 *
 * - `SDL_PROP_WINDOW_WAYLAND_DISPLAY_POINTER`: the wl_display associated with
 *   the window
 * - `SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER`: the wl_surface associated with
 *   the window
 * - `SDL_PROP_WINDOW_WAYLAND_EGL_WINDOW_POINTER`: the wl_egl_window
 *   associated with the window
 * - `SDL_PROP_WINDOW_WAYLAND_XDG_SURFACE_POINTER`: the xdg_surface associated
 *   with the window
 * - `SDL_PROP_WINDOW_WAYLAND_XDG_TOPLEVEL_POINTER`: the xdg_toplevel role
 *   associated with the window
 * - `SDL_PROP_WINDOW_WAYLAND_XDG_POPUP_POINTER`: the xdg_popup role
 *   associated with the window
 * - `SDL_PROP_WINDOW_WAYLAND_XDG_POSITIONER_POINTER`: the xdg_positioner
 *   associated with the window, in popup mode
 *
 * On X11:
 *
 * - `SDL_PROP_WINDOW_X11_DISPLAY_POINTER`: the X11 Display associated with
 *   the window
 * - `SDL_PROP_WINDOW_X11_SCREEN_NUMBER`: the screen number associated with
 *   the window
 * - `SDL_PROP_WINDOW_X11_WINDOW_NUMBER`: the X11 Window associated with the
 *   window
 *
 * \param window the window to query
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetProperty
 * \sa SDL_SetProperty
  }
function SDL_GetWindowProperties(window:PSDL_Window):TSDL_PropertiesID;cdecl;external;
const
  SDL_PROP_WINDOW_SHAPE_POINTER = 'SDL.window.shape';  
  SDL_PROP_WINDOW_ANDROID_WINDOW_POINTER = 'SDL.window.android.window';  
  SDL_PROP_WINDOW_ANDROID_SURFACE_POINTER = 'SDL.window.android.surface';  
  SDL_PROP_WINDOW_UIKIT_WINDOW_POINTER = 'SDL.window.uikit.window';  
  SDL_PROP_WINDOW_UIKIT_METAL_VIEW_TAG_NUMBER = 'SDL.window.uikit.metal_view_tag';  
  SDL_PROP_WINDOW_KMSDRM_DEVICE_INDEX_NUMBER = 'SDL.window.kmsdrm.dev_index';  
  SDL_PROP_WINDOW_KMSDRM_DRM_FD_NUMBER = 'SDL.window.kmsdrm.drm_fd';  
  SDL_PROP_WINDOW_KMSDRM_GBM_DEVICE_POINTER = 'SDL.window.kmsdrm.gbm_dev';  
  SDL_PROP_WINDOW_COCOA_WINDOW_POINTER = 'SDL.window.cocoa.window';  
  SDL_PROP_WINDOW_COCOA_METAL_VIEW_TAG_NUMBER = 'SDL.window.cocoa.metal_view_tag';  
  SDL_PROP_WINDOW_VIVANTE_DISPLAY_POINTER = 'SDL.window.vivante.display';  
  SDL_PROP_WINDOW_VIVANTE_WINDOW_POINTER = 'SDL.window.vivante.window';  
  SDL_PROP_WINDOW_VIVANTE_SURFACE_POINTER = 'SDL.window.vivante.surface';  
  SDL_PROP_WINDOW_WINRT_WINDOW_POINTER = 'SDL.window.winrt.window';  
  SDL_PROP_WINDOW_WIN32_HWND_POINTER = 'SDL.window.win32.hwnd';  
  SDL_PROP_WINDOW_WIN32_HDC_POINTER = 'SDL.window.win32.hdc';  
  SDL_PROP_WINDOW_WIN32_INSTANCE_POINTER = 'SDL.window.win32.instance';  
  SDL_PROP_WINDOW_WAYLAND_DISPLAY_POINTER = 'SDL.window.wayland.display';  
  SDL_PROP_WINDOW_WAYLAND_SURFACE_POINTER = 'SDL.window.wayland.surface';  
  SDL_PROP_WINDOW_WAYLAND_EGL_WINDOW_POINTER = 'SDL.window.wayland.egl_window';  
  SDL_PROP_WINDOW_WAYLAND_XDG_SURFACE_POINTER = 'SDL.window.wayland.xdg_surface';  
  SDL_PROP_WINDOW_WAYLAND_XDG_TOPLEVEL_POINTER = 'SDL.window.wayland.xdg_toplevel';  
  SDL_PROP_WINDOW_WAYLAND_XDG_POPUP_POINTER = 'SDL.window.wayland.xdg_popup';  
  SDL_PROP_WINDOW_WAYLAND_XDG_POSITIONER_POINTER = 'SDL.window.wayland.xdg_positioner';  
  SDL_PROP_WINDOW_X11_DISPLAY_POINTER = 'SDL.window.x11.display';  
  SDL_PROP_WINDOW_X11_SCREEN_NUMBER = 'SDL.window.x11.screen';  
  SDL_PROP_WINDOW_X11_WINDOW_NUMBER = 'SDL.window.x11.window';  
{*
 * Get the window flags.
 *
 * \param window the window to query
 * \returns a mask of the SDL_WindowFlags associated with `window`
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateWindow
 * \sa SDL_HideWindow
 * \sa SDL_MaximizeWindow
 * \sa SDL_MinimizeWindow
 * \sa SDL_SetWindowFullscreen
 * \sa SDL_SetWindowGrab
 * \sa SDL_ShowWindow
  }

function SDL_GetWindowFlags(window:PSDL_Window):TUint32;cdecl;external;
{*
 * Set the title of a window.
 *
 * This string is expected to be in UTF-8 encoding.
 *
 * \param window the window to change
 * \param title the desired window title in UTF-8 format
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowTitle
  }
(* Const before type ignored *)
function SDL_SetWindowTitle(window:PSDL_Window; title:Pchar):longint;cdecl;external;
{*
 * Get the title of a window.
 *
 * \param window the window to query
 * \returns the title of the window in UTF-8 format or "" if there is no
 *          title.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowTitle
  }
(* Const before type ignored *)
function SDL_GetWindowTitle(window:PSDL_Window):Pchar;cdecl;external;
{*
 * Set the icon for a window.
 *
 * \param window the window to change
 * \param icon an SDL_Surface structure containing the icon for the window
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetWindowIcon(window:PSDL_Window; icon:PSDL_Surface):longint;cdecl;external;
{*
 * Request that the window's position be set.
 *
 * If, at the time of this request, the window is in a fixed-size state such
 * as maximized, this request may be deferred until the window returns to a
 * resizable state.
 *
 * This can be used to reposition fullscreen-desktop windows onto a different
 * display, however, exclusive fullscreen windows are locked to a specific
 * display and can only be repositioned programmatically via
 * SDL_SetWindowFullscreenMode().
 *
 * On some windowing systems this request is asynchronous and the new
 * coordinates may not have have been applied immediately upon the return of
 * this function. If an immediate change is required, call SDL_SyncWindow() to
 * block until the changes have taken effect.
 *
 * When the window position changes, an SDL_EVENT_WINDOW_MOVED event will be
 * emitted with the window's new coordinates. Note that the new coordinates
 * may not match the exact coordinates requested, as some windowing systems
 * can restrict the position of the window in certain scenarios (e.g.
 * constraining the position so the window is always within desktop bounds).
 * Additionally, as this is just a request, it can be denied by the windowing
 * system.
 *
 * \param window the window to reposition
 * \param x the x coordinate of the window, or `SDL_WINDOWPOS_CENTERED` or
 *          `SDL_WINDOWPOS_UNDEFINED`
 * \param y the y coordinate of the window, or `SDL_WINDOWPOS_CENTERED` or
 *          `SDL_WINDOWPOS_UNDEFINED`
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowPosition
 * \sa SDL_SyncWindow
  }
function SDL_SetWindowPosition(window:PSDL_Window; x:longint; y:longint):longint;cdecl;external;
{*
 * Get the position of a window.
 *
 * This is the current position of the window as last reported by the
 * windowing system.
 *
 * If you do not need the value for one of the positions a NULL may be passed
 * in the `x` or `y` parameter.
 *
 * \param window the window to query
 * \param x a pointer filled in with the x position of the window, may be NULL
 * \param y a pointer filled in with the y position of the window, may be NULL
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowPosition
  }
function SDL_GetWindowPosition(window:PSDL_Window; x:Plongint; y:Plongint):longint;cdecl;external;
{*
 * Request that the size of a window's client area be set.
 *
 * NULL can safely be passed as the `w` or `h` parameter if the width or
 * height value is not desired.
 *
 * If, at the time of this request, the window in a fixed-size state, such as
 * maximized or fullscreen, the request will be deferred until the window
 * exits this state and becomes resizable again.
 *
 * To change the fullscreen mode of a window, use
 * SDL_SetWindowFullscreenMode()
 *
 * On some windowing systems, this request is asynchronous and the new window
 * size may not have have been applied immediately upon the return of this
 * function. If an immediate change is required, call SDL_SyncWindow() to
 * block until the changes have taken effect.
 *
 * When the window size changes, an SDL_EVENT_WINDOW_RESIZED event will be
 * emitted with the new window dimensions. Note that the new dimensions may
 * not match the exact size requested, as some windowing systems can restrict
 * the window size in certain scenarios (e.g. constraining the size of the
 * content area to remain within the usable desktop bounds). Additionally, as
 * this is just a request, it can be denied by the windowing system.
 *
 * \param window the window to change
 * \param w the width of the window, must be > 0
 * \param h the height of the window, must be > 0
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowSize
 * \sa SDL_SetWindowFullscreenMode
 * \sa SDL_SyncWindow
  }
function SDL_SetWindowSize(window:PSDL_Window; w:longint; h:longint):longint;cdecl;external;
{*
 * Get the size of a window's client area.
 *
 * NULL can safely be passed as the `w` or `h` parameter if the width or
 * height value is not desired.
 *
 * The window pixel size may differ from its window coordinate size if the
 * window is on a high pixel density display. Use SDL_GetWindowSizeInPixels()
 * or SDL_GetRenderOutputSize() to get the real client area size in pixels.
 *
 * \param window the window to query the width and height from
 * \param w a pointer filled in with the width of the window, may be NULL
 * \param h a pointer filled in with the height of the window, may be NULL
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetRenderOutputSize
 * \sa SDL_GetWindowSizeInPixels
 * \sa SDL_SetWindowSize
  }
function SDL_GetWindowSize(window:PSDL_Window; w:Plongint; h:Plongint):longint;cdecl;external;
{*
 * Get the size of a window's borders (decorations) around the client area.
 *
 * Note: If this function fails (returns -1), the size values will be
 * initialized to 0, 0, 0, 0 (if a non-NULL pointer is provided), as if the
 * window in question was borderless.
 *
 * Note: This function may fail on systems where the window has not yet been
 * decorated by the display server (for example, immediately after calling
 * SDL_CreateWindow). It is recommended that you wait at least until the
 * window has been presented and composited, so that the window system has a
 * chance to decorate the window and provide the border dimensions to SDL.
 *
 * This function also returns -1 if getting the information is not supported.
 *
 * \param window the window to query the size values of the border
 *               (decorations) from
 * \param top pointer to variable for storing the size of the top border; NULL
 *            is permitted
 * \param left pointer to variable for storing the size of the left border;
 *             NULL is permitted
 * \param bottom pointer to variable for storing the size of the bottom
 *               border; NULL is permitted
 * \param right pointer to variable for storing the size of the right border;
 *              NULL is permitted
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowSize
  }
function SDL_GetWindowBordersSize(window:PSDL_Window; top:Plongint; left:Plongint; bottom:Plongint; right:Plongint):longint;cdecl;external;
{*
 * Get the size of a window's client area, in pixels.
 *
 * \param window the window from which the drawable size should be queried
 * \param w a pointer to variable for storing the width in pixels, may be NULL
 * \param h a pointer to variable for storing the height in pixels, may be
 *          NULL
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateWindow
 * \sa SDL_GetWindowSize
  }
function SDL_GetWindowSizeInPixels(window:PSDL_Window; w:Plongint; h:Plongint):longint;cdecl;external;
{*
 * Set the minimum size of a window's client area.
 *
 * \param window the window to change
 * \param min_w the minimum width of the window, or 0 for no limit
 * \param min_h the minimum height of the window, or 0 for no limit
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowMinimumSize
 * \sa SDL_SetWindowMaximumSize
  }
function SDL_SetWindowMinimumSize(window:PSDL_Window; min_w:longint; min_h:longint):longint;cdecl;external;
{*
 * Get the minimum size of a window's client area.
 *
 * \param window the window to query
 * \param w a pointer filled in with the minimum width of the window, may be
 *          NULL
 * \param h a pointer filled in with the minimum height of the window, may be
 *          NULL
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowMaximumSize
 * \sa SDL_SetWindowMinimumSize
  }
function SDL_GetWindowMinimumSize(window:PSDL_Window; w:Plongint; h:Plongint):longint;cdecl;external;
{*
 * Set the maximum size of a window's client area.
 *
 * \param window the window to change
 * \param max_w the maximum width of the window, or 0 for no limit
 * \param max_h the maximum height of the window, or 0 for no limit
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowMaximumSize
 * \sa SDL_SetWindowMinimumSize
  }
function SDL_SetWindowMaximumSize(window:PSDL_Window; max_w:longint; max_h:longint):longint;cdecl;external;
{*
 * Get the maximum size of a window's client area.
 *
 * \param window the window to query
 * \param w a pointer filled in with the maximum width of the window, may be
 *          NULL
 * \param h a pointer filled in with the maximum height of the window, may be
 *          NULL
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowMinimumSize
 * \sa SDL_SetWindowMaximumSize
  }
function SDL_GetWindowMaximumSize(window:PSDL_Window; w:Plongint; h:Plongint):longint;cdecl;external;
{*
 * Set the border state of a window.
 *
 * This will add or remove the window's `SDL_WINDOW_BORDERLESS` flag and add
 * or remove the border from the actual window. This is a no-op if the
 * window's border already matches the requested state.
 *
 * You can't change the border state of a fullscreen window.
 *
 * \param window the window of which to change the border state
 * \param bordered SDL_FALSE to remove border, SDL_TRUE to add border
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowFlags
  }
function SDL_SetWindowBordered(window:PSDL_Window; bordered:TSDL_bool):longint;cdecl;external;
{*
 * Set the user-resizable state of a window.
 *
 * This will add or remove the window's `SDL_WINDOW_RESIZABLE` flag and
 * allow/disallow user resizing of the window. This is a no-op if the window's
 * resizable state already matches the requested state.
 *
 * You can't change the resizable state of a fullscreen window.
 *
 * \param window the window of which to change the resizable state
 * \param resizable SDL_TRUE to allow resizing, SDL_FALSE to disallow
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowFlags
  }
function SDL_SetWindowResizable(window:PSDL_Window; resizable:TSDL_bool):longint;cdecl;external;
{*
 * Set the window to always be above the others.
 *
 * This will add or remove the window's `SDL_WINDOW_ALWAYS_ON_TOP` flag. This
 * will bring the window to the front and keep the window above the rest.
 *
 * \param window The window of which to change the always on top state
 * \param on_top SDL_TRUE to set the window always on top, SDL_FALSE to
 *               disable
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowFlags
  }
function SDL_SetWindowAlwaysOnTop(window:PSDL_Window; on_top:TSDL_bool):longint;cdecl;external;
{*
 * Show a window.
 *
 * \param window the window to show
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HideWindow
 * \sa SDL_RaiseWindow
  }
function SDL_ShowWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Hide a window.
 *
 * \param window the window to hide
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_ShowWindow
  }
function SDL_HideWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Raise a window above other windows and set the input focus.
 *
 * \param window the window to raise
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_RaiseWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Request that the window be made as large as possible.
 *
 * Non-resizable windows can't be maximized. The window must have the
 * SDL_WINDOW_RESIZABLE flag set, or this will have no effect.
 *
 * On some windowing systems this request is asynchronous and the new window
 * state may not have have been applied immediately upon the return of this
 * function. If an immediate change is required, call SDL_SyncWindow() to
 * block until the changes have taken effect.
 *
 * When the window state changes, an SDL_EVENT_WINDOW_MAXIMIZED event will be
 * emitted. Note that, as this is just a request, the windowing system can
 * deny the state change.
 *
 * When maximizing a window, whether the constraints set via
 * SDL_SetWindowMaximumSize() are honored depends on the policy of the window
 * manager. Win32 and macOS enforce the constraints when maximizing, while X11
 * and Wayland window managers may vary.
 *
 * \param window the window to maximize
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_MinimizeWindow
 * \sa SDL_RestoreWindow
 * \sa SDL_SyncWindow
  }
function SDL_MaximizeWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Request that the window be minimized to an iconic representation.
 *
 * On some windowing systems this request is asynchronous and the new window
 * state may not have have been applied immediately upon the return of this
 * function. If an immediate change is required, call SDL_SyncWindow() to
 * block until the changes have taken effect.
 *
 * When the window state changes, an SDL_EVENT_WINDOW_MINIMIZED event will be
 * emitted. Note that, as this is just a request, the windowing system can
 * deny the state change.
 *
 * \param window the window to minimize
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_MaximizeWindow
 * \sa SDL_RestoreWindow
 * \sa SDL_SyncWindow
  }
function SDL_MinimizeWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Request that the size and position of a minimized or maximized window be
 * restored.
 *
 * On some windowing systems this request is asynchronous and the new window
 * state may not have have been applied immediately upon the return of this
 * function. If an immediate change is required, call SDL_SyncWindow() to
 * block until the changes have taken effect.
 *
 * When the window state changes, an SDL_EVENT_WINDOW_RESTORED event will be
 * emitted. Note that, as this is just a request, the windowing system can
 * deny the state change.
 *
 * \param window the window to restore
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_MaximizeWindow
 * \sa SDL_MinimizeWindow
 * \sa SDL_SyncWindow
  }
function SDL_RestoreWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Request that the window's fullscreen state be changed.
 *
 * By default a window in fullscreen state uses borderless fullscreen desktop
 * mode, but a specific exclusive display mode can be set using
 * SDL_SetWindowFullscreenMode().
 *
 * On some windowing systems this request is asynchronous and the new
 * fullscreen state may not have have been applied immediately upon the return
 * of this function. If an immediate change is required, call SDL_SyncWindow()
 * to block until the changes have taken effect.
 *
 * When the window state changes, an SDL_EVENT_WINDOW_ENTER_FULLSCREEN or
 * SDL_EVENT_WINDOW_LEAVE_FULLSCREEN event will be emitted. Note that, as this
 * is just a request, it can be denied by the windowing system.
 *
 * \param window the window to change
 * \param fullscreen SDL_TRUE for fullscreen mode, SDL_FALSE for windowed mode
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowFullscreenMode
 * \sa SDL_SetWindowFullscreenMode
 * \sa SDL_SyncWindow
  }
function SDL_SetWindowFullscreen(window:PSDL_Window; fullscreen:TSDL_bool):longint;cdecl;external;
{*
 * Block until any pending window state is finalized.
 *
 * On asynchronous windowing systems, this acts as a synchronization barrier
 * for pending window state. It will attempt to wait until any pending window
 * state has been applied and is guaranteed to return within finite time. Note
 * that for how long it can potentially block depends on the underlying window
 * system, as window state changes may involve somewhat lengthy animations
 * that must complete before the window is in its final requested state.
 *
 * On windowing systems where changes are immediate, this does nothing.
 *
 * \param window the window for which to wait for the pending state to be
 *               applied
 * \returns 0 on success, a positive value if the operation timed out before
 *          the window was in the requested state, or a negative error code on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowSize
 * \sa SDL_SetWindowPosition
 * \sa SDL_SetWindowFullscreen
 * \sa SDL_MinimizeWindow
 * \sa SDL_MaximizeWindow
 * \sa SDL_RestoreWindow
  }
function SDL_SyncWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Return whether the window has a surface associated with it.
 *
 * \param window the window to query
 * \returns SDL_TRUE if there is a surface associated with the window, or
 *          SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowSurface
  }
function SDL_WindowHasSurface(window:PSDL_Window):TSDL_bool;cdecl;external;
{*
 * Get the SDL surface associated with the window.
 *
 * A new surface will be created with the optimal format for the window, if
 * necessary. This surface will be freed when the window is destroyed. Do not
 * free this surface.
 *
 * This surface will be invalidated if the window is resized. After resizing a
 * window this function must be called again to return a valid surface.
 *
 * You may not combine this with 3D or the rendering API on this window.
 *
 * This function is affected by `SDL_HINT_FRAMEBUFFER_ACCELERATION`.
 *
 * \param window the window to query
 * \returns the surface associated with the window, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DestroyWindowSurface
 * \sa SDL_WindowHasSurface
 * \sa SDL_UpdateWindowSurface
 * \sa SDL_UpdateWindowSurfaceRects
  }
function SDL_GetWindowSurface(window:PSDL_Window):PSDL_Surface;cdecl;external;
{*
 * Copy the window surface to the screen.
 *
 * This is the function you use to reflect any changes to the surface on the
 * screen.
 *
 * This function is equivalent to the SDL 1.2 API SDL_Flip().
 *
 * \param window the window to update
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowSurface
 * \sa SDL_UpdateWindowSurfaceRects
  }
function SDL_UpdateWindowSurface(window:PSDL_Window):longint;cdecl;external;
{*
 * Copy areas of the window surface to the screen.
 *
 * This is the function you use to reflect changes to portions of the surface
 * on the screen.
 *
 * This function is equivalent to the SDL 1.2 API SDL_UpdateRects().
 *
 * Note that this function will update _at least_ the rectangles specified,
 * but this is only intended as an optimization; in practice, this might
 * update more of the screen (or all of the screen!), depending on what method
 * SDL uses to send pixels to the system.
 *
 * \param window the window to update
 * \param rects an array of SDL_Rect structures representing areas of the
 *              surface to copy, in pixels
 * \param numrects the number of rectangles
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowSurface
 * \sa SDL_UpdateWindowSurface
  }
(* Const before type ignored *)
function SDL_UpdateWindowSurfaceRects(window:PSDL_Window; rects:PSDL_Rect; numrects:longint):longint;cdecl;external;
{*
 * Destroy the surface associated with the window.
 *
 * \param window the window to update
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowSurface
 * \sa SDL_WindowHasSurface
  }
function SDL_DestroyWindowSurface(window:PSDL_Window):longint;cdecl;external;
{*
 * Set a window's input grab mode.
 *
 * When input is grabbed, the mouse is confined to the window. This function
 * will also grab the keyboard if `SDL_HINT_GRAB_KEYBOARD` is set. To grab the
 * keyboard without also grabbing the mouse, use SDL_SetWindowKeyboardGrab().
 *
 * If the caller enables a grab while another window is currently grabbed, the
 * other window loses its grab in favor of the caller's window.
 *
 * \param window the window for which the input grab mode should be set
 * \param grabbed SDL_TRUE to grab input or SDL_FALSE to release input
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetGrabbedWindow
 * \sa SDL_GetWindowGrab
  }
function SDL_SetWindowGrab(window:PSDL_Window; grabbed:TSDL_bool):longint;cdecl;external;
{*
 * Set a window's keyboard grab mode.
 *
 * Keyboard grab enables capture of system keyboard shortcuts like Alt+Tab or
 * the Meta/Super key. Note that not all system keyboard shortcuts can be
 * captured by applications (one example is Ctrl+Alt+Del on Windows).
 *
 * This is primarily intended for specialized applications such as VNC clients
 * or VM frontends. Normal games should not use keyboard grab.
 *
 * When keyboard grab is enabled, SDL will continue to handle Alt+Tab when the
 * window is full-screen to ensure the user is not trapped in your
 * application. If you have a custom keyboard shortcut to exit fullscreen
 * mode, you may suppress this behavior with
 * `SDL_HINT_ALLOW_ALT_TAB_WHILE_GRABBED`.
 *
 * If the caller enables a grab while another window is currently grabbed, the
 * other window loses its grab in favor of the caller's window.
 *
 * \param window The window for which the keyboard grab mode should be set.
 * \param grabbed This is SDL_TRUE to grab keyboard, and SDL_FALSE to release.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowKeyboardGrab
 * \sa SDL_SetWindowMouseGrab
 * \sa SDL_SetWindowGrab
  }
function SDL_SetWindowKeyboardGrab(window:PSDL_Window; grabbed:TSDL_bool):longint;cdecl;external;
{*
 * Set a window's mouse grab mode.
 *
 * Mouse grab confines the mouse cursor to the window.
 *
 * \param window The window for which the mouse grab mode should be set.
 * \param grabbed This is SDL_TRUE to grab mouse, and SDL_FALSE to release.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowMouseGrab
 * \sa SDL_SetWindowKeyboardGrab
 * \sa SDL_SetWindowGrab
  }
function SDL_SetWindowMouseGrab(window:PSDL_Window; grabbed:TSDL_bool):longint;cdecl;external;
{*
 * Get a window's input grab mode.
 *
 * \param window the window to query
 * \returns SDL_TRUE if input is grabbed, SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowGrab
  }
function SDL_GetWindowGrab(window:PSDL_Window):TSDL_bool;cdecl;external;
{*
 * Get a window's keyboard grab mode.
 *
 * \param window the window to query
 * \returns SDL_TRUE if keyboard is grabbed, and SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowKeyboardGrab
 * \sa SDL_GetWindowGrab
  }
function SDL_GetWindowKeyboardGrab(window:PSDL_Window):TSDL_bool;cdecl;external;
{*
 * Get a window's mouse grab mode.
 *
 * \param window the window to query
 * \returns SDL_TRUE if mouse is grabbed, and SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowKeyboardGrab
 * \sa SDL_GetWindowGrab
  }
function SDL_GetWindowMouseGrab(window:PSDL_Window):TSDL_bool;cdecl;external;
{*
 * Get the window that currently has an input grab enabled.
 *
 * \returns the window if input is grabbed or NULL otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowGrab
 * \sa SDL_SetWindowGrab
  }
function SDL_GetGrabbedWindow:PSDL_Window;cdecl;external;
{*
 * Confines the cursor to the specified area of a window.
 *
 * Note that this does NOT grab the cursor, it only defines the area a cursor
 * is restricted to when the window has mouse focus.
 *
 * \param window The window that will be associated with the barrier.
 * \param rect A rectangle area in window-relative coordinates. If NULL the
 *             barrier for the specified window will be destroyed.
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowMouseRect
 * \sa SDL_SetWindowMouseGrab
  }
(* Const before type ignored *)
function SDL_SetWindowMouseRect(window:PSDL_Window; rect:PSDL_Rect):longint;cdecl;external;
{*
 * Get the mouse confinement rectangle of a window.
 *
 * \param window The window to query
 * \returns A pointer to the mouse confinement rectangle of a window, or NULL
 *          if there isn't one.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowMouseRect
  }
(* Const before type ignored *)
function SDL_GetWindowMouseRect(window:PSDL_Window):PSDL_Rect;cdecl;external;
{*
 * Set the opacity for a window.
 *
 * The parameter `opacity` will be clamped internally between 0.0f
 * (transparent) and 1.0f (opaque).
 *
 * This function also returns -1 if setting the opacity isn't supported.
 *
 * \param window the window which will be made transparent or opaque
 * \param opacity the opacity value (0.0f - transparent, 1.0f - opaque)
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetWindowOpacity
  }
function SDL_SetWindowOpacity(window:PSDL_Window; opacity:single):longint;cdecl;external;
{*
 * Get the opacity of a window.
 *
 * If transparency isn't supported on this platform, opacity will be reported
 * as 1.0f without error.
 *
 * The parameter `opacity` is ignored if it is NULL.
 *
 * This function also returns -1 if an invalid window was provided.
 *
 * \param window the window to get the current opacity value from
 * \param out_opacity the float filled in (0.0f - transparent, 1.0f - opaque)
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetWindowOpacity
  }
function SDL_GetWindowOpacity(window:PSDL_Window; out_opacity:Psingle):longint;cdecl;external;
{*
 * Set the window as a modal for another window.
 *
 * \param modal_window the window that should be set modal
 * \param parent_window the parent window for the modal window
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetWindowModalFor(modal_window:PSDL_Window; parent_window:PSDL_Window):longint;cdecl;external;
{*
 * Explicitly set input focus to the window.
 *
 * You almost certainly want SDL_RaiseWindow() instead of this function. Use
 * this with caution, as you might give focus to a window that is completely
 * obscured by other windows.
 *
 * \param window the window that should get the input focus
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RaiseWindow
  }
function SDL_SetWindowInputFocus(window:PSDL_Window):longint;cdecl;external;
{*
 * Set whether the window may have input focus.
 *
 * \param window the window to set focusable state
 * \param focusable SDL_TRUE to allow input focus, SDL_FALSE to not allow
 *                  input focus
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetWindowFocusable(window:PSDL_Window; focusable:TSDL_bool):longint;cdecl;external;
{*
 * Display the system-level window menu.
 *
 * This default window menu is provided by the system and on some platforms
 * provides functionality for setting or changing privileged state on the
 * window, such as moving it between workspaces or displays, or toggling the
 * always-on-top property.
 *
 * On platforms or desktops where this is unsupported, this function does
 * nothing.
 *
 * \param window the window for which the menu will be displayed
 * \param x the x coordinate of the menu, relative to the origin (top-left) of
 *          the client area
 * \param y the y coordinate of the menu, relative to the origin (top-left) of
 *          the client area
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ShowWindowSystemMenu(window:PSDL_Window; x:longint; y:longint):longint;cdecl;external;
{*
 * Possible return values from the SDL_HitTest callback.
 *
 * \sa SDL_HitTest
  }
{*< Region is normal. No special properties.  }
{*< Region can drag entire window.  }
type
  PSDL_HitTestResult = ^TSDL_HitTestResult;
  TSDL_HitTestResult =  Longint;
  Const
    SDL_HITTEST_NORMAL = 0;
    SDL_HITTEST_DRAGGABLE = 1;
    SDL_HITTEST_RESIZE_TOPLEFT = 2;
    SDL_HITTEST_RESIZE_TOP = 3;
    SDL_HITTEST_RESIZE_TOPRIGHT = 4;
    SDL_HITTEST_RESIZE_RIGHT = 5;
    SDL_HITTEST_RESIZE_BOTTOMRIGHT = 6;
    SDL_HITTEST_RESIZE_BOTTOM = 7;
    SDL_HITTEST_RESIZE_BOTTOMLEFT = 8;
    SDL_HITTEST_RESIZE_LEFT = 9;

{*
 * Callback used for hit-testing.
 *
 * \param win the SDL_Window where hit-testing was set on
 * \param area an SDL_Point which should be hit-tested
 * \param data what was passed as `callback_data` to SDL_SetWindowHitTest()
 * \returns an SDL_HitTestResult value.
 *
 * \sa SDL_SetWindowHitTest
  }
(* Const before type ignored *)
type

  TSDL_HitTest = function (win:PSDL_Window; area:PSDL_Point; data:pointer):TSDL_HitTestResult;cdecl;
{*
 * Provide a callback that decides if a window region has special properties.
 *
 * Normally windows are dragged and resized by decorations provided by the
 * system window manager (a title bar, borders, etc), but for some apps, it
 * makes sense to drag them from somewhere else inside the window itself; for
 * example, one might have a borderless window that wants to be draggable from
 * any part, or simulate its own title bar, etc.
 *
 * This function lets the app provide a callback that designates pieces of a
 * given window as special. This callback is run during event processing if we
 * need to tell the OS to treat a region of the window specially; the use of
 * this callback is known as "hit testing."
 *
 * Mouse input may not be delivered to your application if it is within a
 * special area; the OS will often apply that input to moving the window or
 * resizing the window and not deliver it to the application.
 *
 * Specifying NULL for a callback disables hit-testing. Hit-testing is
 * disabled by default.
 *
 * Platforms that don't support this functionality will return -1
 * unconditionally, even if you're attempting to disable hit-testing.
 *
 * Your callback may fire at any time, and its firing does not indicate any
 * specific behavior (for example, on Windows, this certainly might fire when
 * the OS is deciding whether to drag your window, but it fires for lots of
 * other reasons, too, some unrelated to anything you probably care about _and
 * when the mouse isn't actually at the location it is testing_). Since this
 * can fire at any time, you should try to keep your callback efficient,
 * devoid of allocations, etc.
 *
 * \param window the window to set hit-testing on
 * \param callback the function to call when doing a hit-test
 * \param callback_data an app-defined void pointer passed to **callback**
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_SetWindowHitTest(window:PSDL_Window; callback:TSDL_HitTest; callback_data:pointer):longint;cdecl;external;
{*
 * Set the shape of a transparent window.
 *
 * This sets the alpha channel of a transparent window and any fully
 * transparent areas are also transparent to mouse clicks. If you are using
 * something besides the SDL render API, then you are responsible for setting
 * the alpha channel of the window yourself.
 *
 * The shape is copied inside this function, so you can free it afterwards. If
 * your shape surface changes, you should call SDL_SetWindowShape() again to
 * update the window.
 *
 * The window must have been created with the SDL_WINDOW_TRANSPARENT flag.
 *
 * \param window the window
 * \param shape the surface representing the shape of the window, or NULL to
 *              remove any current shape
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetWindowShape(window:PSDL_Window; shape:PSDL_Surface):longint;cdecl;external;
{*
 * Request a window to demand attention from the user.
 *
 * \param window the window to be flashed
 * \param operation the flash operation
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_FlashWindow(window:PSDL_Window; operation:TSDL_FlashOperation):longint;cdecl;external;
{*
 * Destroy a window.
 *
 * If `window` is NULL, this function will return immediately after setting
 * the SDL error message to "Invalid window". See SDL_GetError().
 *
 * \param window the window to destroy
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreatePopupWindow
 * \sa SDL_CreateWindow
 * \sa SDL_CreateWindowWithProperties
  }
procedure SDL_DestroyWindow(window:PSDL_Window);cdecl;external;
{*
 * Check whether the screensaver is currently enabled.
 *
 * The screensaver is disabled by default since SDL 2.0.2. Before SDL 2.0.2
 * the screensaver was enabled by default.
 *
 * The default can also be changed using `SDL_HINT_VIDEO_ALLOW_SCREENSAVER`.
 *
 * \returns SDL_TRUE if the screensaver is enabled, SDL_FALSE if it is
 *          disabled.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DisableScreenSaver
 * \sa SDL_EnableScreenSaver
  }
function SDL_ScreenSaverEnabled:TSDL_bool;cdecl;external;
{*
 * Allow the screen to be blanked by a screen saver.
 *
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DisableScreenSaver
 * \sa SDL_ScreenSaverEnabled
  }
function SDL_EnableScreenSaver:longint;cdecl;external;
{*
 * Prevent the screen from being blanked by a screen saver.
 *
 * If you disable the screensaver, it is automatically re-enabled when SDL
 * quits.
 *
 * The screensaver is disabled by default since SDL 2.0.2. Before SDL 2.0.2
 * the screensaver was enabled by default.
 *
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_EnableScreenSaver
 * \sa SDL_ScreenSaverEnabled
  }
function SDL_DisableScreenSaver:longint;cdecl;external;
{*
 *  \name OpenGL support functions
  }
{ @  }
{*
 * Dynamically load an OpenGL library.
 *
 * This should be done after initializing the video driver, but before
 * creating any OpenGL windows. If no OpenGL library is loaded, the default
 * library will be loaded upon creation of the first OpenGL window.
 *
 * If you do this, you need to retrieve all of the GL functions used in your
 * program from the dynamic library using SDL_GL_GetProcAddress().
 *
 * \param path the platform dependent OpenGL library name, or NULL to open the
 *             default OpenGL library
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_GetProcAddress
 * \sa SDL_GL_UnloadLibrary
  }
(* Const before type ignored *)
function SDL_GL_LoadLibrary(path:Pchar):longint;cdecl;external;
{*
 * Get an OpenGL function by name.
 *
 * If the GL library is loaded at runtime with SDL_GL_LoadLibrary(), then all
 * GL functions must be retrieved this way. Usually this is used to retrieve
 * function pointers to OpenGL extensions.
 *
 * There are some quirks to looking up OpenGL functions that require some
 * extra care from the application. If you code carefully, you can handle
 * these quirks without any platform-specific code, though:
 *
 * - On Windows, function pointers are specific to the current GL context;
 *   this means you need to have created a GL context and made it current
 *   before calling SDL_GL_GetProcAddress(). If you recreate your context or
 *   create a second context, you should assume that any existing function
 *   pointers aren't valid to use with it. This is (currently) a
 *   Windows-specific limitation, and in practice lots of drivers don't suffer
 *   this limitation, but it is still the way the wgl API is documented to
 *   work and you should expect crashes if you don't respect it. Store a copy
 *   of the function pointers that comes and goes with context lifespan.
 * - On X11, function pointers returned by this function are valid for any
 *   context, and can even be looked up before a context is created at all.
 *   This means that, for at least some common OpenGL implementations, if you
 *   look up a function that doesn't exist, you'll get a non-NULL result that
 *   is _NOT_ safe to call. You must always make sure the function is actually
 *   available for a given GL context before calling it, by checking for the
 *   existence of the appropriate extension with SDL_GL_ExtensionSupported(),
 *   or verifying that the version of OpenGL you're using offers the function
 *   as core functionality.
 * - Some OpenGL drivers, on all platforms, *will* return NULL if a function
 *   isn't supported, but you can't count on this behavior. Check for
 *   extensions you use, and if you get a NULL anyway, act as if that
 *   extension wasn't available. This is probably a bug in the driver, but you
 *   can code defensively for this scenario anyhow.
 * - Just because you're on Linux/Unix, don't assume you'll be using X11.
 *   Next-gen display servers are waiting to replace it, and may or may not
 *   make the same promises about function pointers.
 * - OpenGL function pointers must be declared `APIENTRY` as in the example
 *   code. This will ensure the proper calling convention is followed on
 *   platforms where this matters (Win32) thereby avoiding stack corruption.
 *
 * \param proc the name of an OpenGL function
 * \returns a pointer to the named OpenGL function. The returned pointer
 *          should be cast to the appropriate function signature.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_ExtensionSupported
 * \sa SDL_GL_LoadLibrary
 * \sa SDL_GL_UnloadLibrary
  }
(* Const before type ignored *)
function SDL_GL_GetProcAddress(proc:Pchar):TSDL_FunctionPointer;cdecl;external;
{*
 * Get an EGL library function by name.
 *
 * If an EGL library is loaded, this function allows applications to get entry
 * points for EGL functions. This is useful to provide to an EGL API and
 * extension loader.
 *
 * \param proc the name of an EGL function
 * \returns a pointer to the named EGL function. The returned pointer should
 *          be cast to the appropriate function signature.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_GetCurrentEGLDisplay
  }
(* Const before type ignored *)
function SDL_EGL_GetProcAddress(proc:Pchar):TSDL_FunctionPointer;cdecl;external;
{*
 * Unload the OpenGL library previously loaded by SDL_GL_LoadLibrary().
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_LoadLibrary
  }
procedure SDL_GL_UnloadLibrary;cdecl;external;
{*
 * Check if an OpenGL extension is supported for the current context.
 *
 * This function operates on the current GL context; you must have created a
 * context and it must be current before calling this function. Do not assume
 * that all contexts you create will have the same set of extensions
 * available, or that recreating an existing context will offer the same
 * extensions again.
 *
 * While it's probably not a massive overhead, this function is not an O(1)
 * operation. Check the extensions you care about after creating the GL
 * context and save that information somewhere instead of calling the function
 * every time you need to know.
 *
 * \param extension the name of the extension to check
 * \returns SDL_TRUE if the extension is supported, SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
function SDL_GL_ExtensionSupported(extension:Pchar):TSDL_bool;cdecl;external;
{*
 * Reset all previously set OpenGL context attributes to their default values.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_GetAttribute
 * \sa SDL_GL_SetAttribute
  }
procedure SDL_GL_ResetAttributes;cdecl;external;
{*
 * Set an OpenGL window attribute before window creation.
 *
 * This function sets the OpenGL attribute `attr` to `value`. The requested
 * attributes should be set before creating an OpenGL window. You should use
 * SDL_GL_GetAttribute() to check the values after creating the OpenGL
 * context, since the values obtained can differ from the requested ones.
 *
 * \param attr an SDL_GLattr enum value specifying the OpenGL attribute to set
 * \param value the desired value for the attribute
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_GetAttribute
 * \sa SDL_GL_ResetAttributes
  }
function SDL_GL_SetAttribute(attr:TSDL_GLattr; value:longint):longint;cdecl;external;
{*
 * Get the actual value for an attribute from the current context.
 *
 * \param attr an SDL_GLattr enum value specifying the OpenGL attribute to get
 * \param value a pointer filled in with the current value of `attr`
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_ResetAttributes
 * \sa SDL_GL_SetAttribute
  }
function SDL_GL_GetAttribute(attr:TSDL_GLattr; value:Plongint):longint;cdecl;external;
{*
 * Create an OpenGL context for an OpenGL window, and make it current.
 *
 * Windows users new to OpenGL should note that, for historical reasons, GL
 * functions added after OpenGL version 1.1 are not available by default.
 * Those functions must be loaded at run-time, either with an OpenGL
 * extension-handling library or with SDL_GL_GetProcAddress() and its related
 * functions.
 *
 * SDL_GLContext is an alias for `void *`. It's opaque to the application.
 *
 * \param window the window to associate with the context
 * \returns the OpenGL context associated with `window` or NULL on error; call
 *          SDL_GetError() for more details.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_DeleteContext
 * \sa SDL_GL_MakeCurrent
  }
function SDL_GL_CreateContext(window:PSDL_Window):TSDL_GLContext;cdecl;external;
{*
 * Set up an OpenGL context for rendering into an OpenGL window.
 *
 * The context must have been created with a compatible window.
 *
 * \param window the window to associate with the context
 * \param context the OpenGL context to associate with the window
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_CreateContext
  }
function SDL_GL_MakeCurrent(window:PSDL_Window; context:TSDL_GLContext):longint;cdecl;external;
{*
 * Get the currently active OpenGL window.
 *
 * \returns the currently active OpenGL window on success or NULL on failure;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GL_GetCurrentWindow:PSDL_Window;cdecl;external;
{*
 * Get the currently active OpenGL context.
 *
 * \returns the currently active OpenGL context or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_MakeCurrent
  }
function SDL_GL_GetCurrentContext:TSDL_GLContext;cdecl;external;
{*
 * Get the currently active EGL display.
 *
 * \returns the currently active EGL display or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_EGL_GetCurrentEGLDisplay:TSDL_EGLDisplay;cdecl;external;
{*
 * Get the currently active EGL config.
 *
 * \returns the currently active EGL config or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_EGL_GetCurrentEGLConfig:TSDL_EGLConfig;cdecl;external;
{*
 * Get the EGL surface associated with the window.
 *
 * \param window the window to query
 * \returns the EGLSurface pointer associated with the window, or NULL on
 *          failure.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_EGL_GetWindowEGLSurface(window:PSDL_Window):TSDL_EGLSurface;cdecl;external;
{*
 * Sets the callbacks for defining custom EGLAttrib arrays for EGL
 * initialization.
 *
 * Each callback should return a pointer to an EGL attribute array terminated
 * with EGL_NONE. Callbacks may return NULL pointers to signal an error, which
 * will cause the SDL_CreateWindow process to fail gracefully.
 *
 * The arrays returned by each callback will be appended to the existing
 * attribute arrays defined by SDL.
 *
 * NOTE: These callback pointers will be reset after SDL_GL_ResetAttributes.
 *
 * \param platformAttribCallback Callback for attributes to pass to
 *                               eglGetPlatformDisplay.
 * \param surfaceAttribCallback Callback for attributes to pass to
 *                              eglCreateSurface.
 * \param contextAttribCallback Callback for attributes to pass to
 *                              eglCreateContext.
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_EGL_SetEGLAttributeCallbacks(platformAttribCallback:TSDL_EGLAttribArrayCallback; surfaceAttribCallback:TSDL_EGLIntArrayCallback; contextAttribCallback:TSDL_EGLIntArrayCallback);cdecl;external;
{*
 * Set the swap interval for the current OpenGL context.
 *
 * Some systems allow specifying -1 for the interval, to enable adaptive
 * vsync. Adaptive vsync works the same as vsync, but if you've already missed
 * the vertical retrace for a given frame, it swaps buffers immediately, which
 * might be less jarring for the user during occasional framerate drops. If an
 * application requests adaptive vsync and the system does not support it,
 * this function will fail and return -1. In such a case, you should probably
 * retry the call with 1 for the interval.
 *
 * Adaptive vsync is implemented for some glX drivers with
 * GLX_EXT_swap_control_tear, and for some Windows drivers with
 * WGL_EXT_swap_control_tear.
 *
 * Read more on the Khronos wiki:
 * https://www.khronos.org/opengl/wiki/Swap_Interval#Adaptive_Vsync
 *
 * \param interval 0 for immediate updates, 1 for updates synchronized with
 *                 the vertical retrace, -1 for adaptive vsync
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_GetSwapInterval
  }
function SDL_GL_SetSwapInterval(interval:longint):longint;cdecl;external;
{*
 * Get the swap interval for the current OpenGL context.
 *
 * If the system can't determine the swap interval, or there isn't a valid
 * current context, this function will set *interval to 0 as a safe default.
 *
 * \param interval Output interval value. 0 if there is no vertical retrace
 *                 synchronization, 1 if the buffer swap is synchronized with
 *                 the vertical retrace, and -1 if late swaps happen
 *                 immediately instead of waiting for the next retrace
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_SetSwapInterval
  }
function SDL_GL_GetSwapInterval(interval:Plongint):longint;cdecl;external;
{*
 * Update a window with OpenGL rendering.
 *
 * This is used with double-buffered OpenGL contexts, which are the default.
 *
 * On macOS, make sure you bind 0 to the draw framebuffer before swapping the
 * window, otherwise nothing will happen. If you aren't using
 * glBindFramebuffer(), this is the default and you won't have to do anything
 * extra.
 *
 * \param window the window to change
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GL_SwapWindow(window:PSDL_Window):longint;cdecl;external;
{*
 * Delete an OpenGL context.
 *
 * \param context the OpenGL context to be deleted
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GL_CreateContext
  }
function SDL_GL_DeleteContext(context:TSDL_GLContext):longint;cdecl;external;
{ @  }{ OpenGL support functions  }
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_video_h_  }

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X : longint) : longint;
begin
  SDL_WINDOWPOS_UNDEFINED_DISPLAY:=SDL_WINDOWPOS_UNDEFINED_MASK or X;
end;

{ was #define dname def_expr }
function SDL_WINDOWPOS_UNDEFINED : longint; { return type might be wrong }
  begin
    SDL_WINDOWPOS_UNDEFINED:=SDL_WINDOWPOS_UNDEFINED_DISPLAY(0);
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_WINDOWPOS_ISUNDEFINED(X : longint) : longint;
begin
//  SDL_WINDOWPOS_ISUNDEFINED:=(TX(@($FFFF0000)))=SDL_WINDOWPOS_UNDEFINED_MASK;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_WINDOWPOS_CENTERED_DISPLAY(X : longint) : longint;
begin
  SDL_WINDOWPOS_CENTERED_DISPLAY:=SDL_WINDOWPOS_CENTERED_MASK or X;
end;

{ was #define dname def_expr }
function SDL_WINDOWPOS_CENTERED : longint; { return type might be wrong }
  begin
    SDL_WINDOWPOS_CENTERED:=SDL_WINDOWPOS_CENTERED_DISPLAY(0);
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_WINDOWPOS_ISCENTERED(X : longint) : longint;
begin
//  SDL_WINDOWPOS_ISCENTERED:=(TX(@($FFFF0000)))=SDL_WINDOWPOS_CENTERED_MASK;
end;


end.
