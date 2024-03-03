unit SDL3_timer;

interface

uses
  SDL3_stdinc;

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
//{$ifndef SDL_timer_h_}
//{$define SDL_timer_h_}
{*
 *  \file SDL_timer.h
 *
 *  Header for the SDL time management routines.
  }
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * SDL time constants
  }

const
  SDL_MS_PER_SECOND = 1000;  
  SDL_US_PER_SECOND = 1000000;  
  SDL_NS_PER_SECOND = 1000000000;  
  SDL_NS_PER_MS = 1000000;  
  SDL_NS_PER_US = 1000;  
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_MS_TO_NS(MS : longint) : Int64;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_NS_TO_MS(NS : Int64) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_US_TO_NS(US : longint) : Int64;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_NS_TO_US(NS : Int64) : longint;

{*
 * Get the number of milliseconds since SDL library initialization.
 *
 * \returns an unsigned 64-bit value representing the number of milliseconds
 *          since the SDL library initialized.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTicks:Uint64;cdecl;external;
{*
 * Get the number of nanoseconds since SDL library initialization.
 *
 * \returns an unsigned 64-bit value representing the number of nanoseconds
 *          since the SDL library initialized.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTicksNS:Uint64;cdecl;external;
{*
 * Get the current value of the high resolution counter.
 *
 * This function is typically used for profiling.
 *
 * The counter values are only meaningful relative to each other. Differences
 * between values can be converted to times by using
 * SDL_GetPerformanceFrequency().
 *
 * \returns the current counter value.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPerformanceFrequency
  }
function SDL_GetPerformanceCounter:Uint64;cdecl;external;
{*
 * Get the count per second of the high resolution counter.
 *
 * \returns a platform-specific count per second.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPerformanceCounter
  }
function SDL_GetPerformanceFrequency:Uint64;cdecl;external;
{*
 * Wait a specified number of milliseconds before returning.
 *
 * This function waits a specified number of milliseconds before returning. It
 * waits at least the specified time, but possibly longer due to OS
 * scheduling.
 *
 * \param ms the number of milliseconds to delay
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_Delay(ms:Uint32);cdecl;external;
{*
 * Wait a specified number of nanoseconds before returning.
 *
 * This function waits a specified number of nanoseconds before returning. It
 * waits at least the specified time, but possibly longer due to OS
 * scheduling.
 *
 * \param ns the number of nanoseconds to delay
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_DelayNS(ns:Uint64);cdecl;external;
{*
 * Function prototype for the timer callback function.
 *
 * The callback function is passed the current timer interval and returns
 * the next timer interval, in milliseconds. If the returned value is the same as the one
 * passed in, the periodic alarm continues, otherwise a new alarm is
 * scheduled. If the callback returns 0, the periodic alarm is cancelled.
  }
type

  TSDL_TimerCallback = function (interval:Uint32; param:pointer):Uint32;cdecl;
{*
 * Definition of the timer ID type.
  }

  PSDL_TimerID = ^TSDL_TimerID;
  TSDL_TimerID = Uint32;
{*
 * Call a callback function at a future time.
 *
 * If you use this function, you must pass `SDL_INIT_TIMER` to SDL_Init().
 *
 * The callback function is passed the current timer interval and the user
 * supplied parameter from the SDL_AddTimer() call and should return the next
 * timer interval. If the value returned from the callback is 0, the timer is
 * canceled.
 *
 * The callback is run on a separate thread.
 *
 * Timers take into account the amount of time it took to execute the
 * callback. For example, if the callback took 250 ms to execute and returned
 * 1000 (ms), the timer would only wait another 750 ms before its next
 * iteration.
 *
 * Timing may be inexact due to OS scheduling. Be sure to note the current
 * time with SDL_GetTicksNS() or SDL_GetPerformanceCounter() in case your
 * callback needs to adjust for variances.
 *
 * \param interval the timer delay, in milliseconds, passed to `callback`
 * \param callback the SDL_TimerCallback function to call when the specified
 *                 `interval` elapses
 * \param param a pointer that is passed to `callback`
 * \returns a timer ID or 0 if an error occurs; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RemoveTimer
  }

function SDL_AddTimer(interval:Uint32; callback:TSDL_TimerCallback; param:pointer):TSDL_TimerID;cdecl;external;
{*
 * Remove a timer created with SDL_AddTimer().
 *
 * \param id the ID of the timer to remove
 * \returns SDL_TRUE if the timer is removed or SDL_FALSE if the timer wasn't
 *          found.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_AddTimer
  }
function SDL_RemoveTimer(id:TSDL_TimerID):TSDL_bool;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_timer_h_  }

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_MS_TO_NS(MS : longint) : Int64;
begin
  SDL_MS_TO_NS:=(TUint64(MS))*SDL_NS_PER_MS;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_NS_TO_MS(NS : Int64) : longint;
begin
  SDL_NS_TO_MS:=NS div SDL_NS_PER_MS;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_US_TO_NS(US : longint) : Int64;
begin
  SDL_US_TO_NS:=(TUint64(US))*SDL_NS_PER_US;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_NS_TO_US(NS : Int64) : longint;
begin
  SDL_NS_TO_US:=NS div SDL_NS_PER_US;
end;


end.
