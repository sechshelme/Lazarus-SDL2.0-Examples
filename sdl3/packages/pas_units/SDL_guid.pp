unit SDL_guid;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_guid_h_}
//{$define SDL_guid_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * An SDL_GUID is a 128-bit identifier for an input device that
 *   identifies that device across runs of SDL programs on the same
 *   platform.  If the device is detached and then re-attached to a
 *   different port, or if the base system is rebooted, the device
 *   should still report the same GUID.
 *
 * GUIDs are as precise as possible but are not guaranteed to
 *   distinguish physically distinct but equivalent devices.  For
 *   example, two game controllers from the same vendor with the same
 *   product ID and revision may have the same GUID.
 *
 * GUIDs may be platform-dependent (i.e., the same device may report
 *   different GUIDs on different operating systems).
  }
type
  PSDL_GUID = ^TSDL_GUID;
  TSDL_GUID = record
      data : array[0..15] of Uint8;
    end;
{ Function prototypes  }
{*
 * Get an ASCII string representation for a given ::SDL_GUID.
 *
 * You should supply at least 33 bytes for pszGUID.
 *
 * \param guid the ::SDL_GUID you wish to convert to string
 * \param pszGUID buffer in which to write the ASCII string
 * \param cbGUID the size of pszGUID
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GUIDFromString
  }

function SDL_GUIDToString(guid:TSDL_GUID; pszGUID:Pchar; cbGUID:longint):longint;cdecl;external;
{*
 * Convert a GUID string into a ::SDL_GUID structure.
 *
 * Performs no error checking. If this function is given a string containing
 * an invalid GUID, the function will silently succeed, but the GUID generated
 * will not be useful.
 *
 * \param pchGUID string containing an ASCII representation of a GUID
 * \returns a ::SDL_GUID structure.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GUIDToString
  }
(* Const before type ignored *)
function SDL_GUIDFromString(pchGUID:Pchar):TSDL_GUID;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_guid_h_  }

implementation


end.
