unit SDL_pen;

interface

uses
  SDL3_stdinc, SDL_guid;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//{$ifndef SDL_pen_h_}
//{$define SDL_pen_h_}
//{$include "SDL_error.h"}
//{$include "SDL_guid.h"}
//{$include "SDL_stdinc.h"}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
type
  PSDL_PenID = ^TSDL_PenID;
  TSDL_PenID = Uint32;
{*< SDL_PenIDs identify pens uniquely within a session  }
{*< Reserved invalid ::SDL_PenID is valid  }

{ was #define dname def_expr }
function SDL_PEN_INVALID : Uint32;

{*< Device ID for mouse events triggered by pen events  }
{ was #define dname def_expr }
function SDL_PEN_MOUSEID : Uint32;

{*< Marks unknown information when querying the pen  }
const
  SDL_PEN_INFO_UNKNOWN = -(1);  
{*
 * Pen axis indices
 *
 * Below are the valid indices to the "axis" array from ::SDL_PenMotionEvent and ::SDL_PenButtonEvent.
 * The axis indices form a contiguous range of ints from 0 to ::SDL_PEN_AXIS_LAST, inclusive.
 * All "axis[]" entries are either normalised to  0..1 or report a (positive or negative)
 * angle in degrees, with 0.0 representing the centre.
 * Not all pens/backends support all axes: unsupported entries are always "0.0f".
 *
 * To convert angles for tilt and rotation into vector representation, use
 * SDL_sinf on the XTILT, YTILT, or ROTATION component, e.g., "SDL_sinf(xtilt * SDL_PI_F / 180.0)".
  }
{*< Pen pressure.  Unidirectional: 0..1.0  }
{*< Pen horizontal tilt angle.  Bidirectional: -90.0..90.0 (left-to-right).
						The physical max/min tilt may be smaller than -90.0 / 90.0, cf. SDL_PenCapabilityInfo  }
{*< Pen vertical tilt angle.  Bidirectional: -90.0..90.0 (top-to-down).
						The physical max/min tilt may be smaller than -90.0 / 90.0, cf. SDL_PenCapabilityInfo  }
{*< Pen distance to drawing surface.  Unidirectional: 0.0..1.0  }
{*< Pen barrel rotation.  Bidirectional: -180..179.9 (clockwise, 0 is facing up, -180.0 is facing down).  }
{*< Pen finger wheel or slider (e.g., Airbrush Pen).  Unidirectional: 0..1.0  }
{*< Last valid axis index  }
{*< Last axis index plus 1  }
type
  PSDL_PenAxis = ^TSDL_PenAxis;
  TSDL_PenAxis =  Longint;
  Const
    SDL_PEN_AXIS_PRESSURE = 0;
    SDL_PEN_AXIS_XTILT = 1;
    SDL_PEN_AXIS_YTILT = 2;
    SDL_PEN_AXIS_DISTANCE = 3;
    SDL_PEN_AXIS_ROTATION = 4;
    SDL_PEN_AXIS_SLIDER = 5;
    SDL_PEN_NUM_AXES = 6;
    SDL_PEN_AXIS_LAST = SDL_PEN_NUM_AXES-1;

{ Pen flags.  These share a bitmask space with ::SDL_BUTTON_LEFT and friends.  }
{ Bit for storing that pen is touching the surface  }
  SDL_PEN_FLAG_DOWN_BIT_INDEX = 13;  
{ Bit for storing has-non-eraser-capability status  }
  SDL_PEN_FLAG_INK_BIT_INDEX = 14;  
{ Bit for storing is-eraser or has-eraser-capability property  }
  SDL_PEN_FLAG_ERASER_BIT_INDEX = 15;  
{ Bit for storing has-axis-0 property  }
  SDL_PEN_FLAG_AXIS_BIT_OFFSET = 16;  
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_PEN_CAPABILITY(capbit : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PEN_AXIS_CAPABILITY(axis : longint) : longint;

{*
 * Pen tips
 * @
  }
{*< Regular pen tip (for drawing) touched the surface  }
const
  SDL_PEN_TIP_INK = SDL_PEN_FLAG_INK_BIT_INDEX;  
{*< Eraser pen tip touched the surface  }
  SDL_PEN_TIP_ERASER = SDL_PEN_FLAG_ERASER_BIT_INDEX;  
{* @  }
{*
 * \defgroup SDL_PEN_CAPABILITIES Pen capabilities
 * Pen capabilities reported by ::SDL_GetPenCapabilities
 * @
  }
{*< Pen tip is currently touching the drawing surface.  }

{ was #define dname def_expr }
function SDL_PEN_DOWN_MASK : longint; { return type might be wrong }

{*< Pen has a regular drawing tip (::SDL_GetPenCapabilities).  For events (::SDL_PenButtonEvent, ::SDL_PenMotionEvent, ::SDL_GetPenStatus) this flag is mutually exclusive with ::SDL_PEN_ERASER_MASK .   }
{ was #define dname def_expr }
function SDL_PEN_INK_MASK : longint; { return type might be wrong }

{*< Pen has an eraser tip (::SDL_GetPenCapabilities) or is being used as eraser (::SDL_PenButtonEvent , ::SDL_PenMotionEvent , ::SDL_GetPenStatus)   }
{ was #define dname def_expr }
function SDL_PEN_ERASER_MASK : longint; { return type might be wrong }

{*< Pen provides pressure information in axis ::SDL_PEN_AXIS_PRESSURE  }
{ was #define dname def_expr }
function SDL_PEN_AXIS_PRESSURE_MASK : longint; { return type might be wrong }

{*< Pen provides horizontal tilt information in axis ::SDL_PEN_AXIS_XTILT  }
{ was #define dname def_expr }
function SDL_PEN_AXIS_XTILT_MASK : longint; { return type might be wrong }

{*< Pen provides vertical tilt information in axis ::SDL_PEN_AXIS_YTILT  }
{ was #define dname def_expr }
function SDL_PEN_AXIS_YTILT_MASK : longint; { return type might be wrong }

{*< Pen provides distance to drawing tablet in ::SDL_PEN_AXIS_DISTANCE  }
{ was #define dname def_expr }
function SDL_PEN_AXIS_DISTANCE_MASK : longint; { return type might be wrong }

{*< Pen provides barrel rotation information in axis ::SDL_PEN_AXIS_ROTATION  }
{ was #define dname def_expr }
function SDL_PEN_AXIS_ROTATION_MASK : longint; { return type might be wrong }

{*< Pen provides slider / finger wheel or similar in axis ::SDL_PEN_AXIS_SLIDER  }
{ was #define dname def_expr }
function SDL_PEN_AXIS_SLIDER_MASK : longint; { return type might be wrong }


function   SDL_PEN_AXIS_BIDIRECTIONAL_MASKS:LongInt;


//const
//  SDL_PEN_AXIS_BIDIRECTIONAL_MASKS = SDL_PEN_AXIS_XTILT_MASK or SDL_PEN_AXIS_YTILT_MASK;  
{*< Masks for all axes that may be bidirectional  }
{* @  }
{*
 * Pen types
 *
 * Some pens identify as a particular type of drawing device (e.g., an airbrush or a pencil).
 *
  }
{*< Eraser  }
{*< Generic pen; this is the default.  }
{*< Pencil  }
{*< Brush-like device  }
{*< Airbrush device that "sprays" ink  }
{*< Last valid pen type  }
type
  PSDL_PenSubtype = ^TSDL_PenSubtype;
  TSDL_PenSubtype =  Longint;
  Const
    SDL_PEN_TYPE_ERASER = 1;
    SDL_PEN_TYPE_PEN = 2;
    SDL_PEN_TYPE_PENCIL = 3;
    SDL_PEN_TYPE_BRUSH = 4;
    SDL_PEN_TYPE_AIRBRUSH = 5;
    SDL_PEN_TYPE_LAST = SDL_PEN_TYPE_AIRBRUSH;

{ Function prototypes  }
{*
 * Retrieves all pens that are connected to the system.
 *
 * Yields an array of ::SDL_PenID values. These identify and track pens
 * throughout a session. To track pens across sessions (program restart), use
 * ::SDL_GUID .
 *
 * \param count The number of pens in the array (number of array elements
 *              minus 1, i.e., not counting the terminator 0).
 * \returns A 0 terminated array of ::SDL_PenID values, or NULL on error. The
 *          array must be freed with ::SDL_free(). On a NULL return,
 *          ::SDL_GetError() is set.
 *
 * \since This function is available since SDL 3.0.0
  }

function SDL_GetPens(count:Plongint):PSDL_PenID;cdecl;external;
{*
 * Retrieves the pen's current status.
 *
 * If the pen is detached (cf. ::SDL_PenConnected), this operation may return
 * default values.
 *
 * \param instance_id The pen to query.
 * \param x Out-mode parameter for pen x coordinate. May be NULL.
 * \param y Out-mode parameter for pen y coordinate. May be NULL.
 * \param axes Out-mode parameter for axis information. May be null. The axes
 *             are in the same order as ::SDL_PenAxis.
 * \param num_axes Maximum number of axes to write to "axes".
 * \returns a bit mask with the current pen button states (::SDL_BUTTON_LMASK
 *          etc.), possibly ::SDL_PEN_DOWN_MASK, and exactly one of
 *          ::SDL_PEN_INK_MASK or ::SDL_PEN_ERASER_MASK , or 0 on error (see
 *          ::SDL_GetError()).
 *
 * \since This function is available since SDL 3.0.0
  }
function SDL_GetPenStatus(instance_id:TSDL_PenID; x:Psingle; y:Psingle; axes:Psingle; num_axes:Tsize_t):TUint32;cdecl;external;
{*
 * Retrieves an ::SDL_PenID for the given ::SDL_GUID.
 *
 * \param guid A pen GUID.
 * \returns A valid ::SDL_PenID, or ::SDL_PEN_INVALID if there is no matching
 *          SDL_PenID.
 *
 * \since This function is available since SDL 3.0.0
 *
 * \sa SDL_GUID
  }
function SDL_GetPenFromGUID(guid:TSDL_GUID):TSDL_PenID;cdecl;external;
{*
 * Retrieves the ::SDL_GUID for a given ::SDL_PenID.
 *
 * \param instance_id The pen to query.
 * \returns The corresponding pen GUID; persistent across multiple sessions.
 *          If "instance_id" is ::SDL_PEN_INVALID, returns an all-zeroes GUID.
 *
 * \since This function is available since SDL 3.0.0
 *
 * \sa SDL_PenForID
  }
function SDL_GetPenGUID(instance_id:TSDL_PenID):TSDL_GUID;cdecl;external;
{*
 * Checks whether a pen is still attached.
 *
 * If a pen is detached, it will not show up for ::SDL_GetPens(). Other
 * operations will still be available but may return default values.
 *
 * \param instance_id A pen ID.
 * \returns SDL_TRUE if "instance_id" is valid and the corresponding pen is
 *          attached, or SDL_FALSE otherwise.
 *
 * \since This function is available since SDL 3.0.0
  }
function SDL_PenConnected(instance_id:TSDL_PenID):TSDL_bool;cdecl;external;
{*
 * Retrieves a human-readable description for a ::SDL_PenID.
 *
 * \param instance_id The pen to query.
 * \returns A string that contains the name of the pen, intended for human
 *          consumption. The string might or might not be localised, depending
 *          on platform settings. It is not guaranteed to be unique; use
 *          ::SDL_GetPenGUID() for (best-effort) unique identifiers. The
 *          pointer is managed by the SDL pen subsystem and must not be
 *          deallocated. The pointer remains valid until SDL is shut down.
 *          Returns NULL on error (cf. ::SDL_GetError())
 *
 * \since This function is available since SDL 3.0.0
  }
(* Const before type ignored *)
function SDL_GetPenName(instance_id:TSDL_PenID):Pchar;cdecl;external;
{*
 * Pen capabilities, as reported by ::SDL_GetPenCapabilities()
  }
{*< Physical maximum tilt angle, for XTILT and YTILT, or SDL_PEN_INFO_UNKNOWN .  Pens cannot typically tilt all the way to 90 degrees, so this value is usually less than 90.0.  }
{*< For Wacom devices: wacom tool type ID, otherwise 0 (useful e.g. with libwacom)  }
{*< Number of pen buttons (not counting the pen tip), or SDL_PEN_INFO_UNKNOWN  }
type
  PSDL_PenCapabilityInfo = ^TSDL_PenCapabilityInfo;
  TSDL_PenCapabilityInfo = record
      max_tilt : single;
      wacom_id : TUint32;
      num_buttons : TSint8;
    end;
{*
 * Retrieves capability flags for a given ::SDL_PenID.
 *
 * \param instance_id The pen to query.
 * \param capabilities Detail information about pen capabilities, such as the
 *                     number of buttons
 * \returns a set of capability flags, cf. SDL_PEN_CAPABILITIES
 *
 * \since This function is available since SDL 3.0.0
  }

function SDL_GetPenCapabilities(instance_id:TSDL_PenID; capabilities:PSDL_PenCapabilityInfo):TUint32;cdecl;external;
{*
 * Retrieves the pen type for a given ::SDL_PenID.
 *
 * \param instance_id The pen to query.
 * \returns The corresponding pen type (cf. ::SDL_PenSubtype) or 0 on error.
 *          Note that the pen type does not dictate whether the pen tip is
 *          ::SDL_PEN_TIP_INK or ::SDL_PEN_TIP_ERASER; to determine whether a
 *          pen is being used for drawing or in eraser mode, check either the
 *          pen tip on ::SDL_EVENT_PEN_DOWN, or the flag ::SDL_PEN_ERASER_MASK
 *          in the pen state.
 *
 * \since This function is available since SDL 3.0.0
  }
function SDL_GetPenType(instance_id:TSDL_PenID):TSDL_PenSubtype;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
//{$endif}
{ SDL_pen_h_  }
{ vi: set ts=4 sw=4 expandtab:  }

implementation

{ was #define dname def_expr }
function SDL_PEN_INVALID : TUint32;
  begin
    SDL_PEN_INVALID:=TUint32(0);
  end;

{ was #define dname def_expr }
function SDL_PEN_MOUSEID : TUint32;
  begin
    SDL_PEN_MOUSEID:=TUint32(-(2));
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PEN_CAPABILITY(capbit : longint) : longint;
begin
  SDL_PEN_CAPABILITY:=1 shl capbit;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PEN_AXIS_CAPABILITY(axis : longint) : longint;
begin
  SDL_PEN_AXIS_CAPABILITY:=SDL_PEN_CAPABILITY(axis+SDL_PEN_FLAG_AXIS_BIT_OFFSET);
end;

{ was #define dname def_expr }
function SDL_PEN_DOWN_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_DOWN_MASK:=SDL_PEN_CAPABILITY(SDL_PEN_FLAG_DOWN_BIT_INDEX);
  end;

{ was #define dname def_expr }
function SDL_PEN_INK_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_INK_MASK:=SDL_PEN_CAPABILITY(SDL_PEN_FLAG_INK_BIT_INDEX);
  end;

{ was #define dname def_expr }
function SDL_PEN_ERASER_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_ERASER_MASK:=SDL_PEN_CAPABILITY(SDL_PEN_FLAG_ERASER_BIT_INDEX);
  end;

{ was #define dname def_expr }
function SDL_PEN_AXIS_PRESSURE_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_AXIS_PRESSURE_MASK:=SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_PRESSURE);
  end;

{ was #define dname def_expr }
function SDL_PEN_AXIS_XTILT_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_AXIS_XTILT_MASK:=SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_XTILT);
  end;

{ was #define dname def_expr }
function SDL_PEN_AXIS_YTILT_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_AXIS_YTILT_MASK:=SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_YTILT);
  end;

{ was #define dname def_expr }
function SDL_PEN_AXIS_DISTANCE_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_AXIS_DISTANCE_MASK:=SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_DISTANCE);
  end;

{ was #define dname def_expr }
function SDL_PEN_AXIS_ROTATION_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_AXIS_ROTATION_MASK:=SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_ROTATION);
  end;

{ was #define dname def_expr }
function SDL_PEN_AXIS_SLIDER_MASK : longint; { return type might be wrong }
  begin
    SDL_PEN_AXIS_SLIDER_MASK:=SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_SLIDER);
  end;

function SDL_PEN_AXIS_BIDIRECTIONAL_MASKS: LongInt;
begin
Result   := SDL_PEN_AXIS_XTILT_MASK or SDL_PEN_AXIS_YTILT_MASK;
end;


end.
