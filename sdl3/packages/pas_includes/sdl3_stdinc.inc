{$DEFINE read_interface}

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
 *  \file SDL_stdinc.h
 *
 *  This is a general header that includes C language support.
  }
//{$ifndef SDL_stdinc_h_}
//{$define SDL_stdinc_h_}
//{$include <SDL3/SDL_platform_defines.h>}
//{$if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L}
//{$include <inttypes.h>}
//{$endif}
//{$include <stdarg.h>}
//{$include <stdint.h>}
//{$include <string.h>}
//{$include <wchar.h>}
//{$ifndef SDL_DISABLE_ALLOCA}
//{$ifndef alloca}
//{$ifdef HAVE_ALLOCA_H}
//{$include <alloca.h>}
//(*** was #elif ****){$else defined(SDL_PLATFORM_NETBSD)}
//{$if defined(__STRICT_ANSI__)}
//{$define SDL_DISABLE_ALLOCA}
//{$else}
//{$include <stdlib.h>}
//{$endif}
//(*** was #elif ****){$else defined(__GNUC__)}

//const
//  alloca = __builtin_alloca;  
//(*** was #elif ****){$else defined(_MSC_VER)}
//{$include <malloc.h>}

//const
////  alloca = _alloca;  
//(*** was #elif ****){$else defined(__WATCOMC__)}
//{$include <malloc.h>}
//(*** was #elif ****){$else defined(__BORLANDC__)}
//{$include <malloc.h>}
//(*** was #elif ****){$else defined(__DMC__)}
//{$include <stdlib.h>}
//(*** was #elif ****){$else defined(SDL_PLATFORM_AIX)}
//(** unsupported pragma#pragma alloca*)
//(*** was #elif ****){$else defined(__MRC__)}

function alloca:Pchar;cdecl;external;

//function SDL_SIZE_MAX : Tsize_t;

{*
 * Check if the compiler supports a given builtin.
 * Supported by virtually all clang versions and recent gcc. Use this
 * instead of checking the clang version if possible.
  }
//{$ifdef __has_builtin}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

//function SDL_HAS_BUILTIN(x : longint) : longint;

//{$else}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

//function SDL_HAS_BUILTIN(x : longint) : longint;

//{$endif}
{*
 *  The number of elements in an array.
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

//function SDL_arraysize(array : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
//function SDL_TABLESIZE(table : longint) : longint;

{*
 *  Macro useful for building other macros with strings in them
 *
 *  e.g. #define LOG_ERROR(X) OutputDebugString(SDL_STRINGIFY_ARG(__FUNCTION__) ": " X "\n")
  }
{////////#define SDL_STRINGIFY_ARG(arg)  #arg }
{*
 *  \name Cast operators
 *
 *  Use proper C++ casts when compiled as C++ to be compatible with the option
 *  -Wold-style-cast of GCC (and -Werror=old-style-cast in GCC 4.2 and above).
  }
{ @  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
//function SDL_reinterpret_cast(_type,expression : longint) : Ttype;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
//function SDL_static_cast(_type,expression : longint) : Ttype;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
//function SDL_const_cast(_type,expression : longint) : Ttype;

{ @  }{ Cast operators  }
{ Define a four character code as a Uint32  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
//function SDL_FOURCC(A,B,C,D : longint) : longint;

{*
 *  \name Basic data types
  }
{ @  }
{*
 * A boolean type.
  }
const
  SDL_FALSE = 0;  
  SDL_TRUE = 1;  
type
  PSDL_bool = ^TSDL_bool;
  TSDL_bool = longint;
{*
 * A signed 8-bit integer type.
  }
{ 127  }

{ was #define dname def_expr }
function SDL_MAX_SINT8 : int8;

{ -128  }
{ was #define dname def_expr }
function SDL_MIN_SINT8 : int8;

type
  PSint8 = ^TSint8;
  TSint8 = int8;
{*
 * An unsigned 8-bit integer type.
  }
{ 255  }

{ was #define dname def_expr }
function SDL_MAX_UINT8 : Uint8;

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT8 : Uint8;

type
  PUint8 = ^TUint8;
  TUint8 = uint8;
{*
 * A signed 16-bit integer type.
  }
{ 32767  }

{ was #define dname def_expr }
function SDL_MAX_SINT16 : int16;

{ -32768  }
{ was #define dname def_expr }
function SDL_MIN_SINT16 : int16;

type
  PSint16 = ^TSint16;
  TSint16 = int16;
{*
 * An unsigned 16-bit integer type.
  }
{ 65535  }

{ was #define dname def_expr }
function SDL_MAX_UINT16 : Uint16;

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT16 : Uint16;

type
  PUint16 = ^TUint16;
  TUint16 = uint16;
{*
 * A signed 32-bit integer type.
  }
{ 2147483647  }

{ was #define dname def_expr }
function SDL_MAX_SINT32 : int32;

{ -2147483648  }
{ was #define dname def_expr }
function SDL_MIN_SINT32 : int32;

type
  PSint32 = ^TSint32;
  TSint32 = int32;
{*
 * An unsigned 32-bit integer type.
  }
{ 4294967295  }

{ was #define dname def_expr }
function SDL_MAX_UINT32 : Uint32;

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT32 : Uint32;

type
  PUint32 = ^TUint32;
  TUint32 = uint32;
{*
 * A signed 64-bit integer type.
  }
{ 9223372036854775807  }

{ was #define dname def_expr }
function SDL_MAX_SINT64 : int64;

{ -9223372036854775808  }
{ was #define dname def_expr }
function SDL_MIN_SINT64 : int64;

type
  PSint64 = ^int64;
  TSint64 = int64;
{*
 * An unsigned 64-bit integer type.
  }
{ 18446744073709551615  }

{ was #define dname def_expr }
function SDL_MAX_UINT64 : Uint64;

{ 0  }
{ was #define dname def_expr }
function SDL_MIN_UINT64 : Uint64;

type
  PUint64 = ^Uint64;
  TUint64 = int64;
{ @  }{ Basic data types  }
{*
 *  \name Floating-point constants
  }
{ @  }
{#ifdef FLT_EPSILON }
{#define SDL_FLT_EPSILON FLT_EPSILON }
{#else }
{ 0x0.000002p0  }

const
  SDL_FLT_EPSILON = 1.1920928955078125e-07;  
{#endif }
{ @  }{ Floating-point constants  }
{ Make sure we have macros for printing width-based integers.
 * <stdint.h> should define these but this is not true all platforms.
 * (for example win32)  }
//{$ifndef SDL_PRIs64}
//{$ifdef PRIs64}

//const
//  SDL_PRIs64 = PRIs64;  
//(*** was #elif ****){$else defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_GDK)}
//
//const
//  SDL_PRIs64 = 'I64d';  
//(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
//
//const
//  SDL_PRIs64 = 'ld';  
//{$else}
//
//const
//  SDL_PRIs64 = 'lld';  
//{$endif}
//{$endif}
//{$ifndef SDL_PRIu64}
//{$ifdef PRIu64}
//
//const
//  SDL_PRIu64 = PRIu64;  
//(*** was #elif ****){$else defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_GDK)}
//
//const
//  SDL_PRIu64 = 'I64u';  
//(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
//
//const
//  SDL_PRIu64 = 'lu';  
//{$else}
//
//const
//  SDL_PRIu64 = 'llu';  
//{$endif}
//{$endif}
//{$ifndef SDL_PRIx64}
//{$ifdef PRIx64}
//
//const
//  SDL_PRIx64 = PRIx64;  
//(*** was #elif ****){$else defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_GDK)}
//
//const
//  SDL_PRIx64 = 'I64x';  
//(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
//
//const
//  SDL_PRIx64 = 'lx';  
//{$else}
//
//const
//  SDL_PRIx64 = 'llx';  
//{$endif}
//{$endif}
//{$ifndef SDL_PRIX64}
//{$ifdef PRIX64}
//
//const
//  SDL_PRIX64 = PRIX64;  
//(*** was #elif ****){$else defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_GDK)}
//
//const
//  SDL_PRIX64 = 'I64X';  
//(*** was #elif ****){$else defined(__LP64__) && !defined(SDL_PLATFORM_APPLE)}
//
//const
//  SDL_PRIX64 = 'lX';  
//{$else}
//
//const
//  SDL_PRIX64 = 'llX';  
//{$endif}
//{$endif}
//{$ifndef SDL_PRIs32}
//{$ifdef PRId32}
//
//const
//  SDL_PRIs32 = PRId32;  
//{$else}
//
//const
//  SDL_PRIs32 = 'd';  
//{$endif}
//{$endif}
//{$ifndef SDL_PRIu32}
//{$ifdef PRIu32}
//
//const
//  SDL_PRIu32 = PRIu32;  
//{$else}
//
//const
//  SDL_PRIu32 = 'u';  
//{$endif}
//{$endif}
//{$ifndef SDL_PRIx32}
//{$ifdef PRIx32}
//
//const
//  SDL_PRIx32 = PRIx32;  
//{$else}
//
//const
//  SDL_PRIx32 = 'x';  
//{$endif}
//{$endif}
//{$ifndef SDL_PRIX32}
//{$ifdef PRIX32}
//
//const
//  SDL_PRIX32 = PRIX32;  
//{$else}
//
const
  SDL_PRIX32 = 'X';  
//{$endif}
//{$endif}
{ Annotations to help code analysis tools  }
//{$ifdef SDL_DISABLE_ANALYZE_MACROS}
{#define SDL_IN_BYTECAP(x) }
{#define SDL_INOUT_Z_CAP(x) }
{#define SDL_OUT_Z_CAP(x) }
{#define SDL_OUT_CAP(x) }
{#define SDL_OUT_BYTECAP(x) }
{#define SDL_OUT_Z_BYTECAP(x) }
{#define SDL_PRINTF_FORMAT_STRING }
{#define SDL_SCANF_FORMAT_STRING }
{#define SDL_PRINTF_VARARG_FUNC( fmtargnumber ) }
{#define SDL_PRINTF_VARARG_FUNCV( fmtargnumber ) }
{#define SDL_SCANF_VARARG_FUNC( fmtargnumber ) }
{#define SDL_SCANF_VARARG_FUNCV( fmtargnumber ) }
{#define SDL_WPRINTF_VARARG_FUNC( fmtargnumber ) }
{#define SDL_WSCANF_VARARG_FUNC( fmtargnumber ) }
//{$else}
//{$if defined(_MSC_VER) && (_MSC_VER >= 1600) /* VS 2010 and above */}
//{$include <sal.h>}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

//function SDL_IN_BYTECAP(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_INOUT_Z_CAP(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_Z_CAP(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_CAP(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_BYTECAP(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_Z_BYTECAP(x : longint) : longint;
//
//const
//  SDL_PRINTF_FORMAT_STRING = _Printf_format_string_;  
//  SDL_SCANF_FORMAT_STRING = _Scanf_format_string_impl_;  
//{$else}
{#define SDL_IN_BYTECAP(x) }
{#define SDL_INOUT_Z_CAP(x) }
{//#define SDL_OUT_Z_CAP(x) }
{//#define SDL_OUT_CAP(x) }
{#define SDL_OUT_BYTECAP(x) }
{#define SDL_OUT_Z_BYTECAP(x) }
//{$define SDL_PRINTF_FORMAT_STRING}
//{$define SDL_SCANF_FORMAT_STRING}
//{$endif}
//{$ifdef __GNUC__}
{#define SDL_PRINTF_VARARG_FUNC( fmtargnumber ) __attribute__ (( format( __printf__, fmtargnumber, fmtargnumber+1 ))) }
{#define SDL_PRINTF_VARARG_FUNCV( fmtargnumber ) __attribute__(( format( __printf__, fmtargnumber, 0 ))) }
{#define SDL_SCANF_VARARG_FUNC( fmtargnumber ) __attribute__ (( format( __scanf__, fmtargnumber, fmtargnumber+1 ))) }
{#define SDL_SCANF_VARARG_FUNCV( fmtargnumber ) __attribute__(( format( __scanf__, fmtargnumber, 0 ))) }
{#define SDL_WPRINTF_VARARG_FUNC( fmtargnumber ) /* __attribute__ (( format( __wprintf__, fmtargnumber, fmtargnumber+1 ))) */ }
{#define SDL_WSCANF_VARARG_FUNC( fmtargnumber ) /* __attribute__ (( format( __wscanf__, fmtargnumber, fmtargnumber+1 ))) */ }
//{$else}
{#define SDL_PRINTF_VARARG_FUNC( fmtargnumber ) }
{#define SDL_PRINTF_VARARG_FUNCV( fmtargnumber ) }
{#define SDL_SCANF_VARARG_FUNC( fmtargnumber ) }
{#define SDL_SCANF_VARARG_FUNCV( fmtargnumber ) }
{#define SDL_WPRINTF_VARARG_FUNC( fmtargnumber ) }
{#define SDL_WSCANF_VARARG_FUNC( fmtargnumber ) }
//{$endif}
//{$endif}
{ SDL_DISABLE_ANALYZE_MACROS  }
{#ifndef SDL_COMPILE_TIME_ASSERT }
{#ifdef __cplusplus }
{#if (__cplusplus >= 201103L) }
{#define SDL_COMPILE_TIME_ASSERT(name, x)  static_assert(x, #x) }
{#endif }
{#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L) }
{#define SDL_COMPILE_TIME_ASSERT(name, x) _Static_assert(x, #x) }
{#endif }
{#endif /* !SDL_COMPILE_TIME_ASSERT */ }
{#ifndef SDL_COMPILE_TIME_ASSERT }
{ universal, but may trigger -Wunused-local-typedefs  }
{#define SDL_COMPILE_TIME_ASSERT(name, x)               \ }
{       typedef int SDL_compile_time_assert_ ## name[(x) * 2 - 1] }
{#endif }
{* \cond  }
//{$ifndef DOXYGEN_SHOULD_IGNORE_THIS}
{SDL_COMPILE_TIME_ASSERT(uint8, sizeof(Uint8) == 1); }
{SDL_COMPILE_TIME_ASSERT(sint8, sizeof(Sint8) == 1); }
{SDL_COMPILE_TIME_ASSERT(uint16, sizeof(Uint16) == 2); }
{SDL_COMPILE_TIME_ASSERT(sint16, sizeof(Sint16) == 2); }
{SDL_COMPILE_TIME_ASSERT(uint32, sizeof(Uint32) == 4); }
{SDL_COMPILE_TIME_ASSERT(sint32, sizeof(Sint32) == 4); }
{SDL_COMPILE_TIME_ASSERT(uint64, sizeof(Uint64) == 8); }
{SDL_COMPILE_TIME_ASSERT(sint64, sizeof(Sint64) == 8); }
//{$endif}
{ DOXYGEN_SHOULD_IGNORE_THIS  }
{* \endcond  }
{ Check to make sure enums are the size of ints, for structure packing.
   For both Watcom C/C++ and Borland C/C++ the compiler option that makes
   enums having the size of an int must be enabled.
   This is "-b" for Borland C/C++ and "-ei" for Watcom C/C++ (v11).
 }
{* \cond  }
{#ifndef DOXYGEN_SHOULD_IGNORE_THIS }
{#if !defined(SDL_PLATFORM_ANDROID) && !defined(SDL_PLATFORM_VITA) && !defined(SDL_PLATFORM_3DS) }
{ TODO: include/SDL_stdinc.h:174: error: size of array 'SDL_dummy_enum' is negative  }
type
  PSDL_DUMMY_ENUM = ^TSDL_DUMMY_ENUM;
  TSDL_DUMMY_ENUM =  Longint;
  Const
    DUMMY_ENUM_VALUE = 0;

{SDL_COMPILE_TIME_ASSERT(enum, sizeof(SDL_DUMMY_ENUM) == sizeof(int)); }
{#endif }
{#endif /* DOXYGEN_SHOULD_IGNORE_THIS */ }
{* \endcond  }
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
//{$ifndef SDL_DISABLE_ALLOCA}
{#define SDL_stack_alloc(type, count)    (type*)alloca(sizeof(type)*(count)) }
{#define SDL_stack_free(data) }
//{$else}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }

//function SDL_stack_alloc(_type,count : longint) : Ptype;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
//function SDL_stack_free(data : longint) : longint;

//{$endif}

function SDL_malloc(size:Tsize_t):pointer;cdecl;external;
function SDL_calloc(nmemb:Tsize_t; size:Tsize_t):pointer;cdecl;external;
function SDL_realloc(mem:pointer; size:Tsize_t):pointer;cdecl;external;
procedure SDL_free(mem:pointer);cdecl;external;
type
  PSDL_malloc_func = ^TSDL_malloc_func;
  TSDL_malloc_func = function (size:Tsize_t):pointer;cdecl;

  PSDL_calloc_func = ^TSDL_calloc_func;
  TSDL_calloc_func = function (nmemb:Tsize_t; size:Tsize_t):pointer;cdecl;

  PSDL_realloc_func = ^TSDL_realloc_func;
  TSDL_realloc_func = function (mem:pointer; size:Tsize_t):pointer;cdecl;

  TSDL_free_func = procedure (mem:pointer);cdecl;
  PSDL_free_func = ^TSDL_free_func;
{*
 * Get the original set of SDL memory functions
 *
 * \param malloc_func filled with malloc function
 * \param calloc_func filled with calloc function
 * \param realloc_func filled with realloc function
 * \param free_func filled with free function
 *
 * \since This function is available since SDL 3.0.0.
  }

procedure SDL_GetOriginalMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external;
{*
 * Get the current set of SDL memory functions
 *
 * \param malloc_func filled with malloc function
 * \param calloc_func filled with calloc function
 * \param realloc_func filled with realloc function
 * \param free_func filled with free function
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_GetMemoryFunctions(malloc_func:PSDL_malloc_func; calloc_func:PSDL_calloc_func; realloc_func:PSDL_realloc_func; free_func:PSDL_free_func);cdecl;external;
{*
 * Replace SDL's memory allocation functions with a custom set
 *
 * \param malloc_func custom malloc function
 * \param calloc_func custom calloc function
 * \param realloc_func custom realloc function
 * \param free_func custom free function
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_SetMemoryFunctions(malloc_func:TSDL_malloc_func; calloc_func:TSDL_calloc_func; realloc_func:TSDL_realloc_func; free_func:TSDL_free_func):longint;cdecl;external;
{*
 * Allocate memory aligned to a specific value
 *
 * If `alignment` is less than the size of `void *`, then it will be increased
 * to match that.
 *
 * The returned memory address will be a multiple of the alignment value, and
 * the amount of memory allocated will be a multiple of the alignment value.
 *
 * The memory returned by this function must be freed with SDL_aligned_free()
 *
 * \param alignment the alignment requested
 * \param size the size to allocate
 * \returns a pointer to the aligned memory
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_aligned_free
  }
function SDL_aligned_alloc(alignment:Tsize_t; size:Tsize_t):pointer;cdecl;external;
{*
 * Free memory allocated by SDL_aligned_alloc()
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_aligned_alloc
  }
procedure SDL_aligned_free(mem:pointer);cdecl;external;
{*
 * Get the number of outstanding (unfreed) allocations
 *
 * \returns the number of allocations
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetNumAllocations:longint;cdecl;external;
(* Const before type ignored *)
function SDL_getenv(name:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_setenv(name:Pchar; value:Pchar; overwrite:longint):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
type TSDL_qsort_func=function (para1:pointer; para2:pointer):LongInt;

procedure SDL_qsort(base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:TSDL_qsort_func);cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)

 type SDL_bsearch_func=function (para1:pointer; para2:pointer):longint;
function SDL_bsearch(key:pointer; base:pointer; nmemb:Tsize_t; size:Tsize_t; compare:SDL_bsearch_func):pointer;cdecl;external;
function SDL_abs(x:longint):longint;cdecl;external;
{ NOTE: these double-evaluate their arguments, so you should never have side effects in the parameters  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_min(x,y : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_max(x,y : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_clamp(x,a,b : longint) : longint;

function SDL_isalpha(x:longint):longint;cdecl;external;
function SDL_isalnum(x:longint):longint;cdecl;external;
function SDL_isblank(x:longint):longint;cdecl;external;
function SDL_iscntrl(x:longint):longint;cdecl;external;
function SDL_isdigit(x:longint):longint;cdecl;external;
function SDL_isxdigit(x:longint):longint;cdecl;external;
function SDL_ispunct(x:longint):longint;cdecl;external;
function SDL_isspace(x:longint):longint;cdecl;external;
function SDL_isupper(x:longint):longint;cdecl;external;
function SDL_islower(x:longint):longint;cdecl;external;
function SDL_isprint(x:longint):longint;cdecl;external;
function SDL_isgraph(x:longint):longint;cdecl;external;
function SDL_toupper(x:longint):longint;cdecl;external;
function SDL_tolower(x:longint):longint;cdecl;external;
(* Const before type ignored *)
function SDL_crc16(crc:TUint16; data:pointer; len:Tsize_t):TUint16;cdecl;external;
(* Const before type ignored *)
function SDL_crc32(crc:TUint32; data:pointer; len:Tsize_t):TUint32;cdecl;external;
(* Const before type ignored *)
function SDL_memcpy(dst:pointer; src:pointer; len:Tsize_t):pointer;cdecl;external;
{ Take advantage of compiler optimizations for memcpy  }
//{$ifndef SDL_SLOW_MEMCPY}
//{$ifdef SDL_memcpy}
//{$undef SDL_memcpy}
//{$endif}

//{$endif}
{#define SDL_copyp(dst, src)                                                                 \ }
{     SDL_COMPILE_TIME_ASSERT(SDL_copyp, sizeof (*(dst)) == sizeof (*(src)));              \ }
{    SDL_memcpy((dst), (src), sizeof(*(src))) }
(* Const before type ignored *)

function SDL_memmove(dst:pointer; src:pointer; len:Tsize_t):pointer;cdecl;external;
{ Take advantage of compiler optimizations for memmove  }
//{$ifndef SDL_SLOW_MEMMOVE}
//{$ifdef SDL_memmove}
//{$undef SDL_memmove}
//{$endif}

//{$endif}

function SDL_memset(dst:pointer; c:longint; len:Tsize_t):pointer;cdecl;external;
function SDL_memset4(dst:pointer; val:TUint32; dwords:Tsize_t):pointer;cdecl;external;
{ Take advantage of compiler optimizations for memset  }
//{$ifndef SDL_SLOW_MEMSET}
//{$ifdef SDL_memset}
//{$undef SDL_memset}
//{$endif}

//{$endif}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

//function SDL_zero(x : longint) : longint;

{#define SDL_zerop(x) SDL_memset((x), 0, sizeof(*(x))) }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
//function SDL_zeroa(x : longint) : longint;

(* Const before type ignored *)
(* Const before type ignored *)
function SDL_memcmp(s1:pointer; s2:pointer; len:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
function SDL_wcslen(wstr:Pwchar_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_wcsnlen(wstr:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_wcslcpy(dst:Pwchar_t; src:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_wcslcat(dst:Pwchar_t; src:Pwchar_t; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_wcsdup(wstr:Pwchar_t):Pwchar_t;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsstr(haystack:Pwchar_t; needle:Pwchar_t):Pwchar_t;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsnstr(haystack:Pwchar_t; needle:Pwchar_t; maxlen:Tsize_t):Pwchar_t;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcscmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsncmp(str1:Pwchar_t; str2:Pwchar_t; maxlen:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcscasecmp(str1:Pwchar_t; str2:Pwchar_t):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_wcsncasecmp(str1:Pwchar_t; str2:Pwchar_t; len:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
function SDL_wcstol(str:Pwchar_t; endp:PPwchar_t; base:longint):longint;cdecl;external;
(* Const before type ignored *)
function SDL_strlen(str:Pchar):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_strnlen(str:Pchar; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_strlcpy(dst:Pchar; src:Pchar; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_utf8strlcpy(dst:Pchar; src:Pchar; dst_bytes:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_strlcat(dst:Pchar; src:Pchar; maxlen:Tsize_t):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_strdup(str:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strndup(str:Pchar; maxlen:Tsize_t):Pchar;cdecl;external;
function SDL_strrev(str:Pchar):Pchar;cdecl;external;
function SDL_strupr(str:Pchar):Pchar;cdecl;external;
function SDL_strlwr(str:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strchr(str:Pchar; c:longint):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strrchr(str:Pchar; c:longint):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strstr(haystack:Pchar; needle:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strnstr(haystack:Pchar; needle:Pchar; maxlen:Tsize_t):Pchar;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strcasestr(haystack:Pchar; needle:Pchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_strtok_r(s1:Pchar; s2:Pchar; saveptr:PPchar):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_utf8strlen(str:Pchar):Tsize_t;cdecl;external;
(* Const before type ignored *)
function SDL_utf8strnlen(str:Pchar; bytes:Tsize_t):Tsize_t;cdecl;external;
function SDL_itoa(value:longint; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_uitoa(value:dword; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ltoa(value:longint; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ultoa(value:dword; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_lltoa(value:TSint64; str:Pchar; radix:longint):Pchar;cdecl;external;
function SDL_ulltoa(value:TUint64; str:Pchar; radix:longint):Pchar;cdecl;external;
(* Const before type ignored *)
function SDL_atoi(str:Pchar):longint;cdecl;external;
(* Const before type ignored *)
function SDL_atof(str:Pchar):cdouble;cdecl;external;
(* Const before type ignored *)
function SDL_strtol(str:Pchar; endp:PPchar; base:longint):longint;cdecl;external;
(* Const before type ignored *)
function SDL_strtoul(str:Pchar; endp:PPchar; base:longint):dword;cdecl;external;
(* Const before type ignored *)
function SDL_strtoll(str:Pchar; endp:PPchar; base:longint):TSint64;cdecl;external;
(* Const before type ignored *)
function SDL_strtoull(str:Pchar; endp:PPchar; base:longint):TUint64;cdecl;external;
(* Const before type ignored *)
function SDL_strtod(str:Pchar; endp:PPchar):cdouble;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strcmp(str1:Pchar; str2:Pchar):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strncmp(str1:Pchar; str2:Pchar; maxlen:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strcasecmp(str1:Pchar; str2:Pchar):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_strncasecmp(str1:Pchar; str2:Pchar; len:Tsize_t):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_sscanf(text:Pchar; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_sscanf(text:Pchar; fmt:Pchar):longint;cdecl;external;
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_vsscanf(text:Pchar; fmt:Pchar):longint;varargs; cdecl;external;
(* Const before type ignored *)
function SDL_snprintf(text:Pchar; maxlen:Tsize_t; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_snprintf(text:Pchar; maxlen:Tsize_t; fmt:Pchar):longint;cdecl;external;
(* Const before type ignored *)
function SDL_swprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t; args:array of const):longint;cdecl;external;
function SDL_swprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t):longint;cdecl;external;
(* Const before type ignored *)
function SDL_vsnprintf(text:Pchar; maxlen:Tsize_t; fmt:Pchar):longint;varargs;cdecl;external;
(* Const before type ignored *)
function SDL_vswprintf(text:Pwchar_t; maxlen:Tsize_t; fmt:Pwchar_t):longint;varargs;cdecl;external;
(* Const before type ignored *)
function SDL_asprintf(strp:PPchar; fmt:Pchar; args:array of const):longint;cdecl;external;
function SDL_asprintf(strp:PPchar; fmt:Pchar):longint;cdecl;external;
(* Const before type ignored *)
function SDL_vasprintf(strp:PPchar; fmt:Pchar):longint;varargs;cdecl;external;
{$ifndef SDL_PI_D}
{*< pi (double)  }

const
  SDL_PI_D = 3.141592653589793238462643383279502884;  
{$endif}
{$ifndef SDL_PI_F}
{*< pi (float)  }

const
  SDL_PI_F = 3.141592653589793238462643383279502884;  
{$endif}
{*
 * Use this function to compute arc cosine of `x`.
 *
 * The definition of `y = acos(x)` is `x = cos(y)`.
 *
 * Domain: `-1 <= x <= 1`
 *
 * Range: `0 <= y <= Pi`
 *
 * \param x floating point value, in radians.
 * \returns arc cosine of `x`.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_acos(x:cdouble):cdouble;cdecl;external;
function SDL_acosf(x:single):single;cdecl;external;
function SDL_asin(x:cdouble):cdouble;cdecl;external;
function SDL_asinf(x:single):single;cdecl;external;
function SDL_atan(x:cdouble):cdouble;cdecl;external;
function SDL_atanf(x:single):single;cdecl;external;
function SDL_atan2(y:cdouble; x:cdouble):cdouble;cdecl;external;
function SDL_atan2f(y:single; x:single):single;cdecl;external;
function SDL_ceil(x:cdouble):cdouble;cdecl;external;
function SDL_ceilf(x:single):single;cdecl;external;
function SDL_copysign(x:cdouble; y:cdouble):cdouble;cdecl;external;
function SDL_copysignf(x:single; y:single):single;cdecl;external;
function SDL_cos(x:cdouble):cdouble;cdecl;external;
function SDL_cosf(x:single):single;cdecl;external;
function SDL_exp(x:cdouble):cdouble;cdecl;external;
function SDL_expf(x:single):single;cdecl;external;
function SDL_fabs(x:cdouble):cdouble;cdecl;external;
function SDL_fabsf(x:single):single;cdecl;external;
function SDL_floor(x:cdouble):cdouble;cdecl;external;
function SDL_floorf(x:single):single;cdecl;external;
function SDL_trunc(x:cdouble):cdouble;cdecl;external;
function SDL_truncf(x:single):single;cdecl;external;
function SDL_fmod(x:cdouble; y:cdouble):cdouble;cdecl;external;
function SDL_fmodf(x:single; y:single):single;cdecl;external;
function SDL_log(x:cdouble):cdouble;cdecl;external;
function SDL_logf(x:single):single;cdecl;external;
function SDL_log10(x:cdouble):cdouble;cdecl;external;
function SDL_log10f(x:single):single;cdecl;external;
function SDL_modf(x:cdouble; y:Pdouble):cdouble;cdecl;external;
function SDL_modff(x:single; y:Psingle):single;cdecl;external;
function SDL_pow(x:cdouble; y:cdouble):cdouble;cdecl;external;
function SDL_powf(x:single; y:single):single;cdecl;external;
function SDL_round(x:cdouble):cdouble;cdecl;external;
function SDL_roundf(x:single):single;cdecl;external;
function SDL_lround(x:cdouble):longint;cdecl;external;
function SDL_lroundf(x:single):longint;cdecl;external;
function SDL_scalbn(x:cdouble; n:longint):cdouble;cdecl;external;
function SDL_scalbnf(x:single; n:longint):single;cdecl;external;
function SDL_sin(x:cdouble):cdouble;cdecl;external;
function SDL_sinf(x:single):single;cdecl;external;
function SDL_sqrt(x:cdouble):cdouble;cdecl;external;
function SDL_sqrtf(x:single):single;cdecl;external;
function SDL_tan(x:cdouble):cdouble;cdecl;external;
function SDL_tanf(x:single):single;cdecl;external;
{ The SDL implementation of iconv() returns these error codes  }
{ was #define dname def_expr }
function SDL_ICONV_ERROR : Tsize_t;  

{ was #define dname def_expr }
function SDL_ICONV_E2BIG : Tsize_t;  

{ was #define dname def_expr }
function SDL_ICONV_EILSEQ : Tsize_t;  

{ was #define dname def_expr }
function SDL_ICONV_EINVAL : Tsize_t;  

{ SDL_iconv_* are now always real symbols/types, not macros or inlined.  }
type
  PSDL_iconv_t = ^TSDL_iconv_t;
  TSDL_iconv_t = PSDL_iconv_data_t;
(* Const before type ignored *)
(* Const before type ignored *)

function SDL_iconv_open(tocode:Pchar; fromcode:Pchar):TSDL_iconv_t;cdecl;external;
function SDL_iconv_close(cd:TSDL_iconv_t):longint;cdecl;external;
(* Const before type ignored *)
function SDL_iconv(cd:TSDL_iconv_t; inbuf:PPchar; inbytesleft:Psize_t; outbuf:PPchar; outbytesleft:Psize_t):Tsize_t;cdecl;external;
{*
 * This function converts a buffer or string between encodings in one pass,
 * returning a string that must be freed with SDL_free() or NULL on error.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
function SDL_iconv_string(tocode:Pchar; fromcode:Pchar; inbuf:Pchar; inbytesleft:Tsize_t):Pchar;cdecl;external;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
//function SDL_iconv_utf8_locale(S : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_iconv_utf8_ucs2(S : longint) : PUint16;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_iconv_utf8_ucs4(S : longint) : PUint32;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_iconv_wchar_utf8(S : longint) : longint;

{ force builds using Clang's static analysis tools to use literal C runtime
   here, since there are possibly tests that are ineffective otherwise.  }
//{$if defined(__clang_analyzer__) && !defined(SDL_DISABLE_ANALYZE_MACROS)}
{ The analyzer knows about strlcpy even when the system doesn't provide it  }
//{$if !defined(HAVE_STRLCPY) && !defined(strlcpy)}
(* Const before type ignored *)

function strlcpy(dst:Pchar; src:Pchar; size:Tsize_t):Tsize_t;cdecl;external;
//{$endif}
{ The analyzer knows about strlcat even when the system doesn't provide it  }
//{$if !defined(HAVE_STRLCAT) && !defined(strlcat)}
(* Const before type ignored *)

function strlcat(dst:Pchar; src:Pchar; size:Tsize_t):Tsize_t;cdecl;external;
//{$endif}
//{$if !defined(HAVE_WCSLCPY) && !defined(wcslcpy)}
(* Const before type ignored *)

function wcslcpy(dst:Pwchar_t; src:Pwchar_t; size:Tsize_t):Tsize_t;cdecl;external;
//{$endif}
//{$if !defined(HAVE_WCSLCAT) && !defined(wcslcat)}
(* Const before type ignored *)

function wcslcat(dst:Pwchar_t; src:Pwchar_t; size:Tsize_t):Tsize_t;cdecl;external;
//{$endif}
{ Starting LLVM 16, the analyser errors out if these functions do not have
   their prototype defined (clang-diagnostic-implicit-function-declaration)  }
//{$include <stdlib.h>}
//{$include <stdio.h>}
//
//const
//  SDL_malloc = malloc;  
//  SDL_calloc = calloc;  
//  SDL_realloc = realloc;  
//  SDL_free = free;  
////{$ifndef SDL_memcpy}
//
//const
//  SDL_memcpy = memcpy;  
////{$endif}
////{$ifndef SDL_memmove}
//
//const
//  SDL_memmove = memmove;  
////{$endif}
////{$ifndef SDL_memset}
//
//const
//  SDL_memset = memset;  
////{$endif}

//const
//  SDL_memcmp = memcmp;  
//  SDL_strlcpy = strlcpy;  
//  SDL_strlcat = strlcat;  
//  SDL_strlen = strlen;  
//  SDL_wcslen = wcslen;  
//  SDL_wcslcpy = wcslcpy;  
//  SDL_wcslcat = wcslcat;  
//  SDL_strdup = strdup;  
//  SDL_wcsdup = wcsdup;  
//  SDL_strchr = strchr;  
//  SDL_strrchr = strrchr;  
//  SDL_strstr = strstr;  
//  SDL_wcsstr = wcsstr;  
//  SDL_strtok_r = strtok_r;  
//  SDL_strcmp = strcmp;  
//  SDL_wcscmp = wcscmp;  
//  SDL_strncmp = strncmp;  
//  SDL_wcsncmp = wcsncmp;  
//  SDL_strcasecmp = strcasecmp;  
//  SDL_strncasecmp = strncasecmp;  
//  SDL_sscanf = sscanf;  
//  SDL_vsscanf = vsscanf;  
//  SDL_snprintf = snprintf;  
//  SDL_vsnprintf = vsnprintf;  
//{$endif}
{*
 * If a * b would overflow, return -1. Otherwise store a * b via ret
 * and return 0.
 *
 * \since This function is available since SDL 3.0.0.
  }
{SDL_FORCE_INLINE int SDL_size_mul_overflow (size_t a, }
{                                            size_t b, }
{                                            size_t *ret) }
{ }
{    if (a != 0 && b > SDL_SIZE_MAX / a)  }
{        return -1; }
{     }
{    *ret = a * b; }
{    return 0; }
{ }
//{$if SDL_HAS_BUILTIN(__builtin_mul_overflow)}
{ This needs to be wrapped in an inline rather than being a direct #define,
 * because __builtin_mul_overflow() is type-generic, but we want to be
 * consistent about interpreting a and b as size_t.  }
{int SDL_size_mul_overflow_builtin (size_t a, }
{                                                     size_t b, }
{                                                     size_t *ret) }
{ }
{    return __builtin_mul_overflow(a, b, ret) == 0 ? 0 : -1; }
{ }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

//function SDL_size_mul_overflow(a,b,ret : longint) : longint;

//{$endif}
{*
 * If a + b would overflow, return -1. Otherwise store a + b via ret
 * and return 0.
 *
 * \since This function is available since SDL 3.0.0.
  }
{SDL_FORCE_INLINE int SDL_size_add_overflow (size_t a, }
{                                            size_t b, }
{                                            size_t *ret) }
{ }
{    if (b > SDL_SIZE_MAX - a)  }
{        return -1; }
{     }
{    *ret = a + b; }
{    return 0; }
{ }
//{$if SDL_HAS_BUILTIN(__builtin_add_overflow)}
{ This needs to be wrapped in an inline rather than being a direct #define,
 * the same as the call to __builtin_mul_overflow() above.  }
{SDL_FORCE_INLINE int SDL_size_add_overflow_builtin (size_t a, }
{                                                     size_t b, }
{                                                     size_t *ret) }
{ }
{    return __builtin_add_overflow(a, b, ret) == 0 ? 0 : -1; }
{ }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

//function SDL_size_add_overflow(a,b,ret : longint) : longint;

//{$endif}
{ This is a generic function pointer which should be cast to the type you expect  }
//{$ifdef SDL_FUNCTION_POINTER_IS_VOID_POINTER}
type
  PSDL_FunctionPointer = ^TSDL_FunctionPointer;
//  TSDL_FunctionPointer = pointer;
//{$else}
//type

  TSDL_FunctionPointer = procedure (para1:pointer);cdecl;
//{$endif}
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
//{$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_stdinc_h_  }

{$UNDEF read_interface}


{$DEFINE read_implementation}

{ was #define dname def_expr }
function SDL_SIZE_MAX : Tsize_t;
  begin
    SDL_SIZE_MAX:=Tsize_t(-(1));
  end;

//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_HAS_BUILTIN(x : longint) : longint;
//begin
//  SDL_HAS_BUILTIN:=__has_builtin(x);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_HAS_BUILTIN(x : longint) : longint;
//begin
//  SDL_HAS_BUILTIN:=0;
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_arraysize(array : longint) : longint;
//begin
//  SDL_arraysize:=(sizeof(array))/(sizeof(array[0]));
//end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
//function SDL_TABLESIZE(table : longint) : longint;
//begin
//  SDL_TABLESIZE:=SDL_arraysize(table);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_reinterpret_cast(_type,expression : longint) : Ttype;
//begin
//  SDL_reinterpret_cast:=Ttype(expression);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_static_cast(_type,expression : longint) : Ttype;
//begin
//  SDL_static_cast:=Ttype(expression);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_const_cast(_type,expression : longint) : Ttype;
//begin
//  SDL_const_cast:=Ttype(expression);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_FOURCC(A,B,C,D : longint) : longint;
//begin
//  SDL_FOURCC:=((((SDL_static_cast(Uint32,SDL_static_cast(Uint8,A))) shl 0) or ((SDL_static_cast(Uint32,SDL_static_cast(Uint8,B))) shl 8)) or ((SDL_static_cast(Uint32,SDL_static_cast(Uint8,C))) shl 16)) or ((SDL_static_cast(Uint32,SDL_static_cast(Uint8,D))) shl 24);
//end;

{ was #define dname def_expr }
function SDL_MAX_SINT8 : TSint8;
  begin
    SDL_MAX_SINT8:=TSint8($7F);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT8 : TSint8;
  begin
    SDL_MIN_SINT8:=TSint8( not ($7F));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT8 : TUint8;
  begin
    SDL_MAX_UINT8:=TUint8($FF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT8 : TUint8;
  begin
    SDL_MIN_UINT8:=TUint8($00);
  end;

{ was #define dname def_expr }
function SDL_MAX_SINT16 : TSint16;
  begin
    SDL_MAX_SINT16:=TSint16($7FFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT16 : TSint16;
  begin
    SDL_MIN_SINT16:=TSint16( not ($7FFF));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT16 : TUint16;
  begin
    SDL_MAX_UINT16:=TUint16($FFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT16 : TUint16;
  begin
    SDL_MIN_UINT16:=TUint16($0000);
  end;

{ was #define dname def_expr }
function SDL_MAX_SINT32 : TSint32;
  begin
    SDL_MAX_SINT32:=TSint32($7FFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT32 : TSint32;
  begin
    SDL_MIN_SINT32:=TSint32( not ($7FFFFFFF));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT32 : TUint32;
  begin
    SDL_MAX_UINT32:=TUint32($FFFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT32 : TUint32;
  begin
    SDL_MIN_UINT32:=TUint32($00000000);
  end;

{ was #define dname def_expr }
function SDL_MAX_SINT64 : TSint64;
  begin
    SDL_MAX_SINT64:=TSint64($7FFFFFFFFFFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_SINT64 : TSint64;
  begin
    SDL_MIN_SINT64:=TSint64( not ($7FFFFFFFFFFFFFFF));
  end;

{ was #define dname def_expr }
function SDL_MAX_UINT64 : Uint64;
  begin
    SDL_MAX_UINT64:=TUint64($FFFFFFFFFFFFFFFF);
  end;

{ was #define dname def_expr }
function SDL_MIN_UINT64 : Uint64;
  begin
    SDL_MIN_UINT64:=TUint64($0000000000000000);
  end;

//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_IN_BYTECAP(x : longint) : longint;
//begin
//  SDL_IN_BYTECAP:=_In_bytecount_(x);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_INOUT_Z_CAP(x : longint) : longint;
//begin
//  SDL_INOUT_Z_CAP:=_Inout_z_cap_(x);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_Z_CAP(x : longint) : longint;
//begin
//  SDL_OUT_Z_CAP:=_Out_z_cap_(x);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_CAP(x : longint) : longint;
//begin
//  SDL_OUT_CAP:=_Out_cap_(x);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_BYTECAP(x : longint) : longint;
//begin
//  SDL_OUT_BYTECAP:=_Out_bytecap_(x);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_OUT_Z_BYTECAP(x : longint) : longint;
//begin
//  SDL_OUT_Z_BYTECAP:=_Out_z_bytecap_(x);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_stack_alloc(_type,count : longint) : Ptype;
//begin
//  SDL_stack_alloc:=Ptype(SDL_malloc((sizeof(_type))*count));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_stack_free(data : longint) : longint;
//begin
//  SDL_stack_free:=SDL_free(data);
//end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_min(x,y : longint) : longint;
var
   if_local1 : longint;
(* result types are not known *)
begin
  if x<y then
    if_local1:=x
  else
    if_local1:=y;
  SDL_min:=if_local1;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_max(x,y : longint) : longint;
var
   if_local1 : longint;
(* result types are not known *)
begin
  if x>y then
    if_local1:=x
  else
    if_local1:=y;
  SDL_max:=if_local1;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_clamp(x,a,b : longint) : longint;
var
   if_local1, if_local2 : longint;
(* result types are not known *)
begin
  if x>b then
    if_local1:=b
  else
    if_local1:=x;
  if x<a then
    if_local2:=a
  else
    if_local2:=if_local1;
  SDL_clamp:=if_local2;
end;

//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_zero(x : longint) : longint;
//begin
//  SDL_zero:=SDL_memset(@(x),0,sizeof(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_zeroa(x : longint) : longint;
//begin
//  SDL_zeroa:=SDL_memset(x,0,sizeof(x));
//end;

{ was #define dname def_expr }
function SDL_ICONV_ERROR : Tsize_t;
  begin
    SDL_ICONV_ERROR:=Tsize_t(-(1));
  end;

{ was #define dname def_expr }
function SDL_ICONV_E2BIG : Tsize_t;
  begin
    SDL_ICONV_E2BIG:=Tsize_t(-(2));
  end;

{ was #define dname def_expr }
function SDL_ICONV_EILSEQ : Tsize_t;
  begin
    SDL_ICONV_EILSEQ:=Tsize_t(-(3));
  end;

{ was #define dname def_expr }
function SDL_ICONV_EINVAL : Tsize_t;
  begin
    SDL_ICONV_EINVAL:=Tsize_t(-(4));
  end;

//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_iconv_utf8_locale(S : longint) : longint;
//begin
//  SDL_iconv_utf8_locale:=SDL_iconv_string('','UTF-8',S,(SDL_strlen(S))+1);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_iconv_utf8_ucs2(S : longint) : PUint16;
//begin
//  SDL_iconv_utf8_ucs2:=PUint16(SDL_iconv_string('UCS-2','UTF-8',S,(SDL_strlen(S))+1));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function SDL_iconv_utf8_ucs4(S : longint) : PUint32;
//begin
//  SDL_iconv_utf8_ucs4:=PUint32(SDL_iconv_string('UCS-4','UTF-8',S,(SDL_strlen(S))+1));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_iconv_wchar_utf8(S : longint) : longint;
//begin
//  SDL_iconv_wchar_utf8:=SDL_iconv_string('UTF-8','WCHAR_T',Pchar(S),((SDL_wcslen(S))+1)*(sizeof(wchar_t)));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_size_mul_overflow(a,b,ret : longint) : longint;
//begin
//  SDL_size_mul_overflow:=SDL_size_mul_overflow_builtin(a,b,ret);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_size_add_overflow(a,b,ret : longint) : longint;
//begin
//  SDL_size_add_overflow:=SDL_size_add_overflow_builtin(a,b,ret);
//end;
//

{$UNDEF read_implementation}
