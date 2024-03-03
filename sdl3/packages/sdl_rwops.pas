unit SDL_rwops;

interface

uses
  SDL_stdinc;

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
 *  \file SDL_rwops.h
 *
 *  This file provides a general interface for SDL to read and write
 *  data streams.  It can easily be extended to files, memory, etc.
  }
//{$ifndef SDL_rwops_h_}
//{$define SDL_rwops_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_error.h>}
//{$include <SDL3/SDL_properties.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ RWops types  }
{*< Unknown stream type  }

const
  SDL_RWOPS_UNKNOWN = 0;  
{*< Win32 file  }
  SDL_RWOPS_WINFILE = 1;  
{*< Stdio file  }
  SDL_RWOPS_STDFILE = 2;  
{*< Android asset  }
  SDL_RWOPS_JNIFILE = 3;  
{*< Memory stream  }
  SDL_RWOPS_MEMORY = 4;  
{*< Read-Only memory stream  }
  SDL_RWOPS_MEMORY_RO = 5;  
{ RWops status, set by a read or write operation  }
{*< Everything is ready  }
  SDL_RWOPS_STATUS_READY = 0;  
{*< Read or write I/O error  }
  SDL_RWOPS_STATUS_ERROR = 1;  
{*< End of file  }
  SDL_RWOPS_STATUS_EOF = 2;  
{*< Non blocking I/O, not ready  }
  SDL_RWOPS_STATUS_NOT_READY = 3;  
{*< Tried to write a read-only buffer  }
  SDL_RWOPS_STATUS_READONLY = 4;  
{*< Tried to read a write-only buffer  }
  SDL_RWOPS_STATUS_WRITEONLY = 5;  
{*
 * This is the read/write operation structure -- very basic.
  }
{*
     *  Return the number of bytes in this rwops
     *
     *  \return the total size of the data stream, or -1 on error.
      }
{*
     *  Seek to \c offset relative to \c whence, one of stdio's whence values:
     *  SDL_RW_SEEK_SET, SDL_RW_SEEK_CUR, SDL_RW_SEEK_END
     *
     *  \return the final offset in the data stream, or -1 on error.
      }
{*
     *  Read up to \c size bytes from the data stream to the area pointed
     *  at by \c ptr.
     *
     *  \return the number of bytes read
      }
{*
     *  Write exactly \c size bytes from the area pointed at by \c ptr
     *  to data stream.
     *
     *  \return the number of bytes written
      }
(* Const before type ignored *)
{*
     *  Close and free an allocated SDL_RWops structure.
     *
     *  \return 0 if successful or -1 on write error when flushing data.
      }
{$ifdef SDL_PLATFORM_ANDROID}
(*** was #elif ****){$else defined(SDL_PLATFORM_WIN32) || defined(SDL_PLATFORM_GDK) || defined(SDL_PLATFORM_WINRT)}
{$endif}
type
  PSDL_RWops = ^TSDL_RWops;
  TSDL_RWops = record
      size : function (context:PSDL_RWops):int64;cdecl;
      seek : function (context:PSDL_RWops; offset:int64; whence:longint):int64;cdecl;
      read : function (context:PSDL_RWops; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;
      write : function (context:PSDL_RWops; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;
      close : function (context:PSDL_RWops):longint;cdecl;
      _type : TUint32;
      status : TUint32;
      props : TSDL_PropertiesID;
      hidden : record
          case longint of
            0 : ( androidio : record
                asset : pointer;
              end );
            1 : ( windowsio : record
                append : TSDL_bool;
                h : pointer;
                buffer : record
                    data : pointer;
                    size : Tsize_t;
                    left : Tsize_t;
                  end;
              end );
            2 : ( stdio : record
                autoclose : TSDL_bool;
                fp : pointer;
              end );
            3 : ( mem : record
                base : PUint8;
                here : PUint8;
                stop : PUint8;
              end );
            4 : ( unknown : record
                data1 : pointer;
                data2 : pointer;
              end );
          end;
    end;
{*
 *  \name RWFrom functions
 *
 *  Functions to create SDL_RWops structures from various data streams.
  }
{ @  }
{*
 * Use this function to create a new SDL_RWops structure for reading from
 * and/or writing to a named file.
 *
 * The `mode` string is treated roughly the same as in a call to the C
 * library's fopen(), even if SDL doesn't happen to use fopen() behind the
 * scenes.
 *
 * Available `mode` strings:
 *
 * - "r": Open a file for reading. The file must exist.
 * - "w": Create an empty file for writing. If a file with the same name
 *   already exists its content is erased and the file is treated as a new
 *   empty file.
 * - "a": Append to a file. Writing operations append data at the end of the
 *   file. The file is created if it does not exist.
 * - "r+": Open a file for update both reading and writing. The file must
 *   exist.
 * - "w+": Create an empty file for both reading and writing. If a file with
 *   the same name already exists its content is erased and the file is
 *   treated as a new empty file.
 * - "a+": Open a file for reading and appending. All writing operations are
 *   performed at the end of the file, protecting the previous content to be
 *   overwritten. You can reposition (fseek, rewind) the internal pointer to
 *   anywhere in the file for reading, but writing operations will move it
 *   back to the end of file. The file is created if it does not exist.
 *
 * **NOTE**: In order to open a file as a binary file, a "b" character has to
 * be included in the `mode` string. This additional "b" character can either
 * be appended at the end of the string (thus making the following compound
 * modes: "rb", "wb", "ab", "r+b", "w+b", "a+b") or be inserted between the
 * letter and the "+" sign for the mixed modes ("rb+", "wb+", "ab+").
 * Additional characters may follow the sequence, although they should have no
 * effect. For example, "t" is sometimes appended to make explicit the file is
 * a text file.
 *
 * This function supports Unicode filenames, but they must be encoded in UTF-8
 * format, regardless of the underlying operating system.
 *
 * As a fallback, SDL_RWFromFile() will transparently open a matching filename
 * in an Android app's `assets`.
 *
 * Closing the SDL_RWops will close the file handle SDL is holding internally.
 *
 * \param file a UTF-8 string representing the filename to open
 * \param mode an ASCII string representing the mode to be used for opening
 *             the file.
 * \returns a pointer to the SDL_RWops structure that is created, or NULL on
 *          failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWseek
 * \sa SDL_RWtell
 * \sa SDL_RWwrite
  }
(* Const before type ignored *)
(* Const before type ignored *)

function SDL_RWFromFile(file_:Pchar; mode:Pchar):PSDL_RWops;cdecl;external;
{*
 * Use this function to prepare a read-write memory buffer for use with
 * SDL_RWops.
 *
 * This function sets up an SDL_RWops struct based on a memory area of a
 * certain size, for both read and write access.
 *
 * This memory buffer is not copied by the RWops; the pointer you provide must
 * remain valid until you close the stream. Closing the stream will not free
 * the original buffer.
 *
 * If you need to make sure the RWops never writes to the memory buffer, you
 * should use SDL_RWFromConstMem() with a read-only buffer of memory instead.
 *
 * \param mem a pointer to a buffer to feed an SDL_RWops stream
 * \param size the buffer size, in bytes
 * \returns a pointer to a new SDL_RWops structure, or NULL if it fails; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWseek
 * \sa SDL_RWtell
 * \sa SDL_RWwrite
  }
function SDL_RWFromMem(mem:pointer; size:Tsize_t):PSDL_RWops;cdecl;external;
{*
 * Use this function to prepare a read-only memory buffer for use with RWops.
 *
 * This function sets up an SDL_RWops struct based on a memory area of a
 * certain size. It assumes the memory area is not writable.
 *
 * Attempting to write to this RWops stream will report an error without
 * writing to the memory buffer.
 *
 * This memory buffer is not copied by the RWops; the pointer you provide must
 * remain valid until you close the stream. Closing the stream will not free
 * the original buffer.
 *
 * If you need to write to a memory buffer, you should use SDL_RWFromMem()
 * with a writable buffer of memory instead.
 *
 * \param mem a pointer to a read-only buffer to feed an SDL_RWops stream
 * \param size the buffer size, in bytes
 * \returns a pointer to a new SDL_RWops structure, or NULL if it fails; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWseek
 * \sa SDL_RWtell
  }
(* Const before type ignored *)
function SDL_RWFromConstMem(mem:pointer; size:Tsize_t):PSDL_RWops;cdecl;external;
{ @  }{ RWFrom functions  }
{*
 * Use this function to allocate an empty, unpopulated SDL_RWops structure.
 *
 * Applications do not need to use this function unless they are providing
 * their own SDL_RWops implementation. If you just need an SDL_RWops to
 * read/write a common data source, you should use the built-in
 * implementations in SDL, like SDL_RWFromFile() or SDL_RWFromMem(), etc.
 *
 * You must free the returned pointer with SDL_DestroyRW(). Depending on your
 * operating system and compiler, there may be a difference between the
 * malloc() and free() your program uses and the versions SDL calls
 * internally. Trying to mix the two can cause crashing such as segmentation
 * faults. Since all SDL_RWops must free themselves when their **close**
 * method is called, all SDL_RWops must be allocated through this function, so
 * they can all be freed correctly with SDL_DestroyRW().
 *
 * \returns a pointer to the allocated memory on success, or NULL on failure;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DestroyRW
  }
function SDL_CreateRW:PSDL_RWops;cdecl;external;
{*
 * Use this function to free an SDL_RWops structure allocated by
 * SDL_CreateRW().
 *
 * Applications do not need to use this function unless they are providing
 * their own SDL_RWops implementation. If you just need an SDL_RWops to
 * read/write a common data source, you should use the built-in
 * implementations in SDL, like SDL_RWFromFile() or SDL_RWFromMem(), etc, and
 * call the **close** method on those SDL_RWops pointers when you are done
 * with them.
 *
 * Only use SDL_DestroyRW() on pointers returned by SDL_CreateRW(). The
 * pointer is invalid as soon as this function returns. Any extra memory
 * allocated during creation of the SDL_RWops is not freed by SDL_DestroyRW();
 * the programmer must be responsible for managing that memory in their
 * **close** method.
 *
 * \param context the SDL_RWops structure to be freed
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreateRW
  }
procedure SDL_DestroyRW(context:PSDL_RWops);cdecl;external;
{*
 * Get the properties associated with an SDL_RWops.
 *
 * \param context a pointer to an SDL_RWops structure
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetProperty
 * \sa SDL_SetProperty
  }
function SDL_GetRWProperties(context:PSDL_RWops):TSDL_PropertiesID;cdecl;external;
{*< Seek from the beginning of data  }
const
  SDL_RW_SEEK_SET = 0;  
{*< Seek relative to current read point  }
  SDL_RW_SEEK_CUR = 1;  
{*< Seek relative to the end of data  }
  SDL_RW_SEEK_END = 2;  
{*
 * Use this function to get the size of the data stream in an SDL_RWops.
 *
 * \param context the SDL_RWops to get the size of the data stream from
 * \returns the size of the data stream in the SDL_RWops on success or a
 *          negative error code on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_RWsize(context:PSDL_RWops):TSint64;cdecl;external;
{*
 * Seek within an SDL_RWops data stream.
 *
 * This function seeks to byte `offset`, relative to `whence`.
 *
 * `whence` may be any of the following values:
 *
 * - `SDL_RW_SEEK_SET`: seek from the beginning of data
 * - `SDL_RW_SEEK_CUR`: seek relative to current read point
 * - `SDL_RW_SEEK_END`: seek relative to the end of data
 *
 * If this stream can not seek, it will return -1.
 *
 * SDL_RWseek() is actually a wrapper function that calls the SDL_RWops's
 * `seek` method appropriately, to simplify application development.
 *
 * \param context a pointer to an SDL_RWops structure
 * \param offset an offset in bytes, relative to **whence** location; can be
 *               negative
 * \param whence any of `SDL_RW_SEEK_SET`, `SDL_RW_SEEK_CUR`,
 *               `SDL_RW_SEEK_END`
 * \returns the final offset in the data stream after the seek or a negative
 *          error code on failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWtell
 * \sa SDL_RWwrite
  }
function SDL_RWseek(context:PSDL_RWops; offset:TSint64; whence:longint):TSint64;cdecl;external;
{*
 * Determine the current read/write offset in an SDL_RWops data stream.
 *
 * SDL_RWtell is actually a wrapper function that calls the SDL_RWops's `seek`
 * method, with an offset of 0 bytes from `SDL_RW_SEEK_CUR`, to simplify
 * application development.
 *
 * \param context an SDL_RWops data stream object from which to get the
 *                current offset
 * \returns the current offset in the stream, or -1 if the information can not
 *          be determined.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWseek
 * \sa SDL_RWwrite
  }
function SDL_RWtell(context:PSDL_RWops):TSint64;cdecl;external;
{*
 * Read from a data source.
 *
 * This function reads up `size` bytes from the data source to the area
 * pointed at by `ptr`. This function may read less bytes than requested. It
 * will return zero when the data stream is completely read, or -1 on error.
 * For streams that support non-blocking operation, if nothing was read
 * because it would require blocking, this function returns -2 to distinguish
 * that this is not an error or end-of-file, and the caller can try again
 * later.
 *
 * SDL_RWread() is actually a function wrapper that calls the SDL_RWops's
 * `read` method appropriately, to simplify application development.
 *
 * It is an error to specify a negative `size`, but this parameter is signed
 * so you definitely cannot overflow the return value on a successful run with
 * enormous amounts of data.
 *
 * \param context a pointer to an SDL_RWops structure
 * \param ptr a pointer to a buffer to read data into
 * \param size the number of bytes to read from the data source.
 * \returns the number of bytes read, or 0 on end of file or other error.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWseek
 * \sa SDL_RWwrite
  }
function SDL_RWread(context:PSDL_RWops; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;external;
{*
 * Write to an SDL_RWops data stream.
 *
 * This function writes exactly `size` bytes from the area pointed at by `ptr`
 * to the stream. If this fails for any reason, it'll return less than `size`
 * to demonstrate how far the write progressed. On success, it returns `num`.
 *
 * On error, this function still attempts to write as much as possible, so it
 * might return a positive value less than the requested write size. If the
 * function failed to write anything and there was an actual error, it will
 * return -1. For streams that support non-blocking operation, if nothing was
 * written because it would require blocking, this function returns -2 to
 * distinguish that this is not an error and the caller can try again later.
 *
 * SDL_RWwrite is actually a function wrapper that calls the SDL_RWops's
 * `write` method appropriately, to simplify application development.
 *
 * It is an error to specify a negative `size`, but this parameter is signed
 * so you definitely cannot overflow the return value on a successful run with
 * enormous amounts of data.
 *
 * \param context a pointer to an SDL_RWops structure
 * \param ptr a pointer to a buffer containing data to write
 * \param size the number of bytes to write
 * \returns the number of bytes written, which will be less than `num` on
 *          error; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWprint
 * \sa SDL_RWread
 * \sa SDL_RWseek
  }
(* Const before type ignored *)
function SDL_RWwrite(context:PSDL_RWops; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;external;
{*
 * Print to an SDL_RWops data stream.
 *
 * This function does formatted printing to the stream.
 *
 * \param context a pointer to an SDL_RWops structure
 * \param fmt a printf() style format string
 * \param ... additional parameters matching % tokens in the `fmt` string, if
 *            any
 * \returns the number of bytes written, or 0 on error; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWseek
 * \sa SDL_RWwrite
  }
(* Const before type ignored *)
function SDL_RWprintf(context:PSDL_RWops; fmt:Pchar; args:array of const):Tsize_t;cdecl;external;
function SDL_RWprintf(context:PSDL_RWops; fmt:Pchar):Tsize_t;cdecl;external;
{*
 * Print to an SDL_RWops data stream.
 *
 * This function does formatted printing to the stream.
 *
 * \param context a pointer to an SDL_RWops structure
 * \param fmt a printf() style format string
 * \param ap a variable argument list
 * \returns the number of bytes written, or 0 on error; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWclose
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWseek
 * \sa SDL_RWwrite
  }
(* Const before type ignored *)
function SDL_RWvprintf(context:PSDL_RWops; fmt:Pchar):Tsize_t;varargs;cdecl;external;
{*
 * Close and free an allocated SDL_RWops structure.
 *
 * SDL_RWclose() closes and cleans up the SDL_RWops stream. It releases any
 * resources used by the stream and frees the SDL_RWops itself with
 * SDL_DestroyRW(). This returns 0 on success, or -1 if the stream failed to
 * flush to its output (e.g. to disk).
 *
 * Note that if this fails to flush the stream to disk, this function reports
 * an error, but the SDL_RWops is still invalid once this function returns.
 *
 * \param context SDL_RWops structure to close
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RWFromConstMem
 * \sa SDL_RWFromFile
 * \sa SDL_RWFromMem
 * \sa SDL_RWread
 * \sa SDL_RWseek
 * \sa SDL_RWwrite
  }
function SDL_RWclose(context:PSDL_RWops):longint;cdecl;external;
{*
 * Load all the data from an SDL data stream.
 *
 * The data is allocated with a zero byte at the end (null terminated) for
 * convenience. This extra byte is not included in the value reported via
 * `datasize`.
 *
 * The data should be freed with SDL_free().
 *
 * \param src the SDL_RWops to read all available data from
 * \param datasize if not NULL, will store the number of bytes read
 * \param freesrc if SDL_TRUE, calls SDL_RWclose() on `src` before returning,
 *                even in the case of an error
 * \returns the data, or NULL if there was an error.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_LoadFile_RW(src:PSDL_RWops; datasize:Psize_t; freesrc:TSDL_bool):pointer;cdecl;external;
{*
 * Load all the data from a file path.
 *
 * The data is allocated with a zero byte at the end (null terminated) for
 * convenience. This extra byte is not included in the value reported via
 * `datasize`.
 *
 * The data should be freed with SDL_free().
 *
 * \param file the path to read all available data from
 * \param datasize if not NULL, will store the number of bytes read
 * \returns the data, or NULL if there was an error.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)
function SDL_LoadFile(file_:Pchar; datasize:Psize_t):pointer;cdecl;external;
{*
 *  \name Read endian functions
 *
 *  Read an item of the specified endianness and return in native format.
  }
{ @  }
{*
 * Use this function to read a byte from an SDL_RWops.
 *
 * \param src the SDL_RWops to read from
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on success or SDL_FALSE on failure; call SDL_GetError()
 *          for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU8(src:PSDL_RWops; value:PUint8):TSDL_bool;cdecl;external;
{*
 * Use this function to read 16 bits of little-endian data from an SDL_RWops
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU16LE(src:PSDL_RWops; value:PUint16):TSDL_bool;cdecl;external;
{*
 * Use this function to read 16 bits of little-endian data from an SDL_RWops
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS16LE(src:PSDL_RWops; value:PSint16):TSDL_bool;cdecl;external;
{*
 * Use this function to read 16 bits of big-endian data from an SDL_RWops and
 * return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU16BE(src:PSDL_RWops; value:PUint16):TSDL_bool;cdecl;external;
{*
 * Use this function to read 16 bits of big-endian data from an SDL_RWops and
 * return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS16BE(src:PSDL_RWops; value:PSint16):TSDL_bool;cdecl;external;
{*
 * Use this function to read 32 bits of little-endian data from an SDL_RWops
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU32LE(src:PSDL_RWops; value:PUint32):TSDL_bool;cdecl;external;
{*
 * Use this function to read 32 bits of little-endian data from an SDL_RWops
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS32LE(src:PSDL_RWops; value:PSint32):TSDL_bool;cdecl;external;
{*
 * Use this function to read 32 bits of big-endian data from an SDL_RWops and
 * return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU32BE(src:PSDL_RWops; value:PUint32):TSDL_bool;cdecl;external;
{*
 * Use this function to read 32 bits of big-endian data from an SDL_RWops and
 * return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS32BE(src:PSDL_RWops; value:PSint32):TSDL_bool;cdecl;external;
{*
 * Use this function to read 64 bits of little-endian data from an SDL_RWops
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU64LE(src:PSDL_RWops; value:PUint64):TSDL_bool;cdecl;external;
{*
 * Use this function to read 64 bits of little-endian data from an SDL_RWops
 * and return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS64LE(src:PSDL_RWops; value:PSint64):TSDL_bool;cdecl;external;
{*
 * Use this function to read 64 bits of big-endian data from an SDL_RWops and
 * return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadU64BE(src:PSDL_RWops; value:PUint64):TSDL_bool;cdecl;external;
{*
 * Use this function to read 64 bits of big-endian data from an SDL_RWops and
 * return in native format.
 *
 * SDL byteswaps the data only if necessary, so the data returned will be in
 * the native byte order.
 *
 * \param src the stream from which to read data
 * \param value a pointer filled in with the data read
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_ReadS64BE(src:PSDL_RWops; value:PSint64):TSDL_bool;cdecl;external;
{ @  }{ Read endian functions  }
{*
 *  \name Write endian functions
 *
 *  Write an item of native format to the specified endianness.
  }
{ @  }
{*
 * Use this function to write a byte to an SDL_RWops.
 *
 * \param dst the SDL_RWops to write to
 * \param value the byte value to write
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU8(dst:PSDL_RWops; value:TUint8):TSDL_bool;cdecl;external;
{*
 * Use this function to write 16 bits in native format to an SDL_RWops as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU16LE(dst:PSDL_RWops; value:TUint16):TSDL_bool;cdecl;external;
{*
 * Use this function to write 16 bits in native format to an SDL_RWops as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS16LE(dst:PSDL_RWops; value:TSint16):TSDL_bool;cdecl;external;
{*
 * Use this function to write 16 bits in native format to an SDL_RWops as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU16BE(dst:PSDL_RWops; value:TUint16):TSDL_bool;cdecl;external;
{*
 * Use this function to write 16 bits in native format to an SDL_RWops as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS16BE(dst:PSDL_RWops; value:TSint16):TSDL_bool;cdecl;external;
{*
 * Use this function to write 32 bits in native format to an SDL_RWops as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU32LE(dst:PSDL_RWops; value:TUint32):TSDL_bool;cdecl;external;
{*
 * Use this function to write 32 bits in native format to an SDL_RWops as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS32LE(dst:PSDL_RWops; value:TSint32):TSDL_bool;cdecl;external;
{*
 * Use this function to write 32 bits in native format to an SDL_RWops as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU32BE(dst:PSDL_RWops; value:TUint32):TSDL_bool;cdecl;external;
{*
 * Use this function to write 32 bits in native format to an SDL_RWops as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS32BE(dst:PSDL_RWops; value:TSint32):TSDL_bool;cdecl;external;
{*
 * Use this function to write 64 bits in native format to an SDL_RWops as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU64LE(dst:PSDL_RWops; value:TUint64):TSDL_bool;cdecl;external;
{*
 * Use this function to write 64 bits in native format to an SDL_RWops as
 * little-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in little-endian
 * format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS64LE(dst:PSDL_RWops; value:TSint64):TSDL_bool;cdecl;external;
{*
 * Use this function to write 64 bits in native format to an SDL_RWops as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteU64BE(dst:PSDL_RWops; value:TUint64):TSDL_bool;cdecl;external;
{*
 * Use this function to write 64 bits in native format to an SDL_RWops as
 * big-endian data.
 *
 * SDL byteswaps the data only if necessary, so the application always
 * specifies native format, and the data written will be in big-endian format.
 *
 * \param dst the stream to which data will be written
 * \param value the data to be written, in native format
 * \returns SDL_TRUE on successful write, SDL_FALSE on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_WriteS64BE(dst:PSDL_RWops; value:TSint64):TSDL_bool;cdecl;external;
{ @  }{ Write endian functions  }
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_rwops_h_  }

implementation


end.
