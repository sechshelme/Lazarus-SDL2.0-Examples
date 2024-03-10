unit SDL3_pixels;

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
{*
 *  \file SDL_pixels.h
 *
 *  Header for the enumerated pixel format definitions.
 *
 *  SDL's pixel formats have the following naming convention:
 *
 *      * Names with a list of components and a single bit count, such as
 *        RGB24 and ABGR32, define a platform-independent encoding into
 *        bytes in the order specified. For example, in RGB24 data, each
 *        pixel is encoded in 3 bytes (red, green, blue) in that order,
 *        and in ABGR32 data, each pixel is encoded in 4 bytes
 *        (alpha, blue, green, red) in that order. Use these names if the
 *        property of a format that is important to you is the order of
 *        the bytes in memory or on disk.
 *
 *      * Names with a bit count per component, such as ARGB8888 and
 *        XRGB1555, are "packed" into an appropriately-sized integer in
 *        the platform's native endianness. For example, ARGB8888 is
 *        a sequence of 32-bit integers; in each integer, the most
 *        significant bits are alpha, and the least significant bits are
 *        blue. On a little-endian CPU such as x86, the least significant
 *        bits of each integer are arranged first in memory, but on a
 *        big-endian CPU such as s390x, the most significant bits are
 *        arranged first. Use these names if the property of a format that
 *        is important to you is the meaning of each bit position within a
 *        native-endianness integer.
 *
 *      * In indexed formats such as INDEX4LSB, each pixel is represented
 *        by encoding an index into the palette into the indicated number
 *        of bits, with multiple pixels packed into each byte if appropriate.
 *        In LSB formats, the first (leftmost) pixel is stored in the
 *        least-significant bits of the byte; in MSB formats, it's stored
 *        in the most-significant bits. INDEX8 does not need LSB/MSB
 *        variants, because each pixel exactly fills one byte.
 *
 *  The 32-bit byte-array encodings such as RGBA32 are aliases for the
 *  appropriate 8888 encoding for the current platform. For example,
 *  RGBA32 is an alias for ABGR8888 on little-endian CPUs like x86,
 *  or an alias for RGBA8888 on big-endian CPUs.
  }
//{$ifndef SDL_pixels_h_}
//{$define SDL_pixels_h_}
//{$include <SDL3/SDL_stdinc.h>}
//{$include <SDL3/SDL_endian.h>}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 *  \name Transparency definitions
 *
 *  These define alpha as the opacity of a surface.
  }
{ @  }

const
  SDL_ALPHA_OPAQUE = 255;  
  SDL_ALPHA_TRANSPARENT = 0;  
{ @  }
{* Pixel type.  }
{ appended at the end for compatibility with sdl2-compat:   }
type
  PSDL_PixelType = ^TSDL_PixelType;
  TSDL_PixelType =  Longint;
  Const
    SDL_PIXELTYPE_UNKNOWN = 0;
    SDL_PIXELTYPE_INDEX1 = 1;
    SDL_PIXELTYPE_INDEX4 = 2;
    SDL_PIXELTYPE_INDEX8 = 3;
    SDL_PIXELTYPE_PACKED8 = 4;
    SDL_PIXELTYPE_PACKED16 = 5;
    SDL_PIXELTYPE_PACKED32 = 6;
    SDL_PIXELTYPE_ARRAYU8 = 7;
    SDL_PIXELTYPE_ARRAYU16 = 8;
    SDL_PIXELTYPE_ARRAYU32 = 9;
    SDL_PIXELTYPE_ARRAYF16 = 10;
    SDL_PIXELTYPE_ARRAYF32 = 11;
    SDL_PIXELTYPE_INDEX2 = 12;

{* Bitmap pixel order, high bit -> low bit.  }
type
  PSDL_BitmapOrder = ^TSDL_BitmapOrder;
  TSDL_BitmapOrder =  Longint;
  Const
    SDL_BITMAPORDER_NONE = 0;
    SDL_BITMAPORDER_4321 = 1;
    SDL_BITMAPORDER_1234 = 2;

{* Packed component order, high bit -> low bit.  }
type
  PSDL_PackedOrder = ^TSDL_PackedOrder;
  TSDL_PackedOrder =  Longint;
  Const
    SDL_PACKEDORDER_NONE = 0;
    SDL_PACKEDORDER_XRGB = 1;
    SDL_PACKEDORDER_RGBX = 2;
    SDL_PACKEDORDER_ARGB = 3;
    SDL_PACKEDORDER_RGBA = 4;
    SDL_PACKEDORDER_XBGR = 5;
    SDL_PACKEDORDER_BGRX = 6;
    SDL_PACKEDORDER_ABGR = 7;
    SDL_PACKEDORDER_BGRA = 8;

{* Array component order, low byte -> high byte.  }
type
  PSDL_ArrayOrder = ^TSDL_ArrayOrder;
  TSDL_ArrayOrder =  Longint;
  Const
    SDL_ARRAYORDER_NONE = 0;
    SDL_ARRAYORDER_RGB = 1;
    SDL_ARRAYORDER_RGBA = 2;
    SDL_ARRAYORDER_ARGB = 3;
    SDL_ARRAYORDER_BGR = 4;
    SDL_ARRAYORDER_BGRA = 5;
    SDL_ARRAYORDER_ABGR = 6;

{* Packed component layout.  }
type
  PSDL_PackedLayout = ^TSDL_PackedLayout;
  TSDL_PackedLayout =  Longint;
  Const
    SDL_PACKEDLAYOUT_NONE = 0;
    SDL_PACKEDLAYOUT_332 = 1;
    SDL_PACKEDLAYOUT_4444 = 2;
    SDL_PACKEDLAYOUT_1555 = 3;
    SDL_PACKEDLAYOUT_5551 = 4;
    SDL_PACKEDLAYOUT_565 = 5;
    SDL_PACKEDLAYOUT_8888 = 6;
    SDL_PACKEDLAYOUT_2101010 = 7;
    SDL_PACKEDLAYOUT_1010102 = 8;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_DEFINE_PIXELFOURCC(A,B,C,D : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_DEFINE_PIXELFORMAT(_type,order,layout,bits,bytes : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELFLAG(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELTYPE(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELORDER(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELLAYOUT(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_BITSPERPIXEL(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_BYTESPERPIXEL(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_INDEXED(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_PACKED(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ARRAY(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ALPHA(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_10BIT(format : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_FLOAT(format : longint) : longint;

{ The flag is set to 1 because 0x1? is not in the printable ASCII range  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_ISPIXELFORMAT_FOURCC(format : longint) : UInt32;

{ Note: If you modify this list, update SDL_GetPixelFormatName()  }
{ Aliases for RGBA byte arrays of color data, for the current platform  }
//{$if SDL_BYTEORDER == SDL_BIG_ENDIAN}
//{$else}
//{$endif}
{*< Planar mode: Y + V + U  (3 planes)  }
{*< Planar mode: Y + U + V  (3 planes)  }
{*< Packed mode: Y0+U0+Y1+V0 (1 plane)  }
{*< Packed mode: U0+Y0+V0+Y1 (1 plane)  }
{*< Packed mode: Y0+V0+Y1+U0 (1 plane)  }
{*< Planar mode: Y + U/V interleaved  (2 planes)  }
{*< Planar mode: Y + V/U interleaved  (2 planes)  }
{*< Planar mode: Y + U/V interleaved  (2 planes)  }
{*< Android video texture format  }
type
  PSDL_PixelFormatEnum = ^TSDL_PixelFormatEnum;
  TSDL_PixelFormatEnum =  Longint;
  Const
    SDL_PIXELFORMAT_UNKNOWN = 0;
    //SDL_PIXELFORMAT_INDEX1LSB = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX1,SDL_BITMAPORDER_4321,0,1,0);
    //SDL_PIXELFORMAT_INDEX1MSB = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX1,SDL_BITMAPORDER_1234,0,1,0);
    //SDL_PIXELFORMAT_INDEX2LSB = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX2,SDL_BITMAPORDER_4321,0,2,0);
    //SDL_PIXELFORMAT_INDEX2MSB = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX2,SDL_BITMAPORDER_1234,0,2,0);
    //SDL_PIXELFORMAT_INDEX4LSB = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX4,SDL_BITMAPORDER_4321,0,4,0);
    //SDL_PIXELFORMAT_INDEX4MSB = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX4,SDL_BITMAPORDER_1234,0,4,0);
    //SDL_PIXELFORMAT_INDEX8 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX8,0,0,8,1);
    //SDL_PIXELFORMAT_RGB332 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED8,SDL_PACKEDORDER_XRGB,SDL_PACKEDLAYOUT_332,8,1);
    //SDL_PIXELFORMAT_XRGB4444 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_XRGB,SDL_PACKEDLAYOUT_4444,12,2);
    //SDL_PIXELFORMAT_RGB444 = SDL_PIXELFORMAT_XRGB4444;
    //SDL_PIXELFORMAT_XBGR4444 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_XBGR,SDL_PACKEDLAYOUT_4444,12,2);
    //SDL_PIXELFORMAT_BGR444 = SDL_PIXELFORMAT_XBGR4444;
    //SDL_PIXELFORMAT_XRGB1555 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_XRGB,SDL_PACKEDLAYOUT_1555,15,2);
    //SDL_PIXELFORMAT_RGB555 = SDL_PIXELFORMAT_XRGB1555;
    //SDL_PIXELFORMAT_XBGR1555 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_XBGR,SDL_PACKEDLAYOUT_1555,15,2);
    //SDL_PIXELFORMAT_BGR555 = SDL_PIXELFORMAT_XBGR1555;
    //SDL_PIXELFORMAT_ARGB4444 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_ARGB,SDL_PACKEDLAYOUT_4444,16,2);
    //SDL_PIXELFORMAT_RGBA4444 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_RGBA,SDL_PACKEDLAYOUT_4444,16,2);
    //SDL_PIXELFORMAT_ABGR4444 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_ABGR,SDL_PACKEDLAYOUT_4444,16,2);
    //SDL_PIXELFORMAT_BGRA4444 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_BGRA,SDL_PACKEDLAYOUT_4444,16,2);
    //SDL_PIXELFORMAT_ARGB1555 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_ARGB,SDL_PACKEDLAYOUT_1555,16,2);
    //SDL_PIXELFORMAT_RGBA5551 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_RGBA,SDL_PACKEDLAYOUT_5551,16,2);
    //SDL_PIXELFORMAT_ABGR1555 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_ABGR,SDL_PACKEDLAYOUT_1555,16,2);
    //SDL_PIXELFORMAT_BGRA5551 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_BGRA,SDL_PACKEDLAYOUT_5551,16,2);
    //SDL_PIXELFORMAT_RGB565 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_XRGB,SDL_PACKEDLAYOUT_565,16,2);
    //SDL_PIXELFORMAT_BGR565 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED16,SDL_PACKEDORDER_XBGR,SDL_PACKEDLAYOUT_565,16,2);
    //SDL_PIXELFORMAT_RGB24 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU8,SDL_ARRAYORDER_RGB,0,24,3);
    //SDL_PIXELFORMAT_BGR24 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU8,SDL_ARRAYORDER_BGR,0,24,3);
    //SDL_PIXELFORMAT_XRGB8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_XRGB,SDL_PACKEDLAYOUT_8888,24,4);
    //SDL_PIXELFORMAT_RGBX8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_RGBX,SDL_PACKEDLAYOUT_8888,24,4);
    //SDL_PIXELFORMAT_XBGR8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_XBGR,SDL_PACKEDLAYOUT_8888,24,4);
    //SDL_PIXELFORMAT_BGRX8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_BGRX,SDL_PACKEDLAYOUT_8888,24,4);
    //SDL_PIXELFORMAT_ARGB8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_ARGB,SDL_PACKEDLAYOUT_8888,32,4);
    //SDL_PIXELFORMAT_RGBA8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_RGBA,SDL_PACKEDLAYOUT_8888,32,4);
    //SDL_PIXELFORMAT_ABGR8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_ABGR,SDL_PACKEDLAYOUT_8888,32,4);
    //SDL_PIXELFORMAT_BGRA8888 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_BGRA,SDL_PACKEDLAYOUT_8888,32,4);
    //SDL_PIXELFORMAT_XRGB2101010 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_XRGB,SDL_PACKEDLAYOUT_2101010,32,4);
    //SDL_PIXELFORMAT_XBGR2101010 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_XBGR,SDL_PACKEDLAYOUT_2101010,32,4);
    //SDL_PIXELFORMAT_ARGB2101010 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_ARGB,SDL_PACKEDLAYOUT_2101010,32,4);
    //SDL_PIXELFORMAT_ABGR2101010 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_PACKED32,SDL_PACKEDORDER_ABGR,SDL_PACKEDLAYOUT_2101010,32,4);
    //SDL_PIXELFORMAT_RGB48 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU16,SDL_ARRAYORDER_RGB,0,48,6);
    //SDL_PIXELFORMAT_BGR48 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU16,SDL_ARRAYORDER_BGR,0,48,6);
    //SDL_PIXELFORMAT_RGBA64 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU16,SDL_ARRAYORDER_RGBA,0,64,8);
    //SDL_PIXELFORMAT_ARGB64 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU16,SDL_ARRAYORDER_ARGB,0,64,8);
    //SDL_PIXELFORMAT_BGRA64 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU16,SDL_ARRAYORDER_BGRA,0,64,8);
    //SDL_PIXELFORMAT_ABGR64 = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYU16,SDL_ARRAYORDER_ABGR,0,64,8);
    //SDL_PIXELFORMAT_RGB48_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF16,SDL_ARRAYORDER_RGB,0,48,6);
    //SDL_PIXELFORMAT_BGR48_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF16,SDL_ARRAYORDER_BGR,0,48,6);
    //SDL_PIXELFORMAT_RGBA64_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF16,SDL_ARRAYORDER_RGBA,0,64,8);
    //SDL_PIXELFORMAT_ARGB64_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF16,SDL_ARRAYORDER_ARGB,0,64,8);
    //SDL_PIXELFORMAT_BGRA64_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF16,SDL_ARRAYORDER_BGRA,0,64,8);
    //SDL_PIXELFORMAT_ABGR64_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF16,SDL_ARRAYORDER_ABGR,0,64,8);
    //SDL_PIXELFORMAT_RGB96_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF32,SDL_ARRAYORDER_RGB,0,96,12);
    //SDL_PIXELFORMAT_BGR96_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF32,SDL_ARRAYORDER_BGR,0,96,12);
    //SDL_PIXELFORMAT_RGBA128_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF32,SDL_ARRAYORDER_RGBA,0,128,16);
    //SDL_PIXELFORMAT_ARGB128_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF32,SDL_ARRAYORDER_ARGB,0,128,16);
    //SDL_PIXELFORMAT_BGRA128_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF32,SDL_ARRAYORDER_BGRA,0,128,16);
    //SDL_PIXELFORMAT_ABGR128_FLOAT = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_ARRAYF32,SDL_ARRAYORDER_ABGR,0,128,16);
    //SDL_PIXELFORMAT_RGBA32 = SDL_PIXELFORMAT_RGBA8888;
    //SDL_PIXELFORMAT_ARGB32 = SDL_PIXELFORMAT_ARGB8888;
    //SDL_PIXELFORMAT_BGRA32 = SDL_PIXELFORMAT_BGRA8888;
    //SDL_PIXELFORMAT_ABGR32 = SDL_PIXELFORMAT_ABGR8888;
    //SDL_PIXELFORMAT_RGBX32 = SDL_PIXELFORMAT_RGBX8888;
    //SDL_PIXELFORMAT_XRGB32 = SDL_PIXELFORMAT_XRGB8888;
    //SDL_PIXELFORMAT_BGRX32 = SDL_PIXELFORMAT_BGRX8888;
    //SDL_PIXELFORMAT_XBGR32 = SDL_PIXELFORMAT_XBGR8888;
    //SDL_PIXELFORMAT_RGBA32 = SDL_PIXELFORMAT_ABGR8888;
    //SDL_PIXELFORMAT_ARGB32 = SDL_PIXELFORMAT_BGRA8888;
    //SDL_PIXELFORMAT_BGRA32 = SDL_PIXELFORMAT_ARGB8888;
    //SDL_PIXELFORMAT_ABGR32 = SDL_PIXELFORMAT_RGBA8888;
    //SDL_PIXELFORMAT_RGBX32 = SDL_PIXELFORMAT_XBGR8888;
    //SDL_PIXELFORMAT_XRGB32 = SDL_PIXELFORMAT_BGRX8888;
    //SDL_PIXELFORMAT_BGRX32 = SDL_PIXELFORMAT_XRGB8888;
    //SDL_PIXELFORMAT_XBGR32 = SDL_PIXELFORMAT_RGBX8888;
    //SDL_PIXELFORMAT_YV12 = SDL_DEFINE_PIXELFOURCC('Y','V','1','2');
    //SDL_PIXELFORMAT_IYUV = SDL_DEFINE_PIXELFOURCC('I','Y','U','V');
    //SDL_PIXELFORMAT_YUY2 = SDL_DEFINE_PIXELFOURCC('Y','U','Y','2');
    //SDL_PIXELFORMAT_UYVY = SDL_DEFINE_PIXELFOURCC('U','Y','V','Y');
    //SDL_PIXELFORMAT_YVYU = SDL_DEFINE_PIXELFOURCC('Y','V','Y','U');
    //SDL_PIXELFORMAT_NV12 = SDL_DEFINE_PIXELFOURCC('N','V','1','2');
    //SDL_PIXELFORMAT_NV21 = SDL_DEFINE_PIXELFOURCC('N','V','2','1');
    //SDL_PIXELFORMAT_P010 = SDL_DEFINE_PIXELFOURCC('P','0','1','0');
    //SDL_PIXELFORMAT_EXTERNAL_OES = SDL_DEFINE_PIXELFOURCC('O','E','S',' ');

{*
 * Pixels are a representation of a color in a particular color space.
 *
 * The first characteristic of a color space is the color type. SDL understands two different color types, RGB and YCbCr, or in SDL also referred to as YUV.
 *
 * RGB colors consist of red, green, and blue channels of color that are added together to represent the colors we see on the screen.
 * https://en.wikipedia.org/wiki/RGB_color_model
 *
 * YCbCr colors represent colors as a Y luma brightness component and red and blue chroma color offsets. This color representation takes advantage of the fact that the human eye is more sensitive to brightness than the color in an image. The Cb and Cr components are often compressed and have lower resolution than the luma component.
 * https://en.wikipedia.org/wiki/YCbCr
 *
 * When the color information in YCbCr is compressed, the Y pixels are left at full resolution and each Cr and Cb pixel represents an average of the color information in a block of Y pixels. The chroma location determines where in that block of pixels the color information is coming from.
 *
 * The color range defines how much of the pixel to use when converting a pixel into a color on the display. When the full color range is used, the entire numeric range of the pixel bits is significant. When narrow color range is used, for historical reasons, the pixel uses only a portion of the numeric range to represent colors.
 *
 * The color primaries and white point are a definition of the colors in the color space relative to the standard XYZ color space.
 * https://en.wikipedia.org/wiki/CIE_1931_color_space
 *
 * The transfer characteristic, or opto-electrical transfer function (OETF), is the way a color is converted from mathematically linear space into a non-linear output signals.
 * https://en.wikipedia.org/wiki/Rec._709#Transfer_characteristics
 *
 * The matrix coefficients are used to convert between YCbCr and RGB colors.
  }
{*
 * The color type
  }
type
  PSDL_ColorType = ^TSDL_ColorType;
  TSDL_ColorType =  Longint;
  Const
    SDL_COLOR_TYPE_UNKNOWN = 0;
    SDL_COLOR_TYPE_RGB = 1;
    SDL_COLOR_TYPE_YCBCR = 2;

{*
 * The color range, as described by https://www.itu.int/rec/R-REC-BT.2100-2-201807-I/en
  }
{*< Narrow range, e.g. 16-235 for 8-bit RGB and luma, and 16-240 for 8-bit chroma  }
{*< Full range, e.g. 0-255 for 8-bit RGB and luma, and 1-255 for 8-bit chroma  }
type
  PSDL_ColorRange = ^TSDL_ColorRange;
  TSDL_ColorRange =  Longint;
  Const
    SDL_COLOR_RANGE_UNKNOWN = 0;
    SDL_COLOR_RANGE_LIMITED = 1;
    SDL_COLOR_RANGE_FULL = 2;

{*
 * The color primaries, as described by https://www.itu.int/rec/T-REC-H.273-201612-S/en
  }
{ DCI P3  }
type
  PSDL_ColorPrimaries = ^TSDL_ColorPrimaries;
  TSDL_ColorPrimaries =  Longint;
  Const
    SDL_COLOR_PRIMARIES_UNKNOWN = 0;
    SDL_COLOR_PRIMARIES_BT709 = 1;
    SDL_COLOR_PRIMARIES_UNSPECIFIED = 2;
    SDL_COLOR_PRIMARIES_BT470M = 4;
    SDL_COLOR_PRIMARIES_BT470BG = 5;
    SDL_COLOR_PRIMARIES_BT601 = 6;
    SDL_COLOR_PRIMARIES_SMPTE240 = 7;
    SDL_COLOR_PRIMARIES_GENERIC_FILM = 8;
    SDL_COLOR_PRIMARIES_BT2020 = 9;
    SDL_COLOR_PRIMARIES_XYZ = 10;
    SDL_COLOR_PRIMARIES_SMPTE431 = 11;
    SDL_COLOR_PRIMARIES_SMPTE432 = 12;
    SDL_COLOR_PRIMARIES_EBU3213 = 22;
    SDL_COLOR_PRIMARIES_CUSTOM = 31;

{*
 * The transfer characteristics, as described by https://www.itu.int/rec/T-REC-H.273-201612-S/en
  }
{*< ITU-R BT1361  }
{*< ITU-R BT470M / ITU-R BT1700 625 PAL & SECAM  }
{*< ITU-R BT470BG  }
{*< SMPTE ST 170M  }
{*< SMPTE ST 240M  }
{*< IEC 61966-2-4  }
{*< ITU-R BT1361 Extended Colour Gamut  }
{*< IEC 61966-2-1 (sRGB or sYCC)  }
{*< ITU-R BT2020 for 10-bit system  }
{*< ITU-R BT2020 for 12-bit system  }
{*< SMPTE ST 2084 for 10-, 12-, 14- and 16-bit systems  }
{*< SMPTE ST 428-1  }
{*< ARIB STD-B67, known as "Hybrid log-gamma"  }
type
  PSDL_TransferCharacteristics = ^TSDL_TransferCharacteristics;
  TSDL_TransferCharacteristics =  Longint;
  Const
    SDL_TRANSFER_CHARACTERISTICS_UNKNOWN = 0;
    SDL_TRANSFER_CHARACTERISTICS_BT709 = 1;
    SDL_TRANSFER_CHARACTERISTICS_UNSPECIFIED = 2;
    SDL_TRANSFER_CHARACTERISTICS_GAMMA22 = 4;
    SDL_TRANSFER_CHARACTERISTICS_GAMMA28 = 5;
    SDL_TRANSFER_CHARACTERISTICS_BT601 = 6;
    SDL_TRANSFER_CHARACTERISTICS_SMPTE240 = 7;
    SDL_TRANSFER_CHARACTERISTICS_LINEAR = 8;
    SDL_TRANSFER_CHARACTERISTICS_LOG100 = 9;
    SDL_TRANSFER_CHARACTERISTICS_LOG100_SQRT10 = 10;
    SDL_TRANSFER_CHARACTERISTICS_IEC61966 = 11;
    SDL_TRANSFER_CHARACTERISTICS_BT1361 = 12;
    SDL_TRANSFER_CHARACTERISTICS_SRGB = 13;
    SDL_TRANSFER_CHARACTERISTICS_BT2020_10BIT = 14;
    SDL_TRANSFER_CHARACTERISTICS_BT2020_12BIT = 15;
    SDL_TRANSFER_CHARACTERISTICS_PQ = 16;
    SDL_TRANSFER_CHARACTERISTICS_SMPTE428 = 17;
    SDL_TRANSFER_CHARACTERISTICS_HLG = 18;
    SDL_TRANSFER_CHARACTERISTICS_CUSTOM = 31;

{*
 * The matrix coefficients, as described by https://www.itu.int/rec/T-REC-H.273-201612-S/en
  }
type
  PSDL_MatrixCoefficients = ^TSDL_MatrixCoefficients;
  TSDL_MatrixCoefficients =  Longint;
  Const
    SDL_MATRIX_COEFFICIENTS_IDENTITY = 0;
    SDL_MATRIX_COEFFICIENTS_BT709 = 1;
    SDL_MATRIX_COEFFICIENTS_UNSPECIFIED = 2;
    SDL_MATRIX_COEFFICIENTS_FCC = 4;
    SDL_MATRIX_COEFFICIENTS_BT470BG = 5;
    SDL_MATRIX_COEFFICIENTS_BT601 = 6;
    SDL_MATRIX_COEFFICIENTS_SMPTE240 = 7;
    SDL_MATRIX_COEFFICIENTS_YCGCO = 8;
    SDL_MATRIX_COEFFICIENTS_BT2020_NCL = 9;
    SDL_MATRIX_COEFFICIENTS_BT2020_CL = 10;
    SDL_MATRIX_COEFFICIENTS_SMPTE2085 = 11;
    SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL = 12;
    SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL = 13;
    SDL_MATRIX_COEFFICIENTS_ICTCP = 14;
    SDL_MATRIX_COEFFICIENTS_CUSTOM = 31;

{*
 * The chroma sample location
  }
{*< RGB, no chroma sampling  }
{*< In MPEG-2, MPEG-4, and AVC, Cb and Cr are taken on midpoint of the left-edge of the 2×2 square. In other words, they have the same horizontal location as the top-left pixel, but is shifted one-half pixel down vertically.  }
{*< In JPEG/JFIF, H.261, and MPEG-1, Cb and Cr are taken at the center of 2×2 the square. In other words, they are offset one-half pixel to the right and one-half pixel down compared to the top-left pixel.  }
{*< In HEVC for BT.2020 and BT.2100 content (in particular on Blu-rays), Cb and Cr are sampled at the same location as the group's top-left Y pixel ("co-sited", "co-located").  }
type
  PSDL_ChromaLocation = ^TSDL_ChromaLocation;
  TSDL_ChromaLocation =  Longint;
  Const
    SDL_CHROMA_LOCATION_NONE = 0;
    SDL_CHROMA_LOCATION_LEFT = 1;
    SDL_CHROMA_LOCATION_CENTER = 2;
    SDL_CHROMA_LOCATION_TOPLEFT = 3;

{ Colorspace definition  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function SDL_DEFINE_COLORSPACE(_type,range,primaries,transfer,matrix,chroma : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACETYPE(X : longint) : TSDL_ColorType;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACERANGE(X : longint) : TSDL_ColorRange;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACECHROMA(X : longint) : TSDL_ChromaLocation;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACEPRIMARIES(X : longint) : TSDL_ColorPrimaries;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACETRANSFER(X : longint) : TSDL_TransferCharacteristics;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACEMATRIX(X : longint) : TSDL_MatrixCoefficients;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_YUV_BT601(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_YUV_BT709(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_LIMITED_RANGE(X : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_FULL_RANGE(X : longint) : longint;

{ sRGB is a gamma corrected colorspace, and the default colorspace for SDL rendering and 8-bit RGB surfaces  }
{*< Equivalent to DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709  }
{ This is a linear colorspace and the default colorspace for floating point surfaces. On Windows this is the scRGB colorspace, and on Apple platforms this is kCGColorSpaceExtendedLinearSRGB for EDR content  }
{*< Equivalent to DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709   }
{ HDR10 is a non-linear HDR colorspace and the default colorspace for 10-bit surfaces  }
{*< Equivalent to DXGI_COLOR_SPACE_RGB_FULL_G2084_NONE_P2020   }
{*< Equivalent to DXGI_COLOR_SPACE_YCBCR_FULL_G22_NONE_P709_X601  }
{*< Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601  }
{*< Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601  }
{*< Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709  }
{*< Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709  }
{*< Equivalent to DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P2020  }
{*< Equivalent to DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P2020  }
{ The default colorspace for RGB surfaces if no colorspace is specified  }
{ The default colorspace for YUV surfaces if no colorspace is specified  }
type
  PSDL_Colorspace = ^TSDL_Colorspace;
  TSDL_Colorspace =  Longint;
  Const
    SDL_COLORSPACE_UNKNOWN = 0;
    //SDL_COLORSPACE_SRGB = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_RGB,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT709,SDL_TRANSFER_CHARACTERISTICS_SRGB,SDL_MATRIX_COEFFICIENTS_IDENTITY,SDL_CHROMA_LOCATION_NONE);
    //SDL_COLORSPACE_SRGB_LINEAR = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_RGB,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT709,SDL_TRANSFER_CHARACTERISTICS_LINEAR,SDL_MATRIX_COEFFICIENTS_IDENTITY,SDL_CHROMA_LOCATION_NONE);
    //SDL_COLORSPACE_HDR10 = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_RGB,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT2020,SDL_TRANSFER_CHARACTERISTICS_PQ,SDL_MATRIX_COEFFICIENTS_IDENTITY,SDL_CHROMA_LOCATION_NONE);
    //SDL_COLORSPACE_JPEG = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_YCBCR,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT709,SDL_TRANSFER_CHARACTERISTICS_BT601,SDL_MATRIX_COEFFICIENTS_BT601,SDL_CHROMA_LOCATION_NONE);
    //SDL_COLORSPACE_BT601_LIMITED = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_YCBCR,SDL_COLOR_RANGE_LIMITED,SDL_COLOR_PRIMARIES_BT601,SDL_TRANSFER_CHARACTERISTICS_BT601,SDL_MATRIX_COEFFICIENTS_BT601,SDL_CHROMA_LOCATION_LEFT);
    //SDL_COLORSPACE_BT601_FULL = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_YCBCR,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT601,SDL_TRANSFER_CHARACTERISTICS_BT601,SDL_MATRIX_COEFFICIENTS_BT601,SDL_CHROMA_LOCATION_LEFT);
    //SDL_COLORSPACE_BT709_LIMITED = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_YCBCR,SDL_COLOR_RANGE_LIMITED,SDL_COLOR_PRIMARIES_BT709,SDL_TRANSFER_CHARACTERISTICS_BT709,SDL_MATRIX_COEFFICIENTS_BT709,SDL_CHROMA_LOCATION_LEFT);
    //SDL_COLORSPACE_BT709_FULL = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_YCBCR,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT709,SDL_TRANSFER_CHARACTERISTICS_BT709,SDL_MATRIX_COEFFICIENTS_BT709,SDL_CHROMA_LOCATION_LEFT);
    //SDL_COLORSPACE_BT2020_LIMITED = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_YCBCR,SDL_COLOR_RANGE_LIMITED,SDL_COLOR_PRIMARIES_BT2020,SDL_TRANSFER_CHARACTERISTICS_PQ,SDL_MATRIX_COEFFICIENTS_BT2020_NCL,SDL_CHROMA_LOCATION_LEFT);
    //SDL_COLORSPACE_BT2020_FULL = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_YCBCR,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT2020,SDL_TRANSFER_CHARACTERISTICS_PQ,SDL_MATRIX_COEFFICIENTS_BT2020_NCL,SDL_CHROMA_LOCATION_LEFT);
    //SDL_COLORSPACE_RGB_DEFAULT = SDL_COLORSPACE_SRGB;
    //SDL_COLORSPACE_YUV_DEFAULT = SDL_COLORSPACE_JPEG;

{*
 * The bits of this structure can be directly reinterpreted as an integer-packed
 * color which uses the SDL_PIXELFORMAT_RGBA32 format (SDL_PIXELFORMAT_ABGR8888
 * on little-endian systems and SDL_PIXELFORMAT_RGBA8888 on big-endian systems).
  }
type
  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
      r : Uint8;
      g : Uint8;
      b : Uint8;
      a : Uint8;
    end;

  TSDL_Colour = TSDL_Color;
{*
 * The bits of this structure can be directly reinterpreted as a float-packed
 * color which uses the SDL_PIXELFORMAT_RGBA128_FLOAT format
  }
type
  PSDL_FColor = ^TSDL_FColor;
  TSDL_FColor = record
      r : single;
      g : single;
      b : single;
      a : single;
    end;

  TSDL_FColour = TSDL_FColor;
type
  PSDL_Palette = ^TSDL_Palette;
  TSDL_Palette = record
      ncolors : longint;
      colors : PSDL_Color;
      version : Uint32;
      refcount : longint;
    end;
{*
 *  \note Everything in the pixel format structure is read-only.
  }

  PSDL_PixelFormat = ^TSDL_PixelFormat;
  TSDL_PixelFormat = record
      format : Uint32;
      palette : PSDL_Palette;
      bits_per_pixel : Uint8;
      bytes_per_pixel : Uint8;
      padding : array[0..1] of Uint8;
      Rmask : Uint32;
      Gmask : Uint32;
      Bmask : Uint32;
      Amask : Uint32;
      Rloss : Uint8;
      Gloss : Uint8;
      Bloss : Uint8;
      Aloss : Uint8;
      Rshift : Uint8;
      Gshift : Uint8;
      Bshift : Uint8;
      Ashift : Uint8;
      refcount : longint;
      next : PSDL_PixelFormat;
    end;
{*
 * Get the human readable name of a pixel format.
 *
 * \param format the pixel format to query
 * \returns the human readable name of the specified pixel format or
 *          `SDL_PIXELFORMAT_UNKNOWN` if the format isn't recognized.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before type ignored *)

function SDL_GetPixelFormatName(format:Uint32):Pchar;cdecl;external;
{*
 * Convert one of the enumerated pixel formats to a bpp value and RGBA masks.
 *
 * \param format one of the SDL_PixelFormatEnum values
 * \param bpp a bits per pixel value; usually 15, 16, or 32
 * \param Rmask a pointer filled in with the red mask for the format
 * \param Gmask a pointer filled in with the green mask for the format
 * \param Bmask a pointer filled in with the blue mask for the format
 * \param Amask a pointer filled in with the alpha mask for the format
 * \returns SDL_TRUE on success or SDL_FALSE if the conversion wasn't
 *          possible; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetPixelFormatEnumForMasks
  }
function SDL_GetMasksForPixelFormatEnum(format:Uint32; bpp:Plongint; Rmask:PUint32; Gmask:PUint32; Bmask:PUint32;
           Amask:PUint32):TSDL_bool;cdecl;external;
{*
 * Convert a bpp value and RGBA masks to an enumerated pixel format.
 *
 * This will return `SDL_PIXELFORMAT_UNKNOWN` if the conversion wasn't
 * possible.
 *
 * \param bpp a bits per pixel value; usually 15, 16, or 32
 * \param Rmask the red mask for the format
 * \param Gmask the green mask for the format
 * \param Bmask the blue mask for the format
 * \param Amask the alpha mask for the format
 * \returns one of the SDL_PixelFormatEnum values
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetMasksForPixelFormatEnum
  }
function SDL_GetPixelFormatEnumForMasks(bpp:longint; Rmask:Uint32; Gmask:Uint32; Bmask:Uint32; Amask:Uint32):Uint32;cdecl;external;
{*
 * Create an SDL_PixelFormat structure corresponding to a pixel format.
 *
 * Returned structure may come from a shared global cache (i.e. not newly
 * allocated), and hence should not be modified, especially the palette. Weird
 * errors such as `Blit combination not supported` may occur.
 *
 * \param pixel_format one of the SDL_PixelFormatEnum values
 * \returns the new SDL_PixelFormat structure or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DestroyPixelFormat
  }
function SDL_CreatePixelFormat(pixel_format:Uint32):PSDL_PixelFormat;cdecl;external;
{*
 * Free an SDL_PixelFormat structure allocated by SDL_CreatePixelFormat().
 *
 * \param format the SDL_PixelFormat structure to free
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreatePixelFormat
  }
procedure SDL_DestroyPixelFormat(format:PSDL_PixelFormat);cdecl;external;
{*
 * Create a palette structure with the specified number of color entries.
 *
 * The palette entries are initialized to white.
 *
 * \param ncolors represents the number of color entries in the color palette
 * \returns a new SDL_Palette structure on success or NULL on failure (e.g. if
 *          there wasn't enough memory); call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_DestroyPalette
  }
function SDL_CreatePalette(ncolors:longint):PSDL_Palette;cdecl;external;
{*
 * Set the palette for a pixel format structure.
 *
 * \param format the SDL_PixelFormat structure that will use the palette
 * \param palette the SDL_Palette structure that will be used
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreatePalette
 * \sa SDL_DestroyPalette
  }
function SDL_SetPixelFormatPalette(format:PSDL_PixelFormat; palette:PSDL_Palette):longint;cdecl;external;
{*
 * Set a range of colors in a palette.
 *
 * \param palette the SDL_Palette structure to modify
 * \param colors an array of SDL_Color structures to copy into the palette
 * \param firstcolor the index of the first palette entry to modify
 * \param ncolors the number of entries to modify
 * \returns 0 on success or a negative error code on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreatePalette
 * \sa SDL_CreateSurface
  }
(* Const before type ignored *)
function SDL_SetPaletteColors(palette:PSDL_Palette; colors:PSDL_Color; firstcolor:longint; ncolors:longint):longint;cdecl;external;
{*
 * Free a palette created with SDL_CreatePalette().
 *
 * \param palette the SDL_Palette structure to be freed
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_CreatePalette
  }
procedure SDL_DestroyPalette(palette:PSDL_Palette);cdecl;external;
{*
 * Map an RGB triple to an opaque pixel value for a given pixel format.
 *
 * This function maps the RGB color value to the specified pixel format and
 * returns the pixel value best approximating the given RGB color value for
 * the given pixel format.
 *
 * If the format has a palette (8-bit) the index of the closest matching color
 * in the palette will be returned.
 *
 * If the specified pixel format has an alpha component it will be returned as
 * all 1 bits (fully opaque).
 *
 * If the pixel format bpp (color depth) is less than 32-bpp then the unused
 * upper bits of the return value can safely be ignored (e.g., with a 16-bpp
 * format the return value can be assigned to a Uint16, and similarly a Uint8
 * for an 8-bpp format).
 *
 * \param format an SDL_PixelFormat structure describing the pixel format
 * \param r the red component of the pixel in the range 0-255
 * \param g the green component of the pixel in the range 0-255
 * \param b the blue component of the pixel in the range 0-255
 * \returns a pixel value
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetRGB
 * \sa SDL_GetRGBA
 * \sa SDL_MapRGBA
  }
(* Const before type ignored *)
function SDL_MapRGB(format:PSDL_PixelFormat; r:Uint8; g:Uint8; b:Uint8):Uint32;cdecl;external;
{*
 * Map an RGBA quadruple to a pixel value for a given pixel format.
 *
 * This function maps the RGBA color value to the specified pixel format and
 * returns the pixel value best approximating the given RGBA color value for
 * the given pixel format.
 *
 * If the specified pixel format has no alpha component the alpha value will
 * be ignored (as it will be in formats with a palette).
 *
 * If the format has a palette (8-bit) the index of the closest matching color
 * in the palette will be returned.
 *
 * If the pixel format bpp (color depth) is less than 32-bpp then the unused
 * upper bits of the return value can safely be ignored (e.g., with a 16-bpp
 * format the return value can be assigned to a Uint16, and similarly a Uint8
 * for an 8-bpp format).
 *
 * \param format an SDL_PixelFormat structure describing the format of the
 *               pixel
 * \param r the red component of the pixel in the range 0-255
 * \param g the green component of the pixel in the range 0-255
 * \param b the blue component of the pixel in the range 0-255
 * \param a the alpha component of the pixel in the range 0-255
 * \returns a pixel value
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetRGB
 * \sa SDL_GetRGBA
 * \sa SDL_MapRGB
  }
(* Const before type ignored *)
function SDL_MapRGBA(format:PSDL_PixelFormat; r:Uint8; g:Uint8; b:Uint8; a:Uint8):Uint32;cdecl;external;
{*
 * Get RGB values from a pixel in the specified format.
 *
 * This function uses the entire 8-bit [0..255] range when converting color
 * components from pixel formats with less than 8-bits per RGB component
 * (e.g., a completely white pixel in 16-bit RGB565 format would return [0xff,
 * 0xff, 0xff] not [0xf8, 0xfc, 0xf8]).
 *
 * \param pixel a pixel value
 * \param format an SDL_PixelFormat structure describing the format of the
 *               pixel
 * \param r a pointer filled in with the red component
 * \param g a pointer filled in with the green component
 * \param b a pointer filled in with the blue component
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetRGBA
 * \sa SDL_MapRGB
 * \sa SDL_MapRGBA
  }
(* Const before type ignored *)
procedure SDL_GetRGB(pixel:Uint32; format:PSDL_PixelFormat; r:PUint8; g:PUint8; b:PUint8);cdecl;external;
{*
 * Get RGBA values from a pixel in the specified format.
 *
 * This function uses the entire 8-bit [0..255] range when converting color
 * components from pixel formats with less than 8-bits per RGB component
 * (e.g., a completely white pixel in 16-bit RGB565 format would return [0xff,
 * 0xff, 0xff] not [0xf8, 0xfc, 0xf8]).
 *
 * If the surface has no alpha component, the alpha will be returned as 0xff
 * (100% opaque).
 *
 * \param pixel a pixel value
 * \param format an SDL_PixelFormat structure describing the format of the
 *               pixel
 * \param r a pointer filled in with the red component
 * \param g a pointer filled in with the green component
 * \param b a pointer filled in with the blue component
 * \param a a pointer filled in with the alpha component
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetRGB
 * \sa SDL_MapRGB
 * \sa SDL_MapRGBA
  }
(* Const before type ignored *)
procedure SDL_GetRGBA(pixel:Uint32; format:PSDL_PixelFormat; r:PUint8; g:PUint8; b:PUint8;    a:PUint8);cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_pixels_h_  }

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_DEFINE_PIXELFOURCC(A,B,C,D : longint) : longint;
begin
//  SDL_DEFINE_PIXELFOURCC:=SDL_FOURCC(A,B,C,D);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_DEFINE_PIXELFORMAT(_type,order,layout,bits,bytes : longint) : longint;
begin
  SDL_DEFINE_PIXELFORMAT:=(((((1 shl 28) or (_type shl 24)) or (order shl 20)) or (layout shl 16)) or (bits shl 8)) or (bytes shl 0);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELFLAG(X : longint) : longint;
begin
  SDL_PIXELFLAG:=(X shr 28) and $0F;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELTYPE(X : longint) : longint;
begin
  SDL_PIXELTYPE:=(X shr 24) and $0F;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELORDER(X : longint) : longint;
begin
  SDL_PIXELORDER:=(X shr 20) and $0F;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_PIXELLAYOUT(X : longint) : longint;
begin
  SDL_PIXELLAYOUT:=(X shr 16) and $0F;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_BITSPERPIXEL(X : longint) : longint;
begin
  SDL_BITSPERPIXEL:=(X shr 8) and $FF;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_BYTESPERPIXEL(X : longint) : longint;
var
   if_local1, if_local2 : longint;
(* result types are not known *)
begin
  //if (((X=SDL_PIXELFORMAT_YUY2) or (X=SDL_PIXELFORMAT_UYVY)) or (X=SDL_PIXELFORMAT_YVYU)) or (X=SDL_PIXELFORMAT_P010) then
  //  if_local1:=2
  //else
  //  if_local1:=1;
  //if SDL_ISPIXELFORMAT_FOURCC(X) then
  //  if_local2:=if_local1
  //else
  //  if_local2:=(X shr 0) and $FF;
  //SDL_BYTESPERPIXEL:=if_local2;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_INDEXED(format : longint) : longint;
begin
//  SDL_ISPIXELFORMAT_INDEXED:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@(((((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX1) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX2)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX4)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_INDEX8)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_PACKED(format : longint) : longint;
begin
//  SDL_ISPIXELFORMAT_PACKED:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@((((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED8) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED16)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED32)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ARRAY(format : longint) : longint;
begin
//  SDL_ISPIXELFORMAT_ARRAY:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@((((((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYU8) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYU16)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYU32)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF16)) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF32)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_ALPHA(format : longint) : longint;
begin
//  SDL_ISPIXELFORMAT_ALPHA:=(SDL_ISPIXELFORMAT_PACKED(format)) and (@(((((SDL_PIXELORDER(format))=SDL_PACKEDORDER_ARGB) or ((SDL_PIXELORDER(format))=SDL_PACKEDORDER_RGBA)) or ((SDL_PIXELORDER(format))=SDL_PACKEDORDER_ABGR)) or ((SDL_PIXELORDER(format))=SDL_PACKEDORDER_BGRA)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_10BIT(format : longint) : longint;
begin
//  SDL_ISPIXELFORMAT_10BIT:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@(((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_PACKED32) and (@((SDL_PIXELLAYOUT(format))=SDL_PACKEDLAYOUT_2101010))));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISPIXELFORMAT_FLOAT(format : longint) : longint;
begin
//  SDL_ISPIXELFORMAT_FLOAT:=( not (SDL_ISPIXELFORMAT_FOURCC(format))) and (@(((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF16) or ((SDL_PIXELTYPE(format))=SDL_PIXELTYPE_ARRAYF32)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_ISPIXELFORMAT_FOURCC(format : longint) : Uint32;
begin
//  SDL_ISPIXELFORMAT_FOURCC:=Tformat(@(@((SDL_PIXELFLAG(format))<>1)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_DEFINE_COLORSPACE(_type,range,primaries,transfer,matrix,chroma : longint) : longint;
begin
  SDL_DEFINE_COLORSPACE:=((((((Uint32(_type)) shl 28) or ((Uint32(range)) shl 24)) or ((Uint32(chroma)) shl 20)) or ((Uint32(primaries)) shl 10)) or ((Uint32(transfer)) shl 5)) or ((Uint32(matrix)) shl 0);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACETYPE(X : longint) : TSDL_ColorType;
begin
  SDL_COLORSPACETYPE:=TSDL_ColorType((X shr 28) and $0F);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACERANGE(X : longint) : TSDL_ColorRange;
begin
  SDL_COLORSPACERANGE:=TSDL_ColorRange((X shr 24) and $0F);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACECHROMA(X : longint) : TSDL_ChromaLocation;
begin
  SDL_COLORSPACECHROMA:=TSDL_ChromaLocation((X shr 20) and $0F);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACEPRIMARIES(X : longint) : TSDL_ColorPrimaries;
begin
  SDL_COLORSPACEPRIMARIES:=TSDL_ColorPrimaries((X shr 10) and $1F);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACETRANSFER(X : longint) : TSDL_TransferCharacteristics;
begin
  SDL_COLORSPACETRANSFER:=TSDL_TransferCharacteristics((X shr 5) and $1F);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function SDL_COLORSPACEMATRIX(X : longint) : TSDL_MatrixCoefficients;
begin
//  SDL_COLORSPACEMATRIX:=TSDL_MatrixCoefficients(TX(@($1F)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_YUV_BT601(X : longint) : longint;
begin
//  SDL_ISCOLORSPACE_YUV_BT601:=((SDL_COLORSPACEMATRIX(X))=(SDL_MATRIX_COEFFICIENTS_BT601 or (SDL_COLORSPACEMATRIX(X))))=SDL_MATRIX_COEFFICIENTS_BT470BG;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_YUV_BT709(X : longint) : longint;
begin
//  SDL_ISCOLORSPACE_YUV_BT709:=(SDL_COLORSPACEMATRIX(X))=SDL_MATRIX_COEFFICIENTS_BT709;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_LIMITED_RANGE(X : longint) : longint;
begin
//  SDL_ISCOLORSPACE_LIMITED_RANGE:=(SDL_COLORSPACERANGE(X))=SDL_COLOR_RANGE_LIMITED;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function SDL_ISCOLORSPACE_FULL_RANGE(X : longint) : longint;
begin
//  SDL_ISCOLORSPACE_FULL_RANGE:=(SDL_COLORSPACERANGE(X))=SDL_COLOR_RANGE_LIMITED;
end;


end.
