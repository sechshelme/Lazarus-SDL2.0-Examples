unit SDL_blendmode;

interface

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
 *  \file SDL_blendmode.h
 *
 *  Header file declaring the SDL_BlendMode enumeration
  }
//{$ifndef SDL_blendmode_h_}
//{$define SDL_blendmode_h_}
//{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 *  The blend mode used in SDL_RenderTexture() and drawing operations.
  }
{*< no blending
                                              dstRGBA = srcRGBA  }
{*< alpha blending
                                              dstRGB = (srcRGB * srcA) + (dstRGB * (1-srcA))
                                              dstA = srcA + (dstA * (1-srcA))  }
{*< additive blending
                                              dstRGB = (srcRGB * srcA) + dstRGB
                                              dstA = dstA  }
{*< color modulate
                                              dstRGB = srcRGB * dstRGB
                                              dstA = dstA  }
{*< color multiply
                                              dstRGB = (srcRGB * dstRGB) + (dstRGB * (1-srcA))
                                              dstA = dstA  }
{ Additional custom blend modes can be returned by SDL_ComposeCustomBlendMode()  }
type
  PSDL_BlendMode = ^TSDL_BlendMode;
  TSDL_BlendMode =  Longint;
  Const
    SDL_BLENDMODE_NONE = $00000000;
    SDL_BLENDMODE_BLEND = $00000001;
    SDL_BLENDMODE_ADD = $00000002;
    SDL_BLENDMODE_MOD = $00000004;
    SDL_BLENDMODE_MUL = $00000008;
    SDL_BLENDMODE_INVALID = $7FFFFFFF;

{*
 *  The blend operation used when combining source and destination pixel components
  }
{*< dst + src: supported by all renderers  }
{*< src - dst : supported by D3D9, D3D11, OpenGL, OpenGLES  }
{*< dst - src : supported by D3D9, D3D11, OpenGL, OpenGLES  }
{*< min(dst, src) : supported by D3D9, D3D11  }
{*< max(dst, src) : supported by D3D9, D3D11  }
type
  PSDL_BlendOperation = ^TSDL_BlendOperation;
  TSDL_BlendOperation =  Longint;
  Const
    SDL_BLENDOPERATION_ADD = $1;
    SDL_BLENDOPERATION_SUBTRACT = $2;
    SDL_BLENDOPERATION_REV_SUBTRACT = $3;
    SDL_BLENDOPERATION_MINIMUM = $4;
    SDL_BLENDOPERATION_MAXIMUM = $5;

{*
 *  The normalized factor used to multiply pixel components
  }
{*< 0, 0, 0, 0  }
{*< 1, 1, 1, 1  }
{*< srcR, srcG, srcB, srcA  }
{*< 1-srcR, 1-srcG, 1-srcB, 1-srcA  }
{*< srcA, srcA, srcA, srcA  }
{*< 1-srcA, 1-srcA, 1-srcA, 1-srcA  }
{*< dstR, dstG, dstB, dstA  }
{*< 1-dstR, 1-dstG, 1-dstB, 1-dstA  }
{*< dstA, dstA, dstA, dstA  }
{*< 1-dstA, 1-dstA, 1-dstA, 1-dstA  }
type
  PSDL_BlendFactor = ^TSDL_BlendFactor;
  TSDL_BlendFactor =  Longint;
  Const
    SDL_BLENDFACTOR_ZERO = $1;
    SDL_BLENDFACTOR_ONE = $2;
    SDL_BLENDFACTOR_SRC_COLOR = $3;
    SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR = $4;
    SDL_BLENDFACTOR_SRC_ALPHA = $5;
    SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = $6;
    SDL_BLENDFACTOR_DST_COLOR = $7;
    SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR = $8;
    SDL_BLENDFACTOR_DST_ALPHA = $9;
    SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA = $A;

{*
 * Compose a custom blend mode for renderers.
 *
 * The functions SDL_SetRenderDrawBlendMode and SDL_SetTextureBlendMode accept
 * the SDL_BlendMode returned by this function if the renderer supports it.
 *
 * A blend mode controls how the pixels from a drawing operation (source) get
 * combined with the pixels from the render target (destination). First, the
 * components of the source and destination pixels get multiplied with their
 * blend factors. Then, the blend operation takes the two products and
 * calculates the result that will get stored in the render target.
 *
 * Expressed in pseudocode, it would look like this:
 *
 * ```c
 * dstRGB = colorOperation(srcRGB * srcColorFactor, dstRGB * dstColorFactor);
 * dstA = alphaOperation(srcA * srcAlphaFactor, dstA * dstAlphaFactor);
 * ```
 *
 * Where the functions `colorOperation(src, dst)` and `alphaOperation(src,
 * dst)` can return one of the following:
 *
 * - `src + dst`
 * - `src - dst`
 * - `dst - src`
 * - `min(src, dst)`
 * - `max(src, dst)`
 *
 * The red, green, and blue components are always multiplied with the first,
 * second, and third components of the SDL_BlendFactor, respectively. The
 * fourth component is not used.
 *
 * The alpha component is always multiplied with the fourth component of the
 * SDL_BlendFactor. The other components are not used in the alpha
 * calculation.
 *
 * Support for these blend modes varies for each renderer. To check if a
 * specific SDL_BlendMode is supported, create a renderer and pass it to
 * either SDL_SetRenderDrawBlendMode or SDL_SetTextureBlendMode. They will
 * return with an error if the blend mode is not supported.
 *
 * This list describes the support of custom blend modes for each renderer in
 * SDL 2.0.6. All renderers support the four blend modes listed in the
 * SDL_BlendMode enumeration.
 *
 * - **direct3d**: Supports all operations with all factors. However, some
 *   factors produce unexpected results with `SDL_BLENDOPERATION_MINIMUM` and
 *   `SDL_BLENDOPERATION_MAXIMUM`.
 * - **direct3d11**: Same as Direct3D 9.
 * - **opengl**: Supports the `SDL_BLENDOPERATION_ADD` operation with all
 *   factors. OpenGL versions 1.1, 1.2, and 1.3 do not work correctly with SDL
 *   2.0.6.
 * - **opengles**: Supports the `SDL_BLENDOPERATION_ADD` operation with all
 *   factors. Color and alpha factors need to be the same. OpenGL ES 1
 *   implementation specific: May also support `SDL_BLENDOPERATION_SUBTRACT`
 *   and `SDL_BLENDOPERATION_REV_SUBTRACT`. May support color and alpha
 *   operations being different from each other. May support color and alpha
 *   factors being different from each other.
 * - **opengles2**: Supports the `SDL_BLENDOPERATION_ADD`,
 *   `SDL_BLENDOPERATION_SUBTRACT`, `SDL_BLENDOPERATION_REV_SUBTRACT`
 *   operations with all factors.
 * - **psp**: No custom blend mode support.
 * - **software**: No custom blend mode support.
 *
 * Some renderers do not provide an alpha component for the default render
 * target. The `SDL_BLENDFACTOR_DST_ALPHA` and
 * `SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA` factors do not have an effect in this
 * case.
 *
 * \param srcColorFactor the SDL_BlendFactor applied to the red, green, and
 *                       blue components of the source pixels
 * \param dstColorFactor the SDL_BlendFactor applied to the red, green, and
 *                       blue components of the destination pixels
 * \param colorOperation the SDL_BlendOperation used to combine the red,
 *                       green, and blue components of the source and
 *                       destination pixels
 * \param srcAlphaFactor the SDL_BlendFactor applied to the alpha component of
 *                       the source pixels
 * \param dstAlphaFactor the SDL_BlendFactor applied to the alpha component of
 *                       the destination pixels
 * \param alphaOperation the SDL_BlendOperation used to combine the alpha
 *                       component of the source and destination pixels
 * \returns an SDL_BlendMode that represents the chosen factors and
 *          operations.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetRenderDrawBlendMode
 * \sa SDL_GetRenderDrawBlendMode
 * \sa SDL_SetTextureBlendMode
 * \sa SDL_GetTextureBlendMode
  }

function SDL_ComposeCustomBlendMode(srcColorFactor:TSDL_BlendFactor; dstColorFactor:TSDL_BlendFactor; colorOperation:TSDL_BlendOperation; srcAlphaFactor:TSDL_BlendFactor; dstAlphaFactor:TSDL_BlendFactor; 
           alphaOperation:TSDL_BlendOperation):TSDL_BlendMode;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{//$include <SDL3/SDL_close_code.h>}
//{$endif}
{ SDL_blendmode_h_  }

implementation


end.
