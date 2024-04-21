unit jconfig;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{ Version ID for the JPEG library.
 * Might be useful for tests like "#if JPEG_LIB_VERSION >= 60".
  }

const
  JPEG_LIB_VERSION = 80;  
{ libjpeg-turbo version  }
  LIBJPEG_TURBO_VERSION = '2.1.2';  
{ libjpeg-turbo version in integer form  }
  LIBJPEG_TURBO_VERSION_NUMBER = 2001002;  
{ Support arithmetic encoding  }
  C_ARITH_CODING_SUPPORTED = 1;  
{ Support arithmetic decoding  }
  D_ARITH_CODING_SUPPORTED = 1;  
{ Support in-memory source/destination managers  }
{ #undef MEM_SRCDST_SUPPORTED  }
{ Use accelerated SIMD routines.  }
  WITH_SIMD = 1;  
{
 * Define BITS_IN_JSAMPLE as either
 *   8   for 8-bit sample values (the usual setting)
 *   12  for 12-bit sample values
 * Only 8 and 12 are legal data precisions for lossy JPEG according to the
 * JPEG standard, and the IJG code does not support anything else!
 * We do not support run-time selection of data precision, sorry.
  }
{ use 8 or 12  }
  BITS_IN_JSAMPLE = 8;  
{ Define to 1 if you have the <locale.h> header file.  }
{ Define to 1 if you have the <stddef.h> header file.  }
{ Define to 1 if you have the <stdlib.h> header file.  }
{ Define if you need to include <sys/types.h> to get size_t.  }
  NEED_SYS_TYPES_H = 1;  
{ Define if you have BSD-like bzero and bcopy in <strings.h> rather than
   memset/memcpy in <string.h>.  }
{ #undef NEED_BSD_STRINGS  }
{ Define to 1 if the system has the type `unsigned char'.  }
  HAVE_UNSIGNED_CHAR = 1;  
{ Define to 1 if the system has the type `unsigned short'.  }
  HAVE_UNSIGNED_SHORT = 1;  
{ Compiler does not support pointers to undefined structures.  }
{ #undef INCOMPLETE_TYPES_BROKEN  }
{ Define if your (broken) compiler shifts signed values as if they were
   unsigned.  }
{ #undef RIGHT_SHIFT_IS_UNSIGNED  }
{ Define to empty if `const' does not conform to ANSI C.  }
{ #undef const  }
{ Define to `unsigned int' if <sys/types.h> does not define.  }
{ #undef size_t  }

implementation


end.
