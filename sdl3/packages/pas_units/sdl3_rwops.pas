unit SDL3_rwops;

interface

uses
  SDL3_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  SDL_RWOPS_UNKNOWN = 0;  
  SDL_RWOPS_WINFILE = 1;
  SDL_RWOPS_STDFILE = 2;
  SDL_RWOPS_JNIFILE = 3;
  SDL_RWOPS_MEMORY = 4;
  SDL_RWOPS_MEMORY_RO = 5;
  SDL_RWOPS_STATUS_READY = 0;
  SDL_RWOPS_STATUS_ERROR = 1;
  SDL_RWOPS_STATUS_EOF = 2;
  SDL_RWOPS_STATUS_NOT_READY = 3;
  SDL_RWOPS_STATUS_READONLY = 4;
  SDL_RWOPS_STATUS_WRITEONLY = 5;

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
function SDL_RWFromFile(file_:Pchar; mode:Pchar):PSDL_RWops;cdecl;external;
function SDL_RWFromMem(mem:pointer; size:Tsize_t):PSDL_RWops;cdecl;external;
function SDL_RWFromConstMem(mem:pointer; size:Tsize_t):PSDL_RWops;cdecl;external;
function SDL_CreateRW:PSDL_RWops;cdecl;external;
procedure SDL_DestroyRW(context:PSDL_RWops);cdecl;external;
function SDL_GetRWProperties(context:PSDL_RWops):TSDL_PropertiesID;cdecl;external;
const
  SDL_RW_SEEK_SET = 0;  
  SDL_RW_SEEK_CUR = 1;
  SDL_RW_SEEK_END = 2;

function SDL_RWsize(context:PSDL_RWops):TSint64;cdecl;external;
function SDL_RWseek(context:PSDL_RWops; offset:TSint64; whence:longint):TSint64;cdecl;external;
function SDL_RWtell(context:PSDL_RWops):TSint64;cdecl;external;
function SDL_RWread(context:PSDL_RWops; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;external;
function SDL_RWwrite(context:PSDL_RWops; ptr:pointer; size:Tsize_t):Tsize_t;cdecl;external;
function SDL_RWprintf(context:PSDL_RWops; fmt:Pchar; args:array of const):Tsize_t;cdecl;external;
function SDL_RWprintf(context:PSDL_RWops; fmt:Pchar):Tsize_t;cdecl;external;
function SDL_RWvprintf(context:PSDL_RWops; fmt:Pchar):Tsize_t;varargs;cdecl;external;
function SDL_RWclose(context:PSDL_RWops):longint;cdecl;external;
function SDL_LoadFile_RW(src:PSDL_RWops; datasize:Psize_t; freesrc:TSDL_bool):pointer;cdecl;external;
function SDL_LoadFile(file_:Pchar; datasize:Psize_t):pointer;cdecl;external;
function SDL_ReadU8(src:PSDL_RWops; value:PUint8):TSDL_bool;cdecl;external;
function SDL_ReadU16LE(src:PSDL_RWops; value:PUint16):TSDL_bool;cdecl;external;
function SDL_ReadS16LE(src:PSDL_RWops; value:PSint16):TSDL_bool;cdecl;external;
function SDL_ReadU16BE(src:PSDL_RWops; value:PUint16):TSDL_bool;cdecl;external;
function SDL_ReadS16BE(src:PSDL_RWops; value:PSint16):TSDL_bool;cdecl;external;
function SDL_ReadU32LE(src:PSDL_RWops; value:PUint32):TSDL_bool;cdecl;external;
function SDL_ReadS32LE(src:PSDL_RWops; value:PSint32):TSDL_bool;cdecl;external;
function SDL_ReadU32BE(src:PSDL_RWops; value:PUint32):TSDL_bool;cdecl;external;
function SDL_ReadS32BE(src:PSDL_RWops; value:PSint32):TSDL_bool;cdecl;external;
function SDL_ReadU64LE(src:PSDL_RWops; value:PUint64):TSDL_bool;cdecl;external;
function SDL_ReadS64LE(src:PSDL_RWops; value:PSint64):TSDL_bool;cdecl;external;
function SDL_ReadU64BE(src:PSDL_RWops; value:PUint64):TSDL_bool;cdecl;external;
function SDL_ReadS64BE(src:PSDL_RWops; value:PSint64):TSDL_bool;cdecl;external;
function SDL_WriteU8(dst:PSDL_RWops; value:TUint8):TSDL_bool;cdecl;external;
function SDL_WriteU16LE(dst:PSDL_RWops; value:TUint16):TSDL_bool;cdecl;external;
function SDL_WriteS16LE(dst:PSDL_RWops; value:TSint16):TSDL_bool;cdecl;external;
function SDL_WriteU16BE(dst:PSDL_RWops; value:TUint16):TSDL_bool;cdecl;external;
function SDL_WriteS16BE(dst:PSDL_RWops; value:TSint16):TSDL_bool;cdecl;external;
function SDL_WriteU32LE(dst:PSDL_RWops; value:TUint32):TSDL_bool;cdecl;external;
function SDL_WriteS32LE(dst:PSDL_RWops; value:TSint32):TSDL_bool;cdecl;external;
function SDL_WriteU32BE(dst:PSDL_RWops; value:TUint32):TSDL_bool;cdecl;external;
function SDL_WriteS32BE(dst:PSDL_RWops; value:TSint32):TSDL_bool;cdecl;external;
function SDL_WriteU64LE(dst:PSDL_RWops; value:TUint64):TSDL_bool;cdecl;external;
function SDL_WriteS64LE(dst:PSDL_RWops; value:TSint64):TSDL_bool;cdecl;external;
function SDL_WriteU64BE(dst:PSDL_RWops; value:TUint64):TSDL_bool;cdecl;external;
function SDL_WriteS64BE(dst:PSDL_RWops; value:TSint64):TSDL_bool;cdecl;external;

implementation

end.
