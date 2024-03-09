unit SDL_mutex;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//function SDL_CAPABILITY(x : longint) : longint;
//
//{ was #define dname def_expr }
//function SDL_SCOPED_CAPABILITY : longint; { return type might be wrong }
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_GUARDED_BY(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_PT_GUARDED_BY(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRED_BEFORE(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRED_AFTER(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_REQUIRES(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_REQUIRES_SHARED(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRE(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRE_SHARED(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RELEASE(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RELEASE_SHARED(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RELEASE_GENERIC(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_TRY_ACQUIRE(x,y : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_TRY_ACQUIRE_SHARED(x,y : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_EXCLUDES(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ASSERT_CAPABILITY(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ASSERT_SHARED_CAPABILITY(x : longint) : longint;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RETURN_CAPABILITY(x : longint) : longint;
//
//{ was #define dname def_expr }
//function SDL_NO_THREAD_SAFETY_ANALYSIS : longint; { return type might be wrong }

const
  SDL_MUTEX_TIMEDOUT = 1;  
type
  PSDL_Mutex = ^TSDL_Mutex;
  TSDL_Mutex = Pointer;  {undefined structure}

function SDL_CreateMutex:PSDL_Mutex;cdecl;external;
procedure SDL_LockMutex(mutex:PSDL_Mutex);cdecl;external;
function SDL_TryLockMutex(mutex:PSDL_Mutex):longint;cdecl;external;
procedure SDL_UnlockMutex(mutex:PSDL_Mutex);cdecl;external;
procedure SDL_DestroyMutex(mutex:PSDL_Mutex);cdecl;external;
type
  PSDL_RWLock = ^TSDL_RWLock;
  TSDL_RWLock =  Pointer;  {undefined structure}

const
  SDL_RWLOCK_TIMEDOUT = SDL_MUTEX_TIMEDOUT;  
function SDL_CreateRWLock:PSDL_RWLock;cdecl;external;
procedure SDL_LockRWLockForReading(rwlock:PSDL_RWLock);cdecl;external;
procedure SDL_LockRWLockForWriting(rwlock:PSDL_RWLock);cdecl;external;
function SDL_TryLockRWLockForReading(rwlock:PSDL_RWLock):longint;cdecl;external;
function SDL_TryLockRWLockForWriting(rwlock:PSDL_RWLock):longint;cdecl;external;
procedure SDL_UnlockRWLock(rwlock:PSDL_RWLock);cdecl;external;
procedure SDL_DestroyRWLock(rwlock:PSDL_RWLock);cdecl;external;
type
  PSDL_Semaphore = ^TSDL_Semaphore;
  TSDL_Semaphore =  Pointer;  {undefined structure}

function SDL_CreateSemaphore(initial_value:Uint32):PSDL_Semaphore;cdecl;external;
procedure SDL_DestroySemaphore(sem:PSDL_Semaphore);cdecl;external;
function SDL_WaitSemaphore(sem:PSDL_Semaphore):longint;cdecl;external;
function SDL_TryWaitSemaphore(sem:PSDL_Semaphore):longint;cdecl;external;
function SDL_WaitSemaphoreTimeout(sem:PSDL_Semaphore; timeoutMS:int32):longint;cdecl;external;
function SDL_PostSemaphore(sem:PSDL_Semaphore):longint;cdecl;external;
function SDL_GetSemaphoreValue(sem:PSDL_Semaphore):Uint32;cdecl;external;
type
  PSDL_Condition = ^TSDL_Condition;
  TSDL_Condition =  Pointer;  {undefined structure}

function SDL_CreateCondition:PSDL_Condition;cdecl;external;
procedure SDL_DestroyCondition(cond:PSDL_Condition);cdecl;external;
function SDL_SignalCondition(cond:PSDL_Condition):longint;cdecl;external;
function SDL_BroadcastCondition(cond:PSDL_Condition):longint;cdecl;external;
function SDL_WaitCondition(cond:PSDL_Condition; mutex:PSDL_Mutex):longint;cdecl;external;
function SDL_WaitConditionTimeout(cond:PSDL_Condition; mutex:PSDL_Mutex; timeoutMS:int32):longint;cdecl;external;

implementation

//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_CAPABILITY(x : longint) : longint;
//begin
//  SDL_CAPABILITY:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(capability(x));
//end;
//
//{ was #define dname def_expr }
//function SDL_SCOPED_CAPABILITY : longint; { return type might be wrong }
//  begin
//    SDL_SCOPED_CAPABILITY:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(scoped_lockable);
//  end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_GUARDED_BY(x : longint) : longint;
//begin
//  SDL_GUARDED_BY:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(guarded_by(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_PT_GUARDED_BY(x : longint) : longint;
//begin
//  SDL_PT_GUARDED_BY:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(pt_guarded_by(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRED_BEFORE(x : longint) : longint;
//begin
//  SDL_ACQUIRED_BEFORE:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(acquired_before(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRED_AFTER(x : longint) : longint;
//begin
//  SDL_ACQUIRED_AFTER:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(acquired_after(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_REQUIRES(x : longint) : longint;
//begin
//  SDL_REQUIRES:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(requires_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_REQUIRES_SHARED(x : longint) : longint;
//begin
//  SDL_REQUIRES_SHARED:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(requires_shared_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRE(x : longint) : longint;
//begin
//  SDL_ACQUIRE:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(acquire_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ACQUIRE_SHARED(x : longint) : longint;
//begin
//  SDL_ACQUIRE_SHARED:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(acquire_shared_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RELEASE(x : longint) : longint;
//begin
//  SDL_RELEASE:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(release_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RELEASE_SHARED(x : longint) : longint;
//begin
//  SDL_RELEASE_SHARED:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(release_shared_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RELEASE_GENERIC(x : longint) : longint;
//begin
//  SDL_RELEASE_GENERIC:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(release_generic_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_TRY_ACQUIRE(x,y : longint) : longint;
//begin
//  SDL_TRY_ACQUIRE:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(try_acquire_capability(x,y));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_TRY_ACQUIRE_SHARED(x,y : longint) : longint;
//begin
//  SDL_TRY_ACQUIRE_SHARED:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(try_acquire_shared_capability(x,y));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_EXCLUDES(x : longint) : longint;
//begin
//  SDL_EXCLUDES:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(locks_excluded(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ASSERT_CAPABILITY(x : longint) : longint;
//begin
//  SDL_ASSERT_CAPABILITY:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(assert_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_ASSERT_SHARED_CAPABILITY(x : longint) : longint;
//begin
//  SDL_ASSERT_SHARED_CAPABILITY:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(assert_shared_capability(x));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }   
//function SDL_RETURN_CAPABILITY(x : longint) : longint;
//begin
//  SDL_RETURN_CAPABILITY:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(lock_returned(x));
//end;
//
//{ was #define dname def_expr }
//function SDL_NO_THREAD_SAFETY_ANALYSIS : longint; { return type might be wrong }
//  begin
//    SDL_NO_THREAD_SAFETY_ANALYSIS:=SDL_THREAD_ANNOTATION_ATTRIBUTE__(no_thread_safety_analysis);
//  end;


end.
