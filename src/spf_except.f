( Exception handling initialization.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  Conversion from 16-bit to 32-bit code - 1995-96
  Revision - October 1999
)

USER HANDLER      \ Exception handler

VECT FATAL-HANDLER      ' NOOP ' FATAL-HANDLER TC-VECT!
\ If during exception handling an invalid HANDLER value is found,
\ formed by bugs in stack/return-stack or recursion,
\ then this FATAL-HANDLER vector is executed

: THROW ( k*x n -- k*x | i*x n ) \ 94 EXCEPTION
\ If the top of stack n is non-zero, pop the exception frame from the
\ exception stack, which was placed there during the most recent CATCH. Then
\ restore the data stack depth to what was saved during the corresponding
\ CATCH, and restore the return stack to the depth that was saved at
\ that moment, not the depth that was at the point of THROW
\ (i - same number as i in stack effect of corresponding CATCH),
\ place n on top of data stack and continue
\ execution at the point right after CATCH, which placed this exception
\ frame.
\ If the top of stack is non-zero, and there is no exception frame on the
\ exception stack, the following happens:
\   If n=-1, execute ABORT behavior (from CORE wordset),
\   display nothing.
\   If n=-2, execute ABORT" behavior (from CORE wordset),
\   display string ccc, associated with ABORT", causing THROW.
\   Otherwise system may display implementation-defined
\   message corresponding to THROW with code n. Then
\   execute ABORT behavior (from CORE).
  DUP 0= IF DROP EXIT THEN
  \ DUP 109 = IF DROP EXIT THEN \ broken pipe - do nothing, may cause issues in CGI
  
  HANDLER @  DUP IF  RP!
  R> HANDLER !
  R> SWAP >R
  SP! DROP R>
  EXIT         THEN
  DROP FATAL-HANDLER
;
( THROW generates exception for any top of stack value not equal to 0.
  Code 109 was handled specially for some unknown reason.
  Ideally, this should be caught by the pattern DUP IF ... THROW THEN ...
  However, 109 can occur from error in READ-FILE, but not WRITE-FILE.
  Therefore, code 109 and other non-0 errors should be handled by READ-FILE etc,
  not here in THROW
  2014-03-11 ~ruv
)
' THROW TO THROW-CODE \ Used in target compiler, in TC mode

VECT <SET-EXC-HANDLER> \ Vector for setting OS exception handler

: CATCH ( i*x xt -- j*x 0 | i*x n ) \ 94 EXCEPTION
\ Push an exception frame on the exception stack
\ and execute the word xt (as if by EXECUTE) in such a way that control
\ can be transferred to the point right after CATCH if during execution
\ of xt THROW is called.
\ If execution of xt terminates normally (i.e. the exception frame
\ placed by this CATCH was not popped by THROW),
\ pop the exception frame and place zero on top of data stack,
\ after results of executing xt with EXECUTE. Then continue
\ execution after THROW.
  SP@ >R  HANDLER @ >R
  RP@ HANDLER !
  EXECUTE
  R> HANDLER !
  RDROP
  0
;
: ABORT  \ 94 EXCEPTION EXT
\ Extends CORE ABORT behavior as follows:
  ( i*x -- ) ( R: j*x -- )
\ Executes -1 THROW
  -1 THROW
;
