( Definition of most frequently used "Forth processor" words
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  Conversion from 16-bit to 32-bit code - 1995-96
  Revision - October 1999
)

( Register usage for subroutine-threaded code.
  EAX       Top of Stack
  EBP       Data Stack
 [EBP]      Second item on Stack
  ESP       Return Stack
  EDI       Thread data pointer
)

HEX

\ ================================================================
\ Stack operations

CODE DUP ( x -- x x ) \ 94
\ Duplicate x.
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     RET
END-CODE

' DUP TO 'DUP_V

CODE 2DUP ( x1 x2 -- x1 x2 x1 x2 ) \ 94
\ Duplicate cell pair x1 x2.
     MOV EDX, [EBP]
     MOV -4 [EBP], EAX
     MOV -8 [EBP], EDX
     LEA EBP, -8 [EBP]
     RET
END-CODE

CODE DROP ( x -- ) \ 94
\ Remove x from the stack.
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE
' DROP TO 'DROP_V

CODE MAX ( n1 n2 -- n3 ) \ 94
\ n3 - the greater of n1 and n2.
ARCH-P6 [IF]
     MOV     EDX, [EBP]
     CMP     EDX, EAX
     CMOVG   EAX, EDX
[ELSE]     
     CMP     EAX, [EBP]
     JL # ' DROP
[THEN]     
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE MIN ( n1 n2 -- n3 ) \ 94
 \ n3 - the lesser of n1 and n2.
 ARCH-P6 [IF]
     MOV     EDX, [EBP]
     CMP     EDX, EAX
     CMOVL   EAX, EDX
[ELSE]     
     CMP     EAX, [EBP]
     JG # ' DROP
[THEN]     
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE UMAX ( n1 n2 -- n3 ) \ 94
ARCH-P6 [IF]
     MOV     ECX, [EBP]
     CMP     ECX, EAX
     CMOVA   EAX, ECX
[ELSE]
     CMP     EAX, [EBP]
     JB # ' DROP
[THEN]     
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE UMIN ( n1 n2 -- n3 ) \ 94
ARCH-P6 [IF]
     MOV     ECX, [EBP]
     CMP     ECX, EAX
     CMOVB   EAX, ECX
[ELSE]
     CMP     EAX, [EBP]
     JA # ' DROP
[THEN]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE 0MAX       ( N1 -- N2 ) \ return n2 the greater of n1 and zero
     XOR     EDX, EDX
     CMP     EDX, EAX
ARCH-P6 [IF]     
     CMOVG   EAX, EDX
[ELSE]
     JL SHORT @@1
     MOV EAX, EDX
@@1: 
[THEN]
     RET
END-CODE

CODE 2DROP ( x1 x2 -- ) \ 94
\ Drop cell pair x1 x2 from the stack.
     MOV EAX, 4 [EBP]
     LEA EBP, 8 [EBP]
     RET
END-CODE

CODE SWAP ( x1 x2 -- x2 x1 ) \ 94
\ Exchange the two top stack items
\     XCHG EAX, [EBP]
     MOV   EDX, [EBP]
     MOV   [EBP], EAX
     MOV   EAX, EDX
     RET
END-CODE

CODE 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 ) \ 94
\ Exchange the top two cell pairs.
     MOV ECX, [EBP]
     MOV EDX, 4 [EBP]
     MOV EBX, 8 [EBP]
     MOV 8 [EBP], ECX
     MOV 4 [EBP], EAX
     MOV [EBP], EBX
     MOV EAX, EDX
     RET
END-CODE

CODE OVER ( x1 x2 -- x1 x2 x1 ) \ 94
\ Place a copy of x1 on top of stack.
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     MOV EAX, 4 [EBP]
     RET
END-CODE

CODE 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) \ 94
\ Copy cell pair x1 x2 to the top of stack.
     MOV EDX, 8 [EBP]
     MOV -4 [EBP], EAX
     MOV -8 [EBP], EDX
     MOV EAX, 4 [EBP]
     LEA EBP, -8 [EBP]
     RET
END-CODE

CODE NIP ( x1 x2 -- x2 ) \ 94 CORE EXT
\ Drop the first item below the top of stack.
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE ROT ( x1 x2 x3 -- x2 x3 x1 ) \ 94
\ Rotate the top three stack entries.
     MOV  EDX, [EBP]
     MOV  [EBP], EAX
     MOV  EAX, 4 [EBP]
     MOV  4 [EBP], EDX
     RET
END-CODE

CODE -ROT ( x1 x2 x3 -- x3 x1 x2 ) \ 94
\ Rotate the top three stack entries in reverse.
     MOV  EDX, 4 [EBP]
     MOV  4 [EBP], EAX
     MOV  EAX, [EBP]
     MOV  [EBP], EDX
     RET
END-CODE

CODE PICK ( xu ... x1 x0 u -- xu ... x1 x0 xu ) \ 94 CORE EXT
\ Remove u. Copy xu to the top of stack. Ambiguous condition
\ exists if there are less than u+2 items on the stack
\ before PICK is executed.
        MOV     EAX, [EBP] [EAX*4]
     RET
END-CODE

CODE ROLL ( xu xu-1 ... x0 u -- xu-1 ... x0 xu ) \ 94 CORE EXT
\ Remove u. Rotate u+1 items on the top of stack.
\ Ambiguous condition exists if there are less than u+2 items
\ on the stack before ROLL is executed.
     OR EAX, EAX
     JZ SHORT @@1
     MOV ECX, EAX
     LEA EAX, [EAX*4]
     MOV EDX, EBP
     ADD EDX, EAX
     MOV EBX, [EDX]
@@2: LEA EDX, -4 [EDX]    \  DEC ECX
     MOV EAX, [EDX]       \  MOV EAX, [EDX+ECX*4]
     MOV 4 [EDX], EAX     \  MOV [EDX+ECX*4+4], EAX
     DEC ECX
     JNZ SHORT @@2
     MOV EAX, EBX
     JMP SHORT @@3
@@1: MOV EAX, [EBP]
@@3: LEA EBP, 4 [EBP]
     RET
END-CODE

CODE TUCK ( x1 x2 -- x2 x1 x2 )
\ Copy the first (top) stack item below the second stack item. 
     LEA EBP, -4 [EBP]
     MOV EDX, 4 [EBP]
     MOV 4 [EBP], EAX
     MOV [EBP], EDX
     RET
END-CODE


\ ================================================================
\ Return stack


CODE 2>R   \ 94 CORE EXT
\ Interpretation: Semantics undefined.
\ Execution: ( x1 x2 -- ) ( R: -- x1 x2 )
\ Transfer cell pair x1 x2 to the return stack. Semantically
\ equivalent to SWAP >R >R.
     POP  EBX
     PUSH [EBP]
     PUSH EAX
     LEA EBP, 8 [EBP]
     MOV EAX, -4 [EBP]
     JMP EBX
END-CODE

CODE 2R>  \ 94 CORE EXT
\ Interpretation: Semantics undefined.
\ Execution: ( -- x1 x2 ) ( R: x1 x2 -- )
\ Transfer cell pair x1 x2 from the return stack. Semantically
\ equivalent to R> R> SWAP.
     MOV EBX, [ESP]
     MOV  -4 [EBP], EAX
     MOV ECX, 8 [ESP]
     MOV EAX, 4 [ESP]
     MOV -8 [EBP], ECX
     LEA EBP, -8 [EBP]
     LEA ESP, 0C [ESP]
     JMP EBX
END-CODE

CODE R@ \ 94
\ Execution: ( -- x ) ( R: x -- x )
\ Interpretation: Semantics undefined during interpretation.
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     MOV EAX, 4 [ESP]
     RET
END-CODE   

CODE 2R@  \ 94 CORE EXT
\ Interpretation: Semantics undefined.
\ Execution: ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
\ Copy cell pair x1 x2 from the return stack. Semantically
\ equivalent to R> R> 2DUP >R >R SWAP.
     MOV -4 [EBP], EAX
     MOV EAX, 4 [ESP]
     MOV EBX, 8 [ESP]
     MOV -8 [EBP], EBX
     LEA EBP, -8 [EBP]
     RET
END-CODE

\ ================================================================
\ Memory operations

CODE @ ( a-addr -- x ) \ 94
\ x - value stored at a-addr.
     MOV EAX, [EAX]
     RET
END-CODE

CODE ! ( x a-addr -- ) \ 94
\ Store x at a-addr.
     MOV EDX, [EBP]
     MOV [EAX], EDX
     MOV EAX, 4 [EBP]
     LEA EBP, 8 [EBP]
     RET
END-CODE

CODE C@ ( c-addr -- char ) \ 94
\ Fetch char from c-addr. Upper bits of cell are zeroed.
     MOVZX EAX, BYTE [EAX]
     RET
END-CODE

CODE C! ( char c-addr -- ) \ 94
\ Store char at c-addr.
     MOV EDX, [EBP]
     MOV BYTE [EAX], DL
     MOV EAX, 4 [EBP]
     LEA EBP, 8 [EBP]
     RET
END-CODE

CODE W@ ( c-addr -- word )
\ Fetch word from c-addr. Upper bits of cell are zeroed.
     MOVZX EAX, WORD [EAX]
     RET
END-CODE

CODE W! ( word c-addr -- )
\ Store word at c-addr.
     MOV EDX, [EBP]
     MOV WORD [EAX], DX
     MOV EAX, 4 [EBP]
     LEA EBP, 8 [EBP]
     RET
END-CODE

CODE 2@ ( a-addr -- x1 x2 ) \ 94
\ Fetch cell pair x1 x2 stored at a-addr.
\ x2 is at a-addr, x1 is at the next cell.
\ Equivalent to DUP CELL+ @ SWAP @
     MOV EDX, 4 [EAX]
     LEA EBP, -4 [EBP]
     MOV [EBP], EDX
     MOV EAX, [EAX]
     RET
END-CODE

CODE 2! ( x1 x2 a-addr -- ) \ 94
\ Store cell pair x1 x2 at a-addr,
\ x2 at a-addr, x1 at the next cell.
\ Equivalent to SWAP OVER ! CELL+ !
     MOV EDX, [EBP]
     MOV [EAX], EDX
     MOV EDX, 4 [EBP]
     MOV 4 [EAX], EDX
     LEA EBP, 0C [EBP]
     MOV EAX, -4 [EBP]
     RET
END-CODE

\ ================================================================
\ Arithmetic

CODE 1+ ( n1|u1 -- n2|u2 ) \ 94
\ Add one to n1|u1 giving the sum n2|u2.
     LEA EAX, 1 [EAX]
     RET
END-CODE

CODE 1- ( n1|u1 -- n2|u2 ) \ 94
\ Subtract one from n1|u1 giving the difference n2|u2.
     LEA EAX, -1 [EAX]
     RET
END-CODE

CODE 2+ ( W -> W+2 )
     LEA EAX, 2 [EAX]
     RET
END-CODE

CODE 2- ( W -> W-2 )
     LEA EAX, -2 [EAX]
     RET
END-CODE

CODE 2*
     LEA EAX, [EAX*2]
     RET
END-CODE

CODE + ( n1|u1 n2|u2 -- n3|u3 ) \ 94
\ Add n1|u1 and n2|u2 giving the sum n3|u3.
     ADD EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE CELL+
     LEA EAX, 4 [EAX]
     RET
END-CODE

CODE CELL-
     LEA EAX, -4 [EAX]
     RET
END-CODE

CODE CELLS
     LEA EAX, [EAX*4]
     RET
END-CODE
               
CODE D+ ( d1|ud1 d2|ud2 -- d3|ud3 ) \ 94 DOUBLE
\ Add d1|ud1 and d2|ud2 giving double number d3|ud3.
     MOV EDX, [EBP]
     ADD 8 [EBP], EDX
     ADC EAX, 4 [EBP]
     LEA EBP, 8 [EBP]
     RET     
END-CODE

CODE D- ( d1|ud1 d2|ud2 -- d3|ud3 ) \ 94 DOUBLE
     MOV EDX, [EBP]
     SUB 8 [EBP], EDX
     SBB 4 [EBP], EAX
     MOV EAX, 4 [EBP]
     LEA EBP, 8 [EBP]
     RET
END-CODE

CODE - ( n1|u1 n2|u2 -- n3|u3 ) \ 94
\ Subtract n2|u2 from n1|u1 giving the difference n3|u3.
     NEG EAX
     ADD EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE 1+! ( A -> )
     INC DWORD [EAX]
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE 0! ( A -> )
     MOV DWORD [EAX], # 0 
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE COUNT ( c-addr1 -- c-addr2 u ) \ 94
\ Return character count and string address for counted string at c-addr1.
\ c-addr2 - address of first character after c-addr1.
\ u - contents of c-addr1, which is the count of characters
\ starting at c-addr2.
     LEA EBP, -4 [EBP]
     LEA EDX, 1 [EAX]
     MOVZX EAX, BYTE [EAX]
     MOV [EBP], EDX
     RET
END-CODE

CODE * ( n1|u1 n2|u2 -- n3|u3 ) \ 94
\ Multiply n1|u1 by n2|u2 giving the product n3|u3.
     IMUL DWORD [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE AND ( x1 x2 -- x3 ) \ 94
\ x3 - bit-by-bit logical "and" of x1 and x2.
     AND EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE OR ( x1 x2 -- x3 ) \ 94
\ x3 - bit-by-bit logical "or" of x1 and x2.
     OR EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE XOR ( x1 x2 -- x3 ) \ 94
\ x3 - bit-by-bit logical "exclusive or" of x1 and x2.
     XOR EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE INVERT ( x1 -- x2 ) \ 94
\ Invert all bits of x1 giving the logical inverse x2.
     NOT EAX
     RET
END-CODE

CODE NEGATE ( n1 -- n2 ) \ 94
\ n2 - the arithmetic negation of n1.
       NEG EAX
       RET
END-CODE

CODE ABS ( n -- u ) \ 94
\ u - the absolute value of n.
    MOV     ECX, EAX
    SAR     ECX, 1F
    XOR     EAX, ECX
    SUB     EAX, ECX
    RET
END-CODE

CODE DNEGATE ( d1 -- d2 ) \ 94 DOUBLE
\ d2 is the result of negating d1.
       NEG     EAX
       NEG     DWORD [EBP]
       SBB     EAX, # 0
       RET
END-CODE

CODE NOOP ( -> )
     RET
END-CODE

CODE S>D ( n -- d ) \ 94
\ Convert single number n to double number d with the same numeric value.
     CDQ
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     MOV EAX, EDX
     RET
END-CODE

CODE D>S ( d -- n ) \ 94 DOUBLE
\ n - equivalent of d.
\ Ambiguous condition exists if d is outside the range of
\ signed single-cell numbers.
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE U>D ( U -> D ) \ Zero-extend unsigned number to double
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     XOR EAX, EAX
     RET
END-CODE

CODE C>S ( c -- n )  \ Sign-extend CHAR
     MOVSX  EAX, AL
     RET
END-CODE

CODE UM* ( u1 u2 -- ud ) \ 94
\ ud - product of u1 and u2. All values and arithmetic are unsigned.
       MUL DWORD [EBP]
       MOV [EBP], EAX
       MOV EAX, EDX
       RET
END-CODE

CODE / ( n1 n2 -- n3 ) \ 94
\ Divide n1 by n2, giving quotient n3.
\ Ambiguous condition exists if n2 equals zero.
\ If n1 and n2 differ in sign - result is implementation
\ dependent.
       MOV ECX, EAX
       MOV EAX, [EBP]
       CDQ
       IDIV ECX
       LEA EBP, 4 [EBP]
       RET
END-CODE

CODE U/ ( W1, W2 -> W3 ) \ Unsigned divide W1 by W2
       MOV ECX, EAX
       MOV EAX, [EBP]
       XOR EDX, EDX
       LEA EBP, 4 [EBP]
       DIV ECX
       RET
END-CODE

CODE +! ( n|u a-addr -- ) \ 94
\ Add n|u to the single-cell number at a-addr.
     MOV EDX, [EBP]
     ADD [EAX], EDX
     MOV EAX, 4 [EBP]
     LEA EBP, 8 [EBP]
     RET
END-CODE

CODE MOD ( n1 n2 -- n3 ) \ 94
\ Divide n1 by n2, giving remainder n3.
\ Ambiguous condition exists if n2 equals zero.
\ If n1 and n2 differ in sign - result is implementation
\ dependent.
       MOV ECX, EAX
       MOV EAX, [EBP]
       CDQ
       IDIV ECX
       LEA EBP, 4 [EBP]
       MOV EAX, EDX
       RET
END-CODE

CODE /MOD ( n1 n2 -- n3 n4 ) \ 94
\ Divide n1 by n2, giving remainder n3 and quotient n4.
\ Ambiguous condition exists if n2 is zero.
       MOV ECX, EAX
       MOV EAX, [EBP]
       CDQ
       IDIV ECX
       MOV [EBP], EDX
       RET
END-CODE

CODE UMOD ( W1, W2 -> W3 ) \ Unsigned remainder of W1 divided by W2
       MOV ECX, EAX
       MOV EAX, [EBP]
       XOR EDX, EDX
       DIV ECX
       LEA EBP, 4 [EBP]
       MOV EAX, EDX
       RET
END-CODE

CODE UM/MOD ( ud u1 -- u2 u3 ) \ 94
\ Divide ud by u1, giving quotient u3 and remainder u2.
\ All values and arithmetic are unsigned.
\ Ambiguous condition exists if u1 is zero or if quotient
\ lies outside the range of single-cell unsigned integers.
       MOV ECX, EAX
       MOV EDX, [EBP]
       MOV EAX, 4 [EBP]
       DIV ECX
       LEA EBP, 4 [EBP]
       MOV [EBP], EDX
       RET
END-CODE

CODE 2/ ( x1 -- x2 ) \ 94
\ x2 - result of shifting x1 one bit right, leaving the most-significant bit unchanged.
     D1 C, F8 C,  \    SAR EAX, # 1
     RET
END-CODE


CODE U2/        ( N1 -- N2 ) \ unsigned divide n1 by two
     SHR     EAX, # 1
     RET
END-CODE

CODE */MOD ( n1 n2 n3 -- n4 n5 ) \ 94
\ Multiply n1 by n2, producing intermediate double result d.
\ Divide d by n3, giving remainder n4 and quotient n5.
       MOV     ECX, EAX
       MOV     EAX, [EBP]      \ n2
       IMUL    DWORD 4 [EBP]   \ n1*n2
       IDIV    ECX             \ n1*n2/n3
       MOV     4 [EBP], EDX    \ rem
       LEA EBP, 4 [EBP]
       RET
END-CODE

CODE M* ( n1 n2 -- d ) \ 94
\ d - signed double result of multiplying n1 by n2.
     IMUL DWORD [EBP]
     MOV  [EBP], EAX
     MOV  EAX, EDX 
     RET
END-CODE

CODE LSHIFT ( x1 u -- x2 ) \ 94
\ Shift x1 u bits left. Put zeros into the least-significant
\ bit positions vacated.
\ Ambiguous condition exists if u is greater than or equal to
\ number of bits in a cell.
     MOV ECX, EAX
     MOV EAX, [EBP]
     SHL EAX, CL
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE RSHIFT ( x1 u -- x2 ) \ 94
\ Shift x1 u bits right. Put zeros into the most-significant
\ bit positions vacated.
\ Ambiguous condition exists if u is greater than or equal to
\ number of bits in a cell.
     MOV ECX, EAX
     MOV EAX, [EBP]
     SHR EAX, CL
     LEA EBP, 4 [EBP]     
     RET
END-CODE

CODE ARSHIFT ( u1 n -- n2 )  \ arithmetic shift u1 right by n bits
     MOV     ECX, EAX
     MOV     EAX, [EBP]
     SAR     EAX, CL
     LEA EBP, 4 [EBP]     
     RET
END-CODE


CODE SM/REM ( d1 n1 -- n2 n3 ) \ 94
\ Divide d1 by n1, giving symmetric quotient n3 and remainder n2.
\ Remainder and dividend have the same sign.
\ Ambiguous condition exists if n1 is zero, or if quotient
\ lies outside the range of single-cell signed integers.
     MOV ECX, EAX
     MOV EDX, [EBP]
     MOV EAX, 4 [EBP]
     IDIV ECX
     LEA EBP, 4 [EBP]
     MOV [EBP], EDX
     RET
END-CODE

\ From: Serguei V. Jidkov [mailto:jsv@gorod.bryansk.ru]

CODE FM/MOD ( d1 n1 -- n2 n3 ) \ 94
\ Divide d1 by n1, giving floored quotient n3 and remainder n2.
\ Remainder and divisor have the same sign.
\ Ambiguous condition exists if n1 is zero, or if quotient
\ lies outside the range of single-cell signed integers.
        MOV ECX, EAX
        MOV EDX, 0 [EBP]
        MOV EBX, EDX
        MOV EAX, 4 [EBP]
        IDIV ECX
        TEST EDX, EDX            \ is remainder non-zero?
        JZ  SHORT @@1
        XOR EBX, ECX             \ do dividend and divisor have different signs?
        JNS SHORT @@1
        DEC EAX
        ADD EDX, ECX
@@1:    LEA EBP, 4 [EBP]
        MOV 0 [EBP], EDX
        RET
END-CODE


CODE DIGIT ( char n1 -- n2 true | false )
\ n2 - numeric value of character char when treated as
\ a digit in base n1
\ Implementation note:
\ If n1 or n2 is greater than 255 (i.e., exceeds one byte),
\ behavior may be undefined.
       MOV ECX, EAX
       MOV EAX, [EBP]
       A;  2C C, 30 C,  \  SUB AL, # 30
       JC SHORT @@1
       A;  3C C, A C,   \  CMP AL, # A
       JNC SHORT @@2
@@3:   CMP AL, CL
       JNC SHORT @@1
       MOV [EBP], EAX
       A; B8 C, TRUE W, TRUE W,  \  MOV EAX, # -1
       RET

@@2:   A;  3C C, 11 C,  \  CMP AL, # 11
       JC SHORT @@1
       A;  2C C, 7 C,   \  SUB AL, # 7
       JMP SHORT @@3

@@1:   LEA EBP, 4 [EBP]
       XOR EAX, EAX
       RET
END-CODE

\ ================================================================
\ Comparisons

CODE = ( x1 x2 -- flag ) \ 94
\ flag is "true" if and only if x1 is bit-for-bit the same as x2.
     XOR  EAX, [EBP]
     SUB  EAX, # 1
     SBB  EAX, EAX 
     LEA  EBP, 4 [EBP]
     RET
END-CODE

CODE <> ( x1 x2 -- flag ) \ 94 CORE EXT
\ flag is "true" if and only if x1 is not equal to x2.
     XOR  EAX, [EBP]
     NEG  EAX
     SBB  EAX, EAX
     LEA  EBP, 4 [EBP]
     RET
END-CODE

CODE < ( n1 n2 -- flag ) \ 94
\ flag is "true" if and only if n1 is less than n2.
       CMP  EAX, [EBP]
       SETLE AL
       AND  EAX, # 1
       A; 0x48 C, \ DEC  EAX
       LEA  EBP, 4 [EBP]
       RET
END-CODE

CODE > ( n1 n2 -- flag ) \ 94
\ flag is "true" if and only if n1 is greater than n2.
       CMP  EAX, [EBP]
       SETGE AL
       AND  EAX,  # 1
       A; 0x48 C, \ DEC  EAX
       LEA  EBP, 4 [EBP]
       RET
END-CODE

CODE WITHIN     ( n1 low high -- f1 ) \ f1=true if ((n1 >= low) & (n1 < high))
      MOV  EDX, 4 [EBP]
      SUB  EAX, [EBP]
      SUB  EDX, [EBP]
      SUB  EDX, EAX
      SBB  EAX, EAX
      LEA  EBP, 8 [EBP]
      RET
END-CODE

CODE D< ( d1 d2 -- flag ) \ DOUBLE
\ flag is "true" if and only if d1 is less than d2.
     MOV EDX, [EBP]
     CMP 8 [EBP], EDX
     SBB 4 [EBP], EAX
     MOV EAX, # 0
     JGE SHORT @@1
       DEC EAX
@@1: LEA EBP, 0C [EBP]
     RET
END-CODE

CODE D> ( d1 d2 -- flag ) \ DOUBLE
\ flag is "true" if and only if d1 is greater than d2.
     MOV EDX, 8 [EBP]
     CMP [EBP], EDX
     SBB EAX, 4 [EBP]
     MOV EAX, # 0
     JGE SHORT @@1
       DEC EAX
@@1: LEA EBP, 0C [EBP]
    RET
END-CODE

CODE U< ( u1 u2 -- flag ) \ 94
\ flag is "true" if and only if u1 is less than u2 (unsigned compare).
     CMP  [EBP], EAX
     SBB  EAX, EAX
     LEA  EBP, 4 [EBP]
     RET
END-CODE

CODE U> ( u1 u2 -- flag ) \ 94
\ flag is "true" if and only if u1 is greater than u2 (unsigned compare).
     CMP  EAX, [EBP]
     SBB  EAX, EAX
     LEA  EBP, 4 [EBP]
     RET
END-CODE

CODE 0< ( n -- flag ) \ 94
\ flag is "true" if and only if n is less than zero.
    SAR EAX, # 1F
    RET
END-CODE

CODE 0= ( x -- flag ) \ 94
\ flag is "true" if and only if x equals zero.
     SUB   EAX, # 1
     SBB   EAX, EAX
     RET
END-CODE

CODE 0<> ( x -- flag ) \ 94 CORE EXT
\ flag is "true" if and only if x is not equal to zero.
     NEG   EAX
     SBB   EAX, EAX
     RET
END-CODE

CODE D0= ( xd -- flag ) \ 94 DOUBLE
\ flag is "true" if and only if xd equals zero.
     OR   EAX, [EBP]
     SUB  EAX, # 1
     SBB  EAX, EAX
     LEA  EBP, 4 [EBP]
     RET
END-CODE

CODE  D= ( xd1 xd2 -- flag ) \ 94 DOUBLE
\ flag is true if and only if xd1 is bit-for-bit the same as xd2
     MOV  EDX,   [EBP]
     XOR  EAX, 4 [EBP]
     XOR  EDX, 8 [EBP]
      OR  EAX, EDX
     SUB  EAX, # 1
     SBB  EAX, EAX
     LEA  EBP, 0C [EBP]
     RET
END-CODE

CODE D2* ( xd1 -- xd2 ) \ 94 DOUBLE
\ xd2 is the result of shifting xd1 one bit toward the most-significant
\ bit, filling the vacated least-significant bit with zero     
     D1 C, 65 C, 00 C, \  SHL [EBP], # 1
     D1 C, D0 C, \ RCL EAX, # 1
     RET
END-CODE          

CODE D2/ ( xd1 -- xd2 ) \ 94 DOUBLE
\ xd2 is the result of shifting xd1 one bit toward the least-significant bit,
\ leaving the most-significant bit unchanged
     D1 C, F8 C, \ SAR EAX, # 1
     D1 C, 5D C, 00 C, \  RCR [EBP], # 1
     RET
END-CODE

\ ================================================================
\ Strings

CODE -TRAILING ( c-addr u1 -- c-addr u2 ) \ 94 STRING
\ If u1 is greater than zero, u2 equals u1 less the number of trailing spaces
\ in the character string specified by c-addr and u1. If u1 is zero or the
\ entire string consists of spaces, u2 is zero.
      OR EAX, EAX
      JZ SHORT @@1
      MOV EDX, EDI
      MOV EDI, [EBP]
      ADD EDI, EAX
      LEA EDI, -1 [EDI]
      MOV ECX, EAX
      A; B0 C, 20 C, \ MOV AL, # 20
      STD
      REPZ SCAS BYTE
      JZ SHORT @@2
      INC ECX
@@2:  MOV EAX, ECX
      MOV EDI, EDX
      CLD
@@1:  RET
END-CODE

CODE COMPARE ( c-addr1 u1 c-addr2 u2 -- n ) \ 94 STRING
\ Compare the string specified by c-addr1 u1 to the string specified by c-addr2 u2.
\ Strings are compared, starting at given addresses, character by character, up to
\ the length of the shorter string or until a difference is found. If the two strings
\ are identical, n is zero. If the two strings are identical up to the length of
\ the shorter string, n is minus-one (-1) if u1 is less than u2, otherwise one (1).
\ If the two strings are not identical up to the length of the shorter string, n is
\ minus-one (-1) if the first mismatched character in the string specified by c-addr1 u1
\ has a lesser numeric value than the corresponding character in the string
\ specified by c-addr2 u2, and one (1) otherwise.
      MOV EDX, EDI
      MOV EDI,   [EBP]
      MOV ECX, 4 [EBP]
      MOV ESI, 8 [EBP]
      LEA EBP, 0C [EBP]  \    ADD EBP, # 0C   ####
      CMP ECX, EAX
      PUSHFD
      JC  SHORT @@1
      MOV ECX, EAX
@@1:  JECXZ @@2
      CLD
      REPZ CMPS BYTE
      JZ  SHORT @@2
      POP EBX
      A;  B8 C, -1 DUP W, W, \  MOV EAX, # -1
      JC  SHORT @@3
      NEG EAX
      JMP SHORT @@3
@@2:  XOR EAX, EAX
      POPFD
      JZ  SHORT @@3
      A;  B8 C, -1 DUP W, W, \  MOV EAX, # -1
      JC  SHORT @@3
      NEG EAX
@@3:  MOV EDI, EDX
      RET
END-CODE

CODE SEARCH ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag ) \ 94 STRING
\ Search for string c-addr2 u2 within string c-addr1 u1.
\ If flag is "true", match was found at c-addr3 with u3 characters
\ remaining. If flag is "false", no match was found, c-addr3 equals c-addr1,
\ and u3 equals u1.
      PUSH EDI
      CLD
      MOV EBX,   EAX
      OR EBX, EBX
      JZ SHORT @@5
      MOV EDX, 4 [EBP]
      MOV EDI, 8 [EBP]
      ADD EDX, EDI
@@4:  MOV ESI,   [EBP]
      LODS BYTE
      MOV ECX, EDX
      SUB ECX, EDI
      JECXZ @@1
      REPNZ SCAS BYTE
      JNZ SHORT @@1   \ first char of search string not found
      CMP EBX, # 1
      JZ SHORT @@2   \ search string length is 1, found
      MOV ECX, EBX
      LEA ECX, -1 [ECX]
      MOV EAX, EDX
      SUB EAX, EDI
      CMP EAX, ECX
      JC SHORT @@1  \ remaining string shorter than search string
      PUSH EDI
      REPZ CMPS BYTE
      POP EDI
      JNZ SHORT @@4
@@2:  DEC EDI           \ match found
      SUB EDX, EDI
      MOV 8 [EBP], EDI
      MOV 4 [EBP], EDX
@@5:  A;  B8 C, -1 DUP W, W, \  MOV EAX, # -1
      JMP SHORT @@3
@@1:  XOR EAX, EAX
@@3:  LEA EBP, 4 [EBP]
      POP EDI
      RET
END-CODE

CODE CMOVE ( c-addr1 c-addr2 u -- ) \ 94 STRING
\ If u is greater than zero, copy u consecutive characters from the
\ data space starting at c-addr1 to c-addr2, proceeding character by
\ character from lower to higher addresses.
       MOV EDX, EDI
       MOV ECX, EAX
       MOV EDI, [EBP]
       MOV ESI, 4 [EBP]
       CLD
       \ Do regions overlap?
        \ If not, we can copy DWORDs
       MOV EBX, EDI
       SUB EBX, ESI
       JG  SHORT @@2
       NEG EBX
@@2:   CMP EBX, EAX
       JL  SHORT @@1
       
       \ If not aligned to 4, copy up to 3 bytes first
       MOV  EBX, EDI
       AND  EBX, # 3
       JZ   SHORT @@3
       MOV  ECX, # 4
       SUB  ECX, EBX

       CMP  ECX, EAX
       JL   SHORT @@4
       MOV  ECX, EAX
       JMP  @@1 \ short copy
@@4:
       SUB  EAX, ECX                    
       REP  MOVS BYTE
       MOV  ECX, EAX
@@3:
       SAR ECX, # 2
       \ Could be done faster with MMX instructions
       REP MOVS DWORD
       MOV ECX, EAX
       AND ECX, # 3
@@1:       
       REP MOVS BYTE
       LEA EBP, 0C [EBP]
       MOV EAX, -4 [EBP]
       MOV EDI, EDX
       RET
END-CODE

CODE CMOVE> ( c-addr1 c-addr2 u -- ) \ 94 STRING
\ If u is greater than zero, copy u consecutive characters from the
\ data space starting at c-addr1 to c-addr2, proceeding character by
\ character from higher to lower addresses.
       MOV EDX, EDI
       MOV ECX, EAX
       MOV EDI, [EBP]
       MOV ESI, 4 [EBP]
       STD
       ADD EDI, ECX
       DEC EDI
       ADD ESI, ECX
       DEC ESI
       REP MOVS BYTE
       CLD
       MOV EDI, EDX
       LEA EBP, 0C [EBP]
       MOV EAX, -4 [EBP]
       RET
END-CODE

CODE FILL ( c-addr u char -- ) \ 94
\ If u is greater than zero, store char in u bytes at c-addr.
       MOV EDX, EDI
       MOV ECX, [EBP]
       MOV EDI, 4 [EBP]
       CLD
       \ Can we optimize with DWORD?
       MOV EBX, ECX
       AND EBX, # 3
       JNZ @@1 \ no
       \ Fill with DWORD
       MOV EBX, EAX
       SHL EAX, # 8
       OR  EAX, EBX
       SHL EAX, # 8
       OR  EAX, EBX
       SHL EAX, # 8
       OR  EAX, EBX
       MOV EBX, ECX
       
       SAR ECX, # 2
       REP STOS DWORD
       MOV ECX, EBX
       AND ECX, # 3
@@1:       
       REP STOS BYTE
       MOV EDI, EDX
       LEA EBP, 0C [EBP]
       MOV EAX, -4 [EBP]
       RET
END-CODE

CODE ASCIIZ> ( c-addr -- c-addr u )
       LEA  EBP, -4 [EBP]
       MOV  EDX, EAX
@@1:   MOV  CL, [EAX]
       LEA  EAX, 1 [EAX]
       OR   CL, CL
       JNZ  SHORT @@1
       LEA  EAX, -1 [EAX]
       SUB  EAX, EDX
       MOV  [EBP], EDX
       RET
END-CODE

\ ================================================================
\ Stack pointers

CODE SP! ( A -> )
     LEA EBP,  4 [EAX]
     MOV EAX, -4 [EBP]
     RET
END-CODE

CODE RP! ( A -> )
     POP EBX
     MOV ESP, EAX
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]
     JMP EBX
END-CODE

CODE SP@ ( -> A )
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     MOV EAX, EBP
     RET
END-CODE

CODE RP@ ( -- RP )
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     LEA EAX, 4 [ESP]
     RET
END-CODE

\ ================================================================
\ Thread data (thread local storage)

CODE TlsIndex! ( x -- ) \ Set thread local storage index
     MOV EDI, EAX
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]
     RET
END-CODE

CODE TlsIndex@ ( -- x )
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
     MOV  EAX, EDI
     RET
END-CODE

CODE FS@ ( addr -- x )
     MOV  EAX, FS: [EAX]
     RET
END-CODE

CODE FS! ( x addr -- )
     MOV  EBX, [EBP]
     MOV  FS: [EAX], EBX
     MOV  EAX, 4 [EBP]
     LEA  EBP, 8 [EBP]
     RET
END-CODE

\ ================================================================
\ Loops

CODE J   \ 94
\ Interpretation: Semantics undefined.
\ Execution: ( -- n|u ) ( R: loop-sys -- loop-sys )
\ n|u - copy of next-outer loop index.
\ Ambiguous condition exists if called outside of loop.
      LEA EBP, -4 [EBP]
      MOV [EBP], EAX
      MOV EAX, 10 [ESP]
      RET
END-CODE

( inline code for loop control )

CODE C-DO
   LEA EBP, 8 [EBP]
   MOV  EBX, EAX
\   A; 908D W,  0000 W, 8000 W,   \   LEA  EDX,  80000000 [EAX] 
   ADD  EAX, # 80000000
   SUB  EAX, -8 [EBP]
   MOV  EDX, EAX
   MOV  EAX, -4 [EBP]
   MOV  EDX, EDX  \ FOR OPT
\   PUSH EDX
\   PUSH EBX
      RET
END-CODE

CODE C-?DO
      CMP  EAX, -8 [EBP]
      JNZ  SHORT @@1
\      SIF  0=
        MOV  EAX, -4 [EBP]
        JMP  EBX
\      STHEN
@@1:  PUSH EBX
   MOV  EBX, EAX
\   A; 908D W,  0000 W, 8000 W,   \   LEA  EDX,  80000000 [EAX] 
   ADD  EAX, # 80000000
   SUB  EAX, -8 [EBP]
   MOV  EDX, EAX
   MOV  EAX, -4 [EBP]
   MOV  EDX, EDX  \ FOR OPT
   PUSH EDX
   PUSH EBX
      RET
END-CODE

CODE  ADD[ESP],EAX 
      ADD [ESP] , EAX 
      RET
END-CODE

CODE C-I
      LEA EBP, -4 [EBP]
      MOV [EBP], EAX
      MOV EAX, [ESP]
      RET
END-CODE

CODE C->R     \ 94
     PUSH EAX
     MOV  EAX, [EBP]
     LEA  EBP, 4 [EBP]
     RET
END-CODE

CODE C-R>    \ 94
     LEA  EBP, -4 [EBP]
     MOV  [EBP],  EAX
     POP EAX
     RET
END-CODE

CODE C-RDROP
     ADD  ESP, # 4
     RET
END-CODE

CODE C-?DUP
     OR  EAX, EAX
     JZ SHORT @@1
     LEA EBP, -4 [EBP]
     MOV [EBP], EAX
@@1: RET
END-CODE 

CODE C-EXECUTE ( i*x xt -- j*x ) \ 94
\ Remove xt from the stack and perform the semantics identified by it.
\ Other stack effects are due to the word executed.
     MOV  EDX, EAX
     MOV  EAX, [EBP]
     LEA  EBP, 4 [EBP]
     CALL EDX
     RET
END-CODE

CODE C-EXECUTE2 ( i*x xt -- j*x edx )
\ Remove xt from the stack and perform the semantics identified by it.
\ Other stack effects are due to the word executed.
\ After executing xt, the value of EDX register is also returned,
\ which contains the high part of int64 result when calling
\ C functions.
     MOV  EDX, EAX
     MOV  EAX, [EBP]
     LEA  EBP, 4 [EBP]
     CALL EDX
     LEA  EBP, -4 [EBP]
     MOV  [EBP], EAX
     MOV  EAX, EDX
     RET
END-CODE

\ ================================================================
\ LOCALS support

CODE DRMOVE ( x1 ... xn n*4 -- )
\ Transfer n cells from data stack to return stack
     POP  EDX \ return address
     MOV  ESI, EAX
@@1: 
     PUSH -4 [EBP] [ESI] 
     SUB  ESI, # 4
     JNZ  SHORT @@1
     ADD  EBP, EAX
     MOV  EAX, [EBP]
     LEA  EBP, 4 [EBP]
     JMP  EDX
END-CODE

CODE NR> ( R: x1 ... xn n -- D: x1 ... xn n )
\ Transfer n cells from return stack to data stack
\ If n=0 returns 0
     POP  EDX \ return address
     LEA  EBP, -4 [EBP]     
     MOV  [EBP], EAX
     POP  EAX
     OR   EAX, EAX
     JNZ  @@2
     JMP  EDX

@@2: LEA  EAX, [EAX*4]
     MOV  ESI, EAX
@@1: 
     MOV  EBX, EBP
     SUB  EBX, ESI
     POP  [EBX]
     SUB  ESI, # 4
     JNZ  SHORT @@1
     SUB  EBP, EAX
     SAR  EAX, # 2
     JMP  EDX
END-CODE

CODE N>R ( D: x1 ... xn n -- R: x1 ... xn n )
\ Transfer n cells from data stack to return stack
     LEA  EBP, -4 [EBP]
     MOV  [EBP], EAX
     LEA EAX, 4 [EAX*4]

     POP  EDX \ return address
     MOV  ESI, EAX
@@1: 
     PUSH -4 [EBP] [ESI] 
     SUB  ESI, # 4
     JNZ  SHORT @@1
     ADD  EBP, EAX
     MOV  EAX, [EBP]
     LEA  EBP, 4 [EBP]
     JMP  EDX
END-CODE

CODE NRCOPY ( D: i*x i -- D: i*x i R: i*x i )
\ Copy n cells from data stack to return stack
     MOV  ECX, EAX
     LEA  ECX, [ECX*4]

     POP  EDX \ return address
     JECXZ @@2
     MOV  ESI, ECX
@@1: 
     PUSH -4 [ESI] [EBP]
     SUB  ESI, # 4
     JNZ  SHORT @@1
@@2:
     PUSH EAX
     JMP  EDX
END-CODE

CODE RP+@ ( offs -- x )
\ Fetch cell at offset offs bytes below return stack top (0 RP+@ == RP@ @)
     8B C, 44 C, 04 C, 04 C, \ MOV EAX, 4 [ESP] [EAX]
     RET
END-CODE
     
CODE RP+ ( offs -- addr )
\ Address at offset offs bytes below return stack top
     8D C, 44 C, 04 C, 04 C, \  LEA EAX, 4 [ESP] [EAX]
     RET
END-CODE

CODE RP+! ( x offs -- )
\ Store cell x at offset offs bytes below return stack top
     MOV  EBX, [EBP] A;
     89 C, 5C C, 04 C, 04 C, \   MOV  4 [ESP] [EAX], EBX
     LEA  EBP, 8 [EBP]
     MOV  EAX, -4 [EBP]
     RET
END-CODE

CODE RALLOT ( n -- addr )
\ Allocate n cells on return stack,
\ filling with zeros (prevents 8th page exception)
     POP  EDX
     MOV  ECX, EAX
     XOR  EAX, EAX
@@1: PUSH EAX
     DEC  ECX
     JNZ  SHORT @@1
     MOV  EAX, ESP
     JMP  EDX
END-CODE

CODE (RALLOT) ( n -- )
\ Allocate n cells on return stack
     POP  EDX
     MOV  ECX, EAX
     XOR  EAX, EAX
@@1: PUSH EAX
     DEC  ECX
     JNZ  SHORT @@1
     MOV  EAX, [EBP]
     LEA  EBP, 4 [EBP]
     JMP  EDX
END-CODE

CODE RFREE ( n -- )
\ Free n cells from return stack
     POP  EDX
     LEA  ESP, [ESP] [EAX*4]
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]
     JMP  EDX
END-CODE

CODE (LocalsExit) ( -- )
\ Read size and deallocate that many cells, then return from word
     POP  EBX
     ADD  ESP, EBX
     RET
END-CODE

CODE TIMER@ ( -- tlo thi ) \ Only for Intel Pentium and higher!!!
\ Return processor cycle counter as ud
   MOV -4 [EBP], EAX
   RDTSC
   MOV -8 [EBP], EDX
   LEA EBP, -8 [EBP]
   XCHG EAX, [EBP]
   RET
END-CODE

\ For systems without RDTSC support:
\ : TIMER@ 0 GetTickCount ;

CODE TRAP-CODE ( D: j*x u R: i*x i -- i*x u )
\ Helper code for exception handling, restores data
\ preserved by CATCH from return stack
     POP  EDX
     POP  ESI
     OR   ESI, ESI
     JZ   @@2
     LEA  ESI, [ESI*4]
     MOV  ECX, ESI
@@1: MOV  EBX, -4 [ESI] [ESP]
     MOV  -4 [ESI] [EBP], EBX
     SUB  ESI, # 4
     JNZ  SHORT @@1
     ADD  ESP, ECX
@@2: JMP  EDX
END-CODE

CODE (ENTER) ( {4*params ret_addr} -- 4*params R: ret_addr ebp ) \ 09.09.2002
\ Switch data stack and save EBP to return stack.
\ Used for creating callbacks and WNDPROC, since
\ SP@ >R would work only if EBP points to data stack,
\ and that may not be true inside callback :)
     POP  EBX       \ return address from ENTER
     POP  ESI       \ return address from CALLBACK/WNDPROC
     MOV  EAX, EBP
     MOV  EBP, ESP

     XOR  EDX, EDX
     MOV  ECX, # 32
@@1: PUSH EDX
     DEC  ECX
     JNZ  @@1

     PUSH ESI
     PUSH EAX
     MOV EAX, [EBP]
     LEA EBP, 4 [EBP]

     JMP  EBX
END-CODE

DECIMAL
