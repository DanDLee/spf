\

REQUIRE NSYM: lib/include/facil.f
1 NSYM: strerror

\ Convert error number to string
: ERRSTR ( n -- adr n )
strerror ASCIIZ>
;
