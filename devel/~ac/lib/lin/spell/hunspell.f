REQUIRE DLL          ~ac/lib/ns/so-xt.f
REQUIRE FILE         ~ac/lib/str5.f
REQUIRE UTF8>UNICODE ~ac/lib/lin/iconv/iconv.f 

ALSO SO NEW: libhunspell.dll

: TEST { \ h }
\ S" ru_RU.dic" DROP S" ru_RU.aff" DROP 2 Hunspell_create won't work, because
\ a modified library (by Google and a bit by me :)) is used,
\ which accepts a binary utf8-dictionary in memory, not a koi8-dictionary on disk
  S" ru-RU-3-0.bdic" FILE SWAP 2 Hunspell_create -> h
  h 0= IF EXIT THEN
  S" test" DROP h 2 Hunspell_spell . CR
  S" testp" DROP h 2 Hunspell_spell . CR

  S" apechatka" DROP PAD h 3 Hunspell_suggest 0 ?DO
	PAD @ I CELLS + @ \ DUP 20 DUMP CR
        ASCIIZ> UTF8>UNICODE UNICODE> ANSI>OEM TYPE CR
  LOOP
;

PREVIOUS

TEST

\EOF
Normally it will print:
1
0
pechatka
a pechatka
perchatka
pechatnika
lapchatka
