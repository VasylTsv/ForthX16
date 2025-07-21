\ Forth-94 version of Klaus Schleisiek's dynamic memory allocation (FORML'88) uh 2016-10-28

Variable anchor  0 anchor !

decimal 050 Constant waste

-1 1 rshift Constant #max
#max invert Constant #free  \ sign bit

: size ( mem -- size ) 1 cells - @ #max and ;

: addr&size ( mem -- mem size ) dup size ;

: above ( mem -- >mem )   addr&size + 2 cells + ;

: use ( mem size -- )
    dup >r swap  2dup 1 cells - !  r> #max and + ! ;

: release ( mem size -- )   #free or use ;

: fits? ( size -- mem | false ) >r anchor @
   BEGIN addr&size  r@ u< 0=
         IF r> drop EXIT THEN
         @ dup anchor @ =
   UNTIL 0= r> drop ;

: link ( mem >mem <mem -- )
   >r 2dup cell+ !  over !  r> 2dup !  swap cell+ ! ;

: @links ( mem -- <mem mem> )  dup @  swap cell+ @ ;

: setanchor ( mem -- mem ) 
   dup anchor @ = IF  dup @ anchor ! THEN ;

: unlink ( mem -- ) setanchor  @links 2dup !  swap cell+ ! ;

: allocate ( size -- mem ior )
   dup 0< if -1 exit then
   3 cells max dup >r  fits? ?dup 0= IF r> -8 EXIT THEN ( "dictionary overflow" )
   addr&size r@ -  dup waste u<
   IF  drop  dup @ over unlink  over addr&size use
   ELSE 2 cells -   over r@ use
        over above   dup rot release
        2dup swap @links link THEN
   r> drop  anchor ! 0 ;

: free ( mem -- ior )
   addr&size  over 2 cells -  @ dup 0<
   IF #max and 2 cells +  rot over - rot rot +
   ELSE  drop  over anchor @  dup cell+ @  link THEN
   2dup + cell+ dup @ dup 0<
   IF  #max and swap cell+ unlink  +  2 cells +  release 0 EXIT THEN
   2drop release 0 ;

: resize ( mem newsize -- mem' ior )
    dup 0< if exit then
    over swap  over size  2dup >
    IF ( mem mem size newsize )  swap allocate ?dup IF >r drop 2drop r>  EXIT THEN 
        dup >r swap move free r> swap EXIT THEN
    2drop drop 0 ;

: empty-memory ( addr size -- )
   >r  cell+ dup anchor !   dup 2 cells use  dup 2dup link
   dup above  swap over  dup link
   dup r> 7 cells -  release  above 1 cells -  0 swap ! ;

cr 
cr .( dynamic memory allocation:)
cr .( Use   addr size EMPTY-MEMORY  to initialize,)
cr .( then use the standard memory allocation wordset ALLOCATE FREE RESIZE to manage memory.)
