: @+  dup cell+ swap @ ;

: narray ( n size - <name ) ( - narray )
    create over , dup , * allot&erase ;
    
: entry ( n narray - a )
    cell+ @+ rot * + ;

: narray@ ( narray - n size a )  @+ swap @+ swap ;

0 value aw  \ narray width
0 value xt
: each-narray-entry ( xt narray - ) ( ... a - ? ... )
    xt >r  swap to xt
    aw >r  narray@ over to aw  ( n size a ) -rot * over + swap
    do  i xt execute 0= if  unloop  leave  then  aw +loop 
    r> to aw  r> to xt ;