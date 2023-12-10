\ CommLine.fth - command line operations

((
Copyright (c) 1998, 2006
MicroProcessor Engineering
133 Hill Lane
Southampton SO15 5AF
England

tel:   +44 (0)23 8063 1441
email: mpe@mpeforth.com
       tech-support@mpeforth.com
web:   htpp://www.mpeforth.com
Skype: mpe_sfp

From North America, our telephone and fax numbers are:
       011 44 23 8063 1441
       901 313 4312 (North American access number to UK office)


To do
=====

Change history
==============
20060127 MPE001 Major overhaul
))

only forth definitions
decimal


\ ===========
\ *> tools
\ *S Command Line parser
\ ===========
\ *P VFX Forth includes code which can parse a zero-terminated
\ ** string into *\fo{ARGV[]} and *\fo{ARGC} format as in C.

\ *P As of VFX Forth v3.9, the following changes have been made:
\ *(
\ *B Quoted strings are treated as single entities and the '"'
\ ** characters are removed. Note that '\' characters are not
\ ** processed as special characters.
\ *B The ARGV array is 1024 bytes long. If the the O/S command
\ ** line is longer than this, the additional characters are
\ ** discarded.
\ *)


\ =================
\ +N INTERNAL Words
\ =================
\ +P These words are used by the internals of the command line mechanism.

also system definitions

$0010 constant MAXARGS		\ -- u
MAXARGS cells buffer: *szArgV	\ -- addr
#1024 cell + buffer: CmdLine	\ -- addr ; copied from O/S

: ProcessArg	\ zaddr addr -- zaddr' flag
\ +G Given an address of a non-null zero-terminated string, and
\ +* an address (e.g. in the ARGV array), set the pointer and zero
\ +* terminate the argument string. Update the string address to
\ +* point to the next token. Return flag true if the string  is
\ +* finished.
  swap					\ -- addr zaddr
  dup c@ [char] " =			\ allow for quoted items
  if  1+  [char] " >r  else  bl >r  then
  dup rot !				\ set start address
  begin					\ look for 0 or bl/"
    dup c@  swap 1+ swap
    dup r@ = swap 0= OR
  until					\ -- last+1 ; R: -- char
  r> drop
  dup 1- c@ 0=				\ -- last+1 flag
  over 0 swap 1- c!			\ zero terminate ARGV string
; doNotSin

previous definitions


\ ===============
\ +N Public words
\ ===============

also system

0 value argc	\ -- u
\ *G The number of defined arguments.

: argv[                 \ n -- pointer|0
\ *G Given an index of 0..argc-1 return a pointer to the command
\ ** line token's zero-terminated string. *\fo{0 argv[} returns the
\ ** executable's name. If the argument does not exist, the
\ ** pointer is zero.
  CELLS *szArgv + @
; doNotSin

: ParseCommandLine      \ zaddr --
\ *G Given a pointer to a 0 terminated string parse it as the
\ ** command line in preparation for use with *\fo{ARGC} and
\ ** *\fo{ARGV[}. The string is copied to a local ARGV buffer of
\ ** 1024 bytes.
  zcount #1023 min CmdLine zplace
  CmdLine				\ -- zaddr'
  0 -> argc
  *szArgv MAXARGS CELLS erase		\ prepare array
  *szArgv MAXARGS CELLS bounds do	\ -- zaddr'
    zcount -leading 0= ?leave		\ skip leading spaces, done if at end
    i ProcessArg  inc argc  ?leave
  cell +loop
  drop
  argc cells *szArgv + off		\ clean last+1 argument
; doNotSin

: CommandLine	\ -- c-addr len
\ *G Return the name of the system executable and
\ ** initialise the ARGC/ARGV mechanism. Use
\ ** *\fo{ARGC} and *\fo{ARGV[} to extract the parameter strings.
\ ** The O/S command line is copied to a local ARGV buffer of
\ ** 1024 bytes. Under Windows, the raw command line can be
\ ** accessed using the *\f{GetCommandLine(void)} API call.
\ ** Under Linux, *\fo{OS_GetCommandLine} in the *\fo{SYSTEM}
\ ** vocabulary returns a zero terminated string containing the
\ ** raw commandline.
  [ also system ] OS_GetCommandLine [ previous ]
  ParseCommandLine
  0 argv[ dup
  if  zcount  else  zNull swap  endif
; doNotSin

previous


\ =========
\ Test code
\ =========

((
: cl		\ -- z$
  [ also system ] OS_GetCommandLine [ previous ]
;

create tline$
  z\", \"C:\\Products\\VfxCommunity\\Sources\\Examples\\ForthEd2\\forthed2.exe\" -f \"foo.fth\" -l 55"
))


\ ======
\ *> ###
\ ======

decimal
