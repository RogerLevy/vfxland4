\ MPE VFX Forth for x64 hardware floating point pfackage
\ double precision

((
Copyright (c) 2022, 2023
Wodni & Pelc GmbH
Ödenburgerstraße 21/6/9,
1210 Wien
Austria

Copyright (c) 2000-2009, 2010, 2011, 2015, 2016, 2018, 2019, 2020
MicroProcessor Engineering
133 Hill Lane
Southampton SO15 5AF
England

tel: +44 (0)23 8063 1441
net: mpe@mpeforth.com
     tech-support@mpeforth.com
web: www.mpeforth.com

From North America, our telephone and fax numbers are:
  011 44 23 8063 1441


To do
=====

Change History
==============
20230910 SFP036 Updated intstall and remove code
20201019 DAW035 Corrected F~
20200926 SFP034 Added F~
20200924 SFP033 Added F+!, F-! and FVALUE.
		Fixed 1/F and hence what uses it.
20200623 BGD032 Corrected SFLOATS.
20200529 SFP031 Converted to SSE and x64. FTOS = XMM8, FSP = R13.
20190327 SFP030 Updated for VFX v5.1+ with recognisers.
20180628 SFP029 Updated for VFX 5.0.
20170616 SFP028 Updated for F** and assembler integration.
20161115 SFP027 More changes for FSP=R8.
20160119 SFP026 Only set FPSYSTEM on load, not FPABI.
20160118 SFP025 Changed FSP from USER variable to R8.
20160117 SFP024 Corrected D>F and F>D for d=0.
20160114 SFP023 Changed FTOS to D8 because of the EABI.
20160108 SFP022 Set FPSYSTEM and FPABI on load.
20150529 SFP021 Corrected FSIGN.
20150506 SFP020 Big overhaul. Updates for ANS and Forth 2012.
20150428 SFP019 Converted to VFP double precision - VFP64S.fth.
20150423 SFP018 Converted to VFP single precision - VFP32S.fth.
20150330 SFP017 Overhauled to run on VFX Forth.
		Updated for systems whose shift instructions do not return
		0 for a shift count of 32 or more.
		Reordered for hosted systems.
		Converted to use separate float and data stacks.
20150122 BJC016 Previous change BJC014 did not work properly. Now using 5/4
                rounding for values printed in non exponent format using F.
                Added word NORM-FROUND which applies 5/4 rounding assuming that
                the value is printed 'normally'.
20111201 MPE015 Updated documentation.
20110302 BJC014 Corrected OP-PREPARE which assumes 8 sig figs regardless
                of the actual precision required.  It now uses PLACES to
                ensure that output from F. etc is now rounded to the places
                displayed.
20100630 MPE013 Improved compatibility with VFX Forth for Win/Lin/DOS.
                Minor refactoring.
20100628 BJC012 Changes to FNUMBER? allow use of HEX and other
                integers after REALS executed:
                If BASE is not decimal FNUMBER? always does
                integer conversion.
                FCHECK now recognises the integer prefices
                ('#','$','0x' etc.) so these are converted as
                integers.
                The character used as a decimal separator is
                set in VALUE DPchar, defined here (if not previously).
                This allows use of ',' or indeed other characters.
                The US/UK convention '.' is the default.
20100526 BJC011 CDATA is selected prior to defining all FARRAYs
                for constant data (previously CDATA was assumed,
                and things broke if it wasn't).
20090729 SBD010 Fixed ROUND at limits.
		Improved accuracy in ?10PWR.
20090319 SFP009 Added INTERPRETER versions of F@ F! and F,
		Added FBUFF.
20090220 MPE008 Added Alberto Pasquale's IEEE conversion words.
20071105 SFP007 Made >FLOAT convert only in DECIMAL.
20071012 SFP006 Improved documentation.
20060328 SFP005 Reordered to remove forward references.
20050705 SFP004 Improved FVARIABLE for optimised systems.
20021112 SFP003 DOCGENned it.
20000713 SFP002 Renamed DINT to F>D for consistency. F>D is the ANS word.
		The original F>D was just a synonym. Similarly SINT was
		renamed to F>S
20000706 SFP001 Applied improvements supplied by Hiden Analytical.
		Added minor performance optimisations.
		Made F/ and DINT return clipped values on overflow.
19981029 MSD000 Brought the common stuff out of all the various versions of
         MSD000 SOFTFP.FTH into a single (that is, *this*) file.

Polynomials from
  Cody and Waite
    http://ebook30.com/science/mathematics/190013/william-j.-cody-software-manual-for-the-elementary-functions.html

  Abramowitz & Stegun
    http://www.math.ucla.edu/~cbm/aands/
    http://www.convertit.com/Go/Bioresearchonline/Reference/AMS55.ASP
))


\ =============
\ *! floatsse64
\ =============
\ *T SSE Floating Point

\ **********
\ *S WARNING
\ **********
\ *P As of August 2020, the SSE code is functional, but is slow
\ ** because there is no optimisation. An SSE
\ ** optimiser will be provided in due course. If you need FP
\ ** performance, use the NDP float pack in *\i{Lib/x64/Ndpx64.fth}.

\ ***************
\ *S Introduction
\ ***************
\ *P The Forth data stack and the floating point stack are separate.
\ ** As with the data and return stacks, the floating point stack
\ ** grows down.
\ ** The floating point data storage format is IEEE 64 bit (double
\ ** precision) format.The source code is in the file
\ ** *\i{Lib/x64/FPSSE64S.fth}. The *\fo{Extern:} call mechanism
\ ** requires *\i{Lib/x64/FPSSE64S.fth} when *\fo{float} or *\fo{double}
\ ** arguments are used.

\ *P There are occasions when the 64 bit float format causes
\ ** problems. In these cases you can use the 80 bit floats
\ ** provided in *\i{Lib/x64/Ndpx64.fth}. However, you will
\ ** have to convert them to SSE form for use with the *\fo{Extern:}
\ ** mechanism, unless you use VFX Forth 64 v5.4 or later which
\ ** features automatic conversion of floats in the *\fo{Extern:}
\ ** system.

\ **********************************
\ *S Entering floating-point numbers
\ **********************************
\ *P Floating point number entry is enabled by *\fo{REALS} and
\ ** disabled by *\fo{INTEGERS}.

\ *P Floating-point numbers of the form 0.1234e1 are required
\ ** (see *\fo{FNUMBER?}) during interpretation and compilation
\ ** of source code. Floating-point numbers are compiled as
\ ** literal numbers when in a colon definition (compiling) and
\ ** placed on the stack when outside a definition (interpreting).
\ *P The more flexible word *\fo{>FLOAT} accepts numbers in two
\ ** forms, 1.234 and 0.1234e1. Both words are documented later
\ ** in this chapter. See also the section on *\i{Gotchas} later
\ ** in this chapter.

\ *P Note also that MPE Forths use ',' by default (it can be
\ ** changed) as the double number indicator - it makes life
\ ** much easier for Europeans.

\ *************************************
\ *S The form of floating-point numbers
\ *************************************
\ *P A floating-point number is placed on a separate floating point stack. In
\ ** the Forth literature, this is referred to as separated floating
\ ** point and data stacks. As with the data and return stacks, the
\ ** floating point stack grows down. Items on the float stack are
\ ** in IEEE 64-bit format.

\ *******************************
\ *S Creating and using variables
\ *******************************
\ *P To create a variable, use *\fo{FVARIABLE}. *\fo{FVARIABLE} works in the
\ ** same way as *\fo{VARIABLE}. For example, to create a floating-point
\ ** variable called *\fo{VAR1} you code:
\ *C   FVARIABLE VAR1
\ *P When *\fo{VAR1} is used, it returns the address of the floating-point
\ ** number.

\ *P Two words are used to access floating-point variables,
\ ** *\fo{F@} and *\fo{F!}. These are analogous to *\fo{@} and
\ ** *\fo{!}.

\ *********************
\ *S Creating constants
\ *********************
\ *P To create a floating-point constant, use *\fo{FCONSTANT}, which
\ ** is analogous to *\fo{CONSTANT}. For example, to generate a
\ ** floating-point constant called *\fo{CON1} with a value of 1.234,
\ ** you enter:
\ *C   1.234e0 FCONSTANT FCON1
\ *P When *\fo{FCON1} is executed, it returns 1.234 on the Forth
\ ** stack.

\ ***************************
\ *S Using the supplied words
\ ***************************
\ *P The supplied words split into several groups:
\ *(
\ *B sines, cosines and tangents
\ *B arc sines, cosines and tangents
\ *B arithmetic functions
\ *B logarithms
\ *B powers
\ *B displaying floating-point numbers
\ *B inputting floating-point numbers
\ *)

\ *P The following functions only exist as target words so you
\ ** cannot use them in calculations in your source code when
\ ** outside a colon definition.

\ *N Calculating sines, cosines and tangents
\ *P To calculate sine, cosine and tangent, use *\fo{FSIN}, *\fo{FCOS} and
\ ** *\fo{FTAN} respectively. Angles are expressed in radians.

\ *N Calculating arc sines, cosines and tangents
\ *P To calculate arc sine, cosine and tangent, use *\fo{FASIN}, *\fo{FACOS}
\ *P and *\fo{FATAN} respectively. They return an angle in radians.

\ *N Calculating logarithms
\ *P Two words are supplied to calculate logarithms, *\fo{FLOG} and *\fo{FLN}.
\ ** *\fo{FLOG} calculates a logarithm to base 10 (decimal).
\ ** *\fo{FLN} calculates a logarithm to base e. Both take a
\ ** floating-point number in the range from 0 to Einf.

\ *N Calculating powers
\ *P Three power functions are supplied:
\ *C   FEXP F10^X X^Y

\ *********************
\ *S Degrees or radians
\ *********************
\ *P The angular measurement used in the trigonometric functions
\ ** are in radians. To convert between degrees and radians use
\ ** *\fo{RAD>DEG} or *\fo{DEG>RAD}. *\fo{RAD>DEG} converts an angle from radians
\ ** to degrees. *\fo{DEG>RAD} converts an angle from degrees to radians.

\ ************************************
\ *S Displaying floating-point numbers
\ ************************************
\ *P Two words are available for displaying floating-point numbers,
\ ** *\fo{F.} and *\fo{E.}. The word *\fo{F.} takes a floating-point
\ ** number from the stack and displays it in the form xxxx.xxxxx
\ ** or x.xxxxxEyy depending on the size of the number. The word
\ ** *\fo{E.} displays the number in the latter form.

\ ************************************
\ *S Number formats, ANS and Forth200x
\ ************************************
\ *P The ANS Forth standard specifies that floating point numbers must
\ ** be entered in the form 1.234e5 and must contain a point '.' and
\ ** 'e' or 'E', and that double integers are terminated by a point '.'.

\ *P This situation prevents the use of the standard conversion
\ ** words in international applications because of the
\ ** interchangable use of the '.' and ',' characters in numbers.
\ ** Because of this, VFX Forth uses
\ ** two four-byte arrays, *\fo{FP-CHAR} and *\fo{DP-CHAR}, to
\ ** hold the characters used as the floating point and double
\ ** integer indicator characters.

\ ** The *\fo{FP-CHAR} and *\fo{DP-CHAR} arrays (in the kernel)
\ ** hold up to four character(s) to be treated as indicators. Set
\ ** to '.' for ANS compatibility. Note that they should be
\ ** accessed as one to four byte arrays, terminated by a zero
\ ** byte. The first character of *\fo{FP-CHAR} is used as the
\ ** point character for output.

\ *P By default, *\fo{FP-CHAR} is initialised to '.'
\ ** and *\fo{DP-CHAR} is initialised to ',' and '.'. For strict
\ ** ANS compliance, you should set them as follows.

\ *E \ ANS standard setting
\ **   char . dp-char !
\ **   char . fp-char !
\ ** : ans-floats    \ -- ; for strict ANS compliance
\ **   [char] . dp-char !
\ **   [char] . fp-char !
\ ** ;
\ ** \ MPE defaults
\ **   char , dp-char !
\ **   char . dp-char 1+ c!
\ **   char . fp-char !
\ ** : mpe-floats    \ -- ; for existing and most legacy code
\ **   [char] , dp-char !
\ **   [char] . dp-char 1+ c!
\ **   [char] . fp-char !
\ ** ;

\ *P You can of course set these arrays to hold any values which
\ ** suit your application's language and locale. Note that integer
\ ** conversion is always attempted before floating point
\ ** conversion. This means that if the *\fo{FP-CHAR} and *\fo{DP-CHAR}
\ ** arrays contain the same character, floating point numbers must
\ ** contain 'e' or 'E'. If the arrays are all different, a number
\ ** containing the *\fo{FP-CHAR} will be successfully converted
\ ** as a floating point number, even if it does not contain 'e'
\ ** or 'E'.


\ **********************
\ *S Only one FP package
\ **********************
\ *P Only one float pack can be installed. This is checked at compile time.
\ ** To replace the floating point pack use:
\ *E integers
\ ** remove-FP-pack
\ ** include <sourcefile>

c" FP-PACK" find nip  FPsystem SSE64System =  and
dup [IF]  cr ." SSE64System pack already compiled"  [THEN]
?StopIncluding

c" FP-PACK" find nip  FPsystem 0<> or
[IF]  cr ." Remove existing float pack before compiling this one" abort  [THEN]


\ ***************
\ *S Configuation
\ ***************

create FP-PACK	\ -- addr
\ *G Marks that a float pack is being compiled.

SSE64System -> FpSystem
\ *P The value *\fo{FPSYSTEM} defines which floating point pack is installed
\ ** and active. See the Floating Point chapters for
\ ** further details.
\ ** Each floating point pack defines its own type as follows:
\ *(
\ *B 0 constant NoFPSystem
\ *B 1 constant HFP387System (also 64 bit)
\ *B 2 constant NDP387System (also 64 bit)
\ *B 3 constant OpenGL32System (obsolete)
\ *B 4 constant SSE64System
\ *)
\ *P When *\i{FPSystem} changes, the following files that use *\i{FPSystem}
\ ** are affected:
\ *C   Extern*.fth  kernel64.fth  Tokeniser.fth
\ *C   Lib/x64/Ndpx64.fth  Lib/Hfpx64.fth  Lib/x64/FPSSE64.fth
\ *P At present, only 0, 1, 2 and 4 are valid values of *\i{FPSystem}
\ ** in x64 systems.

\ Define size of items for default memory access
#8 constant FPCELL	\ -- n
\ *G Defines the size of literals and floating point numbers in memory
\ ** and on floating point stacks in memory
FPCELL negate constant -FPCELL

#8 constant /NDPSLOT	\ -- n
\ *G Size of aligned memory buffer used to hold an FP number.
/NDPSLOT negate constant -/NDPSLOT
\ *G Negative of *\fo{/NDPSLOT}


\ ****************
\ *S FP primitives
\ ****************

defer f.s	\ F: f --
\ *G Non-destructive display of the floating point stack.

: finit		\ F: i*f -- ; resets FPU and FP stack
\ *G Reset the floating point stack.
  fs0 @ fsp!
;

: fdepth        \ -- #f
\ *G Floating point equivalent of *\fo{DEPTH}. The result is returned
\ ** on the Forth data stack.
  FS0 @  fsp@ -  FPCELL /  ;		\ possible # of floats on FP stack

[undefined] clz [if]
code CLZ	\ x -- u
\ *G Return the number of leading zeros in x.
  cmp		rbx, # 0
  z, if,
    mov	  	rbx, # 64
  else,
    bsr		rcx, rbx		\ bit position of LH 1
    mov		rbx, # 63
    sub		rbx, rcx
  then,
  next,
end-code
[then]

: DCLZ		\ dx -- u
\ *G Return the number of leading zeros in the double dx.
  clz dup #64 = if			\ -- sx cnt
    drop  clz #64 +
  else
    nip
  then
;

code >fs	\ f64 -- ; F: -- f64
\ *G Move a float from the data stack to the floating point stack.
  sub		r13, # FPCELL		\ update FSP
  movsd		0 [r13], xmm8		\ save old FTOS
  movq		xmm8, rbx		\ TOS->FTOS
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

code fs>	\ F: f64 -- ; -- f64
\ *G Move a float from the float stack to the data stack.
  sub		rbp, # cell		\ save TOS
  mov		0 [rbp], rbx
  movq		rbx, xmm8		\ FTOS -> TOS
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  next,
end-code

code fs@	\ F: f64 -- f64 ; -- f64
\ *G Copy a float from the float stack to the data stack.
  sub		rbp, # cell		\ save TOS
  mov		0 [rbp], rbx
  movq		rbx, xmm8		\ FTOS -> TOS
  next,
end-code

code fps@	\ -- fps
\ *G Read the MXCSR floating point status/control register.
  sub		rbp, # cell
  mov		0 [rbp], # 0
  stmxcsr	0 [rbp]
  xchg		rbx, 0 [rbp]
  next,
end-code

code fps!	\ fps --
\ *G Set the MXCSR floating point status/control register.
  xchg		rbx, 0 [rbp]
  ldmxcsr	0 [rbp]
  add		rbp, # cell
  next,
end-code

code exp@	\ F: f -- f ; -- exp(2)
\ *G Copy the exponent of the top float to the data stack.
\ ** The IEEE exponent offset is removed. The floating
\ ** point number has a mantissa in the range 0.5 <=
\ ** mantisa < 1.0, such that the number is in the form:
\ *C   sign * mantissa * 2^exp
\ *P The exponent returned by *\fo{exp@} and consumed by
\ ** *\fo{exp!} is not the offset 1023 exponent of the IEEE 754
\ ** standard - it is one greater than that. IEEE views the number
\ ** as being in the form:
\ *C   sign * 1.fraction * 2^(exp-1023)
  sub		rbp, # cell
  mov		0 [rbp], rbx
  movq		rbx, xmm8		\ FTOS -> TOS
  shr		rbx, # #52		\ exponent is 11 bits at bit 52
  and		rbx, # $7FF		\ exponent mask to remove sign bit
  sub		rbx, # $3FE		\ remove offset 1023-1
  next,
end-code

code exp!		\ exp(2) -- ; F: f -- f'
\ *G Change/Set the exponent of the top float.
\ ** The IEEE exponent offset is added.
  add		rbx, # $3FE		\ add offset 1023-1
  shl		rbx, # #52		\ put exponent in right place
  mov		rax, # $7FF0:0000:0000:0000  \ RAX = exponent mask
  and		rbx, rax		\ clip exponent
  not		rax			\ invert mask
  movq		rcx, xmm8		\ get FTOS
  and		rcx, rax		\ clear exponent
  or		rcx, rbx		\ merge in new exponent
  movq		xmm8, rcx		\ put FTOS back
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

code F!		\ F: r -- ; addr --
\ *G Stores *\i{r} at *\i{addr}.
  movsd		0 [rbx], xmm8
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

code F@		\ addr -- ; F: -- r
\ *G Fetches *\i{r} from *\i{addr}.
  sub		r13, # FPCELL		\ save FTOS
  movsd		0 [r13], xmm8
  movsd		xmm8, 0 [rbx]		\ fetch from addr
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

code f+!	\ F: f -- ; addr -- ; add f to data at addr
\ *G Add F to the data at ADDR.
  addsd		xmm8, 0 [rbx]		\ add to FTOS from addr
  movsd		0 [rbx], xmm8		\ save FTOS
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

code f-!	\ F: f -- ; addr -- ; sub f from data at addr
\ *G Subtract F from the data at ADDR.
  movsd		xmm9, 0 [rbx]		\ contents of addr
  subsd		xmm9, xmm8		\ subtract FTOS
  movsd		0 [rbx], xmm9		\ save contents
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

synonym DF! F!		\ F: r -- ; addr --
\ *G Stores *\i{r} at *\i{addr} in IEEE 64 bit format.

synonym DF@ F@		\ addr -- ; F: -- r
\ *G Fetches *\i{r} from *\i{addr}, which contains a float
\ ** in IEEE 64 bit format..

code SF!		\ F: r -- ; addr --
\ *G Stores *\i{r} at *\i{addr}.
  cvtsd2ss	xmm9, xmm8		\ convert DF to SS
  movss		0 [rbx], xmm9
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

code SF@		\ addr -- ; F: -- r
\ *G Fetches *\i{r} from *\i{addr}.
  sub		r13, # FPCELL		\ save FTOS
  movsd		0 [r13], xmm8
  movss		xmm9, 0 [rbx]
  cvtss2sd	xmm8, xmm9		\ convert SF to DF
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

: F,		\ F: r --
\ *G Lays a real number into the dictionary, reserving FPCELL bytes.
  here  FPCELL allot  f!  ;

synonym DF, F,
\ *G Lays a real number into the dictionary as an IEEE 64 bit number.

: SF,		\ F: r --
\ *G Lays a real number into the dictionary as an IEEE 32 bit number.
  here  4 allot  sf!  ;

code FDUP  	\ F: r -- r r
\ *G Floating point equivalent of *\fo{DUP}.
  sub		r13, # FPCELL		\ save FTOS
  movsd		0 [r13], xmm8
  next,
end-code

code FOVER	\ F: r1 r2 -- r1 r2 r1
\ *G Floating point equivalent of *\fo{OVER}.
  sub		r13, # FPCELL		\ save FTOS
  movsd		0 [r13], xmm8
  movsd		xmm8, FPCELL [r13]	\ new FTOS
  next,
end-code

code FSWAP	\ F: r1 r2 -- r2 r1
\ *G Floating point equivalent of *\fo{SWAP}.
  movsd		xmm9, 0 [r13]
  movsd		0 [r13], xmm8
  movsd		xmm8, xmm9
  next,
end-code

code FPICK	\ u -- ; F: fu..f0 -- fu..f0 fu
\ *G Floating point equivalent of *\fo{PICK}.
  sub		r13, # FPCELL		\ save FTOS
  movsd		0 [r13], xmm8
  movsd		xmm8, 0 [r13] [rbx*8]	\ get uth item
  mov		rbx, 0 [rbp]		\ restore TOS
  add		rbp, # cell
  next,
end-code

code FROT	\ F: r1 r2 r3 -- r2 r3 r1
\ *G Floating point equivalent of *\fo{ROT}.
  movsd		xmm11, xmm8		\ xmm11 := r3/FTOS
  movsd		xmm10, 0 [r13]		\ xmm10 := r2
  movsd		xmm8, FPCELL [r13]	\ xmm8  := r1
  movsd		0 [r13], xmm11		\ FNOS  := r3
  movsd		FPCELL [r13], xmm10	\ F3OS  := r2
  next,
end-code

code F-ROT		\ F: r1 r2 r3 -- r3 r1 r2
\ *G Floating point equivalent of *\fo{-ROT}.
  movsd		xmm11, xmm8		\ xmm11 := r3/FTOS
  movsd		xmm8, 0 [r13]		\ xmm8  := r2
  movsd		xmm10, FPCELL [r13]	\ xmm10 := r1
  movsd		0 [r13], xmm10		\ FNOS  := r1
  movsd		FPCELL [r13], xmm11	\ F3OS  := r3
  next,
end-code

code FDROP	\ F: r --
\ *G Floating point equivalent of *\fo{DROP}.
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  next,
end-code

code FNIP		\ F: r1 r2 -- r2
\ *G Floating point equivalent of *\fo{NIP}.
  add		r13, # FPCELL		\ update FSP
  next,
end-code

code f>r	\ F: f -- ; R: -- f
\ *G Put float onto return stack.
  pop		rdx
  sub		rsp, # FPCELL
  movsd		0 [rsp], xmm8
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  jmp		rdx
  next,
end-code

code fr>	\ R: f -- ; F: -- f
\ *G Pull float from the return stack.
  pop		rdx
  sub		r13, # FPCELL		\ save FTOS
  movsd		0 [r13], xmm8
  movsd		xmm8, 0 [rsp]		\ load FTOS from return stack
  add		rsp, # FPCELL
  jmp		rdx
  next,
end-code

code flit	\ F: -- f ; inline literal
\ *G Run-time routine for a floating point literal. version.
  sub		r13, # FPCELL		\ save FTOS
  movsd		0 [r13], xmm8
  pop		rax			\ return address
  movsd		xmm8, 0 [rax]		\ get lit
  add		rax, # FPCELL		\ step over it
  jmp		rax			\ return
  next,					\ keep disassembler happy
end-code


\ ********************************
\ *S Floating point defining words
\ ********************************

: FVARIABLE	\ "<spaces>name" -- ; Run: -- addr
\ *G Use in the form: *\fo{FVARIABLE <name>} to create a variable
\ ** that will hold a floating point number.
  FPCELL buffer:  ;

: FCONSTANT	\ F: r -- ; "<spaces>name" -- ; Run: -- r
\ *G Use in the form: *\fo{<float> FCONSTANT <name>} to create a
\ ** constant that returns a floating point number.
  create  f,  does> f@  ;

: FARRAY	\ "<spaces>name" fn-1..f0 n -- ; Run: i -- ; F: -- ri
\ *G Create an initialised array of floating point numbers. Use
\ ** in the form:
\ *C   fn-1 .. f1 f0 n FARRAY <name>
\ *P to create an array of n floating point numbers. When the
\ ** array *\fo{name} is executed, the index i is used to return
\ ** the address of the i'th 0 zero-based element in the array.
\ ** For example:
\ *C   4e0 3e0 2e0 1e0 0e0 5 FARRAY TEST
\ *P will set up an array of five elements.
\ ** Note that the rightmost float (0e0) is element 0.
\ ** Then *\fo{i TEST} will return the *\{i}th element.
  create
    0 do  f,  loop
  does>
    swap FPCELL * + f@
;

: FBUFF		\ u "name" -- ; i -- addr
\ *G Creates a buffer for *\i{u} floats in the current memory
\ ** section. The child action is to return the address of the
\ ** *\i{i}th element (zero-based).
\ *C   10 fbuff foo
\ *P Creates an buffer for ten float elements.
\ *C   3 foo
\ *P Returns the address of element 3 in the buffer.
  create
    FPCELL * allot
  does>
    swap FPCELL * +
;

: fvalCompChild	\ xt --
\ Given the xt of an FVALUE, compile the required action.
  >body postpone literal
  case  OperatorType @off
    0 of  postpone f@  endof		\ Default, fetch
    1 of  postpone f!  endof		\ store, ->, to
    2 of  ( noop )  endof		\ addr
\					\ inc
\					\ dec
    5 of  postpone f+!  endof		\ add
\					\ zero
    7 of  postpone f-!  endof		\ sub
    8 of  postpone drop  postpone FPCELL  endof	\ sizeof
\					\ set
      bad-method
  endcase
; doNotSin

: fvalue	\ F: f -- ; ??? -- ???
\ *G Use in the form: *\fo{<float> FVALUE <name>} to create a
\ ** floating point version of *\fo{VALUE} that will return a
\ ** floating point number by default, and that can accept the
\ ** operators *\fo{TO}, *\fo{ADDR}, *\fo{ADD}, *\fo{SUB}, and
\ ** *\fo{SIZEOF}. )
  create
    f,  ['] fvalCompChild set-compiler
  interp>
    case  OperatorType @off
      0 of  f@  endof			\ default, fetch
      1 of  f!  endof			\ store, ->, to
      2 of  ( noop )  endof		\ addr
\                         		\ inc
\					\ dec
      5 of  f+!  endof			\ add
\                        		\ zero
      7 of  f-!  endof			\ sub
      8 of  drop FPCELL  endof		\ sizeof
\					\ set
         bad-method
    endcase
;


\ *******************
\ *S Type conversions
\ *******************

code FSIGN	\ F: fn -- |fn| ; -- flag ; true if negative
\ *G Return the absolute value of fn and a flag which is true
\ ** if fn is negative.
  sub		rbp, # cell		\ save TOS
  mov		0 [rbp], rbx
  movq		rcx, xmm8		\ get fn
  xor		rbx, rbx		\ TOS -> 0
  test		rcx, rcx
  l, if,
    movsd	xmm9, xmm8		\ copy FTOS
    movq	xmm8, rbx		\ FTOS -> 0e0
    subsd	xmm8, xmm9		\ FTOS -> 0e0-FTOS
    sub		rbx, # 1		\ TOS -> -1
  then,
  next,
end-code

[-short-branches
: D>F		\ d -- ; F: -- fn
\ *G Converts a double integer to a float.
  2dup or if
    dup 0< >r  dabs			\ strip sign
    2dup dclz  dup #127 swap - >r	\ form exponent, 1=2^0 ; -- d clz ; R: -- sign exp
    #11 - dup 0<			\ first '1' bit to bit 52 in upper word
    if  abs drshift  else  dlshift  then
    $000F:FFFF:FFFF:FFFF and 		\ clip
    r> #1023 + $7FF and #52 lshift  or	\ merge exponent
    r> $8000:0000:0000:0000 and or	\ bit 63 is sign bit
  then
  nip  >fs
;

: f>d		\ F: f -- ; -- dint(f)
\ *G Converts a float to a double integer.
\ ** Note that *\fo{F>D} truncates the number towards zero
\ ** according to the ANS specification.
  {: | exp temp[ FPCELL ] -- :}
  exp@ -> exp  temp[ f!
  temp[ @ dup if			\ -- f64 ; exponent in top half
    \ form low portion of double number
    $000F:FFFF:FFFF:FFFF and		\ bits 51:0 are mantissa
    $0010:0000:0000:0000 or		\ bit 52 is hidden
    \ form high portion
    0
    \ shift as required
    #53 exp -  dup 0<			\ find shift count
    if  dlshift  else  drshift  then	\ to integer
    \ apply sign
    temp[ @ 0<				\ sign bit is top bit
    if  dnegate  then
  else					\ -- 0
    0
  then
;
short-branches]

: S>F	\ n -- ; F: -- fn
\ *G Converts a single signed integer to a float.
  s>d d>f
;

: f>s		\ F: f -- ; -- int(f)
\ *G Converts a float to a single integer.
\ ** Note that *\fo{F>S} truncates the number towards zero
\ ** according to the ANS specification.
  f>d drop
;

: FINT		\ F: f1 -- f2
\ *G Chop the number towards zero to produce a floating point
\ ** representation of an integer.
  f>d d>f
;


\ *************
\ *S Arithmetic
\ *************

code FNEGATE	\ F: r1 -- r2
\ *G Floating point negate.
  xor		rax, rax
  movq		xmm9, xmm8
  movq		xmm8, rax		\ FTOS -> 0e0
  subsd		xmm8, xmm9		\ FTOS -> 0e0-FTOS
  next,
end-code

: ?FNEGATE	\ n -- ; F: fn -- fn|-fn
\ *G If n is negative, negate fn.
  0< if  fnegate  then
;

: FABS	\ F: fn -- |fn|
\ *G Floating point absolute.
  fs@ 0< if  fnegate  then
;

code F+		\ F: r1 r2 -- r3
\ *G Floating point addition.
  addsd		xmm8, 0 [r13]		\ FNOS
  add		r13, # FPCELL
  next,
end-code

code F-		\ F: r1 r2 -- r3
\ *G Floating point subtraction; r3 := r1-r2
  movsd		xmm9, xmm8		\ xmm9 := r2
  movsd		xmm8, 0 [r13]		\ xmm8 := r1
  add		r13, # FPCELL
  subsd		xmm8, xmm9		\ xmm8 := r1-r2
  next,
end-code

code F*		\ F: r1 r2 -- r3
\ *G Floating point multiply.
  mulsd		xmm8, 0 [r13]
  add		r13, # FPCELL
  next,
end-code

code F/		\ F: r1 r2 -- r3
\ *G Floating point divide; r3 := r1/r2
  movsd		xmm9, xmm8		\ xmm9 := r2
  movsd		xmm8, 0 [r13]		\ xmm8 := r1
  add		r13, # FPCELL
  divsd		xmm8, xmm9		\ xmm8 := r1/r2
  next,
end-code

code 1/f	\ F: r1 -- 1/r1
\ *G Floating point divide; r3 := r1/r2
  movsd		xmm9, xmm8		\ xmm9 := r1
  mov		rax, # $3FF0:0000:0000:0000	\ FP 1.0
  movq		xmm8, rax		\ xmm8 := 1.0
  divsd		xmm8, xmm9		\ xmm8 := 1/r1
  next,
end-code

code fsqrt  	\ F: f1 -- f2
\ *G F2=sqrt(f1).
  sqrtsd	xmm8, xmm8
  next,
end-code

: FSEPARATE	\ F: f1 f2 -- f3 f4
\ *G Leave the signed integer quotient f4 and remainder f3 when
\ ** f1 is divided by f2. The remainder has the same sign as the
\ ** dividend.
  fover fover f/
  frot frot fabs
  fswap fsign >r fswap
  2 fpick fabs fint f* f-
  r>
  if  fnegate  then
  fswap
; doNotSin

: FFRAC		\ F: f1 f2 -- f3
\ *G Leave the fractional remainder from the division f1/f2. The
\ ** remainder takes the sign of the dividend.
  fseparate fdrop
;


\ ***********************
\ *S Relational operators
\ ***********************

code F0<	\ F: f1 -- ; -- flag
\ *G Floating point *\fo{0<}.
  sub		rbp, # cell		\ save TOS
  mov		0 [rbp], rbx
  xor		rbx, rbx		\ 0
  movq		xmm9, rbx
  cmpltsd	xmm8, xmm9
  movq		rbx, xmm8
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  next,
end-code

code F0>	\ F: f1 -- ; -- flag
\ *G Floating point *\fo{0>}.
  sub		rbp, # cell		\ save TOS
  mov		0 [rbp], rbx
  xor		rbx, rbx		\ 0
  movq		xmm9, rbx
  cmpltsd	xmm9, xmm8
  movq		rbx, xmm9
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  next,
end-code

code F0=	\ F: f1 -- ; -- flag
\ *G Floating point *\fo{0=}.
  sub		rbp, # cell		\ save TOS
  mov		0 [rbp], rbx
  xor		rbx, rbx		\ 0
  movq		xmm9, rbx
  cmpeqsd	xmm8, xmm9
  movq		rbx, xmm8
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  next,
end-code

code F0<>	\ F: f1 -- ; -- flag
\ *G Floating point *\fo{0<>}.
  sub		rbp, # cell		\ save TOS
  mov		0 [rbp], rbx
  xor		rbx, rbx		\ 0
  movq		xmm9, rbx
  cmpneqsd	xmm8, xmm9
  movq		rbx, xmm8
  movsd		xmm8, 0 [r13]		\ restore FTOS
  add		r13, # FPCELL
  next,
end-code

: F=		\ F: f1 f2 -- ; -- flag
\ *G Floating point *\fo{=}.
  f- f0=
;

: F<  		\ F: r1 r2 -- ; -- flag
\ *G Floating point *\fo{<}.
  f- f0<
;

: F>  		\ F: f1 f2 -- ; -- flag
\ *G Floating point *\fo{>}.
  f- f0>
;

: FMAX  	\ F: r1 r2 -- r1|r2
\ *G Floating point *\fo{MAX}.
  fover fover f<
  if  fswap  then
  fdrop
;

: FMIN  	\ F: r1 r2 -- r1|r2
\ *G Floating point *\fo{MIN}.
  fover fover f>
  if  fswap  endif
  fdrop
;

: f~            \ F: f1 f2 f3 -- ; -- flag
\ *G Approximation function. If f3 is positive, flag is true if abs[f1-f2]
\ ** less than f3. IF f3 is zero, flag is true if f1 and f2 encodings are
\ ** the same. If f3 is negative, flag is true if abs[f1-f2] less than
\ ** abs[f3*[abs[f1]+abs[f2]]].
  fdup f0> if                           \ if f3 > 0
    frot frot  f- fabs 			\ -- f3 |f1-f2|
    f>  exit                            \ -- t/f ; SFP007
  then
  fdup f0= if \ if f3 = 0
    fdrop fsign >r			\ DAW035
    fswap fsign >r			\ DAW035
    f= r> r> xor 0= and  exit
  then
  frot frot  fover fover 		\ -- f3 f1 f2 f1 f2
  f- fabs 				\ -- f3 f1 f2 |f1-f2|
  frot frot 				\ -- f3 |f1-f2| f1 f2
  fabs fswap fabs 			\ -- f3 |f1-f2| |f1| |f2|
  f+ frot fabs f* 			\ -- |f1-f2| |f3|*(|f1|+|f2|)
  f< 					\ -- t/f
;
((
: f~            \ F: f1 f2 f3 -- ; -- flag
 \ *G Approximation function. If f3 is positive, flag is true if abs[f1-f2]
 \ ** to f1. If f3 is negative, flag is true if abs[f1-f2] less than
 \ ** abs[f3*abs[f1+f2]].
  fdup f0> if                           \ if f3 > 0
    frot frot  f- fabs			\ -- f3 |f1-f2|
    f>  exit                            \ -- t/f ; SFP007
  then
  fdup f0= if				\ if f3 = 0
    fdrop  fdup fsign >r
    fover fsign >r
    f= r> r> xor 0= and  exit
  then
  frot frot  fover fover		\ -- f3 f1 f2 f1 f2
  f- fabs				\ -- f3 f1 f2 |f1-f2|
  frot frot				\ -- f3 |f1-f2| f1 f2
  fabs fswap fabs			\ -- f3 |f1-f2| |f1| |f2|
  f+ frot fabs f*			\ -- |f1-f2| |f3|*(|f1|+|f2|)
  f<					\ -- t/f
;
))


\ ****************
\ *S Miscellaneous
\ ****************

: FALIGNED	\ addr -- f-addr
\ *G Aligns the address to accept an 8-byte float.
  FPCELL 1- +  FPCELL negate and  ;

: FALIGN  	\ --
\ *G Aligns the dictionary to accept an 8-byte float.
  here dup faligned swap - allot  ;

synonym DFALIGNED FALIGNED	\ addr -- f-addr
\ *G Aligns the address to accept an 8-byte float.

synonym DFALIGN FALIGN  	\ --
\ *G Aligns the dictionary to accept an 8-byte float.

Synonym SFALIGNED ALIGNED	\ addr -- f-addr
\ *G Aligns the address to accept a 4-byte float.

Synonym SFALIGN ALIGN	  	\ --
\ *G Aligns the dictionary to accept a 4-byte float.

: FLOAT+  	\ f-addr1 -- f-addr2
\ *G Increments addr by 8, the size of a float.
  FPCELL +  ;
: FLOATS  	\ n1 -- n2
\ *G Returns *\i{n2}, the size of *\i{n1} floats.
  FPCELL *  ;
Synonym DFLOAT+ FLOAT+ 	\ f-addr1 -- f-addr2
\ *G Increments *\i{addr} by 8, the size of a D-float.
Synonym DFLOATS FLOATS 	\ n1 -- n2
\ *G Returns *\i{n2}, the size of *\i{n1} D-floats.
Synonym SFLOAT+ 4+ 	\ f-addr1 -- f-addr2
\ *G Increments *\i{addr} by 4, the size of an S-float.
Synonym SFLOATS 4* 	\ n1 -- n2
\ *G Returns *\i{n2}, the size of *\i{n1} S-floats.


\ ***************************
\ *S Powers of ten operations
\ ***************************
\ *P Floating point IEEE numbers have the following approximate
\ ** ranges:
\ *(
\ *B Single precision - 10^-44 to 10^+38
\ *B Double precision - 10^-323 to 10^308
\ *)
\ *P As a result, the input code is different for 32 bit and 64
\ ** bit floats.

$0000:0000:0000:0000 >fs fconstant F%0
\ *G Floating point 0.0.
$0AC8:0628:64AC:6F43 >fs fconstant F%10^-256
\ *G Floating point 1.0e-256.
$3949:F623:D5A8:A733 >fs fconstant F%10^-32
\ *G Floating point 1.0e-32.
$3C9C:D2B2:97D8:89BC >fs fconstant F%10^-16
\ *G Floating point 1.0e-16.
$3FB9:9999:9999:999A >fs fconstant F%.1
\ *G Floating point 0.1.
$3FF0:0000:0000:0000 >fs fconstant F%1
\ *G Floating point 1.0.
$4000:0000:0000:0000 >fs fconstant F%2
\ *G Floating point 1.0.
$4024:0000:0000:0000 >fs fconstant F%10
\ *G Floating point 10.0.
$4341:C379:37E0:8000 >fs fconstant F%10^16
\ *G Floating point 1.0e16.
$4693:B8B5:B505:6E17 >fs fconstant F%10^32
\ *G Floating point 1.0e32.
$7515:4FDD:7F73:BF3C >fs fconstant F%10^256
\ *G Floating point 1.0e256.

( F# 1.0E15 ) $430C:6BF5:2634:0000 >fs ( F# 1.0E14 ) $42D6:BCC4:1E90:0000 >fs
( F# 1.0E13 ) $42A2:309C:E540:0000 >fs ( F# 1.0E12 ) $426D:1A94:A200:0000 >fs
( F# 1.0E11 ) $4237:4876:E800:0000 >fs ( F# 1.0E10 ) $4202:A05F:2000:0000 >fs
( F# 1.0E09 ) $41CD:CD65:0000:0000 >fs ( F# 1.0E08 ) $4197:D784:0000:0000 >fs
( F# 1.0E07 ) $4163:12D0:0000:0000 >fs ( F# 1.0E06 ) $412E:8480:0000:0000 >fs
( F# 1.0E05 ) $40F8:6A00:0000:0000 >fs ( F# 1.0E04 ) $40C3:8800:0000:0000 >fs
( F# 1.0E03 ) $408F:4000:0000:0000 >fs ( F# 1.0E02 ) $4059:0000:0000:0000 >fs
( F# 1.0E01 ) $4024:0000:0000:0000 >fs ( F# 1.0e00 ) $3FF0:0000:0000:0000 >fs
 16 FARRAY POWERS-OF-10E1
\ *G An array of 16 powers of ten starting at 10^0
\ ** in steps of 1.

( F# 1.0E256 ) $7515:4FDD:7F73:BF3C >fs ( F# 1.0E240 ) $71C3:3234:DE7A:D7E3 >fs
( F# 1.0E224 ) $6E71:4A52:DFFC:6799 >fs ( F# 1.0E208 ) $6B1F:25C1:86A6:F04C >fs
( F# 1.0E192 ) $67CC:0E1E:F1A7:24EB >fs ( F# 1.0E176 ) $6479:4514:5230:B378 >fs
( F# 1.0E160 ) $6126:C2D4:256F:FCC3 >fs ( F# 1.0E144 ) $5DD4:8057:38B5:1A75 >fs
( F# 1.0E128 ) $5A82:7748:F930:1D32 >fs ( F# 1.0E112 ) $5730:A1F5:B813:2466 >fs
( F# 1.0E96  ) $53DD:F675:62D8:B363 >fs ( F# 1.0E80  ) $508A:FCEF:51F0:FB5F >fs
( F# 1.0E64  ) $4D38:4F03:E93F:F9F5 >fs ( F# 1.0E48  ) $49E5:E531:A0A1:C873 >fs
( F# 1.0E32  ) $4693:B8B5:B505:6E17 >fs ( F# 1.0E16  ) $4341:C379:37E0:8000 >fs
( F# 1.0E00  ) $3FF0:0000:0000:0000 >fs
17 FARRAY POWERS-OF-10E16
\ *G An array of 17 powers of ten starting at 10^0
\ ** in steps of 16.

( F# 1.0E-15 ) $3CD2:03AF:9EE7:5616 >fs ( F# 1.0E-14 ) $3D06:849B:86A1:2B9B >fs
( F# 1.0E-13 ) $3D3C:25C2:6849:7682 >fs ( F# 1.0E-12 ) $3D71:9799:812D:EA11 >fs
( F# 1.0E-11 ) $3DA5:FD7F:E179:6495 >fs ( F# 1.0E-10 ) $3DDB:7CDF:D9D7:BDBB >fs
( F# 1.0E-09 ) $3E11:2E0B:E826:D695 >fs ( F# 1.0E-08 ) $3E45:798E:E230:8C3A >fs
( F# 1.0E-07 ) $3E7A:D7F2:9ABC:AF48 >fs ( F# 1.0E-06 ) $3EB0:C6F7:A0B5:ED8D >fs
( F# 1.0E-05 ) $3EE4:F8B5:88E3:68F1 >fs ( F# 1.0E-04 ) $3F1A:36E2:EB1C:432D >fs
( F# 1.0E-03 ) $3F50:624D:D2F1:A9FC >fs ( F# 1.0E-02 ) $3F84:7AE1:47AE:147B >fs
( F# 1.0E-01 ) $3FB9:9999:9999:999A >fs ( F# 1.0e-00 ) $3FF0:0000:0000:0000 >fs
16 FARRAY POWERS-OF-10E-1
\ *G An array of 16 powers of ten starting at 10^0
\ ** in steps of -1.

( F# 1.0E-256 ) $0AC8:0628:64AC:6F43 >fs ( F# 1.0E-240 ) $0E1A:AC0B:F9B9:E65C >fs
( F# 1.0E-224 ) $116D:9CA7:9D89:462A >fs ( F# 1.0E-208 ) $14C0:701B:D527:B497 >fs
( F# 1.0E-192 ) $1812:3FF0:6EEA:8479 >fs ( F# 1.0E-176 ) $1B64:42E4:FB67:1960 >fs
( F# 1.0E-160 ) $1EB6:7E9C:127B:6E74 >fs ( F# 1.0E-144 ) $2208:F957:4DCF:8A70 >fs
( F# 1.0E-128 ) $255B:BA08:CF8C:979D >fs ( F# 1.0E-112 ) $28AE:C866:B79E:0CBB >fs
( F# 1.0E-96  ) $2C01:1680:5EFF:AEAA >fs ( F# 1.0E-80  ) $2F52:F8AC:174D:6123 >fs
( F# 1.0E-64  ) $32A5:0FFD:44F4:A73D >fs ( F# 1.0E-48  ) $35F7:624F:8A76:2FD8 >fs
( F# 1.0E-32  ) $3949:F623:D5A8:A733 >fs ( F# 1.0E-16  ) $3C9C:D2B2:97D8:89BC >fs
( F# 1.0E00   ) $3FF0:0000:0000:0000 >fs
17 FARRAY POWERS-OF-10E-16
\ *G An array of 17 powers of ten starting at 10^0
\ ** in steps of -16.

: RAISE_POWER	\ exp(10) -- ; F: f -- f'
\ *G Raise the power in preparation for number formatting.
  DUP >R  #15 AND ?DUP
  IF  POWERS-OF-10E1 F*  THEN
  R@ 4 rshift  #15 AND ?DUP
  IF  #16 MIN POWERS-OF-10E16 F*  THEN
  R> 8 rshift  0
  ?DO  f%10^256 F*  LOOP
; doNotSin

: SINK_FRACTION	\ exp(10) -- ; F: f -- f'
\ *G Reduce the power in preparation for number formatting.
  ABS DUP >R  #15 AND ?DUP
  IF  POWERS-OF-10E-1 F*  THEN
  R@ 4 rshift #15 AND ?DUP
  IF  POWERS-OF-10E-16 F*  THEN
  R> 8 rshift  0
  ?DO  f%10^-256 F*  LOOP
; doNotSin

: *10^X		\  exp(10) -- ; F: f -- f'
\ *G Generate float' = float *10^dec_exp.
  DUP 0< IF
    SINK_FRACTION
  ELSE
    RAISE_POWER
  THEN
; doNotSin

: f2/		\ F: f1 -- f2
\ *G Divide by 2.0; *\i{f2=f1/2.0}.
  f%2 f/  ;


\ ***********************
\ *S Floating point input
\ ***********************
\ *P Note that number conversion takes place in *\fo{PAD}.

: CONVERT-EXP	\ c-addr --
\ *G If the character at c-addr is 'D' convert it to 'E'.
  count [char] D scan if
    [char] E swap c!
  else
    drop
  then
;

: CONVERT-FPCHAR	\ c-addr --
\ *G Convert the f.p. char '.' to the double char ',' for
\ ** conversion.
  count fp-char c@ scan if
    [char] , swap c!
  else
    drop
  then
;

: ALL-BLANKS?	\ c-addr len -- flag
\ *G Return true if string is all blanks (spaces).
\ ** A null string (len=0) returns false.
  dup 0= if  2drop 0  exit  then
  true -rot
  bounds ?Do
    i c@ bl <>
    if  drop false leave  then
  Loop
;

[-short-branches
: FCHECK	\ -- am lm ae le e-flag .-flag
\ *G Check the input string at *\fo{PAD}, returning the separated
\ ** mantissa and exponent flags. The e-flag is returned true
\ ** if the string contained an exponent indicator 'E' and
\ ** the .-flag is returned true if a '.' was found.
\ returns NOT FP, if number has base prefix
\ e.g. '$','#','0x' etc.  (Base suffix NOT supported)
  base @ >r                             \ preserve number base
  pad count 2dup OverrideBase d= if     \ not integer - NOTE: WORKS ONLY FOR PREFIXES
\  pad count 2dup check-prefix d= if     \ not integer - NOTE: WORKS ONLY FOR PREFIXES
    pad count bounds ?do
      i c@ [char] e = if  [char] E i c!  then
    loop
    pad count 2dup [char] E scan dup >r   \ ap lp ae le --
    tuck >r >r - r> r>                    \ am lm ae le --
    dup if  1 /string  then               \ am lm ae' le' --
    pad count fp-char c@ scan nip
    r> swap
  else                                  \ integer prefix detected
    0 0 0 0                             \ dummies
    false false                         \ do DP or exponent
  endif
  r> base !                             \ restore number base
; doNotSin
short-branches]

: doMNUM	\ c-addr u -- d 2 | 0
\ *G Convert the mantissa string to a double number and 2. If
\ ** conversion fails, just return 0.
  (integer?) dup 1 =
  if  drop s>d 2  then
; doNotSin

: doENUM	\ c-addr u -- n 1 | 0 ; str as above
\ *G Convert the exponent string to a single number and 1. If
\ ** conversion fails, just return 0.
  dup if
    (integer?)  dup 2 =
    if  2drop drop 0  then
  else
    2drop  0 1
  then
; doNotSin

: FIXEXP     \ dmant exp(10) -- ; F: -- f
\ *G Convert a double integer mantissa and a single integer
\ ** exponent into a floating point number.
  >R d>f R> *10^X			\ d>f ?
; doNotSin

[-short-branches
: isFnumber?	\ caddr len -- 0 | n 1 | d 2 | -1 ; F: -- [f]
\ *G Behaves like the integer version of *\fo{isNumber?} except that if
\ ** the number is in F.P. format and *\fo{BASE} is decimal, a floating
\ ** point conversion is attempted. If conversion is successful,
\ ** the floating point number is left on the float stack and
\ ** the result code is 2. This word only accepts text with an
\ ** 'E' as a floating point indicator, e.g, 1.2345e0.
\ ** If *\fo{BASE is not decimal all numbers are treated as integers.
\ ** The integer prefixes '#','$','0x' etc. are recognised and
\ ** cause integer conversion to be used.
  pad uplace				\ copy string to pad
  fcheck drop                           \ valid f.p. number?
  BASE @ #10 = and if                   \ and base is DECIMAL
    pad convert-fpchar                  \   convert f.p. char ready for conversion to a double
    doEnum if                           \   convert exponent ok?
      -rot  over c@ [char] , <> ?undef  \     must be at least one digit for d.p.
      doMnum if                         \     convert mantissa ok?
        rot dpl @  dup 0>
	if  -  else  drop  then
        fixexp  -1
      else
        drop 0
      then
    else
      2drop 0
    then
    dup #l !  -1 dpl !
  else
    2drop 2drop  pad integer?           \ try to convert as integer
  then
; doNotSin

: FNUMBER?	\ addr -- 0 | n 1 | d 2 | -1 ; F: -- [f]
\ *G Behaves like the integer version of *\fo{Number?} except that if
\ ** the number is in F.P. format and *\fo{BASE} is decimal, a floating
\ ** point conversion is attempted. See *\fo{isFnumber?} above
\ ** for more details.
  count isFnumber?
;

((
defer is-old-number?
  action-of isNumber? to-do is-old-number?

: isFnumber?  	\ caddr len -- 0 | n 1 | d 2 | -2 ; F: -- r
\ *G Behaves like the integer version of *\fo{isNumber?} except
\ ** that if  integer conversion fails, and *\fo{BASE} is decimal,
\ ** a floating point conversion is attempted. If conversion is
\ ** successful, the floating point number is left on the float
\ ** stack and the result code is -2.
  2dup 2>r  is-old-number? dup 0= if	\ test for integer first
    base @ #10 = if			\ not integer, floats must be in DECIMAL
      drop  2r@ (>float)		\ >FLOAT returns well formed flag
\      -2 and				\ good float returns -2,
    then
  then
  2r> 2drop
; doNotSin

: Fnumber?  	\ caddr -- 0 | n 1 | d 2 | -2 ; F: -- r
\ *G As *\fo{isFnumber?} above, but takes a counted string.
  count isFnumber?
;

))

: >FLOAT	\ c-addr u -- true|false ; F: -- [f]
\ *G Try to convert the string at c-addr/u to a floating point number.
\ ** If conversion is successful, flag is returned true, and a floating
\ ** number is returned on the float stack, otherwise just flag=0 is
\ ** returned. This word accepts several forms, e.g. 1.2345e0, 1.2345,
\ ** 12345 and converts them to a float. Note that double numbers
\ ** (containing a ',') cannot be converted.
\ ** Number conversion is decimal only, regardless of the current *\fo{BASE}.
  base @ >r decimal			\ SFP007
  pad uplace			        \ copy string to pad
  pad convert-exp                       \ make sure exponent char is an 'E'
  fcheck or if                          \ valid f.p. number?
    pad convert-fpchar                  \   convert f.p. char ready for conversion to a double
    doEnum if                           \   convert exponent ok?
      -rot doMnum if                    \     convert mantissa ok?
        rot dpl @  dup 0>
	if  -  else  drop  then
        fixexp  true
      else
        drop 0
      then
    else
      2drop 0
    then
    dup #l !  -1 dpl !
  else
    2drop				\ discard exponent string
    case  2dup 2>r (integer?)		\   try to convert integer to f.p.
      0 of  2r@ all-blanks? dup		\    all blanks is a special case returning f.p. 0
            if  0 >fs  then
	endof
      1 of  s>f true  endof		\    convert single integer to float
      2 of  d>f true  endof		\    convert double
    endcase
    2r> 2drop
  then
  r> base !				\ SFP007
;
short-branches]

defer FLITERAL	\ Comp: F: r -- ; Run: F: -- r
\ *G Compiles a float as a literal into the current definition.
\ ** At execution time, a float is returned. For example,
\ ** *\fo{[ %PI F2* ] FLITERAL} will compile 2PI as a floating point
\ ** literal. Note that *\fo{FLITERAL} is immediate.
  IMMEDIATE

: (F#)  	\ addr -- -1|0 ; F: -- [f]
\ *G The primitive for *\fo{F#} below.
  fnumber?
  case
    -1 of  -1  endof			\ float
     0 of  0   endof
     1 of  s>f -1  endof		\ single integer
     2 of  dpl @ -1 <>                  \ see if double
           if  d>f  then                \ convert to fp
           -1
       endof
  endcase
; doNotSin

: F#  		\ F: -- [f] ; or compiles it (state smart)
\ *G If interpreting, takes text from the input stream and,
\ ** if possible converts it to a f.p. number on the stack.
\ ** Numbers in integer format will be converted to floating-point.
\ ** If compiling, the converted number is compiled.
  bl word (f#) ?undef  state @
  if  postpone fliteral  then
;
IMMEDIATE


\ ************************
\ *S Floating point output
\ ************************

8 value precision	\ -- u
\ *G Number of significant digits output.

: set-precision		\ u --
\ *G Set the number of significant digits used for output.
  1 max  #18 min  to precision  ;

: exp(10)  	\ F: f -- f ; -- exp[10]
\ *G Generate the power of ten corresponding to the float's power of two.
  fdup f0=
  if  precision exit  then
  exp@ dup 0< >r			\ F: -- f ; -- exp(2) ; R: -- sign
  abs 8651 28738 */mod nip		\ l10(2)= 0.30102999512840141972301482357854 ; SBD010
  r> if  negate 1-  then
; doNotSin

64 +user fopbuff	\ -- addr
\ *G Buffer in which output string is built.
32 +user frepbuff	\ -- addr
\ *G Buffer for use as the output of *\fo{REPRESENT}.

f# 0.5e0 fconstant %ROUNDPOINT

: roundfp	\ F: +f -- +f'
\ *G Add 0.5e(exp-precision-1).
  exp(10) precision - 1-		\ will add 0.5e(exp-precision-1)
  %roundpoint *10^x f+
;

: REPRESENT	\ F: r -- ; c-addr u -- n flag1 flag2
\ *G Assume that the floating number is of the form +/-0.xxxxEyy.
\ ** Place the significand xxxxx at c-addr with a maximum of u digits.
\ ** Return n the signed integer version of yy. Return flag1 true
\ ** if f is negative, and return flag2 true if the results are
\ ** valid. In this implementation all errors are handled by
\ ** exceptions, and so flag2 is always true.
  2dup [char] 0 fill			\ initialise buffer
  fdup f0=
  if  fdrop 2drop 1 0 -1 exit  then
  fsign >r				\ -- caddr u ; R: -- sgn ; F: -- |f|
  roundfp				\ round by adding 0.5e(exp-precision-1)
  dup exp(10) dup >r - 1-		\ -- caddr u u-exp-1 ; R: -- sgn exp ; F: -- f
  *10^x					\ -- caddr u ; R: -- sgn exp ; F: -- f' ; scale to integer
  f>d <# #S #>				\ -- caddr u caddr2 len2 ; R: -- sgn exp
  rot 2dup - >r				\ -- caddr caddr2 len2 u ; R: -- sgn exp len2-u
  min rot swap move			\ -- ; R: -- sgn exp len2-u
  r> r> + 1+  r>  -1
;

: (.sign)	\ flag $out --
\ *G Add '-' or nothing to the output string.
  swap if  s" -" 2 pick append  then  drop
;

: (.mant)	\ binp $out n --
\ *G Add the mantissa string at *\i{binp), produced by *\fo{REPRESENT},
\ ** to a counted string at *\i{$out) with *\i{n} digits before the
\ ** decimal point.
  {: binp $out n | u -- :}
  precision n min to u
  binp u $out append  s" ." $out append	\ add chars before '.' and '.'
  binp u + precision u - $out append	\ add chars after '.'
;

: (.exp)	\ exp(10) $out --
\ *G Add the exponent to the output string.
  >r
  s" e" r@ append
  dup 0< if  s" -"  else  s" +"  then r@ append
  abs s>d <# # #s #> r> append		\ at least two digits
;

: (.initfop)	\ f -- ; -- exp(10)
\ *G initialise output conversion.
  0 fopbuff c!				\ zero string count
  frepbuff precision represent drop	\ -- n flag1 ; convert
  fopbuff (.sign)			\ -- n ; sign
;

: (fs.)		\ F: f -- ; -- caddr len
\ *G Produce a string containing the number in scientific notation.
  (.initfop)				\ initialise output conversion
  frepbuff fopbuff 1 (.mant)		\ convert mantissa
  1- fopbuff (.exp)			\ convert exponent
  fopbuff count
;

: (fe.)		\ F: f -- ; -- caddr len
\ *G Produce a string containing the number in engineering notation.
  (.initfop)				\ initialise output conversion
  1- s>d 3 fm/mod 3 * swap 1+ >r	\ -- exp' ; R: -- #leading
  frepbuff fopbuff r> (.mant)		\ convert mantissa
  fopbuff (.exp)
  fopbuff count
;

: ff?		\ f: f -- f ; -- flag
\ *G Return true if the number can be represented in free format.
  fdup f0=  exp(10) 0 precision within  or
;

: (ff.)		\ F: f -- ; -- caddr len
\ *G Produce a string containing the number in free notation.
\ ** If the number cannot be displayed in free notation,
\ ** scientific notation is uesed.
  ff? 0=
  if  (fs.) exit  then
  (.initfop)				\ initialise output conversion
  >r frepbuff fopbuff r> (.mant)	\ convert mantissa
  fopbuff count
;

: fs.		\ F: f --
\ *G Display *\i{f} in scientific notation:
\ *C   x.xxxxxE[-]yy
  (fs.) type space
; doNotSin

: fe.		\ F: f --
\ *G Display *\i{f} in engineering notation:
\ *C   x.xxxxxE[-]yy
\ *P where the mantissa is 1 <= mantissa < 1000 and the exponent
\ ** is a multiple of three.
  (fe.) type space
; doNotSin

: ff.		\ F: f --
\ *G Display *\i{f} in free notation:
\ *C   xxx.xxxxx
  (ff.) type space
; doNotSin

: F.  		\ F: f --
\ *G Print the f.p. number in free format, xxxx.yyyy, if
\ ** possible. Otherwise display using the x.xxxxEyy format.
  ff?
  if  ff.  else  fs.  then
; doNotSin


\ ***********
\ *S Rounding
\ ***********
\ *P Rounding modes are specified in the range 0..3 and are
\ ** converted when used.
\ *(
\ *B 0 - round to nearest (even),
\ *B 1 - round down towards -infinity (floored).
\ *B 2 - round up towards +inifinity (ceiling)
\ *B 3 - round towards zero (truncate)
\ *)

code rmode>	\ -- oldmode
\ *G Get the current rounding mode.
  sub		rbp, # 2 cells			\ save TOS
  mov		cell [rbp], rbx			\ old TOS to deep stack
  stmxcsr	0 [rbp]				\ get current mode
  mov		ebx, 0 [rbp] 			\ TOS := mode
  add		rbp, # cell
  shr		ebx, # #13			\ convert bits 14:13 to 1:0
  and 		ebx, # 3
  next,
end-code

code >rmode	\ newmode --
\ *G Set the current rounding mode.
  shl		ebx, # #13			\ mode is 0..3, in bits 14:13
  sub		rbp, # cell			\ working space
  stmxcsr 	0 [rbp]				\ used for new value
  mov		eax, 0 [rbp]			\ read
  and		eax, # $FFFF:9FFF		\ remove mask bits
  or		eax, ebx			\ set new mask bits
  mov		0 [rbp], eax			\ put back
  ldmxcsr 	0 [rbp]				\ write
  mov		rbx, cell [rbp]			\ restore TOS
  add		rbp, # 2 cells
  next,
end-code

code >rmode>	\ newmode -- oldmode
\ *G Set the current rounding mode and get the previous one.
  shl		ebx, # #13			\ mode is 0..3, in bits 14:13
  sub		rbp, # cell			\ working space
  stmxcsr 	0 [rbp]				\ used for new value
  stmxcsr	4 [rbp]				\ old value
  mov		eax, 0 [rbp]			\ read
  and		eax, # $FFFF:9FFF		\ remove mask bits
  or		eax, ebx			\ set new mask bits
  mov		0 [rbp], eax			\ put back
  ldmxcsr 	0 [rbp]				\ write
  mov		ebx, 4 [rbp]			\ old value
  shr		ebx, # #13
  and		ebx, # 3
  add		rbp, # cell			\ remove working space
  next,
end-code

code (fround)	\ F: f1 -- f1'
\ *G Round the number to an integer value according to the
\ ** current rounding mode.
  cvtsd2si	rax, xmm8			\ float to int
  cvtsi2sd	xmm8, rax			\ int to float
  next,
end-code

: fround	\ F: f1 -- f1'
\ *G Round to nearest.
  0 >rmode>  (fround)  >rmode
;

: floor		\ F: f1 -- f1'
\ *G Round to -infinity.
  1 >rmode>  (fround)  >rmode
;

: ceil		\ F: f1 -- f1'
\ *G Round towards +infinity.
  2 >rmode>  (fround)  >rmode
;

: roundup       \ F: f1 -- f1'
\ *G Round towards +infinity.
  2 >rmode>  (fround)  >rmode
;

: ftrunc	\ F: f1 -- f1'
\ *G Round the number towards zero.
\ ** on the FP stack.
  3 >rmode>  (fround)  >rmode
;

: rounded	\ -- ; set SSE to round to nearest
\ *G Set SSE to round to nearest for all operations other than
\ ** *\fo{FINT}, *\fo{FLOOR} and *\fo{CEIL}.
  0 >rmode  ;
rounded

: floored	\ -- ; set SSE to floor
\ *G Set SSE to round to floor for all operations other than
\ ** *\fo{FROUND}, *\fo{FINT}, *\fo{FTRUNC} and *\fo{ROUNDUP}.
  1 >rmode  ;

: roundedup	\ -- ; set NDP to round up
\ *G Set NDP to round up for all operations other than
\ ** *\fo{FROUND}, *\fo{FINT} and *\fo{FLOOR}.
  2 >rmode  ;

: truncated	\ -- ; set NDP to chop to 0
\ *G Set NDP to chop to 0 for all operations other than
\ ** *\fo{FROUND}, *\fo{FLOOR} and *\fo{ROUNDUP}.
  3 >rmode  ;



\ *************************
\ *S Trigonmetric functions
\ *************************
\ *P N.B. All angles are in radians.

\ XC mant/exp     IEEE64
(  1518500249 0 ) $3FE6:A09E:6640:0000 >fs fconstant F%SQR.5	\ 0.70710678
(  1686629716 2 ) $4009:21FB:5500:0000 >fs fconstant F%PI	\ 3.14159266
(  1686629715 1 ) $3FF9:21FB:54C0:0000 >fs fconstant F%PI/2	\ 1.57079633
(  1686629716 0 ) $3FE9:21FB:5500:0000 >fs fconstant F%PI/4	\ 0.78539816
( 1367130550 -1 ) $3FD4:5F30:6D80:0000 >fs fconstant F%1/PI	\ 0.31830988
(  1367130550 0 ) $3FE4:5F30:6D80:0000 >fs fconstant F%2/PI	\ 0.63661977
( 1686626304 18 ) $4109:21F8:0000:0000 >fs fconstant F%SINMAX	\ 205887
(  1677721600 8 ) $4069:0000:0000:0000 >fs fconstant F%ALNMAX	\ 200
(  1549082004 1 ) $3FF7:1547:6500:0000 >fs fconstant F%1/LN2	\ 1.44269504
(  1488522236 0 ) $3FE6:2E42:FF00:0000 >fs fconstant F%LN2	\ 0.6931472
(  1660944384 7 ) $4058:C000:0000:0000 >fs fconstant F%ALGMAX	\ 99
(  1783446570 2 ) $400A:934F:0A80:0000 >fs fconstant F%1/LG2	\ 3.3219281
( 1292913983 -1 ) $3FD3:4413:4FC0:0000 >fs fconstant F%LG2	\ 0.30102999
                                    90 s>f fconstant F%90
                                   180 s>f fconstant F%180
                                   360 s>f fconstant F$360

: DEG>RAD	\ F: n1 -- n2
\ *G Convert degrees to radians.
  f%180 f/ f%pi f*
;

: RAD>DEG	\ F: n1 -- n2
\ *G convert radians to degrees.
  f%180 f* f%pi f/
;

: ?TR-ERR	\ x --
  abort" Overflow in trig. function"
;

f# -1.6666666E-1
f# 8.3333315E-3
f# -1.98409E-4
f# 2.7526E-6
f# -2.39E-8
5 farray SINARRAY

: (SIN)  	\ F: f1 -- f2
  fdup fdup f* fdup 0 sinarray f*
  4 1
  do  i sinarray f+ fover f*  loop
  4 sinarray f+ f* f%1 f+ f*
; doNotSin

: SINFN		\ F: f1 -- f2
  fdup f%1/pi f*			\ ang ang/pi --
  fint fdup f>s >r			\ ang int[ang/pi] --
  f%pi f* f-                            \ ang-pi*int[ang/pi] --
  fdup f%pi/2 f>
  if  f%pi fswap f-  then
  (sin)
  r> 1 and
  if  fnegate  then
;

: (FSIN)	\ F: f1 -- f2
  fsign  >r
  fdup f%sinmax f> ?tr-err
  sinfn
  r> ?fnegate
;

: FSIN		\ F: f1 -- f2
\ *G f2=sin(f1).
  (FSIN)
;

: FCOS		\ F: f1 -- f2
\ *G f2=cos(f1).
  f%pi/2 f+  (fsin)
;

f# 3.3333140E-1
f# 1.3339240E-1
f# 5.3374060E-2
f# 2.4565089E-2
f# 2.9005250E-3
f# 9.5168091E-3
6 farray TANARRAY

: (TAN)		\ F: f1 -- f2
  fdup fdup f* fdup 0 tanarray f*
  5 1
  do  i tanarray f+ fover f*  loop
  5 tanarray f+ f* f%1 f+ f*
; doNotSin

[-short-branches
: FTAN		\ F: f1 -- f2
\ *G f2=tan(f1).
  fsign >r
  fdup f%sinmax f> ?tr-err
  fdup f%2/pi f* fint fswap fover f%pi/2 f* f-
  fdup f%pi/4 f> if
    f%pi/2 f- (tan) fswap f%1 f+
  else
    (tan) fswap
  then
  f>s 1 and
  if  1/f fnegate  then
  r>
  if  fnegate  then
; doNotSin
short-branches]

f# 1.5707963E0
f# -2.1459880E-1
f# 8.8978987E-2
f# -5.0174305E-2
f# 3.0891881E-2
f# -1.7088126E-2
f# 6.6700901E-3
f# -1.2624911E-3
8 farray ASINARRAY

: ARCFN  	\ F: f1 -- f2
  fdup 0 asinarray f*
  7 1
  do  i asinarray f+ fover f*  loop
  7 asinarray f+
  fswap f%1 fswap f- fsqrt f*
;

f# -3.3333145E-1
f# 1.9993551E-1
f# -1.4208899E-1
f# 1.0656264E-1
f# -7.5289640E-2
f# 4.2909614E-2
f# -1.6165737E-2
f# 2.8662257E-3
8 farray ATANARRAY

: ATFN		\ F: f1 -- f2
  fdup fdup f* fdup 0 atanarray f*
  7 1
  do  i atanarray f+ fover f*  loop
  7 atanarray f+ f* f%1 f+ f*
; doNotSin

: FASIN  	\ F: f1 -- f2
\ *G f2=arcsin(f1).
  fsign >r
  arcfn f%pi/2 fswap f-
  r> if  fnegate  then
;

: FACOS  	\ F: f1 -- f2
\ *G f2=arccos(f1).
  fsign >r
  arcfn
  r> if  f%pi fswap f-  then
;

[-short-branches
: FATAN  	\ F: f1 -- f2
\ *G f2=arctan(f1).
  fsign >r
  fdup f%1 f> if
    1/f  atfn f%pi/2 fswap f-
  else
    atfn
  then
  r> if  fnegate  then
;
short-branches]


\ ************************
\ *S Logarithms and Powers
\ ************************

f# 9.9999642E-1
f# -4.9987412E-1
f# 3.3179903E-1
f# -2.4073381E-1
f# 1.6765407E-1
f# -9.5329390E-2
f# 3.6088494E-2
f# -6.4535442E-3
8 farray LOGARRAY

(  1488522234 0 ) $3FE6:2E42:FE80:0000 >fs fconstant F%D8	\ 0.6931472
( 1865280597 -1 ) $3FDB:CB7B:1540:0000 >fs fconstant F%D9	\ 0.43429448

: FLN  		\ F: f1 -- f2
\ *G Take the logarithm of f1 to base e and return the result.
  fdup f0> 0=
  abort" Invalid argument to FLN/FLOG"
  exp@ 1- >r  1 exp!
  f%1 f-
  fdup 0 logarray f*
  7 1 do
    i logarray f+ fover f*
  loop
  7 logarray f+ f*  r> s>f  f%d8 f* f+
; doNotSin

: FLOG  	\ F: f1 -- f2
\ *G Take the logarithm of f1 to base 10 and return the result.
  fln f%d9 f*
; doNotSin

f# 1.1512928E0
f# 6.6273088E-1
f# 2.5439357E-1
f# 7.2951737E-2
f# 1.7421120E-2
f# 2.5549180E-3
f# 9.3264267E-4
7 farray 10ARRAY

: (10^X)  	\ F: f1 -- f2
  fdup 0 10array f*
  6 1 do
    i 10array f+ fover f*
  loop
  6 10array f+ f*
  f%1 f+  fdup f*
;

f# -1.0000000E0
f# 4.9999992E-1
f# -1.6666530E-1
f# 4.1657348E-2
f# -8.3013598E-3
f# 1.3298820E-3
f# -1.4131610E-4
7 farray E-ARRAY

: (E^X)  	\ F: f1 -- f2
  fdup 0 e-array f*
  6 1 do
    i e-array f+ fover f*
  loop
  6 e-array f+ f*
  f%1 f+  f%1 fswap  f/
;

: FEXP  	\ F: f1 -- f2
\ *G f2=e^f1.
  fsign >r
  fdup f%alnmax f> abort" Overflow in FEXP"
  fdup f%1/ln2 f*
  fdup f>s >r
  fint f%ln2 f* f-
  (e^x)
  r> exp@ + exp!			\ add to exponent
  r> if  1/f  then
; doNotSin

Synonym FE^X FEXP  	\ F: f1 -- f2
\ *G Compatibility word.

f# 1.0e-5 fconstant f%minexpm1

: fexpm1	\ r1 -- r2
\ *G Raise e to the power *\i{r1} and subtract one, giving *\i{r2}.
  fdup fabs f%minexpm1 f<
  if  fdup fdup f* f%2 f/ f+  else  fexp f%1 f-  then
;

: F10^X  	\ F: f1 -- f2
\ *G f2=10^f1
  fsign >r
  fdup f%algmax f> abort" overflow in f10^x"
  fdup f%1/lg2 f*
  fdup f>s >r
  fint f%lg2 f* f-
  (10^x)
  r> exp@ + exp!			\ add to exponent
  r> if  1/f  then
; doNotSin

[-short-branches
: FX^N  	\ n -- ; F: fx -- fx^n
\ *G fx^n=x^n where x is a float and n is an integer.
  dup 0= if				\ check for n=0, result=1.0
    drop fdrop  f%1  exit
  then
  dup >r abs				\ -- |n| ; R: -- n
  f%1 fswap
  begin					\ F: -- fx fconst ; -- |n| ; R: -- n
    dup 0>
  while
    dup 1 rshift dup 1 lshift rot -	\ F: -- fx fconst ; |n|/2 |n|'-|n|
    if
      fswap fover f* fswap
    then
    fdup f*
  repeat
  drop fdrop
  r> 0<
  if  1/f  then
; doNotSin

: F**  	\ F: fx fy -- fx^fy
\ *G fn=X^Y where X and Y are both floats.
\ ** If fx<=0e0, 0e0 is returned. This behaviour is required
\ ** by the Forth Scientific Library.
\ ** If fy=0e0, 1e0 is returned.
  fover f0> 0= if			\ 0^y = 0
     fdrop fdrop  f%0  exit
  then
  fdup f0= if				\ x^0 = 1
    fdrop fdrop  f%1  exit
  then
  fdup fdup fint f- f>r
  f>s >r fdup r>
  fx^n fswap
  fr> fdup f0<> if
    fover f0< abort" Result of F** is complex"
    fswap fdup f0< >r
    fabs flog f*  f10^x
    r> if  1/f  then
    f*
  else
    fdrop fdrop
  then
; doNotSin
short-branches]

Synonym FX^Y F**	\ --
\ *G Compatibility word for old code.


\ **********************************
\ *S COSEC SEC COTAN and hyberbolics
\ **********************************

: fcosec        \ F: f -- cosec(f)
( *G Floating point cosecant. )
  fsin 1/f  ;

: fsec          \ F: f -- sec(f)
( *G Floating point secant. )
  fcos 1/f  ;

: fcotan        \ f: f -- cot(f)
( *G Floating point cotangent. )
  ftan 1/f  ;

: fsinh         \ F: f -- sinh(f) ; (e^x - 1/e^x)/2
( *G Floating point hyberbolic sine. )
  fexp  fdup 1/f f-  f2/  ;

: fcosh         \ F: f -- cosh(f) ; (e^x + 1/e^x)/2
( *G Floating point hyberbolic cosine. )
  fexp  fdup 1/f f+  f2/  ;

: ftanh         \ F: f -- tanh(f) ; (e^x - 1/e^x)/(e^x + 1/e^x)
( *G Floating point hyberbolic tangent. )
  fexp fdup 1/f  fover fover f-		\ GEK029
  frot frot f+  f/
;

: fasinh        \ F: f -- asinh(f) ; ln(f+sqrt(1+f*f))
( *G Floating point hyberbolic arcsine. )
  fdup
  fdup f*  f%1 f+  fsqrt
  f+  fln  ;

: facosh        \ F: f -- acosh(f) ; ln(f+sqrt(f*f-1))
( *G Floating point hyberbolic arccosine. )
  fdup
  fdup f*  f%1 f-  fsqrt
  f+  fln  ;

: fatanh        \ F: f -- atanh(f) ; ln((1+f)/(1-f))/2
( *G Floating point hyberbolic arctangent. )
  f%1 fover f+  f%1 frot f-  f/
  fln  f2/  ;


\ ******************
\ *S Debugging tools
\ ******************

(( for DocGen
defer f.s	\ F: f --
\ *G Non-destructive display of the floating point stack.
))

: (f.s)		\ F: f --
\ *G Non-destructive display of the floating point stack.
\ ** Default action of *\fo{F.S}.
  fdepth if
    0 fdepth 1-
    do  i fpick f.  -1 +loop
    ." --"
  then
;

$22 value ignSSEmask	\ --
\ *G When the prompt checks for an error, it ignores the bits in
\ ** the MXCSR register that are set in *\fo{ignSSEmask}. By
\ ** default this is just the Precision Flag, which is set
\ ** when floating point is inexact and the Denormal Flag.

: .FSysPrompt	\ --
\ *G Replacement system prompt that adds floating point stack
\ ** depth display. Used in the form:
\ *C   ' .FSysPrompt is .prompt
  .SysPrompt
  fdepth if
    ."  F:-" fdepth .
  endif
  fps@  ignSSEmask invert and		\ ignore masked bits
  dup $03F and if			\ just look at flags
    cr ." SSE Exception " dup .dword
    dup $FFFF:FFC0 and fps!		\ clear all flags
  endif
  drop
;


\ **********************************
\ *S Plugging floats into the system
\ **********************************

also disassembler
[+switch dasm-switch	\ fix up the disassembler
  ' flit run:  base @ decimal
  	       DisAddr dup
               cr ." ( FPlit: " f@ f. ." )"
               FPCELL +  set-DisAddr
               base !  ;
switch]
previous

: (rliteral)	\ F: f -- ; F: -- f
\ *G Compiles a float as a literal into the current definition.
\ ** At execution time, a float is returned. For example,
\ ** *\fo{[ %PI F2* ] FLITERAL} will compile 2PI as a floating point
\ ** literal. The default action of *\fo{FLITERAL}. Note that *\fo{FLITERAL}
\ ** is immediate, whereas *\fo{(RLITERAL)} is not.
  [ also h-c ]
  GenTokens? if				\ SFP027...
    fdup genFlitToken  0 -> GenTokens?
    postpone flit  f,  1 -> GenTokens?
  else
    postpone flit  f,
  endif					\ ...SFP027
  [ previous ]
;
assign (rliteral) to-do fliteral

: #fliteral              \ n -- ; F: real --
  drop  postpone fliteral
;

' noop  ' (rliteral)  ' (rliteral)  RecType: r:SSE64	\ -- struct
\ *G Contains the three recogniser actions for floating point literals.

: rec-SSEfloats	\ caddr u -- r:SSE64 | r:fail ; F: -- [f]
\ *G The parser part of the floating point recogniser.
  >float if  r:SSE64  else  r:fail  then
;

: reals		\ -- ; turn FP system on
\ *G Switch the system  to permit floating point number input.
\ You must use *\fo{reals} befor the first use of the FP pack.
  assign #fliteral to-do c#fliteral
  assign .FSysPrompt to-do .prompt
  SSE64System to FPsystem  8 to /FPL				\ float size
  ['] rec-SSEfloats forth-recognizer +stack-bot
;

: integers	\ -- ; turn FP system off
\ *G Switch the system not to recognise floating point input.
  assign (fliteral) to-do c#fliteral
  assign .SysPrompt to-do .prompt
  ['] rec-SSEfloats forth-recognizer -stack
;

\ ********************
\ *S Installation code
\ ********************

SSE64System -> FpSystem
\ *P The value *\fo{FPSYSTEM} defines which floating point pack is installed
\ ** and active. See the Floating Point chapters for
\ ** further details.
\ ** Each floating point pack defines its own type as follows:
\ *(
\ *B 0 constant NoFPSystem
\ *B 1 constant HFP387System (also 64 bit)
\ *B 2 constant NDP387System (also 64 bit)
\ *B 3 constant OpenGL32System (obsolete)
\ *B 4 constant SSE64System
\ *)
\ *P When *\i{FPSystem} changes, the following files that use *\i{FPSystem}
\ ** are affected:
\ *C   Extern*.fth  kernel64.fth  Tokeniser.fth
\ *C   Lib/x64/Ndpx64.fth  Lib/x64/FPSSE64.fth
\ *P At present, only 0, 1, 2 and 4 are valid values of *\i{FPSystem}
\ ** in 64 systems.

: SSE64setup	\ --
\ *G Set up the Forth system for 64 bit SSE floats. Performed at start
\ ** up.
  SSE64System -> FpSystem  finit
;
' SSE64setup is init-FP-pack  ' init-FP-pack AtCold  init-FP-pack


\ **********
\ *S Gotchas
\ **********
\ *P The ANS and Forth-2012 specifications define the format of
\ ** floating point numbers during text interpretation as:
\ *E Convertible string := <significand><exponent>
\ **
\ ** <significand> := [<sign>]<digits>[.<digits0>]
\ ** <exponent>    := E[<sign>]<digits0>
\ ** <sign>        := { + | - }
\ ** <digits>      := <digit><digits0>
\ ** <digits0>     := <digit>*
\ ** <digit>       := { 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 }

\ *P The format above is handled by the word *\fo{FNUMBER?}. The word
\ ** *\fo{>FLOAT} accepts the more relaxed format below.
\ *E Convertible string := <significand>[<exponent>]
\ **
\ ** <significand> := [<sign>]{<digits>[.<digits0>] | .<digits> }
\ ** <exponent>    := <marker><digits0>
\ ** <marker>      := {<e-form> | <sign-form>}
\ ** <e-form>      := <e-char>[<sign-form>]
\ ** <sign-form>   := { + | - }
\ ** <e-char>      := { D | d | E | e }

\ *P This restriction makes it difficult to use the text interpreter
\ ** during program execution as it requires floating point numbers
\ ** to contain 'D' or 'E' indicators, which is not profane practice.
\ ** A quick kluge to fix this is to change *\fo{isFnumber?} as below.
\ *E Replace:
\ **   fcheck drop if                       \ valid f.p. number?
\ ** with:
\ **   fcheck or if                         \ valid f.p. number?
\ *P Note that this change can/will cause problems if number base
\ ** is not *\fo{DECIMAL}.

finit reals


\ *********
\ Test code
\ *********


\ ======
\ *> ###
\ ======

