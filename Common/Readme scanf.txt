**************************************
*  Scanf for Delphi  and  DeFormat   *
************************************** 
 
These are the programs which I hope will help Delphi users and those
who port programs from C/C++ to Delphi. 

The current version (1.0) is distributed with the complete source code.
I welcome any comments or suggestions, especially what concerns bugs, 
compatibility conflicts, and additional features.  


If you are familiar with scanf and do not intend to use DeFormat,
proceed directly to "Compatibility with C/C++ scanf". 


The package is free for private and academic use.
For commercial use, please contact the author.

***********************************************

 Evgeni Sorokin 
 TU Vienna 
 Gusshausstrasse 27/359-4             
 A-1040 Vienna                         
 Austria
 email: sorokin@ps1.iaee.tuwien.ac.at
 
***********************************************



************
*  Purpose *
************

Scanf package provide tools for formatted input, using the same 
principle as Format function. Its original purpose was to provide 
experienced C/C++ users a scanf-type function for converting C/C++ 
codes to Delphi.

I hope however, that Delphi users can also benefit from a DeFormat 
function, even if they never used scanf. In addition, the package 
also contains support of Comp type as an integer, which is not 
available in Delphi 2 and 3.


****************************
* Structure of the package *
****************************

The package consists of the two units:

  Scanf.pas 
  Scanf_c.pas

of which the first is the user-oriented interface to the second, 
where actual conversion takes place.

In the Scnaf. pas:

  Functions following C/C++ semantics:
 
    sscanf
    fscanf

  The two functions are identical except that sscanf uses PChar as input 
  and in fscanf, input is taken from a TStream. A valid implementation of 
  TStream must support moving the Position pointer backwards. 
  In what follows, sscanf and fscanf will be referred as to "scanf".

  Routines following Delphi Format semantics:

    DeFormat
    StrDeFmt
    DeFormatBuf

  are different callers of one and the same function DeFormat_core. 
  The functions directly follow calling patterns of Format, StrFmt and FormatBuf,
  corrrspondingly. 
  In the rest of this document these functions are referred to as "DeFormat".


  Extensions to the library functions with thousand separators' support:

     TextToFloatS (like TextToFloat)
     StrToCurrS   (like StrToCurr)
     StrToFloatS  (like StrToFloat)
 
  Decimal, hex, and octal representations of an int64 (Comp) type (the first 
  two are probably obsolete for Delphi 4):

     int64ToStr
     int64ToHex
     int64ToOct
 
  A function for scanning the currency in "formatted" form:

     StrToCurrF
 
The package also includes the scTutor program (source code in the Tutor subdir; 
compiled under Delphi 3 in $Q+ mode and with exceptions enabled). This program 
serves both learning  purposes for those uncommon with scanf, and as a handy 
standalone check utility helping to trace the problem if any. Additionally, 
the users are encouraged to have a look at Examples.pas file, which is executed 
in the initiation part of the Learn.pas unit. You might stepwise follow the calls 
and inspect the results to get comfortable with scanf and DeFormat ideology.

  
***************************************
* Extensions to the library functions *
***************************************

Starting with version 1.0, scanf package uses its own scanners.
Compared to the library Val and TextToFloat functions scanf package 
supports following extended features:

 + Floating-point conversion accepts (-)INF  and (-)NAN numbers
   (must be upper-case).
 + Hex conversion accepts minus sign (i.e. -$ffffffff gives 1).
 + Both "$" and "0x" hex prefixes are allowed.
 + Octal integer format supported. 
 + Comp type under D2, D3 is scanned as integer.
 + Thousand separators in floating-point numbers are supported.
 + Currency type is supported.

   

****************
* How it works *
****************

DeFormat, StrDeFmt, and DeFormatBuf are direct counterparts of library 
Format, StrFmt, and DeFormatBuf functions. The syntax of the format 
string is the generally the same, with size specifiers and pattern type
(search-set) added. Note, however, that you should provide *pointers* 
to the variables, rather than variables themselves in the Args 
array. The only exception is the PChar variable, which is supplied by 
itself, since it is already a pointer.

DeFormat and scanf compare format string with input string character by 
character (case sensitive or insensitive, see "Implementation Notes" below) 
until it either finds a discrepance or a format specifier. The input string 
is then converted according to the format specifier, which is almost the 
same as used by Format function with following changes:

 1. Width parameter specify MAXIMUM width of the input to be 
    converted. In Format, it was MINIMUM width. Left justification
    character ("-") and precision are allowed, but ignored.

 2. Any white space in the format string corresponds to a white space of
    any length (including zero-length) in the input string. Additionally, 
    any white space is automatically skipped before all format conversions 
    excluding "c" and pattern types.

 3. If string format type ("s") is given, then the source string is copied   
    into the specified location until next white space is found. 

 4. Pattern scan behaves in the same manner, but it does not skip initial 
    white space and scans the input as long as all characters satisfy the 
    pattern. Pattern constructor is like a "set of char" constructor,
    but without commas and quotes and using "-" instead of "..". 
    A "^" character placed immediately after the opening bracket denotes
    logical negations of the pattern. 
    Examples: [a-zA-Z], [^0-9]

 5. Integer type specifier "i" allows input of integers in decimal, hex 
    (and octal in scanf) bases. If the number starts with "$", "0x", or 
    "0X", then it is assumed to be hex, otherwise decimal. In scanf, if a
    number starts with "0" then it is considered octal (this is a standard 
    behaviour of scanf in C). 

 6. An input field is scanned until the first inconvertible character is met 
    or until Width is reached. The incorrect input may still result in 
    conversion and assignment if the input starts with legal characters. 
    For example, decimal conversion of "7FF" field will be scanned as 7, and
    octal number "5678" as octal 567.

 7. Types. All possible types have the meaning according to the following table:

========================================================================
 Type letter            DeFormat                        scanf      
------------------------------------------------------------------------
   d,D                decimal integer               decimal integer
   u,U             unsigned decimal integer    unsigned decimal integer
   x,X                  hex integer                    hex integer
   o,O                 octal integer                  octal integer
   i,I            integer(hex or decimal)(*)    integer(hex,decimal,octal)(*)

   n,N                 floating-point              number of characters
                  with thousand separators (*)  scanned so far, cardinal(*)     
     
f,F,e,E,g,G           floating-point                 floating-point  

    m                    currency                     not supported(*)
                  with thousand separators (*)

    M                formatted currency                not supported(*)
                  

    s                    string                          string
   [..]                  pattern                         pattern

    c                    character(s)                 character(s)

   p,P           32-bit pointer in hex form     32-bit pointer in hex form

========================================================================
(*) Marks differences between scanf and  DeFormat.


 8. Size specifiers. Since DeFormat and scanf get only pointers in the Args 
    array (with the exception of integers for indirect "*" flags), you must 
    provide additional information about the size of the variable if it 
    differs from default. Size specifier immediately precedes the format 
    letter and has the meaning according to the following table:

========================================================================
    Type        Size spec.              DeFormat              scanf      
------------------------------------------------------------------------
                (default)               integer              integer
 all integer      H, l8                 ShortInt             ShortInt
   types:         h, l16                SmallInt             SmallInt  
  i,d,u,x         l, l32                LongInt              LongInt 
                 L, ll, l64              Comp                 Comp


 floating-      (default)               Extended(*)         Single(*)    
  point            H                      Real                Real
  types:           h                     Single              Single
  f,g,e            l                     Double              Double
                  L, ll                 Extended            Extended 
                             
   n                               like floating-point     like integer        

  m,M           not allowed            currency(*)        not supported(*)    

  p,P           not allowed             pointer              pointer

 string         (default)                PChar                PChar
 types:            h                  ShortString           ShortString
 s, pattern        l                   AnsiString            AnsiString     

========================================================================
(*) Marks differences between scanf and  DeFormat.


 9. Modifiers. The usage and meaning of modifiers in DeFormat is the same
    as in Format (except that precision and justification modifiers are
    ignored). In scanf, there are only two possible modifiers: assignment 
    suppression ("*" character right after the "%" sign) and width as a 
    decimal integer. With DeFormat, assignment suppression is achieved by 
    supplying NIL pointer into the Args array. With scanf, NIL pointer is 
    illegal and terminates scanning if reached.

10. String types automatically append #0 to the end of the string if it is
    a PChar or AnsiString. For PChar type, you should provide enough allocated 
    space, including this additional character. If "%ls" format specifier is
    used, then AnsiString is created anew and necessary space is allocated 
    automatically.

11. Return value. Both functions return the number of successfully assigned 
    variables. In addition, scanf retuns -1 if it reaches the end of the input
    and no assignments have been made. The core functions scanf_core and 
    DeFormat_core also return the final positions in the input and format
    strings (see "Implementation Notes"). Note that "n" specifier in scanf does
    not count. If an exception is raised, then return value is undefined 
    (see "Exceptions Handling").

12. Currency support. In DeFormat, %m specifier scans in a numerical currency 
    value (with thousand separators and decimal separators). %M specifier scans in 
    a formatted currency value, using CurrencyString, CurrencyFormat and 
    NegCurrFormat.



**********************************
* Compatibility with C/C++ scanf *
**********************************

Scanf for Delphi has been designed to be as compatible to the C/C++
scanf() as possible. I took GNU scanf for Win32 (Cygnis) 
for a basic set and added some extensions from Borland C++ 5.01. 

Hex string identification accepts both C-type ("0x") and Pascal-type ("$")
hex prefixes. While this may create some problems with "i" type when "$" 
sign is used as currency specifier, it is undispensible for Delphi 
environment. If in doubt, use explicit "d" or "x" specifiers.

A "very short" size specifier "H" has been added. For integer types, it 
means an 8-bit integer (ShortInt or Byte), for floating-point types, 
the old Real type. All integer and floating-point type specifiers are 
case-insensitive.

Since there is no wide character support in the current version, 
upper-case C and S specifiers behave as lower-case counterparts, but 
this may change in the future. You should be safe using all-lower-case
specifiers, though.

The table below summarizes the differences in types:

======================================================================
  scanf        Delphi               BC++                  Cygnis
  spec.         type                type                   type  
======================================================================

   hs       ShortString              char *                char *  
   hS       SHortString              char *                  -
   ls       AnsiString             widechar *              char * 
   lS       AnsiString             widechar *                - 
    s         PChar                  char *                char *
    S         PChar                widechar *                - 
    c         char                   char                   char
    C         char                 widechar                  - 
   hc         char                   char                   char    
   hC         char                   char                    -    
   lc         char                 widechar                 char
   lC         char                 widechar                  -

   Hi       ShortInt                  -                      -
  l8i       ShortInt                 char                -
   hi       SmallInt               short int              short int
  l16i      SmallInt               short int                 -
    i       Integer                   int                   int
  l32i      Integer                   int                    -
   li       LongInt                long int               long int
   Li       int64 (Comp)            __int64                  - 
  l64i      int64 (Comp)            __int64                  - 

          "i" stands for any integer type (i, d, u, o, n, and x).


   Hf         Real                     -                     -
   hf        Single                  float                 float
    f        Single                  float                 float
   lf        Double                  double                double
   Lf       Extended               long double           long double    

          "f" stands for any floating-point type (e, g, and f).   

======================================================================


**********************
* Exception handling *
**********************

In {$Q+} mode, overflow in integer, floating-point, and currency raises 
EOverflow exception.

If scanf_c.pas unit is compiled with DEFORMAT_EXCEPTIONS or SCANF_EXCEPTIONS 
defined, following exceptions are raised:

 - If a numerical field can not be converted.
 - If function attempts an assignment, but index of the pointer lies outside
   of Low(Args)..High(Args)
 - If format specifier has incorrect syntax.
 - If scanf attempts to assign to NIL.
 - If type of a variable for indirect input in DeFormat is not vtInteger.

Important note: If an exception is raised, then the return values of scanf_core 
and DeFormat_core are undefined (normally they correspond to some pointer value).
This is an unfortunate feature of Delphi exception handler. It is therefore 
reasanble to use either exceptions or check the return values, but not both.


************************
* Implementation notes *
************************

The routines in the scanf_c.pas unit are loosely documented and badly structured. 
They are not supposed to be debugged, but optimized for flexibility and 
performance. You are NOT encouraged to debug them yourself, but rather contact 
the author. The source code is provided only for the recompilation purposes. 

The actual conversion is performed by assembler-level routines and by scanf_core, 
scanf_stream, DeFormat_core, and StrToCurrF_core functions in the Scanf_c.pas unit. 
The user-friendly functions in the Scanf.pas unit provide corresponding calls and 
entry points. You may wish to create your own procedures using the conversion motors. 
For this purpose, all core routines set input pointers to point past the last 
successfully scanned (converted) character, which allows to analyse the failure 
and continue processing, if necessary.  

The programs in the package do not use VCL calls, do not modify static variables 
and do not modify input strings. They should be therefore reentrant and thread-safe. 
In fact, %M specifier in DeFormat works recursively. 

NOTE: The fscanf (scanf_stream) routine uses and modifies the Position property of 
the input stream. For some stream implementations, this may be not reentrant.
The valid TStream implementation MUST support moving Position backward.

In accordance to C and Pascal traditions, scanf is case-sensitive 
but DeFormat is case-insensitive (this is slightly slower).
This behaviour can be changed in the Scanf_c.pas unit by commenting (out) the
corresponding compiler directives in the unit header. 


Not implemented:

 - wide string support


*****************
* Release Notes *
*****************

26.08.1998 
  Beta version 0.9 released.

09.09.1998 
  Added octal base support. 
  Removed bugs in variable indexing.
  Optimized hex scanner and search-set parser.

20.10.1998
  Added independent decimal integer and floating point scanners and removed
  library Val routine. FP scanner supports thousand separators and currency.
  Restructured package, moving assembler level routines into separate unit.
  Removed bug in Int64ToXXX, producing empty string on zero value.

10.11.1998
  Converted case-sensitivity and verbose switches to compiler directives.
  Added overflow control.
  Added TextToFloatS routine.
  Adapted to Delphi 4.

20.11.1998
  Added formatted currency conversion (StrToCurrF_core) and %M as separate specifier
  for DeFormat family.
  Added StrToFloatS and StrToCurrS.
  A couple of bugs removed.

7.12.1998
  Added scTutor program.

12.12.1998
  Fscanf added.

17.12.1998 
  Assembler-level routines included back into the scanf_c.pas unit.