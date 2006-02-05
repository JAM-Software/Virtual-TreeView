(* E. Sorokin  1998, Ver 1.2 *)
(* Assembler-level scanner routines and core functions for scanf and DeFormat *)

{$WRITEABLECONST OFF} {$EXTENDEDSYNTAX ON}

// {$DEFINE SCANF_CASE_SENSITIVE}  // uncomment to make scanf case-insensitive.
{$DEFINE DEFORMAT_CASE_SENSITIVE}  // comment out to make DeFormat case-sensitive
// {$DEFINE SCANF_EXCEPTIONS}      // uncomment to generate exceptions in scanf_core
// {$DEFINE DEFORMAT_EXCEPTIONS}   // uncomment to generate exceptions in DeFormat_core
unit Scanf_c;

interface

uses Classes;

{$ifndef Ver110}        // This is Delphi 4 
type int64    = comp;
     real48   = real;
     longword = cardinal;
{$endif}

const  scOK       = 1;
       scOverflow = 8;
       scEOF = -1;

function Scanf_core(var Buffer: PChar; var Format : PChar;
                    Pointers : array of Pointer) : Integer;

function Scanf_stream(Inp : TStream; var Format : PChar;
                      Pointers : array of Pointer)
                      : Integer;

function DeFormat_core(var Buffer : PChar; BufLen: Cardinal;
                       var Format : PChar; FmtLen: Cardinal;
                       Args: array of const;
                       DecSep, ThSep : char): Cardinal;

function StrToCurrF_core(var Buffer : PChar; BufLen : Cardinal;
                         var Res : Currency;
                         CurrStr : PChar; CurrF, NegCurrF : byte;
                         DecSep, ThSep : char) : Integer;

// Scans Str, assuming it starts with first digit or decimal point (no sign!)
// EShift is the integer, added to the decimal exponent
// (should be 4 for Currency and 0 otherwise, but may be used for any other purpose).
// On success returns scOK, conversion result on st(0)
// On error returns 0, st(0) undefined.
// In $Q+ mode, returns scOverflow bit is set on overflow, conversion result on st(0).
function Ext_scanner(var Str : PChar; Width : cardinal; EShift : integer; DecSep, ThSep : char) : integer;

{$IFDEF VER90}   // This is Delphi 2, no resourcestrings
  const
{$ELSE}
  resourcestring
{$ENDIF}
  SInvalidInteger = '''%s'' is not a valid integer value';
  SInvalidFloat = '''%s'' is not a valid floating point value';
  SInvalidFormat = 'Format ''%s'' invalid or incompatible with argument';
  SArgumentMissing = 'No argument for format ''%s''';
  SOverflow = 'Floating point overflow';
  SIntOverflow = 'Integer overflow';
  SInvalidCurrency = '''%s'' is not a valid currency value';

implementation

uses SysUtils;

function Hex_scanner(var Str : PChar; var P : int64; Width : cardinal) : integer; register;
asm
// Validity checks
        JECXZ   @@ret           {Width was 0}
        PUSH    EBX             {Save registers to be preserved}
        PUSH    ESI
        PUSH    EDI
//  Initialize
{$IFOPT Q+}
            PUSH    EBP
            XOR     EBP,EBP     {EBP used for overflow tracking}
{$ENDIF}
        PUSH    EAX             {Put @Str on stack }
        MOV     EAX,[EAX]
        OR      EAX,EAX
        JE      @@Str0          {Str was NIL, quit}
        XOR     EBX,EBX
        XOR     ESI,ESI         {EDI:ESI will be a 64-bit accumulator}
        XOR     EDI,EDI

// Main loop start
@@Loop: MOV     BL,[EAX]         {Get char}
        CMP     BL,'a'
        JB      @@upcase
        SUB     BL,'a' - 'A'
@@upcase:
        SUB     BL,'0'
        CMP     BL, 9
        JBE     @@digOk
        SUB     BL,'A' - '0'
        CMP     BL,5
        JA      @@StopScan       {found illegal char}
        ADD     BL,10
//-------- Multiply and add start
@@digOk:
        SHLD    EDI,ESI,4        {64-bit unsigned multiplication by 16}
{$IFOPT Q+}
            ADC     EBP,0        {If CF was set at least once, EDX > 0}
{$ENDIF}
        SHL     ESI,4
        OR      ESI,EBX          {Add BL}
//-------- Multiply and add end
        INC     EAX
        DEC     ECX
        JNZ     @@Loop
// Main loop end

@@StopScan:
        POP     EBX               {Original @Str}
        XOR     ECX,ECX
        CMP     [EBX],EAX         {Were there valid chars?}
        JE      @@Exit            {No valid characters found}
        MOV     [EBX],EAX         {Store new Str}
        INC     ECX               {Set 1 as Result}
        OR      EDX,EDX
        JZ      @@Exit            {Pointer was NIL, no assignment}
        MOV     [EDX+4],EDI       {store high dword of _int64}
        MOV     [EDX],ESI         {store low  dword}
@@Exit:
{$IFOPT Q+}
            OR      EBP,EBP
            POP     EBP
            JNA     @@NoOverflow
            OR      ECX,scOverflow
@@NoOverflow:
{$ENDIF}
        POP     EDI               {restore registers to be preserved}
        POP     ESI
        POP     EBX
@@ret:
        MOV     EAX,ECX           {set return code to Result}
        RET
@@Str0:
        XOR     ECX,ECX
        POP     EBX
        JMP     @@Exit
end;

function Oct_scanner(var Str : PChar; var P : int64; Width : cardinal) : integer; register;
asm
// Validity checks
        JECXZ   @@ret           {Width was 0}
        PUSH    EBX             {Save registers to be preserved}
        PUSH    ESI
        PUSH    EDI
//  Initialize
{$IFOPT Q+}
            PUSH    EBP
            XOR     EBP,EBP     {EBP used for overflow tracking}
{$ENDIF}
        PUSH    EAX             {Put @Str on stack }
        MOV     EAX,[EAX]
        OR      EAX,EAX
        JE      @@Str0          {Str was NIL, quit}
        XOR     EBX,EBX
        XOR     ESI,ESI         {EDI:ESI will be a 64-bit accumulator}
        XOR     EDI,EDI

// Main loop start
@@loop: MOV     BL,[EAX]          {Get char}
        SUB     BL,'0'+8
        ADD     BL,8
        JNC     @@StopScan        {found illegal char}

        SHLD    EDI,ESI,3         {64-bit unsigned multiplication by 8}
{$IFOPT Q+}
            ADC     EBP,0         {Once CF set, EDX > 0}
{$ENDIF}
        SHL     ESI,3
        OR      ESI,EBX           {Add BL}

        INC     EAX
        DEC     ECX
        JNZ     @@loop
// Main loop end

@@StopScan:
        POP     EBX               {Original @Str}
        XOR     ECX,ECX
        CMP     [EBX],EAX         {Were there valid chars?}
        JE      @@Exit            {No valid characters found}
        MOV     [EBX],EAX         {Store new Str}
        INC     ECX               {Set 1 as Result}
        OR      EDX,EDX
        JZ      @@Exit            {Pointer was NIL, no assignment}
        MOV     [EDX+4],EDI       {store high dword of _int64}
        MOV     [EDX],ESI         {store low  dword}
@@Exit:
{$IFOPT Q+}
            OR      EBP,EBP
            POP     EBP
            JNA     @@NoOverflow
            OR      ECX,scOverflow
@@NoOverflow:
{$ENDIF}
        POP     EDI               {restore registers to be preserved}
        POP     ESI
        POP     EBX
@@ret:
        MOV     EAX,ECX           {set return code to Result}
        RET
@@Str0:
        XOR     ECX,ECX
        POP     EBX
        JMP     @@Exit
end;


function Dec_scanner(var Str : PChar; var P : int64; Width : cardinal) : integer; register;
asm
// Validity checks
        JECXZ   @@ret           {Width was 0}
        PUSH    EBX             {Save registers to be preserved}
        PUSH    ESI
        PUSH    EDI
//  Initialize
{$IFOPT Q+}
            PUSH    EBP
            XOR     EBP,EBP     {EBP used for overflow tracking}
{$ENDIF}
        PUSH    EAX             {Put @Str on stack }
        MOV     EAX,[EAX]
        OR      EAX,EAX
        JE      @@Str0          {Str was NIL, quit}
        PUSH    EDX             {Put P on stack }
        MOV     ESI,EAX         {use ESI for Str}
        XOR     EBX,EBX
        XOR     EDI,EDI         {EDI:EAX will be a 64-bit accumulator}
        XOR     EAX,EAX

// Main loop start
@@loop: MOV     BL,[ESI]        {Get char}
        SUB     BL,'0'+10
        ADD     BL,10
        JNC     @@StopScan      {found illegal char}
//=== Unsigned multiplication by 10 start
{$IFOPT Q+}
        CMP     EDI,$33333333   { $FFFFFFFF div 5}
        JNA     @@Less
        INC     EBP
@@Less:
{$ENDIF}
        LEA     EDI,[EDI+4*EDI]
        MOV     EDX,10
        MUL     EDX
        ADD     EAX,EBX         {Add BL}
        ADC     EDX,0
{$IFOPT Q+}
            SHL     EDI,1
            ADC     EBP,0
            ADD     EDI,EDX
            ADC     EBP,0
{$ELSE}
        LEA     EDI,[EDI*2+EDX]
{$ENDIF}
//=== Unsigned multiplication by 10 end
        INC     ESI
        DEC     ECX
        JNZ     @@loop
// Main loop end

@@StopScan:
        POP     EDX               {Get back P}
        POP     EBX               {Original @Str}
        XOR     ECX,ECX
        CMP     [EBX],ESI         {Were there valid chars?}
        JE      @@Exit            {No valid characters found}
        MOV     [EBX],ESI         {Store new Str}
        INC     ECX               {Set 1 as Result}
        OR      EDX,EDX
        JZ      @@Exit            {Pointer was NIL, no assignment}
        MOV     [EDX+4],EDI       {store high dword of _int64}
        MOV     [EDX],EAX         {store low  dword}
@@Exit:
{$IFOPT Q+}
            OR      EBP,EBP
            POP     EBP
            JNA     @@NoOverflow
            OR      ECX,scOverflow
@@NoOverflow:
{$ENDIF}
        POP     EDI               {restore registers to be preserved}
        POP     ESI
        POP     EBX
@@ret:
        MOV     EAX,ECX           {set return code to Result}
        RET
@@Str0:
        XOR     ECX,ECX
        POP     EBX
        JMP     @@Exit
end;


const single10   : single =   10.0;
      single1000 : single = 1000.0;
      // 10.0 and 1000.0 have exact representations, single precision is enough
      // FMUL with single is the fastest [The 386 Book].

function Ext_scanner(var Str : PChar; Width : cardinal; EShift : integer; DecSep, ThSep : char) : integer; register;
var Tmp : integer;
    SaveCW : word;
    NewCW : word;
asm
// Initialization start
  or    edx,edx      // check Width
  jz    @@ret
  fnstcw SaveCW      // Save FPU control word
  push  edi
  push  esi
  push  ebx
  fnclex             // just in case some garbage is there
  mov   NewCW,$33f   // full precision, ignore errors
  push  eax          // remember @str point on stack
  mov   esi,[eax]    // ESI will be Str
  fldcw NewCW
  fldz               // initialize accumulator
  push  ecx          // remember EFactor

  xor   ecx,ecx      // fractional part length
  xor   edi,edi      // exponent
  xor   ebx,ebx      // general pusrpose
// Initialization end

// Integer part start
  xor	  eax,eax      // inititate integer loop
@@intloop:
	mov   al,[esi]
	sub 	al,'0'+10
	add	  al,10
	jnc 	@@ThSep      // two checks in one!
  inc   esi          // accept
	fmul	single10
  inc   ebx          // use ebx as digit counter
	mov 	Tmp,eax
	fiadd	Tmp
	dec   edx
  jnz   @@intloop
  jmp   @@OK
// Integer part end

//============ ThSep block ========================
// edi counts thousand groups, edi=0 means no thousand groups
@@ThSep:
  cmp   ThSep,0
  je    @@DecSep     // ThSep=#0, forget it
  sub   ebx,1+3      // ebx must be 1, 2 or 3
  add   ebx,3
  jnc   @@bDecSep
  sub   edx,4
  jc    @@ThDecSep   // Less than 4 characters left, ThSep not allowed
  mov   ebx,[esi]
@@Thloop:
  xor   eax,eax
  cmp   bl, ThSep
  jne   @@ThDecSep
  inc   edi
  shr   ebx,8         // first digit in bl
  inc   esi
	sub 	bl,'0'+10
	add	  bl,10
	jnc 	@@error
  mov   al,bl
  shr   ebx,8         // second digit in bl
  inc   esi
	sub 	bl,'0'+10
	add	  bl,10
  jnc   @@error
  lea   eax,[eax+eax*4]
  add   eax,eax
  add   al,bl        // al <= 90, no carry possible
  shr   ebx,8        // third digit in bl
  inc   esi
	sub 	bl,'0'+10
	add	  bl,10
  jnc   @@error
	fmul	single1000   // finally...
  lea   eax,[eax+eax*4]
  lea   eax,[ebx+eax*2]
	mov 	Tmp,eax
  inc   esi
	fiadd	Tmp
  sub   edx,4        // Try next thousand group
  jc    @@ThDecSep   // Less than 4 characters left, exit Thloop
  mov   ebx,[esi]
  jmp   @@Thloop
//============ End of ThSep block ====================

// Fractional part start
@@ThdecSep:
  add   edx,4        // compensate edx check in ThSep block
	mov   al,[esi]
	sub 	al,'0'
@@bdecSep:
  inc   ebx          // compensate ebx check in ThSep block
@@decSep:
  mov   ah,DecSep
  sub   ah,'0'
  cmp   al,ah
  jne   @@E          // not DecSep, try E
  inc   esi
  dec   edx
  jz    @@OK         // edx=0, finished anyway
  xor	  eax,eax      // initiate fraction loop
  mov   ecx,edx      // remember edx in ecx
@@fracloop:
	mov   al,[esi]
	sub 	al,'0'+10
	add	  al,10
	jnc 	@@fracEnd    // two checks in one!
  inc   esi
	fmul	single10
	mov 	Tmp,eax
	fiadd	Tmp
	dec   edx
  jnz   @@fracloop
  xor   edi,edi      // bring edi back to 0
  jmp   @@OK         // edx = 0, nothing to subtract from ecx
@@fracEnd:
  sub   ecx,edx      // ecx now is number of digits in a fraction part
// Fractional part end

// Exponent start
@@E:
  cmp   edi,0
  mov   edi,0
  jnz   @@OK         // edi was not 0, no exp
  add   ebx,ecx
  jz    @@Error      // ebx=0 and ecx=0 mean no digits found, error
	mov	  al,[esi]
	and	  al,$df       // Upcase(al)
	cmp	  al,'E'
	jnz	  @@OK
  inc   esi
  dec   edx
  jz    @@OK         // width reached
	mov	  al,[esi]
  xor   ebx,ebx      // use ebx as '-' flag
  cmp   al,'+'
  jz    @@pmyes
  cmp   al,'-'
  jnz   @@doexp
  dec   ebx
@@pmyes:
  inc   esi
  dec   edx
  jz    @@OK         // width reached
@@doexp:
  xor	  eax,eax      // inititate exponent loop
@@exploop:
	mov   al,[esi]
	sub 	al,'0'+10
	add	  al,10
	jnc 	@@EndExp     // two checks in one!
  inc   esi          // accept
	lea   edi,[edi+4*edi] // edi:=edi*5
	lea   edi,[2*edi+eax]   // edi = edi + edi + eax
	dec   edx
  jnz   @@exploop
@@EndExp:
  cmp   ebx,0        // ebx <> 0 means '-'
  jz    @@OK
  neg   edi
// Exponent end

// Combine parts start
// At this point, edi is the exponent and ecx is the fractional part length
@@OK:
  mov   eax,edi      // get exponent into eax
  sub   eax,ecx      // decrease by fractional part length
  pop   ecx          // restore EFactor
  add   eax,ecx      // add to exponent
  jz    @@NoPower
  call  FPower10     // multiply st(0) by eax^10
@@NoPower:
  mov   edx,1        // return Result=1
{$IFOPT Q+}
  fstsw ax
  and   eax,8        // FPU overflow mask
  or    edx,eax      // simply add to edx
{$ENDIF}
  jmp   @@finish
// Combine parts end

@@error:
  fstp  st(0)        // clear up FPU stack
  pop   eax          // clear up stack
  xor   edx,edx      // set Result to 0

@@finish:
  fnclex             // clear up
  fldcw SaveCW       // restore FPU control word
  pop   eax
  mov   [eax],esi    // store new Str
  pop   ebx
  pop   esi
  pop   edi
@@ret:
  mov   eax,edx      // set result
end;

const singleINFasLongint : longint = $7f800000;
      q_singleNANasLongint : longint = $7fC00000; // quiet NAN
//    s_singleNANasLongint : longint = $7f800001; // signalling NAN
var   singleINF : single absolute singleINFasLongint;
      q_singleNAN : single absolute q_singleNANasLongint;
//    s_singleNAN : single absolute s_singleNANasLongint;


type TCharSet = set of AnsiChar;
     PInt = ^Integer;

     TscRec = packed record  // Has size of an integer
              Case byte of
                0: ( Typ : byte; Size : char; Flags : word;);
                1: ( SizeType : word; iFlags : smallInt;);
              end;

// Somewhat complicated structure of possible variable types.
const scIllegal =  -1;                // illegal value, no assignment possible
      scChar    =  vtChar;
      scPChar   =  vtPChar;
      scInteger =  vtInteger;
      scFloat   =  vtExtended;
      scCurrency=  vtCurrency;
      scCharSet =  $7e;
      scNspec   =  $7f;               // "n" specifier
      scUpperL  =  Ord('L') shl 8;    // Size specifiers
      scLowerL  =  Ord('l') shl 8;
      scUpperH  =  Ord('H') shl 8;
      scLowerh  =  Ord('h') shl 8;

      scShortInt=  scInteger + scUpperH;
      scSmallInt=  scInteger + scLowerH;
      scLongInt =  scInteger + scLowerL;
      scInt64   =  scInteger + scUpperL;  // Comp in D2,D3
      scReal    =  scFloat   + scUpperH;
      scSingle  =  scFloat   + scLowerH;
      scDouble  =  scFloat   + scLowerL;
      scExtended=  scFloat   + scUpperL;
      scAnsi    =  scLowerL;
      scShort   =  scLowerH;
      scShortSet=  scCharSet + scShort;
      scAnsiSet =  scCharSet + scAnsi;
      scString  =  scPChar   + scShort;
      scAnsiStr =  scPChar   + scAnsi;

   // Flags constants
      scHex       =   $1;
      scDecimal   =   $2;
      scOctal     =   $4;
      scBaseMask  = scHex or scDecimal or scOctal;
      sc1000Sep   =  $10;
      scIntSpec   =  $20;  // l8, l16, l32, l64 can be used only with integer types
      scUnsigned  =  $40;  // Not really implemented, as in BC++
      scFormatted =  $80;  // Formatted currency in DeFormat
      scNoAssign  =$8000;  // Just negative bit

      defWidth = MaxInt;   // no width limit by default

const
  CurrFmts : array [0..3] of string =
    ( '%s%%m',         // 0 = $1
      '%%m%s',         // 1 = 1$
      '%s %%m',        // 2 = $ 1
      '%%m %s'         // 3 = 1 $
    );
  NegCurrFmts : array [0..15] of string =
    ( '(%s%%m)',       // 0  = ($1)
      '-%s%%m',        // 1  = -$1
      '%s%%m',         // 2  = $-1
      '%s%%m-',        // 3  = $1-
      '(%%m%s)',       // 4  = (1$)
      '%%m%s',         // 5  = -1$
      '%%m-%s',        // 6  = 1-$
      '%%m%s-',        // 7  = 1$-
      '%%m %s',        // 8  = -1 $
      '-%s %%m',       // 9  = -$ 1
      '%%m %s-',       // 10 = 1 $-
      '%s%%m-',        // 11 = $ 1-
      '%s %%m',        // 12 = $ -1
      '%%m- %s',       // 13 = 1- $
      '(%s %%m)',      // 14 = ($ 1)
      '(%%m %s)'       // 15 = (1 $)
     );

function StrToCurrF_core(var Buffer : PChar; BufLen : Cardinal;
                         var Res : currency;
                         CurrStr : PChar; CurrF, NegCurrF : byte;
                         DecSep, ThSep : char) : Integer;
var Fmt : string;
    FPtr : PChar;
begin
  Fmt:=Format( NegCurrFmts[NegCurrF], [CurrStr]); // Try negative first
  FPtr:=PChar(Fmt);
{$IFDEF DEFORMAT_EXCEPTIONS}
  try
{$ENDIF}
  Result:=DeFormat_core(Buffer, Buflen, FPtr, Length(Fmt), [@Res], DecSep, ThSep);
{$IFDEF DEFORMAT_EXCEPTIONS}
  except on EConvertError do {nothing}; end;
{$ENDIF}
  if (Result=1) then
    Case NegCurrF of
      1,4,9,14,15 : Res:=-Res;
      2,5,8,12 : {Negative and positive formats are compatible} ;
    else{3,6,7,10,11,13:} If Fptr^=#0 then Res:=-Res;
    end
  else begin {try positive}
    Fmt:=Format(CurrFmts[CurrF],[CurrStr]);
    FPtr:=PChar(Fmt);
    Result:=DeFormat_core(Buffer, Buflen, FPtr, Length(Fmt), [@Res], DecSep, ThSep);
  end;
end;

// *********** Common routines *******************************************//

// Scan numerical string (float, decimal, $hex or 0x..)
// Str    : points to the first char in a string
// Width  : maximum length and assumed to not exceed actual length
// FType  : type and length of result, see scXXXX constants
// P      : where to put the conversion result if scNoAssign is not set.
// Result : 0 on error, > 0 on success (e.g. scOverflow in $Q+ mode)
function NumScan(var Str : PChar; Width : integer;
                 FType : integer; P : Pointer;
                 DecSep, ThSep : char) : integer;
var X : extended;
    i64 : int64 absolute X;
    L : longint absolute X;
    Cur : Currency absolute X;
{$IFOPT Q+} SaveCW, NewCW : word; {$ENDIF}
    Negative : boolean;

Label doHex, doDec, doOct, Cont;
begin
  With TscRec(FType) do begin
    Negative:=False;
    Case Str^ of // Get sign, if any
      '-' : begin Negative:=True; Inc(Str); Dec(Width); end;
      '+' : begin Inc(Str); Dec(Width); end;
    end;
    Case Typ of
      scInteger :
        begin
          Case (Flags and scBaseMask) of
            scHex     : doHex: Result:=Hex_Scanner( Str, i64, Width);
            scOctal   : doOct: Result:=Oct_Scanner( Str, i64, Width);
            scDecimal : doDec: Result:=Dec_Scanner( Str, i64, Width);
            else // Base decision block
              Case Str^ of
               '$' : begin Inc(Str); Dec(Width); goto doHex; end;
               '0' : If (Str[1] = 'x') or (Str[1] = 'X') then begin
                       Inc(Str,2); Dec(Width,2); goto doHex;
                     end else
                       if (Flags and scOctal <> 0) then goto doOct else goto doDec;
               else goto doDec;
               end;
          end;
          if (Result <= 0) then Exit;
          if Negative then
          asm
            neg dword ptr [X][4]
            neg dword ptr [X]
            sbb dword ptr [X][4],0
          end;
        end;
      scCurrency :
        begin
          Result:=Ext_Scanner(Str, Width, 4, DecSep, ThSep);
          if Result > 0 then begin
            {$IFOPT Q-}
            if iFlags >= 0 then begin
              if Negative then asm fchs; end;
              asm
                mov  eax, [P]
                fistp qword ptr [eax];
                fnstsw ax
                fnclex               // Clear possible exceptions
              end;
            end else asm fstp; end;  // Just clear the FPU stack
            {$ELSE}
            if Negative then asm fchs; end;
            asm
              fstcw SaveCW
              mov    NewCW,$33f     // Mask exceptions
              fldcw  NewCW
              fistp qword ptr [X];
              fnstsw ax
              and   eax,8+1        // FPU overflow and invalidop mask
              jz    @@OK
              or    [Result],8
        @@OK: fldcw SaveCW
              fnclex               // Clear exceptions
            end;
            if iFlags <> 0 then Currency(P^):=Cur;
            {$ENDIF}
          end;
          Exit;
        end;
      scFloat :
        begin
          if (Width >=3) then begin // Check NAN and INF
            Case ( (PInt(Str))^ and $00ffffff) of
              $004e414e {'NAN'#0}: begin if Negative then X:=-q_singleNAN else X:=q_singleNAN; Inc(Str,3);  Result:=scOK; goto Cont; end;
              $00464e49 {'INF'#0}: begin if Negative then X:=-singleINF else X:=singleINF;   Inc(Str,3);  Result:=scOK; goto Cont; end;
            end;
          end;
          Result:=Ext_Scanner(Str, Width, 0, DecSep, Char(Ord(ThSep)*Integer( (Flags and sc1000Sep) <> 0)));
          if (Result <= 0) or (FType < 0) then Exit else begin
            if Negative then asm fchs; end;
            asm fstp [X]; end;
          end;
        Cont:
        end;
      end; {Case Typ}
    if (FType > 0) then begin // If scNoAssign not set, assign
      Case SizeType of
{$IFOPT R-}
        scShortInt : ShortInt(P^):=L;
        scSmallInt : SmallInt(P^):=L;
        scInteger, scLongInt : LongInt(P^) :=L;
{$ELSE}     //signed and unsigned have different ranges
        scShortInt : If (Flags and scUnsigned) = 0 then ShortInt(P^):=L else Byte(P^):=L;
        scSmallInt : If (Flags and scUnsigned) = 0 then SmallInt(P^):=L else Word(P^):=L;
        scInteger, scLongInt : If (Flags and scUnsigned) = 0 then LongInt(P^) :=L else LongWord(P^):=L;
{$ENDIF}
        scSingle   : Single(P^):=X;
        scReal     : real48(P^):=X;
        scDouble   : Double(P^):=X;
        scExtended : Extended(P^):=X;
        scInt64    : int64(P^):=i64;
      end;
    end;
  end;
end;

// Parse a search-set specifier in format string
// Return value : set of allowed characters
// Format is updated to the first unparsed character
// On error result is False
function ParseSet(var Format : PChar; FmtEnd : PChar; var Res : TCharSet) : boolean;
type T32Int = array [0..7] of integer;
var TI : T32Int absolute Res;
    i : integer;
    Neg : boolean;
begin // Assume that Format points to a character after opening "["
  Res:=[]; Result:=False;
  If Format >= FmtEnd then Exit;
  If Format^='^' then begin // Check for '^'
    Neg:=True;
    Inc(Format);
    If Format >= FmtEnd then Exit;
  end else Neg:=False;
  Case Format^ of   // Check special cases: first ']' or '-'
    ']','-' :
      begin
        //  Res:=Res+[Format^];
        TI[Byte(Format^) shr 5]:=TI[Byte(Format^) shr 5] or (1 shl ( Byte(Format^) and 31));
        Inc(Format);
      end;
  end;
  while Format < FmtEnd do
    if Format^ = ']' then begin
      Result:=True;
      Inc(Format);
      Break;
    end else
      if (Format[1]='-') and (Format[2] <> ']') then begin // Range
        Res:=Res+[Format^..Format[2]];
        Inc(Format,3);
      end else begin
        // Res:=Res + [Format^];
        TI[Byte(Format^) shr 5]:=TI[Byte(Format^) shr 5] or (1 shl ( Byte(Format^) and 31));
        Inc(Format);
      end;
  if Neg  then
    // Res:=[#0..#255]-Res;
    for i:=0 to 7 do TI[i]:= not TI[i];
end;

// Process [size] type portions of a format specifier
// On error, scIllegal is returned
function GetSizeType(var Format : PChar) : TscRec;
var FType : TscRec;
    PW : ^word absolute Format;
begin
  Integer(Result):=scIllegal; Integer(FType):=0;// Initialization
  With FType do begin
    Case Format^ of  // check size specifiers
      'H','h','L' :
         begin
           Size:=Format^;
           Inc(Format);
         end;
      'l' :
         begin
           Inc(Format);
           Case Format^ of
             '8' : begin Size:='H'; Inc(Format); Flags:=Flags or scIntSpec; end;
             'l' : begin Size:='L'; Inc(Format); end;
           else
             Case PW^ of
              $3631 {'16'} : begin Size:='h'; Inc(Format,2); Flags:=Flags or scIntSpec; end;
              $3332 {'32'} : begin Size:='l'; Inc(Format,2); Flags:=Flags or scIntSpec; end;
              $3436 {'64'} : begin Size:='L'; Inc(Format,2); Flags:=Flags or scIntSpec; end;
             else Size:='l';
             end;
           end;
         end;
    end;
    Case UpCase(Format^) of
      #0  : Exit;
      '[' : Typ:=scCharSet;
      'I' : begin Typ:=scInteger; Flags:=Flags or scDecimal or scHex or scOctal; end;
      'D' : begin Typ:=scInteger; Flags:=Flags or scDecimal; end;
      'U' : begin Typ:=scInteger; Flags:=Flags or scDecimal or scUnsigned; end;
      'X' : begin Typ:=scInteger; Flags:=Flags or scHex; end;
      'O' : begin Typ:=scInteger; Flags:=Flags or scOctal; end;
      'F','G','E' : begin Typ:=scFloat; Flags:=Flags or scDecimal; end;
      'N' : Typ:=scNspec;
      'S' : Typ:=scPChar;
         // No size specs. allowed:
      'C' : if Size=#0 then Typ:=scChar else Exit;
      'P' : if Size = #0 then begin
              Typ:=scInteger; Flags:=Flags or scHex;
            end else Exit;
      'M' : if Size=#0 then begin
              Typ:=scCurrency;
              if Format^='M' then Flags:=Flags or scFormatted;
            end else Exit;
      else begin Dec(Format); Exit; end;
    end;
    Case Typ of
      scPChar, scCharSet :
        Case Size of
          'H', 'L' : Exit; // 'H' and 'L' not allowed with strings
        end;
    end;
    Inc(Format);
  end; { With Ftype}
  Result:=FType;
end;

// ***********  DeFormat ***********************************************//

function DeFormat_core(var Buffer : PChar; BufLen: Cardinal;
                       var Format : PChar; FmtLen: Cardinal;
                       Args: array of TVarRec;
                       DecSep, ThSep : char): Cardinal;
var  // Many of these will be optimized out
  Count : cardinal;
  Index, Width : integer;
  Buf, Marker, Fmt : PChar;
  {$IFDEF DEFORMAT_EXCEPTIONS} FMArker : PChar; {$ENDIF}
  Temp : integer;
  Ptr : Pointer;
  BufEnd : PChar absolute BufLen;
  FmtEnd : PChar absolute FmtLen;
  SCR : TscRec;
  FType : integer absolute SCR;
  theSet : TCharSet;

Label BadArg, BadFormat, CopyStr, DoFloat;

  function ScanToFormat : boolean;
  begin
    ScanToFormat:=False;
    while (Fmt < FmtEnd) and (Buf < BufEnd) do begin
      case Fmt^ of
        '%' :  begin
                Inc(Fmt);
                if Fmt^='%' then  // this is "%%", check for "%" in Buf
                  If Buf^ <> '%' then Exit
                  else begin Inc(Buf); Inc(Fmt); end
                else begin
                  ScanToFormat:=True; // Start of Fmt specifier found.
                  Exit;
                end;
              end;
         #0..#32 : begin
                     while (Buf < BufEnd) and (Buf^ <=' ') do Inc(Buf);
                     while (Fmt < FmtEnd) and (Fmt^ <=' ') do Inc(Fmt);
                   end;
         else begin
           {$IFNDEF DEFORMAT_CASE_SENSITIVE}
             if Buf^ <> Fmt^ then Exit;
           {$ELSE}
             if UpCase(Buf^) <> UpCase(Fmt^) then Exit;
           {$ENDIF}
           Inc(Buf);
           Inc(Fmt);
         end;
       end;
    end; {While}
  end;

  // Returns:  -1 in case of '*' error
  //           0 on success
  //           1 in case of numerical input error
  function GetNumFields : integer;
  var Temp : cardinal;
      GotTemp, GotIndex : boolean;
  begin
    GotIndex:=False;
    Temp := 0;
    repeat
      GotTemp:=False;
      if Fmt^='*' then begin  // Get indirect entry into Temp
        Inc(Fmt);
        If (Index <= High(Args)) and (Args[Index].VType=vtInteger) then
        begin
          Temp:=Args[Index].VInteger;
          Inc(Index);
          GotTemp:=True;
        end else begin Result:=-1; Exit; end;
      end else begin          // Get explicit numerical entry into Temp
        if Fmt^='-' then Inc(Fmt);
        Temp:=0;
        while (Fmt^ >= '0') and (Fmt^ <= '9') do begin
          Temp:=Temp*10+Ord(Fmt^)-Ord('0');
          Inc(Fmt); GotTemp:=True;
        end;
      end;
      Case Fmt^ of
        ':' : If (not GotIndex) and GotTemp then begin
                Index:=Temp; Inc(Fmt); GotIndex:=True;
              end else begin
                Result:=1; Exit;
              end;
        '.' : begin
                If GotTemp then Width:=Temp;
                Inc(Fmt); // OK, now ignore precision in any form
                If Fmt^='*' then begin Inc(Index); Inc(Format); end
                else while (Fmt^ >= '0') and (Fmt^ <= '9') do Inc(Fmt);
                Break;
              end;
        else begin
          If GotTemp then Width:=Temp;
          Break;
        end;
      end; {Case}
    until Fmt^ <> ':';
    Result:=0;
  end;

begin
  Buf:=Buffer; Fmt:=Format; FmtEnd:=Fmt+FmtLen; BufEnd:=Buf+BufLen; Index:=0;  Result:=0; // initialization
{$IFDEF DEFORMAT_EXCEPTIONS}
try
{$ENDIF}
  while ScanToFormat do   // ScanToFormat returns False if end is reached
  with SCR do begin
    Width:=0;
    {$IFDEF DEFORMAT_EXCEPTIONS} FMarker:=Fmt-1; {$ENDIF}
    Case GetNumFields of
     -1 : BadArg : begin
                   {$IFDEF DEFORMAT_EXCEPTIONS}
                     raise EConvertError.CreateFmt(SArgumentMissing,[Copy(FMarker, 1, Fmt-FMarker+1)]);
                   {$ENDIF}
                     Break;
                   end;
      1 : BadFormat : begin
                      {$IFDEF DEFORMAT_EXCEPTIONS}
                        raise EConvertError.CreateFmt(SInvalidFormat, [Copy(FMarker, 1, Fmt-FMarker+2)]);
                      {$ENDIF}
                        Break;
                      end;
    end;
    SCR:=GetSizeType(Fmt);
    Ptr:=NIL;
    if (Ftype > 0) then begin
      if Index > High(Args) then goto BadArg else Ptr:=Args[Index].VPointer;
      if Ptr=NIL then Flags:=Flags or scNoAssign; // DeFormat treats NIL as NoAssign
    end;
    if Typ = scNSpec then begin
      Flags:=Flags or sc1000Sep;
      Typ:=scFloat;
    end;
    Case Typ of
      scPChar, scFloat, scInteger, scCurrency : // Skip blank space for these formats
        begin
          while (Buf <= BufEnd) and (Buf^ <= ' ') do Inc(Buf);
          if Buf >= BufEnd then Break;
        end;
      scCharSet : if not ParseSet(Fmt, FmtEnd, theSet) then goto BadFormat;
      scChar    : if Width = 0 then Inc(Width);
    end;
    if Width = 0 then Width:=defWidth;
    if Width > BufEnd-Buf then Width := BufEnd-Buf;  // Set maximum width
    Marker:=Buf;
    Case Typ of
      scChar    : begin
                    if FType > 0 then
                      Case Args[Index].VType of
                        vtPChar, vtPointer : Move(Buf^, Ptr^, Width);
                        else goto BadArg;
                      end;
                    Inc(Buf, Width);
                  end;
      scCharSet : begin
                    for Count:=1 to Width do if (Buf^ in theSet) then Inc(Buf) else Break;
                    goto CopyStr;
                  end;
      scPChar :   begin
                    for Count:=1 to Width do if (Buf^ > ' ') then Inc(Buf) else Break;
        CopyStr :   If (FType > 0) then
                      Case Size of   // explicit size modifier has precedence
                        'l' : SetString(AnsiString(Ptr^), Marker, Buf-Marker);
                        'h' : SetString(ShortString(Ptr^), Marker, Buf-Marker);
                        else   // No size specified, use implicit sizes
                        Case Args[Index].VType of
                          vtPointer, vtPChar, vtAnsiString :
                          begin
                            Move(Marker^, Ptr^, Buf-Marker); // this may fail on vtAnsiString!
                            PChar(Ptr)[Buf-Marker]:=#0;
                            if Args[Index].VType = vtAnsiString then PInt(PChar(Ptr)-4)^:=Buf-Marker; {SetLength}
                          end;
                          vtString : SetString(ShortString(Ptr^), Marker, Buf-Marker);
                          else goto BadArg;
                        end;
                      end;
                    end;
      scInteger : begin
                    if (FType > 0) and (Args[Index].VType <> vtPointer) then goto BadArg; {Check argument conflict}
                    if (Flags and scBaseMask) <> scOctal then Flags:=Flags and not scOctal; // No implicit octal
                    Temp:=NumScan(Buf, Width, FType, Ptr, DecSep, ThSep);
                    if Temp <= 0 then begin
                    {$IFDEF DEFORMAT_EXCEPTIONS}
                      raise EConvertError.CreateFmt(SInvalidInteger, [Copy(Marker, 0, Marker-Buf+1)]);
                    {$ENDIF}
                      Break;
                    end;
                    {$IFOPT Q+}
                    if (Temp and scOverflow) <> 0 then begin
                      raise EIntOverflow.Create(SOverflow + ' while scanning ' + Copy(Buffer,1, Buf-Buffer));
                      Break;
                    end;
                    {$ENDIF}
                  end;
      scCurrency : if (Flags and scFormatted) <> 0 then begin
                     Temp:=StrToCurrF_core(Buf, Width, Currency(Ptr^), PChar(CurrencyString),
                           CurrencyFormat, NegCurrFormat, DecSep, ThSep);
                     if Temp <= 0 then begin
                     {$IFDEF DEFORMAT_EXCEPTIONS}
                       raise EConvertError.CreateFmt(SInvalidCurrency, [Copy(Marker, 0, Marker-Buf+1)]);
                     {$ENDIF}
                       Break;
                     end;
                   end else
                     if FType >= 0 then
                       Case Args[Index].Vtype of
                         vtPointer, vtCurrency : goto doFloat;
                         else goto BadArg;
                       end;
      scFloat :   begin
                    if SizeType = scFloat then Size:='L';  // Delphi default is Extended
                    if FType >= 0 then {Check argument conflict}
                      Case Args[Index].VType of
                        vtPointer : {OK};
                        vtExtended : if (Typ <> scFloat)
                                     {$IFNDEF DELPHI110}
                                        and (SizeType <> scInt64) // before Delphi 4, Comp is floating point type
                                     {$ENDIF}
                                     then goto BadArg;
                        {$IFDEF DELPHI110}
                        vtInt64    : if (SizeType <> scInt64) then goto BadArg;
                        {$ENDIF}
                        else goto BadArg;
                      end;
        doFloat:    Temp:=NumScan(Buf, Width, FType, Ptr, DecSep, ThSep);
                    if Temp <=0 then begin
                    {$IFDEF DEFORMAT_EXCEPTIONS}
                      raise EConvertError.CreateFmt(SInvalidFloat, [Copy(Marker, 0, Marker-Buf+1)]);
                    {$ENDIF}
                      Break;
                    end;
                    {$IFOPT Q+}
                    if (Temp and scOverflow) <> 0 then begin
                      raise EOverflow.Create(SOverflow + ' while scanning ' + Copy(Buffer, 1, Buf-Buffer));
                      Break;
                    end;
                    {$ENDIF}
                  end;
      else goto BadFormat;  // scIllegal or something unsupported
    end; { Case }
    If FType > 0 then Inc(Result);
    Inc(Index);
  end; { While, With }
{$IFDEF DEFORMAT_EXCEPTIONS}
finally
{$ENDIF}
  Buffer:=Buf; Format:=Fmt; // return final positions
{$IFDEF DEFORMAT_EXCEPTIONS}
end;
{$ENDIF}
end;

// ***********  scanf ************************************************//

function scGetFType( var Fmt : PChar; var Width : cardinal)  : integer;
var Temp : integer;
    NoAssign : integer;
    Marker : PChar;
begin
  Result:=scIllegal;
  If Fmt^='*' then begin
    NoAssign:=scNoAssign shl 16;
    Inc(Fmt);
  end else NoAssign:=0;
  Marker:=Fmt; Temp:=0;
  while (Fmt^ >= '0') and (Fmt^ <= '9') do begin
    Temp:=Temp*10+Ord(Fmt^)-Ord('0');
    Inc(Fmt);
  end;
  If Fmt > Marker then if Temp > 0 then Width:=Temp else Exit;
  If Fmt^ <> #0 then Result:=Integer(GetSizeType(Fmt)) or NoAssign;
end;

procedure scCopyStr(Size : char; Dest : pointer; Src : PChar; Width : integer);
begin
  Case Size of
    'l' : SetString(AnsiString(Dest^), Src, Width);
    'h' : SetString(ShortString(Dest^), Src, Width);
    else begin
      Move(Src^, Dest^, Width);
      PChar(Dest)[Width]:=#0
    end;
  end;  
end;

function Scanf_core(var Buffer: PChar; var Format : PChar;
                    Pointers : array of Pointer)
                    : Integer;

var  // Many of these will be optimized out
  Width, Count, Index : cardinal;
  Buf, Fmt, Marker : PChar;
  {$IFDEF SCANF_EXCEPTIONS} FMarker : PChar; {$ENDIF}
  Temp : integer;
  FmtEnd : PChar absolute Format;
  SCR : TscRec;
  FType : integer absolute SCR;
  theSet : TCharSet;
  BSet : byte absolute theSet;
  Ptr : Pointer;

  function ScanToFormat : boolean;
  begin
    ScanToFormat:=False;
    while Fmt^ <> #0 do begin
      case Fmt^ of
        '%' : begin
                Inc(Fmt);
                if (Fmt^='%') then  // this is "%%", check for "%" in Buf
                   if (Buf^ <> '%') then Exit else begin
                     Inc(Buf); Inc(Fmt);
                   end
                else begin
                  ScanToFormat:=True; // Start of Fmt specifier found.
                  Exit;
                end;
              end;
         #1..#32 : begin  // Skip blank space in Buf and Fmt
                     while (Fmt^>#0) and (Fmt^ <=' ') do Inc(Fmt);
                     while (Buf^>#0) and (Buf^ <=' ') do Inc(Buf);
                   end;
         else begin
           {$IFNDEF SCANF_CASE_SENSITIVE}
             if Buf^ <> Fmt^ then Exit;
           {$ELSE}
             if UpCase(Buf^) <> UpCase(Fmt^) then Exit;
           {$ENDIF}
           Inc(Buf);
           Inc(Fmt);
         end;
       end;
    end;
  end;

begin
  Fmt:=Format; FmtEnd:=Fmt+Length(Fmt); Result:=0; Buf:=Buffer; Index:=0; // initialization
  {$IFDEF SCANF_EXCEPTIONS}
  try
  {$ENDIF}
  While ScanToFormat do   // ScanToFormat returns False if end of Buf or Fmt is reached
  With SCR do begin
    Width:=0;
    {$IFDEF SCANF_EXCEPTIONS} FMarker:=Fmt-1; {$ENDIF}
    FType:=scGetFType(Fmt,Width); // GetFType returns scIllegal on any error
    Ptr:=NIL;
    If (FType > 0) then begin
      if (Integer(Index) <= High(Pointers)) then Ptr:=Pointers[Index];
      if (Ptr=NIL) then begin // scanf aborts if assignment to NIL is requested
      {$IFDEF SCANF_EXCEPTIONS}
        raise EConvertError.CreateFmt(SArgumentMissing, [Copy(FMarker-1, 0, Fmt-FMarker+1)]);
      {$ENDIF}
        Break;
      end;
    end;
    Case Typ of
      scPChar, scFloat, scInteger, scCurrency : // Skip blank space for these formats
            while (Buf^ <> #0) and (Buf^ <= ' ') do Inc(Buf);
      scCharSet : begin
                    if not ParseSet(Fmt, FmtEnd, theSet) then FType:=scIllegal
                    else BSet:=BSet and $fe;  // Mask out #0
                  end;
      scChar : if Width = 0 then Inc(Width);
    end;
    if Buf^ = #0 then Break;
    if Width=0 then Width:=defWidth;
    Marker:=Buf;
    Case Typ of
      scNspec : Case Size of
                  'H': Byte(ptr^):=Buf-Buffer;
                  'h': Word(ptr^):=Buf-Buffer;
                  'l',#0 : LongWord(ptr^):=Buf-Buffer;
                  'L'   : int64(ptr^):=Buf-Buffer;
                end;
      scChar    : begin
                    for Count:=1 to Width do if (Buf^ <> #0) then Inc(Buf) else Break;
                    If (FType > 0) then Move(Marker^, ptr^, Buf-Marker);
                  end;
      scCharSet : begin
                    for Count:=1 to Width do if (Buf^ in theSet) then Inc(Buf) else Break;
                    if (FType > 0) then scCopyStr(Size, Ptr, Marker, Buf-Marker);
                  end;
      scPChar :   begin
                    for Count:=1 to Width do if (Buf^ > ' ') then Inc(Buf) else Break;
                    if (FType > 0) then scCopyStr(Size, Ptr, Marker, Buf-Marker);
                  end;
    scInteger :  begin
                   Temp:=NumScan(Buf, Width, FType, ptr,#0,#0);
                   if Temp <= 0 then begin
                   {$IFDEF SCANF_EXCEPTIONS}
                     raise EConvertError.CreateFmt(SInvalidInteger, [Copy(Marker, 1, Buf-Marker+1)]);
                   {$ENDIF}
                     Break;
                   end;
                   {$IFOPT Q+}
                   if (Temp and scOverflow) <> 0 then begin
                     raise EIntOverflow.Create(SOverflow + ' while scanning ' + Copy(Marker,1, Buf-Marker));
                     Break;
                   end;
                   {$ENDIF}
                 end;
    scFloat   : begin
                  if SizeType = scFloat then Size:='h';   // scanf default is single
                  Temp:=NumScan(Buf, Width, FType, ptr,'.',#0); // scan in C style
                  if Temp <=0 then begin
                  {$IFDEF SCANF_EXCEPTIONS}
                    raise EConvertError.CreateFmt(SInvalidFloat, [Copy(Marker, 1, Buf-Marker+1)]);
                  {$ENDIF}
                    Break;
                  end;
                  {$IFOPT Q+}
                  if (Temp and scOverflow) <> 0 then begin
                    raise EOverflow.Create(SOverflow + ' while scanning ' + Copy(Marker,1, Buf-Marker));
                    Break;
                  end;
                  {$ENDIF}
                end;
      else begin // scIllegal etc.
        {$IFDEF SCANF_EXCEPTIONS}
        raise EConvertError.CreateFmt(SInvalidFormat, [Copy(FMarker, 1, Fmt-FMarker+1)]);
        {$ENDIF}
        Break;
      end;
    end; { Case }
    If (FType > 0) then begin
      if (Typ <> scNSpec) then Inc(Result);  // NSpec does not count!
      Inc(Index);
    end;
  end; { While, With }
{$IFDEF SCANF_EXCEPTIONS}
 finally
{$ENDIF}
  Buffer:=Buf; Format:=Fmt;// return final positions
{$IFDEF SCANF_EXCEPTIONS}
    end;
{$ENDIF}
end;


function Scanf_stream(Inp : TStream; var Format : PChar;
                      Pointers : array of Pointer)
                      : Integer;

var  
  Width, Count, Index : cardinal;
  Buf, Fmt : PChar;
  {$IFDEF SCANF_EXCEPTIONS} FMArker : PChar; {$ENDIF}
  NI, Pos, Marker : integer;
  Temp : integer;
  FmtEnd : PChar absolute Format;
  SCR : TscRec;
  FType : integer absolute SCR;
  theSet : TCharSet;
  BSet : byte absolute theSet;
  Ptr : Pointer;
  Mem : TMemoryStream;

  function GetCh : integer;
  begin
    if Inp.Read(NI, 1) < 1 then NI:=-1;
    Result:=NI;
  end;

  function ScanToFormat : boolean;
  begin
    ScanToFormat:=False;
    while Fmt^ <> #0 do begin
      Case Fmt^ of
        '%' : begin
                Inc(Fmt);
                  if (Fmt^='%') then begin // this is "%%", check for "%" in Buf
                     if (Char(GetCh) = '%') then Inc(Fmt);
                   end
                else begin
                  ScanToFormat:= (GetCh >=0); // Start of Fmt specifier found.
                  Exit;
                end;
              end;
         #1..#32 : begin  // Skip blank space in Buf and Fmt
                     while (Fmt^>#0) and (Fmt^ <=' ') do Inc(Fmt);
                     repeat until Char(GetCh) > ' ';
                     If NI < 0 then Break else Inp.Seek(-1,soFromCurrent) {UnGetCh};
                   end;
        else begin
           {$IFNDEF SCANF_CASE_SENSITIVE}
             if Char(GetCh) <> Fmt^ then Break;
           {$ELSE}
             if UpCase(Char(GetCh)) <> UpCase(Fmt^) then Break;
           {$ENDIF}
           Inc(Fmt);
        end;
      end;
    end;
  end;

begin
  NI:=0; Fmt:=Format; FmtEnd:=Fmt+Length(Fmt); Result:=0; Index:=0; // initialization
  Pos:=Inp.Position; Mem:=TMemoryStream.Create;
  {$IFDEF SCANF_EXCEPTIONS}
  try
  {$ENDIF}
  While ScanToFormat do   // ScanToFormat returns False if end reached.
                          // If True then next character in NI pending.
  With SCR do begin
    Width:=0;
    {$IFDEF SCANF_EXCEPTIONS} FMarker:=Fmt-1; {$ENDIF}
    FType:=scGetFType(Fmt,Width); // GetFType returns scIllegal on any error
    Ptr:=NIL;
    If (FType > 0) then begin
      if (Integer(Index) <= High(Pointers)) then Ptr:=Pointers[Index];
      if (Ptr=NIL) then begin // scanf aborts if assignment to NIL is requested
      {$IFDEF SCANF_EXCEPTIONS}
        raise EConvertError.CreateFmt(SArgumentMissing, [Copy(FMarker-1, 0, Fmt-FMarker+1)]);
      {$ENDIF}
        Break;
      end;
    end;
    Case Typ of
      scPChar, scFloat, scInteger, scCurrency : // Skip blank space for these formats
            while (Char(NI) <= ' ') do GetCh;
      scCharSet : begin
                    if not ParseSet(Fmt, FmtEnd, theSet) then FType:=scIllegal
                    else BSet:=BSet and $fe;  // Mask out #0
                  end;
      scChar : if Width = 0 then Inc(Width);
    end;
    if NI < 0 then Break;
    if Width=0 then Width:=defWidth;
    Marker:=Inp.Position;
    Mem.Position:=0;
    Case Typ of
      scNspec : Case Size of
                  'H': Byte(ptr^):=Marker-Pos;
                  'h': Word(ptr^):=Marker-Pos;
                  'l',#0 : LongWord(ptr^):=Marker-Pos;
                  'L'   : int64(ptr^):=Marker-Pos;
                end;
      scChar    : for Count:=1 to Width do begin
                    If (FType > 0) then begin PChar(Ptr)^:=Char(NI); Inc( PChar(Ptr)); end;
                    if GetCh < 0 then Break;
                  end;
      scCharSet : begin
                    for Count:=1 to Width do
                      if (Char(NI) in theSet) then begin
                        Mem.Write(NI,1); if GetCh < 0 then Break;
                       end else begin
                         Inp.Seek(-1,soFromCurrent) {UnGetCh}; Break;
                       end;
                    if FType > 0 then scCopyStr(Size, Ptr, Mem.Memory, Mem.Position);
                  end;
      scPChar :   begin
                    for Count:=1 to Width do
                      if (Char(NI) > ' ') then begin
                        Mem.Write(NI,1); if GetCh < 0 then Break;
                       end else begin
                         Inp.Seek(-1,soFromCurrent) {UnGetCh}; Break;
                       end;
                    if FType > 0 then scCopyStr(Size, Ptr, Mem.Memory, Mem.Position);
                  end;
     scInteger :  begin
                   for Count:=1 to Width do
                     if (Char(NI) in ['+','-','0'..'9']) then begin
                       Mem.Write(NI,1); if GetCh < 0 then Break;
                      end else begin
                        Inp.Seek(-1,soFromCurrent) {UnGetCh}; Break;
                      end;
                   Buf:=PChar(Mem.Memory);
                   Temp:=NumScan(Buf, Width, FType, ptr,#0,#0);
                   Inp.Seek(Buf-PChar(Mem.Memory)-Mem.Position, soFromCurrent);
                   if Temp <= 0 then begin
                   {$IFDEF SCANF_EXCEPTIONS}
                     raise EConvertError.CreateFmt(SInvalidInteger, [Copy(PChar(Mem.Memory), 1, Buf-PChar(Mem.Memory)+1)]);
                   {$ENDIF}
                     Break;
                   end;
                   {$IFOPT Q+}
                   if (Temp and scOverflow) <> 0 then begin
                     raise EIntOverflow.Create(SOverflow + ' while scanning ' + Copy(PChar(Mem.Memory), 1, Buf-PChar(Mem.Memory)));
                     Break;
                   end;
                   {$ENDIF}
                 end;
     scFloat   : begin
                  if SizeType = scFloat then Size:='h';   // scanf default is single
                  for Count:=1 to Width do
                    if (Char(NI) in['.','+','-','0'..'9','e','E']) then begin
                      Mem.Write(NI,1); if GetCh < 0 then Break;
                     end else begin
                       Inp.Seek(-1,soFromCurrent) {UnGetCh}; Break;
                     end;
                  Buf:=PChar(Mem.Memory);
                  Temp:=NumScan(Buf, Width, FType, ptr,'.',#0);
                  Inp.Seek(Buf-PChar(Mem.Memory)-Mem.Position, soFromCurrent);
                  if Temp <=0 then begin
                  {$IFDEF SCANF_EXCEPTIONS}
                    raise EConvertError.CreateFmt(SInvalidFloat, [Copy(PChar(Mem.Memory), 1, Buf-PChar(Mem.Memory)+1)]);
                  {$ENDIF}
                    Break;
                  end;
                  {$IFOPT Q+}
                  if (Temp and scOverflow) <> 0 then begin
                    raise EOverflow.Create(SOverflow + ' while scanning ' + Copy(PChar(Mem.Memory), 1, Buf-PChar(Mem.Memory)));
                    Break;
                  end;
                  {$ENDIF}
                end;
     else begin // scIllegal etc.
        {$IFDEF SCANF_EXCEPTIONS}
        raise EConvertError.CreateFmt(SInvalidFormat, [Copy(FMarker, 1, Fmt-FMarker+1)]);
        {$ENDIF}
        Break;
      end;
    end; { Case }
    If (FType > 0) then begin
      if (Typ <> scNSpec) then Inc(Result);  // NSpec does not count!
      Inc(Index);
    end;
    if (NI < 0) and (Result = 0) then Result:=scEOF;
  end; { While, With }
{$IFDEF SCANF_EXCEPTIONS}
 finally
{$ENDIF}
   Format:=Fmt; // return final position
   Mem.Free;    // clean up
{$IFDEF SCANF_EXCEPTIONS}
    end;
{$ENDIF}
end;


end.




