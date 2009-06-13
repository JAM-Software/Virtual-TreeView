//----------------------------------------------------------------------------------------------------------------------
// Include file to determine which compiler is currently being used to build the project/component.
// This file is the C/C++ equivalent of the Compilers.inc file for the Delphi compiler and
// contains only C specific defines.
//
// Portions created by Mike Lischke are
// Copyright (C) 1999-2005 Mike Lischke. All Rights Reserved.
//
//----------------------------------------------------------------------------------------------------------------------
//
// This unit is released under the MIT license:
// Copyright (c) 1999-2005 Mike Lischke (support@soft-gems.net, www.soft-gems.net).
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
// OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// You are asked to give the author(s) the due credit. This means that you acknowledge the work of the author(s)
// in the product documentation, about box, help or wherever a prominent place is.
//
//----------------------------------------------------------------------------------------------------------------------
//
// The following symbols are defined:
//
// - COMPILER_1    : BCB 1.x is the compiler.
// - COMPILER_1_UP : BCB 1.x or higher is the compiler.
// - COMPILER_3    : BCB 3.x is the compiler.
// - COMPILER_3_UP : BCB 3.x or higher is the compiler.
// - COMPILER_4    : BCB 4.x is the compiler.
// - COMPILER_4_UP : BCB 4.x or higher is the compiler.
// - COMPILER_5    : BCB 5.x is the compiler.
// - COMPILER_5_UP : BCB 5.x or higher is the compiler.
// - COMPILER_6    : BCB 6.x is the compiler.
// - COMPILER_6_UP : BCB 6.x or higher is the compiler.
// - COMPILER_10    : BDS 2006 is the compiler.
// - COMPILER_10_UP : BDS 2006 or higher is the compiler.
//
// Only defined if Windows is the target:
// - CPPB        : Any version of BCB up to version 6 is being used.
// - CPPB_1      : BCB v1.x is being used.
// - CPPB_3      : BCB v3.x is being used.
// - CPPB_3_UP   : BCB v3.x or higher is being used.
// - CPPB_4      : BCB v4.x is being used.
// - CPPB_4_UP   : BCB v4.x or higher is being used.
// - CPPB_5      : BCB v5.x is being used.
// - CPPB_5_UP   : BCB v5.x or higher is being used.
// - CPPB_6      : BCB v6.x is being used.
// - CPPB_6_UP   : BCB v6.x or higher is being used.
//
//----------------------------------------------------------------------------------------------------------------------

#ifdef _Windows
  #if (__BORLANDC__ >= 0x580)
    // DELPHI and BCB are no longer defined, only COMPILER
    #define COMPILER_10
  #endif

  #if (__BORLANDC__ >= 0x560)
    #define COMPILER_6
    #define CPPB
    #define CPPB_6
  #endif

  #if (__BORLANDC__ >= 0x550)
    #define COMPILER_5
    #define CPPB
    #define CPPB_5
  #endif

  #if (__BORLANDC__ >= 0x540)
    #define COMPILER_4
    #define CPPB
    #define CPPB_4
  #endif

  #if (__BORLANDC__ >= 0x530)
    #define COMPILER_3
    #define CPPB
    #define CPPB_3
  #endif

  #if (__BORLANDC__ >= 0x520)
    #define COMPILER_2 // C++ Builder v1 compiler is really v2
    #define COMPILER_1 // C++ Builder v1 compiler is really v2
    #define CPPB
    #define CPPB_1
  #endif

  #ifdef CPPB_3
    #define CPPB_3_UP
  #endif

  #ifdef CPPB_4
    #define CPPB_3_UP
    #define CPPB_4_UP
  #endif

  #ifdef CPPB_5
    #define CPPB_3_UP
    #define CPPB_4_UP
    #define CPPB_5_UP
  #endif

  #ifdef CPPB_6
    #define CPPB_3_UP
    #define CPPB_4_UP
    #define CPPB_5_UP
    #define CPPB_6_UP
  #endif

#endif Win32

// Compiler defines not specific to a particlular platform.

#ifdef COMPILER_1
  #define COMPILER_1_UP
#endif

#ifdef COMPILER_3
  #define COMPILER_1_UP
  #define COMPILER_2_UP
  #define COMPILER_3_UP
#endif

#ifdef COMPILER_4
  #define COMPILER_1_UP
  #define COMPILER_2_UP
  #define COMPILER_3_UP
  #define COMPILER_4_UP
#endif

#ifdef COMPILER_5
  #define COMPILER_1_UP
  #define COMPILER_2_UP
  #define COMPILER_3_UP
  #define COMPILER_4_UP
  #define COMPILER_5_UP
#endif

#ifdef COMPILER_6
  #define COMPILER_1_UP
  #define COMPILER_2_UP
  #define COMPILER_3_UP
  #define COMPILER_4_UP
  #define COMPILER_5_UP
  #define COMPILER_6_UP
#endif

#ifdef COMPILER_10
  #define COMPILER_1_UP
  #define COMPILER_2_UP
  #define COMPILER_3_UP
  #define COMPILER_4_UP
  #define COMPILER_5_UP
  #define COMPILER_6_UP
  #define COMPILER_10_UP
  #define CPPB_1_UP
  #define CPPB_3_UP
  #define CPPB_4_UP
  #define CPPB_5_UP
  #define CPPB_6_UP
#endif

//----------------------------------------------------------------------------------------------------------------------


