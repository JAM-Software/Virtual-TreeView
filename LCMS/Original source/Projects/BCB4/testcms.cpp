
#pragma hdrstop
#include <condefs.h>


//---------------------------------------------------------------------------
#pragma argsused

#define main xmain
#include "..\..\testbed\testcms.c"
USELIB("..\..\Lib\BC\lcms110.lib");
//---------------------------------------------------------------------------
#undef main

int main(int argc, char *argv[])
{
        return xmain(argc, argv);
}

