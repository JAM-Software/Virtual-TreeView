//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("VirtualTrees.res");
USEUNIT("..\Source\VirtualTrees.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("VCLX50.bpi");
USEPACKAGE("ThemeManager.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
