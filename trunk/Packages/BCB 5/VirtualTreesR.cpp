//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("VirtualTreesR.res");
USEUNIT("..\..\Source\VirtualTrees.pas");
USEUNIT("..\..\Source\VTAccessibilityFactory.pas");
USEUNIT("..\..\Source\VTAccessibility.pas");
USEUNIT("..\..\Source\MSAAIntf.pas");
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
