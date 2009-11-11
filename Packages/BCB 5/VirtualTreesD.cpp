//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("VirtualTreesD.res");
USERES("..\..\Design\VirtualTreesReg.dcr");
USEUNIT("..\..\Design\VirtualTreesReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("VCLX50.bpi");
USEPACKAGE("VirtualTreesR.bpi");
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
