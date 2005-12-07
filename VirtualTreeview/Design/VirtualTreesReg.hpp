// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTreesReg.pas' rev: 6.00

#ifndef VirtualTreesRegHPP
#define VirtualTreesRegHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <VTHeaderPopup.hpp>	// Pascal unit
#include <VirtualTrees.hpp>	// Pascal unit
#include <ColnEdit.hpp>	// Pascal unit
#include <PropertyCategories.hpp>	// Pascal unit
#include <VCLEditors.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Virtualtreesreg
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TVirtualTreeEditor;
class PASCALIMPLEMENTATION TVirtualTreeEditor : public Designeditors::TDefaultEditor 
{
	typedef Designeditors::TDefaultEditor inherited;
	
public:
	virtual void __fastcall Edit(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TVirtualTreeEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TDefaultEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualTreeEditor(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Virtualtreesreg */
using namespace Virtualtreesreg;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualTreesReg
