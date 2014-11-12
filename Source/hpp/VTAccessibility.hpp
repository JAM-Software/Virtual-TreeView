// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VTAccessibility.pas' rev: 28.00 (Windows)

#ifndef VtaccessibilityHPP
#define VtaccessibilityHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Winapi.ActiveX.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <Winapi.oleacc.hpp>	// Pascal unit
#include <VirtualTrees.hpp>	// Pascal unit
#include <VTAccessibilityFactory.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vtaccessibility
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TVirtualTreeAccessibility;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualTreeAccessibility : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	Virtualtrees::TVirtualStringTree* FVirtualTree;
	
public:
	__fastcall TVirtualTreeAccessibility(Virtualtrees::TVirtualStringTree* AVirtualTree);
	HRESULT __stdcall Get_accParent(/* out */ _di_IDispatch &ppdispParent);
	HRESULT __stdcall Get_accChildCount(/* out */ int &pcountChildren);
	HRESULT __stdcall Get_accChild(const System::OleVariant varChild, /* out */ _di_IDispatch &ppdispChild);
	HRESULT __stdcall Get_accName(const System::OleVariant varChild, /* out */ System::WideString &pszName);
	HRESULT __stdcall Get_accValue(const System::OleVariant varChild, /* out */ System::WideString &pszValue);
	HRESULT __stdcall Get_accDescription(const System::OleVariant varChild, /* out */ System::WideString &pszDescription);
	HRESULT __stdcall Get_accRole(const System::OleVariant varChild, /* out */ System::OleVariant &pvarRole);
	HRESULT __stdcall Get_accState(const System::OleVariant varChild, /* out */ System::OleVariant &pvarState);
	HRESULT __stdcall Get_accHelp(const System::OleVariant varChild, /* out */ System::WideString &pszHelp);
	HRESULT __stdcall Get_accHelpTopic(/* out */ System::WideString &pszHelpFile, const System::OleVariant varChild, /* out */ int &pidTopic);
	HRESULT __stdcall Get_accKeyboardShortcut(const System::OleVariant varChild, /* out */ System::WideString &pszKeyboardShortcut);
	HRESULT __stdcall Get_accFocus(/* out */ System::OleVariant &pvarChild);
	HRESULT __stdcall Get_accSelection(/* out */ System::OleVariant &pvarChildren);
	HRESULT __stdcall Get_accDefaultAction(const System::OleVariant varChild, /* out */ System::WideString &pszDefaultAction);
	HRESULT __stdcall accSelect(int flagsSelect, const System::OleVariant varChild);
	HRESULT __stdcall accLocation(/* out */ int &pxLeft, /* out */ int &pyTop, /* out */ int &pcxWidth, /* out */ int &pcyHeight, const System::OleVariant varChild);
	HRESULT __stdcall accNavigate(int navDir, const System::OleVariant varStart, /* out */ System::OleVariant &pvarEndUpAt);
	HRESULT __stdcall accHitTest(int xLeft, int yTop, /* out */ System::OleVariant &pvarChild);
	HRESULT __stdcall accDoDefaultAction(const System::OleVariant varChild);
	HRESULT __stdcall Set_accName(const System::OleVariant varChild, const System::WideString pszName);
	HRESULT __stdcall Set_accValue(const System::OleVariant varChild, const System::WideString pszValue);
	HRESULT __stdcall GetIDsOfNames(const GUID &IID, void * Names, int NameCount, int LocaleID, void * DispIDs);
	HRESULT __stdcall GetTypeInfo(int Index, int LocaleID, /* out */ void *TypeInfo);
	HRESULT __stdcall GetTypeInfoCount(/* out */ int &Count);
	HRESULT __stdcall Invoke(int DispID, const GUID &IID, int LocaleID, System::Word Flags, void *Params, void * VarResult, void * ExcepInfo, void * ArgErr);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualTreeAccessibility(void) { }
	
private:
	void *__IAccessible;	// IAccessible 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {618736E0-3C3D-11CF-810C-00AA00389B71}
	operator _di_IAccessible()
	{
		_di_IAccessible intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IAccessible*(void) { return (IAccessible*)&__IAccessible; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00020400-0000-0000-C000-000000000046}
	operator _di_IDispatch()
	{
		_di_IDispatch intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IDispatch*(void) { return (IDispatch*)&__IAccessible; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TVirtualTreeItemAccessibility;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualTreeItemAccessibility : public TVirtualTreeAccessibility
{
	typedef TVirtualTreeAccessibility inherited;
	
public:
	HIDESBASE HRESULT __stdcall Get_accParent(/* out */ _di_IDispatch &ppdispParent);
	HIDESBASE HRESULT __stdcall Get_accChildCount(/* out */ int &pcountChildren);
	HIDESBASE HRESULT __stdcall Get_accChild(const System::OleVariant varChild, /* out */ _di_IDispatch &ppdispChild);
	HIDESBASE HRESULT __stdcall Get_accName(const System::OleVariant varChild, /* out */ System::WideString &pszName);
	HIDESBASE HRESULT __stdcall Get_accValue(const System::OleVariant varChild, /* out */ System::WideString &pszValue);
	HIDESBASE HRESULT __stdcall Get_accDescription(const System::OleVariant varChild, /* out */ System::WideString &pszDescription);
	HIDESBASE HRESULT __stdcall Get_accRole(const System::OleVariant varChild, /* out */ System::OleVariant &pvarRole);
	HIDESBASE HRESULT __stdcall Get_accState(const System::OleVariant varChild, /* out */ System::OleVariant &pvarState);
	HIDESBASE HRESULT __stdcall accLocation(/* out */ int &pxLeft, /* out */ int &pyTop, /* out */ int &pcxWidth, /* out */ int &pcyHeight, const System::OleVariant varChild);
	HIDESBASE HRESULT __stdcall Get_accFocus(/* out */ System::OleVariant &pvarChild);
public:
	/* TVirtualTreeAccessibility.Create */ inline __fastcall TVirtualTreeItemAccessibility(Virtualtrees::TVirtualStringTree* AVirtualTree) : TVirtualTreeAccessibility(AVirtualTree) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVirtualTreeItemAccessibility(void) { }
	
private:
	void *__IAccessible;	// IAccessible 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {618736E0-3C3D-11CF-810C-00AA00389B71}
	operator _di_IAccessible()
	{
		_di_IAccessible intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IAccessible*(void) { return (IAccessible*)&__IAccessible; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TVTMultiColumnItemAccessibility;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTMultiColumnItemAccessibility : public TVirtualTreeItemAccessibility
{
	typedef TVirtualTreeItemAccessibility inherited;
	
private:
	HRESULT __stdcall GetItemDescription(const System::OleVariant varChild, /* out */ System::WideString &pszDescription, bool IncludeMainColumn);
	
public:
	HIDESBASE HRESULT __stdcall Get_accName(const System::OleVariant varChild, /* out */ System::WideString &pszName);
	HIDESBASE HRESULT __stdcall Get_accDescription(const System::OleVariant varChild, /* out */ System::WideString &pszDescription);
public:
	/* TVirtualTreeAccessibility.Create */ inline __fastcall TVTMultiColumnItemAccessibility(Virtualtrees::TVirtualStringTree* AVirtualTree) : TVirtualTreeItemAccessibility(AVirtualTree) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TVTMultiColumnItemAccessibility(void) { }
	
private:
	void *__IAccessible;	// IAccessible 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {618736E0-3C3D-11CF-810C-00AA00389B71}
	operator _di_IAccessible()
	{
		_di_IAccessible intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IAccessible*(void) { return (IAccessible*)&__IAccessible; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TVTDefaultAccessibleProvider;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDefaultAccessibleProvider : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
public:
	_di_IAccessible __fastcall CreateIAccessible(Virtualtrees::TBaseVirtualTree* ATree);
public:
	/* TObject.Create */ inline __fastcall TVTDefaultAccessibleProvider(void) : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVTDefaultAccessibleProvider(void) { }
	
private:
	void *__IVTAccessibleProvider;	// Vtaccessibilityfactory::IVTAccessibleProvider 
	
public:
	operator Vtaccessibilityfactory::IVTAccessibleProvider*(void) { return (Vtaccessibilityfactory::IVTAccessibleProvider*)&__IVTAccessibleProvider; }
	
};

#pragma pack(pop)

class DELPHICLASS TVTDefaultAccessibleItemProvider;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDefaultAccessibleItemProvider : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
public:
	_di_IAccessible __fastcall CreateIAccessible(Virtualtrees::TBaseVirtualTree* ATree);
public:
	/* TObject.Create */ inline __fastcall TVTDefaultAccessibleItemProvider(void) : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVTDefaultAccessibleItemProvider(void) { }
	
private:
	void *__IVTAccessibleProvider;	// Vtaccessibilityfactory::IVTAccessibleProvider 
	
public:
	operator Vtaccessibilityfactory::IVTAccessibleProvider*(void) { return (Vtaccessibilityfactory::IVTAccessibleProvider*)&__IVTAccessibleProvider; }
	
};

#pragma pack(pop)

class DELPHICLASS TVTMultiColumnAccessibleItemProvider;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTMultiColumnAccessibleItemProvider : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
public:
	_di_IAccessible __fastcall CreateIAccessible(Virtualtrees::TBaseVirtualTree* ATree);
public:
	/* TObject.Create */ inline __fastcall TVTMultiColumnAccessibleItemProvider(void) : System::TInterfacedObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TVTMultiColumnAccessibleItemProvider(void) { }
	
private:
	void *__IVTAccessibleProvider;	// Vtaccessibilityfactory::IVTAccessibleProvider 
	
public:
	operator Vtaccessibilityfactory::IVTAccessibleProvider*(void) { return (Vtaccessibilityfactory::IVTAccessibleProvider*)&__IVTAccessibleProvider; }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Vtaccessibility */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VTACCESSIBILITY)
using namespace Vtaccessibility;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VtaccessibilityHPP
