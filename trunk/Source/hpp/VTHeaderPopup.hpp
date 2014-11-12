// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VTHeaderPopup.pas' rev: 28.00 (Windows)

#ifndef VtheaderpopupHPP
#define VtheaderpopupHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <VirtualTrees.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vtheaderpopup
{
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TVTHeaderPopupOption : unsigned char { poOriginalOrder, poAllowHideAll, poResizeToFitItem };

typedef System::Set<TVTHeaderPopupOption, TVTHeaderPopupOption::poOriginalOrder, TVTHeaderPopupOption::poResizeToFitItem> TVTHeaderPopupOptions;

enum DECLSPEC_DENUM TAddPopupItemType : unsigned char { apNormal, apDisabled, apHidden };

typedef void __fastcall (__closure *TAddHeaderPopupItemEvent)(Virtualtrees::TBaseVirtualTree* const Sender, const Virtualtrees::TColumnIndex Column, TAddPopupItemType &Cmd);

typedef void __fastcall (__closure *TColumnChangeEvent)(Virtualtrees::TBaseVirtualTree* const Sender, const Virtualtrees::TColumnIndex Column, bool Visible);

typedef Vcl::Menus::TMenuItem TVTMenuItem;

class DELPHICLASS TVTHeaderPopupMenu;
class PASCALIMPLEMENTATION TVTHeaderPopupMenu : public Vcl::Menus::TPopupMenu
{
	typedef Vcl::Menus::TPopupMenu inherited;
	
private:
	TVTHeaderPopupOptions FOptions;
	TAddHeaderPopupItemEvent FOnAddHeaderPopupItem;
	TColumnChangeEvent FOnColumnChange;
	
protected:
	virtual void __fastcall DoAddHeaderPopupItem(const Virtualtrees::TColumnIndex Column, /* out */ TAddPopupItemType &Cmd);
	virtual void __fastcall DoColumnChange(Virtualtrees::TColumnIndex Column, bool Visible);
	void __fastcall OnMenuItemClick(System::TObject* Sender);
	
public:
	virtual void __fastcall Popup(int x, int y);
	
__published:
	__property TVTHeaderPopupOptions Options = {read=FOptions, write=FOptions, default=0};
	__property TAddHeaderPopupItemEvent OnAddHeaderPopupItem = {read=FOnAddHeaderPopupItem, write=FOnAddHeaderPopupItem};
	__property TColumnChangeEvent OnColumnChange = {read=FOnColumnChange, write=FOnColumnChange};
public:
	/* TPopupMenu.Create */ inline __fastcall virtual TVTHeaderPopupMenu(System::Classes::TComponent* AOwner) : Vcl::Menus::TPopupMenu(AOwner) { }
	/* TPopupMenu.Destroy */ inline __fastcall virtual ~TVTHeaderPopupMenu(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Vtheaderpopup */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VTHEADERPOPUP)
using namespace Vtheaderpopup;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VtheaderpopupHPP
