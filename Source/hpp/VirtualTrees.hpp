// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VirtualTrees.pas' rev: 28.00 (Windows)

#ifndef VirtualtreesHPP
#define VirtualtreesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.oleacc.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.ImgList.hpp>	// Pascal unit
#include <Winapi.ActiveX.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <Vcl.Printers.hpp>	// Pascal unit
#include <System.Types.hpp>	// Pascal unit
#include <Winapi.CommCtrl.hpp>	// Pascal unit
#include <Vcl.Themes.hpp>	// Pascal unit
#include <Winapi.UxTheme.hpp>	// Pascal unit
#include <Winapi.ShlObj.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <System.Generics.Collections.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------
#include <objidl.h>
#include <oleidl.h>
#include <oleacc.h>
#include <ShlObj.hpp>
#pragma link "VirtualTreesR.lib"

namespace Virtualtrees
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EVirtualTreeError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EVirtualTreeError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EVirtualTreeError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EVirtualTreeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EVirtualTreeError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EVirtualTreeError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EVirtualTreeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EVirtualTreeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EVirtualTreeError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EVirtualTreeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EVirtualTreeError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EVirtualTreeError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EVirtualTreeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EVirtualTreeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EVirtualTreeError(void) { }
	
};

#pragma pack(pop)

typedef unsigned *PCardinal;

typedef System::Word TAutoScrollInterval;

enum DECLSPEC_DENUM TVirtualNodeState : unsigned char { vsInitialized, vsChecking, vsCutOrCopy, vsDisabled, vsDeleting, vsExpanded, vsHasChildren, vsVisible, vsSelected, vsOnFreeNodeCallRequired, vsAllChildrenHidden, vsClearing, vsMultiline, vsHeightMeasured, vsToggling, vsFiltered };

typedef System::Set<TVirtualNodeState, TVirtualNodeState::vsInitialized, TVirtualNodeState::vsFiltered> TVirtualNodeStates;

enum DECLSPEC_DENUM TVirtualNodeInitState : unsigned char { ivsDisabled, ivsExpanded, ivsHasChildren, ivsMultiline, ivsSelected, ivsFiltered, ivsReInit };

typedef System::Set<TVirtualNodeInitState, TVirtualNodeInitState::ivsDisabled, TVirtualNodeInitState::ivsReInit> TVirtualNodeInitStates;

enum DECLSPEC_DENUM TScrollBarStyle : unsigned char { sbmRegular, sbm3D };

enum DECLSPEC_DENUM TVTColumnOption : unsigned char { coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coFixed, coSmartResize, coAllowFocus, coDisableAnimatedResize, coWrapCaption, coUseCaptionAlignment, coEditable };

typedef System::Set<TVTColumnOption, TVTColumnOption::coAllowClick, TVTColumnOption::coEditable> TVTColumnOptions;

enum DECLSPEC_DENUM TVTHeaderHitPosition : unsigned char { hhiNoWhere, hhiOnColumn, hhiOnIcon, hhiOnCheckbox };

typedef System::Set<TVTHeaderHitPosition, TVTHeaderHitPosition::hhiNoWhere, TVTHeaderHitPosition::hhiOnCheckbox> TVTHeaderHitPositions;

enum DECLSPEC_DENUM THitPosition : unsigned char { hiAbove, hiBelow, hiNowhere, hiOnItem, hiOnItemButton, hiOnItemButtonExact, hiOnItemCheckbox, hiOnItemIndent, hiOnItemLabel, hiOnItemLeft, hiOnItemRight, hiOnNormalIcon, hiOnStateIcon, hiToLeft, hiToRight, hiUpperSplitter, hiLowerSplitter };

typedef System::Set<THitPosition, THitPosition::hiAbove, THitPosition::hiLowerSplitter> THitPositions;

enum DECLSPEC_DENUM TCheckType : unsigned char { ctNone, ctTriStateCheckBox, ctCheckBox, ctRadioButton, ctButton };

enum DECLSPEC_DENUM TCheckState : unsigned char { csUncheckedNormal, csUncheckedPressed, csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed };

enum DECLSPEC_DENUM TCheckImageKind : unsigned char { ckLightCheck, ckDarkCheck, ckLightTick, ckDarkTick, ckFlat, ckXP, ckCustom, ckSystemFlat, ckSystemDefault };

enum DECLSPEC_DENUM TVTNodeAttachMode : unsigned char { amNoWhere, amInsertBefore, amInsertAfter, amAddChildFirst, amAddChildLast };

enum DECLSPEC_DENUM TDropMode : unsigned char { dmNowhere, dmAbove, dmOnNode, dmBelow };

enum DECLSPEC_DENUM TDragOperation : unsigned char { doCopy, doMove, doLink };

typedef System::Set<TDragOperation, TDragOperation::doCopy, TDragOperation::doLink> TDragOperations;

enum DECLSPEC_DENUM TVTImageKind : unsigned char { ikNormal, ikSelected, ikState, ikOverlay };

enum DECLSPEC_DENUM TVTHintMode : unsigned char { hmDefault, hmHint, hmHintAndDefault, hmTooltip };

enum DECLSPEC_DENUM TVTTooltipLineBreakStyle : unsigned char { hlbDefault, hlbForceSingleLine, hlbForceMultiLine };

typedef System::Set<System::Uitypes::TMouseButton, _DELPHI_SET_ENUMERATOR(System::Uitypes::TMouseButton::mbLeft), _DELPHI_SET_ENUMERATOR(System::Uitypes::TMouseButton::mbMiddle)> TMouseButtons;

enum DECLSPEC_DENUM TItemEraseAction : unsigned char { eaColor, eaDefault, eaNone };

enum DECLSPEC_DENUM TVTPaintOption : unsigned char { toHideFocusRect, toHideSelection, toHotTrack, toPopupMode, toShowBackground, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toFullVertGridLines, toAlwaysHideSelection, toUseBlendedSelection, toStaticBackground, toChildrenAbove, toFixedIndent, toUseExplorerTheme, toHideTreeLinesIfThemed, toShowFilteredNodes };

typedef System::Set<TVTPaintOption, TVTPaintOption::toHideFocusRect, TVTPaintOption::toShowFilteredNodes> TVTPaintOptions;

enum DECLSPEC_DENUM TVTAnimationOption : unsigned char { toAnimatedToggle, toAdvancedAnimatedToggle };

typedef System::Set<TVTAnimationOption, TVTAnimationOption::toAnimatedToggle, TVTAnimationOption::toAdvancedAnimatedToggle> TVTAnimationOptions;

enum DECLSPEC_DENUM TVTAutoOption : unsigned char { toAutoDropExpand, toAutoExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale, toAutoFreeOnCollapse, toDisableAutoscrollOnEdit, toAutoBidiColumnOrdering };

typedef System::Set<TVTAutoOption, TVTAutoOption::toAutoDropExpand, TVTAutoOption::toAutoBidiColumnOrdering> TVTAutoOptions;

enum DECLSPEC_DENUM TVTSelectionOption : unsigned char { toDisableDrawSelection, toExtendedFocus, toFullRowSelect, toLevelSelectConstraint, toMiddleClickSelect, toMultiSelect, toRightClickSelect, toSiblingSelectConstraint, toCenterScrollIntoView, toSimpleDrawSelection, toAlwaysSelectNode, toRestoreSelection };

typedef System::Set<TVTSelectionOption, TVTSelectionOption::toDisableDrawSelection, TVTSelectionOption::toRestoreSelection> TVTSelectionOptions;

enum DECLSPEC_DENUM TVTMiscOption : unsigned char { toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toReadOnly, toVariableNodeHeight, toFullRowDrag, toNodeHeightResize, toNodeHeightDblClickResize, toEditOnClick, toEditOnDblClick, toReverseFullExpandHotKey };

typedef System::Set<TVTMiscOption, TVTMiscOption::toAcceptOLEDrop, TVTMiscOption::toReverseFullExpandHotKey> TVTMiscOptions;

enum DECLSPEC_DENUM TVTExportMode : unsigned char { emAll, emChecked, emUnchecked, emVisibleDueToExpansion, emSelected };

enum DECLSPEC_DENUM TVTOperationKind : unsigned char { okAutoFitColumns, okGetMaxColumnWidth, okSortNode, okSortTree };

typedef System::Set<TVTOperationKind, TVTOperationKind::okAutoFitColumns, TVTOperationKind::okSortTree> TVTOperationKinds;

typedef System::TMetaClass* TVirtualTreeClass;

struct TVirtualNode;
typedef TVirtualNode *PVirtualNode;

typedef int TColumnIndex;

typedef unsigned TColumnPosition;

struct DECLSPEC_DRECORD TCacheEntry
{
public:
	TVirtualNode *Node;
	unsigned AbsoluteTop;
};


typedef System::DynamicArray<TCacheEntry> TCache;

typedef System::DynamicArray<PVirtualNode> TNodeArray;

class DELPHICLASS TCustomVirtualTreeOptions;
class DELPHICLASS TBaseVirtualTree;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomVirtualTreeOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TBaseVirtualTree* FOwner;
	TVTPaintOptions FPaintOptions;
	TVTAnimationOptions FAnimationOptions;
	TVTAutoOptions FAutoOptions;
	TVTSelectionOptions FSelectionOptions;
	TVTMiscOptions FMiscOptions;
	TVTExportMode FExportMode;
	void __fastcall SetAnimationOptions(const TVTAnimationOptions Value);
	void __fastcall SetAutoOptions(const TVTAutoOptions Value);
	void __fastcall SetMiscOptions(const TVTMiscOptions Value);
	void __fastcall SetPaintOptions(const TVTPaintOptions Value);
	void __fastcall SetSelectionOptions(const TVTSelectionOptions Value);
	
protected:
	__property TVTAnimationOptions AnimationOptions = {read=FAnimationOptions, write=SetAnimationOptions, default=0};
	__property TVTAutoOptions AutoOptions = {read=FAutoOptions, write=SetAutoOptions, default=1369};
	__property TVTExportMode ExportMode = {read=FExportMode, write=FExportMode, default=0};
	__property TVTMiscOptions MiscOptions = {read=FMiscOptions, write=SetMiscOptions, default=16809};
	__property TVTPaintOptions PaintOptions = {read=FPaintOptions, write=SetPaintOptions, default=7008};
	__property TVTSelectionOptions SelectionOptions = {read=FSelectionOptions, write=SetSelectionOptions, default=0};
	
public:
	__fastcall virtual TCustomVirtualTreeOptions(TBaseVirtualTree* AOwner);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	__property TBaseVirtualTree* Owner = {read=FOwner};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCustomVirtualTreeOptions(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TTreeOptionsClass;

class DELPHICLASS TVirtualTreeOptions;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualTreeOptions : public TCustomVirtualTreeOptions
{
	typedef TCustomVirtualTreeOptions inherited;
	
__published:
	__property AnimationOptions = {default=0};
	__property AutoOptions = {default=1369};
	__property ExportMode = {default=0};
	__property MiscOptions = {default=16809};
	__property PaintOptions = {default=7008};
	__property SelectionOptions = {default=0};
public:
	/* TCustomVirtualTreeOptions.Create */ inline __fastcall virtual TVirtualTreeOptions(TBaseVirtualTree* AOwner) : TCustomVirtualTreeOptions(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TVirtualTreeOptions(void) { }
	
};

#pragma pack(pop)

struct TVTReference;
typedef TVTReference *PVTReference;

struct DECLSPEC_DRECORD TVTReference
{
public:
	unsigned Process;
	TBaseVirtualTree* Tree;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TVirtualNode
{
private:
	struct DECLSPEC_DRECORD _TVirtualNode__1
	{
	};
	
	
	
public:
	unsigned Index;
	unsigned ChildCount;
	System::Word NodeHeight;
	TVirtualNodeStates States;
	System::Byte Align;
	TCheckState CheckState;
	TCheckType CheckType;
	System::Byte Dummy;
	unsigned TotalCount;
	unsigned TotalHeight;
	TVirtualNode *Parent;
	TVirtualNode *PrevSibling;
	TVirtualNode *NextSibling;
	TVirtualNode *FirstChild;
	TVirtualNode *LastChild;
	_TVirtualNode__1 Data;
};
#pragma pack(pop)


struct DECLSPEC_DRECORD TVTHeaderHitInfo
{
public:
	int X;
	int Y;
	System::Uitypes::TMouseButton Button;
	System::Classes::TShiftState Shift;
	TColumnIndex Column;
	TVTHeaderHitPositions HitPosition;
};


struct DECLSPEC_DRECORD THitInfo
{
public:
	TVirtualNode *HitNode;
	THitPositions HitPositions;
	TColumnIndex HitColumn;
	System::Types::TPoint HitPoint;
};


enum DECLSPEC_DENUM Virtualtrees__4 : unsigned char { sdLeft, sdUp, sdRight, sdDown };

typedef System::Set<Virtualtrees__4, Virtualtrees__4::sdLeft, Virtualtrees__4::sdDown> TScrollDirections;

typedef System::DynamicArray<tagFORMATETC> TFormatEtcArray;

typedef System::DynamicArray<System::Word> TFormatArray;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TInternalStgMedium
{
public:
	System::Word Format;
	tagSTGMEDIUM Medium;
};
#pragma pack(pop)


typedef System::DynamicArray<TInternalStgMedium> TInternalStgMediumArray;

class DELPHICLASS TEnumFormatEtc;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TEnumFormatEtc : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TBaseVirtualTree* FTree;
	TFormatEtcArray FFormatEtcArray;
	int FCurrentIndex;
	
public:
	__fastcall TEnumFormatEtc(TBaseVirtualTree* Tree, TFormatEtcArray AFormatEtcArray);
	HRESULT __stdcall Clone(/* out */ _di_IEnumFORMATETC &Enum);
	HRESULT __stdcall Next(int celt, /* out */ void *elt, System::PLongInt pceltFetched);
	HRESULT __stdcall Reset(void);
	HRESULT __stdcall Skip(int celt);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TEnumFormatEtc(void) { }
	
private:
	void *__IEnumFORMATETC;	// IEnumFORMATETC 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000103-0000-0000-C000-000000000046}
	operator _di_IEnumFORMATETC()
	{
		_di_IEnumFORMATETC intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IEnumFORMATETC*(void) { return (IEnumFORMATETC*)&__IEnumFORMATETC; }
	#endif
	
};

#pragma pack(pop)

__interface IVTDragManager;
typedef System::DelphiInterface<IVTDragManager> _di_IVTDragManager;
__interface  INTERFACE_UUID("{C4B25559-14DA-446B-8901-0C879000EB16}") IVTDragManager  : public System::IInterface 
{
	
public:
	virtual void __stdcall ForceDragLeave(void) = 0 ;
	virtual _di_IDataObject __stdcall GetDataObject(void) = 0 ;
	virtual TBaseVirtualTree* __stdcall GetDragSource(void) = 0 ;
	virtual bool __stdcall GetDropTargetHelperSupported(void) = 0 ;
	virtual bool __stdcall GetIsDropTarget(void) = 0 ;
	__property _di_IDataObject DataObject = {read=GetDataObject};
	__property TBaseVirtualTree* DragSource = {read=GetDragSource};
	__property bool DropTargetHelperSupported = {read=GetDropTargetHelperSupported};
	__property bool IsDropTarget = {read=GetIsDropTarget};
};

class DELPHICLASS TVTDataObject;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDataObject : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TBaseVirtualTree* FOwner;
	bool FForClipboard;
	TFormatEtcArray FFormatEtcArray;
	TInternalStgMediumArray FInternalStgMediumArray;
	_di_IDataAdviseHolder FAdviseHolder;
	
protected:
	System::_di_IInterface __fastcall CanonicalIUnknown(System::_di_IInterface TestUnknown);
	bool __fastcall EqualFormatEtc(const tagFORMATETC &FormatEtc1, const tagFORMATETC &FormatEtc2);
	int __fastcall FindFormatEtc(const tagFORMATETC &TestFormatEtc, const TFormatEtcArray FormatEtcArray);
	Winapi::Activex::PStgMedium __fastcall FindInternalStgMedium(System::Word Format);
	NativeUInt __fastcall HGlobalClone(NativeUInt HGlobal);
	bool __fastcall RenderInternalOLEData(const tagFORMATETC &FormatEtcIn, tagSTGMEDIUM &Medium, HRESULT &OLEResult);
	HRESULT __fastcall StgMediumIncRef(const tagSTGMEDIUM &InStgMedium, tagSTGMEDIUM &OutStgMedium, bool CopyInMedium, _di_IDataObject DataObject);
	__property bool ForClipboard = {read=FForClipboard, nodefault};
	__property TFormatEtcArray FormatEtcArray = {read=FFormatEtcArray, write=FFormatEtcArray};
	__property TInternalStgMediumArray InternalStgMediumArray = {read=FInternalStgMediumArray, write=FInternalStgMediumArray};
	__property TBaseVirtualTree* Owner = {read=FOwner};
	
public:
	__fastcall virtual TVTDataObject(TBaseVirtualTree* AOwner, bool ForClipboard);
	__fastcall virtual ~TVTDataObject(void);
	virtual HRESULT __stdcall DAdvise(const tagFORMATETC &FormatEtc, int advf, const _di_IAdviseSink advSink, /* out */ int &dwConnection);
	virtual HRESULT __stdcall DUnadvise(int dwConnection);
	virtual HRESULT __stdcall EnumDAdvise(/* out */ _di_IEnumSTATDATA &enumAdvise);
	virtual HRESULT __stdcall EnumFormatEtc(int Direction, /* out */ _di_IEnumFORMATETC &EnumFormatEtc);
	virtual HRESULT __stdcall GetCanonicalFormatEtc(const tagFORMATETC &FormatEtc, /* out */ tagFORMATETC &FormatEtcOut);
	virtual HRESULT __stdcall GetData(const tagFORMATETC &FormatEtcIn, /* out */ tagSTGMEDIUM &Medium);
	virtual HRESULT __stdcall GetDataHere(const tagFORMATETC &FormatEtc, /* out */ tagSTGMEDIUM &Medium);
	virtual HRESULT __stdcall QueryGetData(const tagFORMATETC &FormatEtc);
	virtual HRESULT __stdcall SetData(const tagFORMATETC &FormatEtc, tagSTGMEDIUM &Medium, BOOL DoRelease);
private:
	void *__IDataObject;	// IDataObject 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {0000010E-0000-0000-C000-000000000046}
	operator _di_IDataObject()
	{
		_di_IDataObject intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IDataObject*(void) { return (IDataObject*)&__IDataObject; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TVTDragManager;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDragManager : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TBaseVirtualTree* FOwner;
	TBaseVirtualTree* FDragSource;
	bool FIsDropTarget;
	_di_IDataObject FDataObject;
	_di_IDropTargetHelper FDropTargetHelper;
	BOOL FFullDragging;
	_di_IDataObject __stdcall GetDataObject(void);
	TBaseVirtualTree* __stdcall GetDragSource(void);
	bool __stdcall GetDropTargetHelperSupported(void);
	bool __stdcall GetIsDropTarget(void);
	
public:
	__fastcall virtual TVTDragManager(TBaseVirtualTree* AOwner);
	__fastcall virtual ~TVTDragManager(void);
	HRESULT __stdcall DragEnter(const _di_IDataObject DataObject, int KeyState, const System::Types::TPoint Pt, int &Effect);
	HRESULT __stdcall DragLeave(void);
	HRESULT __stdcall DragOver(int KeyState, const System::Types::TPoint Pt, int &Effect);
	HRESULT __stdcall Drop(const _di_IDataObject DataObject, int KeyState, const System::Types::TPoint Pt, int &Effect);
	void __stdcall ForceDragLeave(void);
	HRESULT __stdcall GiveFeedback(int Effect);
	HRESULT __stdcall QueryContinueDrag(BOOL EscapePressed, int KeyState);
private:
	void *__IDropTarget;	// IDropTarget 
	void *__IDropSource;	// IDropSource 
	void *__IVTDragManager;	// IVTDragManager 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000122-0000-0000-C000-000000000046}
	operator _di_IDropTarget()
	{
		_di_IDropTarget intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IDropTarget*(void) { return (IDropTarget*)&__IDropTarget; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {00000121-0000-0000-C000-000000000046}
	operator _di_IDropSource()
	{
		_di_IDropSource intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IDropSource*(void) { return (IDropSource*)&__IDropSource; }
	#endif
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {C4B25559-14DA-446B-8901-0C879000EB16}
	operator _di_IVTDragManager()
	{
		_di_IVTDragManager intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IVTDragManager*(void) { return (IVTDragManager*)&__IVTDragManager; }
	#endif
	
};

#pragma pack(pop)

struct TVTHintData;
typedef TVTHintData *PVTHintData;

struct DECLSPEC_DRECORD TVTHintData
{
public:
	TBaseVirtualTree* Tree;
	TVirtualNode *Node;
	TColumnIndex Column;
	System::Types::TRect HintRect;
	System::UnicodeString DefaultHint;
	System::UnicodeString HintText;
	System::Classes::TBiDiMode BidiMode;
	System::Classes::TAlignment Alignment;
	TVTTooltipLineBreakStyle LineBreakStyle;
};


enum DECLSPEC_DENUM THintAnimationType : unsigned char { hatNone, hatFade, hatSlide, hatSystemDefault };

class DELPHICLASS TVirtualTreeHintWindow;
class PASCALIMPLEMENTATION TVirtualTreeHintWindow : public Vcl::Controls::THintWindow
{
	typedef Vcl::Controls::THintWindow inherited;
	
private:
	TVTHintData FHintData;
	Vcl::Graphics::TBitmap* FBackground;
	Vcl::Graphics::TBitmap* FDrawBuffer;
	Vcl::Graphics::TBitmap* FTarget;
	int FTextHeight;
	bool __fastcall AnimationCallback(int Step, int StepSize, void * Data);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	bool __fastcall GetHintWindowDestroyed(void);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMShowWindow(Winapi::Messages::TWMShowWindow &Message);
	
protected:
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	void __fastcall InternalPaint(int Step, int StepSize);
	virtual void __fastcall Paint(void);
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground};
	__property Vcl::Graphics::TBitmap* DrawBuffer = {read=FDrawBuffer};
	__property TVTHintData HintData = {read=FHintData};
	__property bool HintWindowDestroyed = {read=GetHintWindowDestroyed, nodefault};
	__property Vcl::Graphics::TBitmap* Target = {read=FTarget};
	__property int TextHeight = {read=FTextHeight, nodefault};
	
public:
	__fastcall virtual TVirtualTreeHintWindow(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TVirtualTreeHintWindow(void);
	virtual void __fastcall ActivateHint(const System::Types::TRect &Rect, const System::UnicodeString AHint);
	virtual System::Types::TRect __fastcall CalcHintRect(int MaxWidth, const System::UnicodeString AHint, void * AData);
	virtual bool __fastcall IsHintMsg(tagMSG &Msg);
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualTreeHintWindow(HWND ParentWindow) : Vcl::Controls::THintWindow(ParentWindow) { }
	
};


typedef System::Byte TVTTransparency;

typedef System::Int8 TVTBias;

enum DECLSPEC_DENUM TVTDragMoveRestriction : unsigned char { dmrNone, dmrHorizontalOnly, dmrVerticalOnly };

enum DECLSPEC_DENUM Virtualtrees__9 : unsigned char { disHidden, disInDrag, disPrepared, disSystemSupport };

typedef System::Set<Virtualtrees__9, Virtualtrees__9::disHidden, Virtualtrees__9::disSystemSupport> TVTDragImageStates;

class DELPHICLASS TVTDragImage;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTDragImage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TBaseVirtualTree* FOwner;
	Vcl::Graphics::TBitmap* FBackImage;
	Vcl::Graphics::TBitmap* FAlphaImage;
	Vcl::Graphics::TBitmap* FDragImage;
	System::Types::TPoint FImagePosition;
	System::Types::TPoint FLastPosition;
	TVTTransparency FTransparency;
	TVTBias FPreBlendBias;
	TVTBias FPostBlendBias;
	bool FFade;
	TVTDragMoveRestriction FRestriction;
	System::Uitypes::TColor FColorKey;
	TVTDragImageStates FStates;
	bool __fastcall GetVisible(void);
	
protected:
	void __fastcall InternalShowDragImage(HDC ScreenDC);
	void __fastcall MakeAlphaChannel(Vcl::Graphics::TBitmap* Source, Vcl::Graphics::TBitmap* Target);
	
public:
	__fastcall TVTDragImage(TBaseVirtualTree* AOwner);
	__fastcall virtual ~TVTDragImage(void);
	bool __fastcall DragTo(const System::Types::TPoint &P, bool ForceRepaint);
	void __fastcall EndDrag(void);
	System::Types::TRect __fastcall GetDragImageRect(void);
	void __fastcall HideDragImage(void);
	void __fastcall PrepareDrag(Vcl::Graphics::TBitmap* DragImage, const System::Types::TPoint &ImagePosition, const System::Types::TPoint &HotSpot, const _di_IDataObject DataObject);
	void __fastcall RecaptureBackground(TBaseVirtualTree* Tree, const System::Types::TRect &R, HRGN VisibleRegion, bool CaptureNCArea, bool ReshowDragImage);
	void __fastcall ShowDragImage(void);
	bool __fastcall WillMove(const System::Types::TPoint &P);
	__property System::Uitypes::TColor ColorKey = {read=FColorKey, write=FColorKey, default=-16777211};
	__property bool Fade = {read=FFade, write=FFade, default=0};
	__property TVTDragMoveRestriction MoveRestriction = {read=FRestriction, write=FRestriction, default=0};
	__property TVTBias PostBlendBias = {read=FPostBlendBias, write=FPostBlendBias, default=0};
	__property TVTBias PreBlendBias = {read=FPreBlendBias, write=FPreBlendBias, default=0};
	__property TVTTransparency Transparency = {read=FTransparency, write=FTransparency, default=128};
	__property bool Visible = {read=GetVisible, nodefault};
};

#pragma pack(pop)

enum DECLSPEC_DENUM TVirtualTreeColumnStyle : unsigned char { vsText, vsOwnerDraw };

enum DECLSPEC_DENUM TVTHeaderColumnLayout : unsigned char { blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom };

enum DECLSPEC_DENUM TSortDirection : unsigned char { sdAscending, sdDescending };

class DELPHICLASS TVirtualTreeColumn;
class DELPHICLASS TVirtualTreeColumns;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualTreeColumn : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FText;
	System::UnicodeString FHint;
	int FLeft;
	int FWidth;
	TColumnPosition FPosition;
	int FMinWidth;
	int FMaxWidth;
	TVirtualTreeColumnStyle FStyle;
	System::Uitypes::TImageIndex FImageIndex;
	System::Classes::TBiDiMode FBiDiMode;
	TVTHeaderColumnLayout FLayout;
	int FMargin;
	int FSpacing;
	TVTColumnOptions FOptions;
	NativeInt FTag;
	System::Classes::TAlignment FAlignment;
	System::Classes::TAlignment FCaptionAlignment;
	int FLastWidth;
	System::Uitypes::TColor FColor;
	bool FBonusPixel;
	float FSpringRest;
	System::UnicodeString FCaptionText;
	bool FCheckBox;
	TCheckType FCheckType;
	TCheckState FCheckState;
	System::Types::TRect FImageRect;
	bool FHasImage;
	TSortDirection FDefaultSortDirection;
	System::Classes::TAlignment __fastcall GetCaptionAlignment(void);
	int __fastcall GetLeft(void);
	bool __fastcall IsBiDiModeStored(void);
	bool __fastcall IsCaptionAlignmentStored(void);
	bool __fastcall IsColorStored(void);
	void __fastcall SetAlignment(const System::Classes::TAlignment Value);
	void __fastcall SetBiDiMode(System::Classes::TBiDiMode Value);
	void __fastcall SetCaptionAlignment(const System::Classes::TAlignment Value);
	void __fastcall SetCheckBox(bool Value);
	void __fastcall SetCheckState(TCheckState Value);
	void __fastcall SetCheckType(TCheckType Value);
	void __fastcall SetColor(const System::Uitypes::TColor Value);
	void __fastcall SetImageIndex(System::Uitypes::TImageIndex Value);
	void __fastcall SetLayout(TVTHeaderColumnLayout Value);
	void __fastcall SetMargin(int Value);
	void __fastcall SetMaxWidth(int Value);
	void __fastcall SetMinWidth(int Value);
	void __fastcall SetOptions(TVTColumnOptions Value);
	void __fastcall SetPosition(TColumnPosition Value);
	void __fastcall SetSpacing(int Value);
	void __fastcall SetStyle(TVirtualTreeColumnStyle Value);
	void __fastcall SetWidth(int Value);
	
protected:
	void __fastcall ComputeHeaderLayout(HDC DC, const System::Types::TRect &Client, bool UseHeaderGlyph, bool UseSortGlyph, System::Types::TPoint &HeaderGlyphPos, System::Types::TPoint &SortGlyphPos, System::Types::TSize &SortGlyphSize, System::Types::TRect &TextBounds, unsigned DrawFormat, bool CalculateTextRect = false);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall GetAbsoluteBounds(int &Left, int &Right);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	virtual System::UnicodeString __fastcall GetText(void);
	virtual void __fastcall SetText(const System::UnicodeString Value);
	HIDESBASE TVirtualTreeColumns* __fastcall GetOwner(void);
	void __fastcall ReadHint(System::Classes::TReader* Reader);
	void __fastcall ReadText(System::Classes::TReader* Reader);
	void __fastcall WriteHint(System::Classes::TWriter* Writer);
	void __fastcall WriteText(System::Classes::TWriter* Writer);
	__property bool HasImage = {read=FHasImage, nodefault};
	__property System::Types::TRect ImageRect = {read=FImageRect};
	
public:
	__fastcall virtual TVirtualTreeColumn(System::Classes::TCollection* Collection);
	__fastcall virtual ~TVirtualTreeColumn(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall Equals(System::TObject* OtherColumnObj);
	virtual System::Types::TRect __fastcall GetRect(void);
	void __fastcall LoadFromStream(System::Classes::TStream* const Stream, int Version);
	void __fastcall ParentBiDiModeChanged(void);
	void __fastcall ParentColorChanged(void);
	void __fastcall RestoreLastWidth(void);
	void __fastcall SaveToStream(System::Classes::TStream* const Stream);
	bool __fastcall UseRightToLeftReading(void);
	__property int Left = {read=GetLeft, nodefault};
	__property TVirtualTreeColumns* Owner = {read=GetOwner};
	
__published:
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property System::Classes::TBiDiMode BiDiMode = {read=FBiDiMode, write=SetBiDiMode, stored=IsBiDiModeStored, nodefault};
	__property System::Classes::TAlignment CaptionAlignment = {read=GetCaptionAlignment, write=SetCaptionAlignment, stored=IsCaptionAlignmentStored, default=0};
	__property System::UnicodeString CaptionText = {read=FCaptionText, stored=false};
	__property TCheckType CheckType = {read=FCheckType, write=SetCheckType, default=2};
	__property TCheckState CheckState = {read=FCheckState, write=SetCheckState, default=0};
	__property bool CheckBox = {read=FCheckBox, write=SetCheckBox, default=0};
	__property System::Uitypes::TColor Color = {read=FColor, write=SetColor, stored=IsColorStored, nodefault};
	__property TSortDirection DefaultSortDirection = {read=FDefaultSortDirection, write=FDefaultSortDirection, default=0};
	__property System::UnicodeString Hint = {read=FHint, write=FHint, stored=false};
	__property System::Uitypes::TImageIndex ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property TVTHeaderColumnLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property int Margin = {read=FMargin, write=SetMargin, default=4};
	__property int MaxWidth = {read=FMaxWidth, write=SetMaxWidth, default=10000};
	__property int MinWidth = {read=FMinWidth, write=SetMinWidth, default=10};
	__property TVTColumnOptions Options = {read=FOptions, write=SetOptions, default=35071};
	__property TColumnPosition Position = {read=FPosition, write=SetPosition, nodefault};
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=3};
	__property TVirtualTreeColumnStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property NativeInt Tag = {read=FTag, write=FTag, default=0};
	__property System::UnicodeString Text = {read=GetText, write=SetText, stored=false};
	__property int Width = {read=FWidth, write=SetWidth, default=50};
};

#pragma pack(pop)

typedef System::TMetaClass* TVirtualTreeColumnClass;

typedef System::DynamicArray<TVirtualTreeColumn*> TColumnsArray;

typedef System::DynamicArray<unsigned> TCardinalArray;

typedef System::DynamicArray<TColumnIndex> TIndexArray;

class DELPHICLASS TVTHeader;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVirtualTreeColumns : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TVirtualTreeColumn* operator[](TColumnIndex Index) { return Items[Index]; }
	
private:
	TVTHeader* FHeader;
	Vcl::Graphics::TBitmap* FHeaderBitmap;
	TColumnIndex FHoverIndex;
	TColumnIndex FDownIndex;
	TColumnIndex FTrackIndex;
	TColumnIndex FClickIndex;
	bool FCheckBoxHit;
	TIndexArray FPositionToIndex;
	int FDefaultWidth;
	bool FNeedPositionsFix;
	bool FClearing;
	HIDESBASE int __fastcall GetCount(void);
	HIDESBASE TVirtualTreeColumn* __fastcall GetItem(TColumnIndex Index);
	bool __fastcall GetNewIndex(const System::Types::TPoint &P, TColumnIndex &OldIndex);
	void __fastcall SetDefaultWidth(int Value);
	HIDESBASE void __fastcall SetItem(TColumnIndex Index, TVirtualTreeColumn* Value);
	
protected:
	TColumnIndex FDragIndex;
	TColumnIndex FDropTarget;
	bool FDropBefore;
	void __fastcall AdjustAutoSize(TColumnIndex CurrentIndex, bool Force = false);
	TColumnIndex __fastcall AdjustDownColumn(const System::Types::TPoint &P);
	bool __fastcall AdjustHoverColumn(const System::Types::TPoint &P);
	void __fastcall AdjustPosition(TVirtualTreeColumn* Column, unsigned Position);
	bool __fastcall CanSplitterResize(const System::Types::TPoint &P, TColumnIndex Column);
	virtual void __fastcall DoCanSplitterResize(const System::Types::TPoint &P, TColumnIndex Column, bool &Allowed);
	void __fastcall DrawButtonText(HDC DC, System::UnicodeString Caption, const System::Types::TRect &Bounds, bool Enabled, bool Hot, unsigned DrawFormat, bool WrapCaption);
	void __fastcall FixPositions(void);
	int __fastcall GetColumnAndBounds(const System::Types::TPoint &P, int &ColumnLeft, int &ColumnRight, bool Relative = true);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall HandleClick(const System::Types::TPoint &P, System::Uitypes::TMouseButton Button, bool Force, bool DblClick);
	void __fastcall IndexChanged(int OldIndex, int NewIndex);
	void __fastcall InitializePositionArray(void);
	virtual void __fastcall Notify(System::Classes::TCollectionItem* Item, System::Classes::TCollectionNotification Action);
	void __fastcall ReorderColumns(bool RTL);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	void __fastcall UpdatePositions(bool Force = false);
	__property Vcl::Graphics::TBitmap* HeaderBitmap = {read=FHeaderBitmap};
	__property TIndexArray PositionToIndex = {read=FPositionToIndex};
	__property TColumnIndex HoverIndex = {read=FHoverIndex, nodefault};
	__property TColumnIndex DownIndex = {read=FDownIndex, nodefault};
	__property bool CheckBoxHit = {read=FCheckBoxHit, nodefault};
	
public:
	__fastcall virtual TVirtualTreeColumns(TVTHeader* AOwner);
	__fastcall virtual ~TVirtualTreeColumns(void);
	HIDESBASE virtual TVirtualTreeColumn* __fastcall Add(void);
	void __fastcall AnimatedResize(TColumnIndex Column, int NewWidth);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE virtual void __fastcall Clear(void);
	virtual TColumnIndex __fastcall ColumnFromPosition(const System::Types::TPoint &P, bool Relative = true)/* overload */;
	virtual TColumnIndex __fastcall ColumnFromPosition(TColumnPosition PositionIndex)/* overload */;
	virtual bool __fastcall Equals(System::TObject* OtherColumnsObj);
	void __fastcall GetColumnBounds(TColumnIndex Column, int &Left, int &Right);
	TColumnIndex __fastcall GetFirstVisibleColumn(bool ConsiderAllowFocus = false);
	TColumnIndex __fastcall GetLastVisibleColumn(bool ConsiderAllowFocus = false);
	TColumnIndex __fastcall GetFirstColumn(void);
	TColumnIndex __fastcall GetNextColumn(TColumnIndex Column);
	TColumnIndex __fastcall GetNextVisibleColumn(TColumnIndex Column, bool ConsiderAllowFocus = false);
	TColumnIndex __fastcall GetPreviousColumn(TColumnIndex Column);
	TColumnIndex __fastcall GetPreviousVisibleColumn(TColumnIndex Column, bool ConsiderAllowFocus = false);
	int __fastcall GetScrollWidth(void);
	TColumnsArray __fastcall GetVisibleColumns(void);
	int __fastcall GetVisibleFixedWidth(void);
	bool __fastcall IsValidColumn(TColumnIndex Column);
	void __fastcall LoadFromStream(System::Classes::TStream* const Stream, int Version);
	virtual void __fastcall PaintHeader(HDC DC, const System::Types::TRect &R, int HOffset)/* overload */;
	virtual void __fastcall PaintHeader(Vcl::Graphics::TCanvas* TargetCanvas, const System::Types::TRect &R, const System::Types::TPoint &Target, int RTLOffset = 0x0)/* overload */;
	void __fastcall SaveToStream(System::Classes::TStream* const Stream);
	int __fastcall TotalWidth(void);
	__property int Count = {read=GetCount, nodefault};
	__property TColumnIndex ClickIndex = {read=FClickIndex, nodefault};
	__property int DefaultWidth = {read=FDefaultWidth, write=SetDefaultWidth, default=50};
	__property TVirtualTreeColumn* Items[TColumnIndex Index] = {read=GetItem, write=SetItem/*, default*/};
	__property TVTHeader* Header = {read=FHeader};
	__property TColumnIndex TrackIndex = {read=FTrackIndex, nodefault};
};

#pragma pack(pop)

typedef System::TMetaClass* TVirtualTreeColumnsClass;

typedef System::Int8 TVTConstraintPercent;

class DELPHICLASS TVTFixedAreaConstraints;
class PASCALIMPLEMENTATION TVTFixedAreaConstraints : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TVTHeader* FHeader;
	TVTConstraintPercent FMaxHeightPercent;
	TVTConstraintPercent FMaxWidthPercent;
	TVTConstraintPercent FMinHeightPercent;
	TVTConstraintPercent FMinWidthPercent;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall SetConstraints(int Index, TVTConstraintPercent Value);
	
protected:
	void __fastcall Change(void);
	__property TVTHeader* Header = {read=FHeader};
	
public:
	__fastcall TVTFixedAreaConstraints(TVTHeader* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	
__published:
	__property TVTConstraintPercent MaxHeightPercent = {read=FMaxHeightPercent, write=SetConstraints, index=0, default=0};
	__property TVTConstraintPercent MaxWidthPercent = {read=FMaxWidthPercent, write=SetConstraints, index=1, default=0};
	__property TVTConstraintPercent MinHeightPercent = {read=FMinHeightPercent, write=SetConstraints, index=2, default=0};
	__property TVTConstraintPercent MinWidthPercent = {read=FMinWidthPercent, write=SetConstraints, index=3, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TVTFixedAreaConstraints(void) { }
	
};


enum DECLSPEC_DENUM TVTHeaderStyle : unsigned char { hsThickButtons, hsFlatButtons, hsPlates };

enum DECLSPEC_DENUM TVTHeaderOption : unsigned char { hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoOwnerDraw, hoRestrictDrag, hoShowHint, hoShowImages, hoShowSortGlyphs, hoVisible, hoAutoSpring, hoFullRepaintOnResize, hoDisableAnimatedResize, hoHeightResize, hoHeightDblClickResize, hoHeaderClickAutoSort };

typedef System::Set<TVTHeaderOption, TVTHeaderOption::hoAutoResize, TVTHeaderOption::hoHeaderClickAutoSort> TVTHeaderOptions;

enum DECLSPEC_DENUM THeaderState : unsigned char { hsAutoSizing, hsDragging, hsDragPending, hsLoading, hsColumnWidthTracking, hsColumnWidthTrackPending, hsHeightTracking, hsHeightTrackPending, hsResizing, hsScaling, hsNeedScaling };

typedef System::Set<THeaderState, THeaderState::hsAutoSizing, THeaderState::hsNeedScaling> THeaderStates;

enum DECLSPEC_DENUM TSmartAutoFitType : unsigned char { smaAllColumns, smaNoColumn, smaUseColumnOption };

enum DECLSPEC_DENUM TChangeReason : unsigned char { crIgnore, crAccumulated, crChildAdded, crChildDeleted, crNodeAdded, crNodeCopied, crNodeMoved };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTHeader : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TBaseVirtualTree* FOwner;
	TVirtualTreeColumns* FColumns;
	int FHeight;
	Vcl::Graphics::TFont* FFont;
	bool FParentFont;
	TVTHeaderOptions FOptions;
	TVTHeaderStyle FStyle;
	System::Uitypes::TColor FBackground;
	TColumnIndex FAutoSizeIndex;
	Vcl::Menus::TPopupMenu* FPopupMenu;
	TColumnIndex FMainColumn;
	int FMaxHeight;
	int FMinHeight;
	int FDefaultHeight;
	TVTFixedAreaConstraints* FFixedAreaConstraints;
	Vcl::Imglist::TCustomImageList* FImages;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	TColumnIndex FSortColumn;
	TSortDirection FSortDirection;
	TVTDragImage* FDragImage;
	int FLastWidth;
	void __fastcall FontChanged(System::TObject* Sender);
	TColumnIndex __fastcall GetMainColumn(void);
	bool __fastcall GetUseColumns(void);
	bool __fastcall IsFontStored(void);
	void __fastcall SetAutoSizeIndex(TColumnIndex Value);
	void __fastcall SetBackground(System::Uitypes::TColor Value);
	void __fastcall SetColumns(TVirtualTreeColumns* Value);
	void __fastcall SetDefaultHeight(int Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetHeight(int Value);
	void __fastcall SetImages(Vcl::Imglist::TCustomImageList* const Value);
	void __fastcall SetMainColumn(TColumnIndex Value);
	void __fastcall SetMaxHeight(int Value);
	void __fastcall SetMinHeight(int Value);
	void __fastcall SetOptions(TVTHeaderOptions Value);
	void __fastcall SetParentFont(bool Value);
	void __fastcall SetSortColumn(TColumnIndex Value);
	void __fastcall SetSortDirection(const TSortDirection Value);
	void __fastcall SetStyle(TVTHeaderStyle Value);
	
protected:
	THeaderStates FStates;
	System::Types::TPoint FDragStart;
	System::Types::TPoint FTrackStart;
	System::Types::TPoint FTrackPoint;
	bool __fastcall CanSplitterResize(const System::Types::TPoint &P);
	virtual bool __fastcall CanWriteColumns(void);
	virtual void __fastcall ChangeScale(int M, int D);
	virtual bool __fastcall DetermineSplitterIndex(const System::Types::TPoint &P);
	virtual void __fastcall DoAfterAutoFitColumn(TColumnIndex Column);
	virtual void __fastcall DoAfterColumnWidthTracking(TColumnIndex Column);
	virtual void __fastcall DoAfterHeightTracking(void);
	virtual bool __fastcall DoBeforeAutoFitColumn(TColumnIndex Column, TSmartAutoFitType SmartAutoFitType);
	virtual void __fastcall DoBeforeColumnWidthTracking(TColumnIndex Column, System::Classes::TShiftState Shift);
	virtual void __fastcall DoBeforeHeightTracking(System::Classes::TShiftState Shift);
	virtual void __fastcall DoCanSplitterResize(const System::Types::TPoint &P, bool &Allowed);
	virtual bool __fastcall DoColumnWidthDblClickResize(TColumnIndex Column, const System::Types::TPoint &P, System::Classes::TShiftState Shift);
	virtual bool __fastcall DoColumnWidthTracking(TColumnIndex Column, System::Classes::TShiftState Shift, System::Types::TPoint &TrackPoint, const System::Types::TPoint &P);
	virtual Vcl::Menus::TPopupMenu* __fastcall DoGetPopupMenu(TColumnIndex Column, const System::Types::TPoint &Position);
	virtual bool __fastcall DoHeightTracking(System::Types::TPoint &P, System::Classes::TShiftState Shift);
	virtual bool __fastcall DoHeightDblClickResize(System::Types::TPoint &P, System::Classes::TShiftState Shift);
	virtual void __fastcall DoSetSortColumn(TColumnIndex Value);
	void __fastcall DragTo(const System::Types::TPoint &P);
	void __fastcall FixedAreaConstraintsChanged(System::TObject* Sender);
	virtual TVirtualTreeColumnsClass __fastcall GetColumnsClass(void);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	System::Classes::TShiftState __fastcall GetShiftState(void);
	bool __fastcall HandleHeaderMouseMove(Winapi::Messages::TWMMouse &Message);
	virtual bool __fastcall HandleMessage(Winapi::Messages::TMessage &Message);
	void __fastcall ImageListChange(System::TObject* Sender);
	void __fastcall PrepareDrag(const System::Types::TPoint &P, const System::Types::TPoint &Start);
	void __fastcall ReadColumns(System::Classes::TReader* Reader);
	virtual void __fastcall RecalculateHeader(void);
	void __fastcall RescaleHeader(void);
	void __fastcall UpdateMainColumn(void);
	void __fastcall UpdateSpringColumns(void);
	void __fastcall WriteColumns(System::Classes::TWriter* Writer);
	
public:
	__fastcall virtual TVTHeader(TBaseVirtualTree* AOwner);
	__fastcall virtual ~TVTHeader(void);
	bool __fastcall AllowFocus(TColumnIndex ColumnIndex);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall AutoFitColumns(bool Animated = true, TSmartAutoFitType SmartAutoFitType = (TSmartAutoFitType)(0x2), int RangeStartCol = 0xffffffff, int RangeEndCol = 0xffffffff);
	virtual bool __fastcall InHeader(const System::Types::TPoint &P);
	virtual bool __fastcall InHeaderSplitterArea(const System::Types::TPoint &P);
	void __fastcall Invalidate(TVirtualTreeColumn* Column, bool ExpandToBorder = false);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* const Stream);
	int __fastcall ResizeColumns(int ChangeBy, TColumnIndex RangeStartCol, TColumnIndex RangeEndCol, TVTColumnOptions Options = (TVTColumnOptions() << TVTColumnOption::coVisible ));
	void __fastcall RestoreColumns(void);
	virtual void __fastcall SaveToStream(System::Classes::TStream* const Stream);
	__property TVTDragImage* DragImage = {read=FDragImage};
	__property THeaderStates States = {read=FStates, nodefault};
	__property TBaseVirtualTree* Treeview = {read=FOwner};
	__property bool UseColumns = {read=GetUseColumns, nodefault};
	
__published:
	__property TColumnIndex AutoSizeIndex = {read=FAutoSizeIndex, write=SetAutoSizeIndex, nodefault};
	__property System::Uitypes::TColor Background = {read=FBackground, write=SetBackground, default=-16777201};
	__property TVirtualTreeColumns* Columns = {read=FColumns, write=SetColumns, stored=false};
	__property int DefaultHeight = {read=FDefaultHeight, write=SetDefaultHeight, default=19};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont, stored=IsFontStored};
	__property TVTFixedAreaConstraints* FixedAreaConstraints = {read=FFixedAreaConstraints, write=FFixedAreaConstraints};
	__property int Height = {read=FHeight, write=SetHeight, default=19};
	__property Vcl::Imglist::TCustomImageList* Images = {read=FImages, write=SetImages};
	__property TColumnIndex MainColumn = {read=GetMainColumn, write=SetMainColumn, default=0};
	__property int MaxHeight = {read=FMaxHeight, write=SetMaxHeight, default=10000};
	__property int MinHeight = {read=FMinHeight, write=SetMinHeight, default=10};
	__property TVTHeaderOptions Options = {read=FOptions, write=SetOptions, default=522};
	__property bool ParentFont = {read=FParentFont, write=SetParentFont, default=0};
	__property Vcl::Menus::TPopupMenu* PopupMenu = {read=FPopupMenu, write=FPopupMenu};
	__property TColumnIndex SortColumn = {read=FSortColumn, write=SetSortColumn, default=-1};
	__property TSortDirection SortDirection = {read=FSortDirection, write=SetSortDirection, default=0};
	__property TVTHeaderStyle Style = {read=FStyle, write=SetStyle, default=0};
};

#pragma pack(pop)

typedef System::TMetaClass* TVTHeaderClass;

__interface IVTEditLink;
typedef System::DelphiInterface<IVTEditLink> _di_IVTEditLink;
__interface  INTERFACE_UUID("{2BE3EAFA-5ACB-45B4-9D9A-B58BCC496E17}") IVTEditLink  : public System::IInterface 
{
	
public:
	virtual bool __stdcall BeginEdit(void) = 0 ;
	virtual bool __stdcall CancelEdit(void) = 0 ;
	virtual bool __stdcall EndEdit(void) = 0 ;
	virtual bool __stdcall PrepareEdit(TBaseVirtualTree* Tree, PVirtualNode Node, TColumnIndex Column) = 0 ;
	virtual System::Types::TRect __stdcall GetBounds(void) = 0 ;
	virtual void __stdcall ProcessMessage(Winapi::Messages::TMessage &Message) = 0 ;
	virtual void __stdcall SetBounds(const System::Types::TRect R) = 0 ;
};

enum DECLSPEC_DENUM TVTUpdateState : unsigned char { usBegin, usBeginSynch, usSynch, usUpdate, usEnd, usEndSynch };

enum DECLSPEC_DENUM TVTDropMarkMode : unsigned char { dmmNone, dmmLeft, dmmRight };

struct DECLSPEC_DRECORD THeaderPaintInfo
{
public:
	Vcl::Graphics::TCanvas* TargetCanvas;
	TVirtualTreeColumn* Column;
	System::Types::TRect PaintRectangle;
	System::Types::TRect TextRectangle;
	bool IsHoverIndex;
	bool IsDownIndex;
	bool IsEnabled;
	bool ShowHeaderGlyph;
	bool ShowSortGlyph;
	bool ShowRightBorder;
	TVTDropMarkMode DropMark;
	System::Types::TPoint GlyphPos;
	System::Types::TPoint SortGlyphPos;
};


enum DECLSPEC_DENUM Virtualtrees__51 : unsigned char { hpeBackground, hpeDropMark, hpeHeaderGlyph, hpeSortGlyph, hpeText };

typedef System::Set<Virtualtrees__51, Virtualtrees__51::hpeBackground, Virtualtrees__51::hpeText> THeaderPaintElements;

enum DECLSPEC_DENUM Virtualtrees__61 : unsigned char { tsCancelHintAnimation, tsChangePending, tsCheckPropagation, tsCollapsing, tsToggleFocusedSelection, tsClearPending, tsClipboardFlushing, tsCopyPending, tsCutPending, tsDrawSelPending, tsDrawSelecting, tsEditing, tsEditPending, tsExpanding, tsNodeHeightTracking, tsNodeHeightTrackPending, tsHint, tsInAnimation, tsIncrementalSearching, tsIncrementalSearchPending, tsIterating, tsKeyCheckPending, tsLeftButtonDown, tsLeftDblClick, tsMouseCheckPending, tsMiddleButtonDown, tsMiddleDblClick, tsNeedRootCountUpdate, tsOLEDragging, tsOLEDragPending, tsPainting, tsRightButtonDown, tsRightDblClick, tsPopupMenuShown, tsScrolling, tsScrollPending, tsSizing, tsStopValidation, tsStructureChangePending, tsSynchMode, 
	tsThumbTracking, tsToggling, tsUpdateHiddenChildrenNeeded, tsUpdating, tsUseCache, tsUserDragObject, tsUseThemes, tsValidating, tsPreviouslySelectedLocked, tsValidationNeeded, tsVCLDragging, tsVCLDragPending, tsVCLDragFinished, tsWheelPanning, tsWheelScrolling, tsWindowCreating, tsUseExplorerTheme };

typedef System::Set<Virtualtrees__61, Virtualtrees__61::tsCancelHintAnimation, Virtualtrees__61::tsUseExplorerTheme> TVirtualTreeStates;

enum DECLSPEC_DENUM Virtualtrees__71 : unsigned char { csStopValidation, csUseCache, csValidating, csValidationNeeded };

typedef System::Set<Virtualtrees__71, Virtualtrees__71::csStopValidation, Virtualtrees__71::csValidationNeeded> TChangeStates;

enum DECLSPEC_DENUM TVTDragImageKind : unsigned char { diComplete, diMainColumnOnly, diNoImage };

enum DECLSPEC_DENUM TVTDragType : unsigned char { dtOLE, dtVCL };

enum DECLSPEC_DENUM TVTInternalPaintOption : unsigned char { poBackground, poColumnColor, poDrawFocusRect, poDrawSelection, poDrawDropMark, poGridLines, poMainOnly, poSelectedOnly, poUnbuffered };

typedef System::Set<TVTInternalPaintOption, TVTInternalPaintOption::poBackground, TVTInternalPaintOption::poUnbuffered> TVTInternalPaintOptions;

enum DECLSPEC_DENUM TVTLineStyle : unsigned char { lsCustomStyle, lsDotted, lsSolid };

enum DECLSPEC_DENUM TVTLineType : unsigned char { ltNone, ltBottomRight, ltTopDown, ltTopDownRight, ltRight, ltTopRight, ltLeft, ltLeftBottom };

enum DECLSPEC_DENUM TVTLineMode : unsigned char { lmNormal, lmBands };

typedef System::DynamicArray<TVTLineType> TLineImage;

typedef System::Word TVTScrollIncrement;

enum DECLSPEC_DENUM TVTExportType : unsigned char { etRTF, etHTML, etText, etExcel, etWord, etCustom };

typedef bool __fastcall (__closure *TVTNodeExportEvent)(TBaseVirtualTree* Sender, TVTExportType aExportType, PVirtualNode Node);

typedef void __fastcall (__closure *TVTColumnExportEvent)(TBaseVirtualTree* Sender, TVTExportType aExportType, TVirtualTreeColumn* Column);

typedef void __fastcall (__closure *TVTTreeExportEvent)(TBaseVirtualTree* Sender, TVTExportType aExportType);

class DELPHICLASS TScrollBarOptions;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TScrollBarOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FAlwaysVisible;
	TBaseVirtualTree* FOwner;
	System::Uitypes::TScrollStyle FScrollBars;
	TScrollBarStyle FScrollBarStyle;
	TVTScrollIncrement FIncrementX;
	TVTScrollIncrement FIncrementY;
	void __fastcall SetAlwaysVisible(bool Value);
	void __fastcall SetScrollBars(System::Uitypes::TScrollStyle Value);
	void __fastcall SetScrollBarStyle(TScrollBarStyle Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall TScrollBarOptions(TBaseVirtualTree* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property bool AlwaysVisible = {read=FAlwaysVisible, write=SetAlwaysVisible, default=0};
	__property TVTScrollIncrement HorizontalIncrement = {read=FIncrementX, write=FIncrementX, default=20};
	__property System::Uitypes::TScrollStyle ScrollBars = {read=FScrollBars, write=SetScrollBars, default=3};
	__property TScrollBarStyle ScrollBarStyle = {read=FScrollBarStyle, write=SetScrollBarStyle, default=0};
	__property TVTScrollIncrement VerticalIncrement = {read=FIncrementY, write=FIncrementY, default=20};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TScrollBarOptions(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVTColors;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVTColors : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TBaseVirtualTree* FOwner;
	System::StaticArray<System::Uitypes::TColor, 17> FColors;
	System::Uitypes::TColor __fastcall GetColor(const int Index);
	void __fastcall SetColor(const int Index, const System::Uitypes::TColor Value);
	System::Uitypes::TColor __fastcall GetBackgroundColor(void);
	System::Uitypes::TColor __fastcall GetHeaderFontColor(void);
	System::Uitypes::TColor __fastcall GetNodeFontColor(void);
	
public:
	__fastcall TVTColors(TBaseVirtualTree* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property System::Uitypes::TColor BackGroundColor = {read=GetBackgroundColor, nodefault};
	__property System::Uitypes::TColor HeaderFontColor = {read=GetHeaderFontColor, nodefault};
	__property System::Uitypes::TColor NodeFontColor = {read=GetNodeFontColor, nodefault};
	
__published:
	__property System::Uitypes::TColor BorderColor = {read=GetColor, write=SetColor, index=7, default=-16777201};
	__property System::Uitypes::TColor DisabledColor = {read=GetColor, write=SetColor, index=0, default=-16777200};
	__property System::Uitypes::TColor DropMarkColor = {read=GetColor, write=SetColor, index=1, default=-16777203};
	__property System::Uitypes::TColor DropTargetColor = {read=GetColor, write=SetColor, index=2, default=-16777203};
	__property System::Uitypes::TColor DropTargetBorderColor = {read=GetColor, write=SetColor, index=11, default=-16777203};
	__property System::Uitypes::TColor FocusedSelectionColor = {read=GetColor, write=SetColor, index=3, default=-16777203};
	__property System::Uitypes::TColor FocusedSelectionBorderColor = {read=GetColor, write=SetColor, index=9, default=-16777203};
	__property System::Uitypes::TColor GridLineColor = {read=GetColor, write=SetColor, index=4, default=-16777201};
	__property System::Uitypes::TColor HeaderHotColor = {read=GetColor, write=SetColor, index=14, default=-16777200};
	__property System::Uitypes::TColor HotColor = {read=GetColor, write=SetColor, index=8, default=-16777208};
	__property System::Uitypes::TColor SelectionRectangleBlendColor = {read=GetColor, write=SetColor, index=12, default=-16777203};
	__property System::Uitypes::TColor SelectionRectangleBorderColor = {read=GetColor, write=SetColor, index=13, default=-16777203};
	__property System::Uitypes::TColor SelectionTextColor = {read=GetColor, write=SetColor, index=15, default=-16777202};
	__property System::Uitypes::TColor TreeLineColor = {read=GetColor, write=SetColor, index=5, default=-16777200};
	__property System::Uitypes::TColor UnfocusedColor = {read=GetColor, write=SetColor, index=16, default=-16777201};
	__property System::Uitypes::TColor UnfocusedSelectionColor = {read=GetColor, write=SetColor, index=6, default=-16777201};
	__property System::Uitypes::TColor UnfocusedSelectionBorderColor = {read=GetColor, write=SetColor, index=10, default=-16777201};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TVTColors(void) { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TVTImageInfo
{
public:
	int Index;
	int XPos;
	int YPos;
	bool Ghosted;
	Vcl::Imglist::TCustomImageList* Images;
};


enum DECLSPEC_DENUM TVTImageInfoIndex : unsigned char { iiNormal, iiState, iiCheck, iiOverlay };

enum DECLSPEC_DENUM Virtualtrees__02 : unsigned char { suoRepaintHeader, suoRepaintScrollBars, suoScrollClientArea, suoUpdateNCArea };

typedef System::Set<Virtualtrees__02, Virtualtrees__02::suoRepaintHeader, Virtualtrees__02::suoUpdateNCArea> TScrollUpdateOptions;

enum DECLSPEC_DENUM TVTButtonStyle : unsigned char { bsRectangle, bsTriangle };

enum DECLSPEC_DENUM TVTButtonFillMode : unsigned char { fmTreeColor, fmWindowColor, fmShaded, fmTransparent };

struct DECLSPEC_DRECORD TVTPaintInfo
{
public:
	Vcl::Graphics::TCanvas* Canvas;
	TVTInternalPaintOptions PaintOptions;
	TVirtualNode *Node;
	TColumnIndex Column;
	TColumnPosition Position;
	System::Types::TRect CellRect;
	System::Types::TRect ContentRect;
	int NodeWidth;
	System::Classes::TAlignment Alignment;
	System::Classes::TAlignment CaptionAlignment;
	System::Classes::TBiDiMode BidiMode;
	System::Types::TPoint BrushOrigin;
	System::StaticArray<TVTImageInfo, 4> ImageInfo;
};


typedef bool __fastcall (__closure *TVTAnimationCallback)(int Step, int StepSize, void * Data);

enum DECLSPEC_DENUM TVTIncrementalSearch : unsigned char { isAll, isNone, isInitializedOnly, isVisibleOnly };

enum DECLSPEC_DENUM TVTSearchDirection : unsigned char { sdForward, sdBackward };

enum DECLSPEC_DENUM TVTSearchStart : unsigned char { ssAlwaysStartOver, ssLastHit, ssFocusedNode };

enum DECLSPEC_DENUM TVTNodeAlignment : unsigned char { naFromBottom, naFromTop, naProportional };

enum DECLSPEC_DENUM TVTDrawSelectionMode : unsigned char { smDottedRectangle, smBlendedRectangle };

enum DECLSPEC_DENUM TVTCellPaintMode : unsigned char { cpmPaint, cpmGetContentMargin };

enum DECLSPEC_DENUM TVTCellContentMarginType : unsigned char { ccmtAllSides, ccmtTopLeftOnly, ccmtBottomRightOnly };

class DELPHICLASS TClipboardFormats;
class PASCALIMPLEMENTATION TClipboardFormats : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	TBaseVirtualTree* FOwner;
	
public:
	__fastcall virtual TClipboardFormats(TBaseVirtualTree* AOwner);
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual void __fastcall Insert(int Index, const System::UnicodeString S);
	__property TBaseVirtualTree* Owner = {read=FOwner};
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TClipboardFormats(void) { }
	
};


__interface TVTGetNodeProc;
typedef System::DelphiInterface<TVTGetNodeProc> _di_TVTGetNodeProc;
__interface TVTGetNodeProc  : public System::IInterface 
{
	
public:
	virtual void __fastcall Invoke(TBaseVirtualTree* Sender, PVirtualNode Node, void * Data, bool &Abort) = 0 ;
};

typedef void __fastcall (__closure *TVTChangingEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, bool &Allowed);

typedef void __fastcall (__closure *TVTCheckChangingEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TCheckState &NewState, bool &Allowed);

typedef void __fastcall (__closure *TVTChangeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node);

typedef void __fastcall (__closure *TVTStructureChangeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TChangeReason Reason);

typedef void __fastcall (__closure *TVTEditCancelEvent)(TBaseVirtualTree* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTEditChangingEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTEditChangeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column);

typedef void __fastcall (__closure *TVTFreeNodeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node);

typedef void __fastcall (__closure *TVTFocusChangingEvent)(TBaseVirtualTree* Sender, PVirtualNode OldNode, PVirtualNode NewNode, TColumnIndex OldColumn, TColumnIndex NewColumn, bool &Allowed);

typedef void __fastcall (__closure *TVTFocusChangeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column);

typedef void __fastcall (__closure *TVTAddToSelectionEvent)(TBaseVirtualTree* Sender, PVirtualNode Node);

typedef void __fastcall (__closure *TVTRemoveFromSelectionEvent)(TBaseVirtualTree* Sender, PVirtualNode Node);

typedef void __fastcall (__closure *TVTGetImageEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TVTImageKind Kind, TColumnIndex Column, bool &Ghosted, int &ImageIndex);

typedef void __fastcall (__closure *TVTGetImageExEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TVTImageKind Kind, TColumnIndex Column, bool &Ghosted, int &ImageIndex, Vcl::Imglist::TCustomImageList* &ImageList);

typedef void __fastcall (__closure *TVTGetImageTextEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TVTImageKind Kind, TColumnIndex Column, System::UnicodeString &ImageText);

typedef void __fastcall (__closure *TVTHotNodeChangeEvent)(TBaseVirtualTree* Sender, PVirtualNode OldNode, PVirtualNode NewNode);

typedef void __fastcall (__closure *TVTInitChildrenEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, unsigned &ChildCount);

typedef void __fastcall (__closure *TVTInitNodeEvent)(TBaseVirtualTree* Sender, PVirtualNode ParentNode, PVirtualNode Node, TVirtualNodeInitStates &InitialStates);

typedef void __fastcall (__closure *TVTPopupEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &P, bool &AskParent, Vcl::Menus::TPopupMenu* &PopupMenu);

typedef void __fastcall (__closure *TVTHelpContextEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, int &HelpContext);

typedef void __fastcall (__closure *TVTCreateEditorEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, /* out */ _di_IVTEditLink &EditLink);

typedef void __fastcall (__closure *TVTSaveTreeEvent)(TBaseVirtualTree* Sender, System::Classes::TStream* Stream);

typedef void __fastcall (__closure *TVTSaveNodeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, System::Classes::TStream* Stream);

typedef void __fastcall (__closure *TVTHeaderClickEvent)(TVTHeader* Sender, const TVTHeaderHitInfo &HitInfo);

typedef void __fastcall (__closure *TVTHeaderMouseEvent)(TVTHeader* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);

typedef void __fastcall (__closure *TVTHeaderMouseMoveEvent)(TVTHeader* Sender, System::Classes::TShiftState Shift, int X, int Y);

typedef void __fastcall (__closure *TVTBeforeHeaderHeightTrackingEvent)(TVTHeader* Sender, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TVTAfterHeaderHeightTrackingEvent)(TVTHeader* Sender);

typedef void __fastcall (__closure *TVTHeaderHeightTrackingEvent)(TVTHeader* Sender, System::Types::TPoint &P, System::Classes::TShiftState Shift, bool &Allowed);

typedef void __fastcall (__closure *TVTHeaderHeightDblClickResizeEvent)(TVTHeader* Sender, System::Types::TPoint &P, System::Classes::TShiftState Shift, bool &Allowed);

typedef void __fastcall (__closure *TVTHeaderNotifyEvent)(TVTHeader* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTHeaderDraggingEvent)(TVTHeader* Sender, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTHeaderDraggedEvent)(TVTHeader* Sender, TColumnIndex Column, int OldPosition);

typedef void __fastcall (__closure *TVTHeaderDraggedOutEvent)(TVTHeader* Sender, TColumnIndex Column, const System::Types::TPoint &DropPosition);

typedef void __fastcall (__closure *TVTHeaderPaintEvent)(TVTHeader* Sender, Vcl::Graphics::TCanvas* HeaderCanvas, TVirtualTreeColumn* Column, const System::Types::TRect &R, bool Hover, bool Pressed, TVTDropMarkMode DropMark);

typedef void __fastcall (__closure *TVTHeaderPaintQueryElementsEvent)(TVTHeader* Sender, THeaderPaintInfo &PaintInfo, THeaderPaintElements &Elements);

typedef void __fastcall (__closure *TVTAdvancedHeaderPaintEvent)(TVTHeader* Sender, THeaderPaintInfo &PaintInfo, const THeaderPaintElements Elements);

typedef void __fastcall (__closure *TVTBeforeAutoFitColumnsEvent)(TVTHeader* Sender, TSmartAutoFitType &SmartAutoFitType);

typedef void __fastcall (__closure *TVTBeforeAutoFitColumnEvent)(TVTHeader* Sender, TColumnIndex Column, TSmartAutoFitType &SmartAutoFitType, bool &Allowed);

typedef void __fastcall (__closure *TVTAfterAutoFitColumnEvent)(TVTHeader* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTAfterAutoFitColumnsEvent)(TVTHeader* Sender);

typedef void __fastcall (__closure *TVTColumnClickEvent)(TBaseVirtualTree* Sender, TColumnIndex Column, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TVTColumnDblClickEvent)(TBaseVirtualTree* Sender, TColumnIndex Column, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TVTColumnWidthDblClickResizeEvent)(TVTHeader* Sender, TColumnIndex Column, System::Classes::TShiftState Shift, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTBeforeColumnWidthTrackingEvent)(TVTHeader* Sender, TColumnIndex Column, System::Classes::TShiftState Shift);

typedef void __fastcall (__closure *TVTAfterColumnWidthTrackingEvent)(TVTHeader* Sender, TColumnIndex Column);

typedef void __fastcall (__closure *TVTColumnWidthTrackingEvent)(TVTHeader* Sender, TColumnIndex Column, System::Classes::TShiftState Shift, System::Types::TPoint &TrackPoint, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTGetHeaderCursorEvent)(TVTHeader* Sender, HICON &Cursor);

typedef void __fastcall (__closure *TVTBeforeGetMaxColumnWidthEvent)(TVTHeader* Sender, TColumnIndex Column, bool &UseSmartColumnWidth);

typedef void __fastcall (__closure *TVTAfterGetMaxColumnWidthEvent)(TVTHeader* Sender, TColumnIndex Column, int &MaxWidth);

typedef void __fastcall (__closure *TVTCanSplitterResizeColumnEvent)(TVTHeader* Sender, const System::Types::TPoint &P, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTCanSplitterResizeHeaderEvent)(TVTHeader* Sender, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeMovedEvent)(TBaseVirtualTree* Sender, PVirtualNode Node);

typedef void __fastcall (__closure *TVTNodeMovingEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, PVirtualNode Target, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeCopiedEvent)(TBaseVirtualTree* Sender, PVirtualNode Node);

typedef void __fastcall (__closure *TVTNodeCopyingEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, PVirtualNode Target, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeClickEvent)(TBaseVirtualTree* Sender, const THitInfo &HitInfo);

typedef void __fastcall (__closure *TVTNodeHeightTrackingEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, System::Types::TPoint &TrackPoint, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTNodeHeightDblClickResizeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, const System::Types::TPoint &P, bool &Allowed);

typedef void __fastcall (__closure *TVTCanSplitterResizeNodeEvent)(TBaseVirtualTree* Sender, const System::Types::TPoint &P, PVirtualNode Node, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTCreateDragManagerEvent)(TBaseVirtualTree* Sender, /* out */ _di_IVTDragManager &DragManager);

typedef void __fastcall (__closure *TVTCreateDataObjectEvent)(TBaseVirtualTree* Sender, /* out */ _di_IDataObject &IDataObject);

typedef void __fastcall (__closure *TVTDragAllowedEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, bool &Allowed);

typedef void __fastcall (__closure *TVTDragOverEvent)(TBaseVirtualTree* Sender, System::TObject* Source, System::Classes::TShiftState Shift, System::Uitypes::TDragState State, const System::Types::TPoint &Pt, TDropMode Mode, int &Effect, bool &Accept);

typedef void __fastcall (__closure *TVTDragDropEvent)(TBaseVirtualTree* Sender, System::TObject* Source, _di_IDataObject DataObject, TFormatArray Formats, System::Classes::TShiftState Shift, const System::Types::TPoint &Pt, int &Effect, TDropMode Mode);

typedef void __fastcall (__closure *TVTRenderOLEDataEvent)(TBaseVirtualTree* Sender, const tagFORMATETC &FormatEtcIn, /* out */ tagSTGMEDIUM &Medium, bool ForClipboard, HRESULT &Result);

typedef void __fastcall (__closure *TVTGetUserClipboardFormatsEvent)(TBaseVirtualTree* Sender, TFormatEtcArray &Formats);

typedef void __fastcall (__closure *TVTBeforeItemEraseEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, const System::Types::TRect &ItemRect, System::Uitypes::TColor &ItemColor, TItemEraseAction &EraseAction);

typedef void __fastcall (__closure *TVTAfterItemEraseEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, const System::Types::TRect &ItemRect);

typedef void __fastcall (__closure *TVTBeforeItemPaintEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, const System::Types::TRect &ItemRect, bool &CustomDraw);

typedef void __fastcall (__closure *TVTAfterItemPaintEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, const System::Types::TRect &ItemRect);

typedef void __fastcall (__closure *TVTBeforeCellPaintEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, TColumnIndex Column, TVTCellPaintMode CellPaintMode, const System::Types::TRect &CellRect, System::Types::TRect &ContentRect);

typedef void __fastcall (__closure *TVTAfterCellPaintEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, TColumnIndex Column, const System::Types::TRect &CellRect);

typedef void __fastcall (__closure *TVTPaintEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas);

typedef void __fastcall (__closure *TVTBackgroundPaintEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, const System::Types::TRect &R, bool &Handled);

typedef void __fastcall (__closure *TVTGetLineStyleEvent)(TBaseVirtualTree* Sender, void * &Bits);

typedef void __fastcall (__closure *TVTMeasureItemEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, int &NodeHeight);

typedef void __fastcall (__closure *TVTCompareEvent)(TBaseVirtualTree* Sender, PVirtualNode Node1, PVirtualNode Node2, TColumnIndex Column, int &Result);

typedef void __fastcall (__closure *TVTIncrementalSearchEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, const System::UnicodeString SearchText, int &Result);

typedef void __fastcall (__closure *TVTOperationEvent)(TBaseVirtualTree* Sender, TVTOperationKind OperationKind);

enum DECLSPEC_DENUM TVTHintKind : unsigned char { vhkText, vhkOwnerDraw };

typedef void __fastcall (__closure *TVTHintKindEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, TVTHintKind &Kind);

typedef void __fastcall (__closure *TVTDrawHintEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* HintCanvas, PVirtualNode Node, const System::Types::TRect &R, TColumnIndex Column);

typedef void __fastcall (__closure *TVTGetHintSizeEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, System::Types::TRect &R);

typedef void __fastcall (__closure *TVTBeforeDrawLineImageEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, int Level, int &PosX);

typedef void __fastcall (__closure *TVTGetNodeDataSizeEvent)(TBaseVirtualTree* Sender, int &NodeDataSize);

typedef void __fastcall (__closure *TVTKeyActionEvent)(TBaseVirtualTree* Sender, System::Word &CharCode, System::Classes::TShiftState &Shift, bool &DoDefault);

typedef void __fastcall (__closure *TVTScrollEvent)(TBaseVirtualTree* Sender, int DeltaX, int DeltaY);

typedef void __fastcall (__closure *TVTUpdatingEvent)(TBaseVirtualTree* Sender, TVTUpdateState State);

typedef void __fastcall (__closure *TVTGetCursorEvent)(TBaseVirtualTree* Sender, System::Uitypes::TCursor &Cursor);

typedef void __fastcall (__closure *TVTStateChangeEvent)(TBaseVirtualTree* Sender, const TVirtualTreeStates &Enter, const TVirtualTreeStates &Leave);

typedef void __fastcall (__closure *TVTGetCellIsEmptyEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, bool &IsEmpty);

typedef void __fastcall (__closure *TVTScrollBarShowEvent)(TBaseVirtualTree* Sender, int Bar, bool Show);

typedef PVirtualNode __fastcall (__closure *TGetFirstNodeProc)(void);

typedef PVirtualNode __fastcall (__closure *TGetNextNodeProc)(PVirtualNode Node, bool ConsiderChildrenAbove/* = false*/);

enum DECLSPEC_DENUM TVZVirtualNodeEnumerationMode : unsigned char { vneAll, vneChecked, vneChild, vneCutCopy, vneInitialized, vneLeaf, vneLevel, vneNoInit, vneSelected, vneVisible, vneVisibleChild, vneVisibleNoInitChild, vneVisibleNoInit };

struct TVTVirtualNodeEnumeration;
typedef TVTVirtualNodeEnumeration *PVTVirtualNodeEnumeration;

struct DECLSPEC_DRECORD TVTVirtualNodeEnumerator
{
private:
	TVirtualNode *FNode;
	bool FCanModeNext;
	TVTVirtualNodeEnumeration *FEnumeration;
	PVirtualNode __fastcall GetCurrent(void);
	
public:
	bool __fastcall MoveNext(void);
	__property PVirtualNode Current = {read=GetCurrent};
};


struct DECLSPEC_DRECORD TVTVirtualNodeEnumeration
{
private:
	TVZVirtualNodeEnumerationMode FMode;
	TBaseVirtualTree* FTree;
	bool FConsiderChildrenAbove;
	TVirtualNode *FNode;
	unsigned FNodeLevel;
	TCheckState FState;
	bool FIncludeFiltered;
	
public:
	TVTVirtualNodeEnumerator __fastcall GetEnumerator(void);
	
private:
	PVirtualNode __fastcall GetNext(PVirtualNode Node);
};


class DELPHICLASS TVclStyleScrollBarsHook;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TVclStyleScrollBarsHook : public Vcl::Themes::TMouseTrackControlStyleHook
{
	typedef Vcl::Themes::TMouseTrackControlStyleHook inherited;
	
private:
	class DELPHICLASS TVclStyleScrollBarWindow;
	#pragma pack(push,8)
	class PASCALIMPLEMENTATION TVclStyleScrollBarWindow : public Vcl::Controls::TWinControl
	{
		typedef Vcl::Controls::TWinControl inherited;
		
private:
		TVclStyleScrollBarsHook* FScrollBarWindowOwner;
		bool FScrollBarVertical;
		bool FScrollBarVisible;
		bool FScrollBarEnabled;
		HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Msg);
		HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TMessage &Msg);
		HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
		
protected:
		virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
		
public:
		__fastcall virtual TVclStyleScrollBarWindow(System::Classes::TComponent* AOwner);
		__property TVclStyleScrollBarsHook* ScrollBarWindowOwner = {read=FScrollBarWindowOwner, write=FScrollBarWindowOwner};
		__property bool ScrollBarVertical = {read=FScrollBarVertical, write=FScrollBarVertical, nodefault};
		__property bool ScrollBarVisible = {read=FScrollBarVisible, write=FScrollBarVisible, nodefault};
		__property bool ScrollBarEnabled = {read=FScrollBarEnabled, write=FScrollBarEnabled, nodefault};
public:
		/* TWinControl.CreateParented */ inline __fastcall TVclStyleScrollBarWindow(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
		/* TWinControl.Destroy */ inline __fastcall virtual ~TVclStyleScrollBarWindow(void) { }
		
	};
	
	#pragma pack(pop)
	
	
private:
	System::Types::TRect FHorzScrollBarDownButtonRect;
	Vcl::Themes::TThemedScrollBar FHorzScrollBarDownButtonState;
	System::Types::TRect FHorzScrollBarRect;
	Vcl::Themes::TThemedScrollBar FHorzScrollBarSliderState;
	System::Types::TRect FHorzScrollBarSliderTrackRect;
	System::Types::TRect FHorzScrollBarUpButtonRect;
	Vcl::Themes::TThemedScrollBar FHorzScrollBarUpButtonState;
	TVclStyleScrollBarWindow* FHorzScrollBarWindow;
	bool FLeftMouseButtonDown;
	int FPrevScrollPos;
	float FScrollPos;
	System::Types::TRect FVertScrollBarDownButtonRect;
	Vcl::Themes::TThemedScrollBar FVertScrollBarDownButtonState;
	System::Types::TRect FVertScrollBarRect;
	Vcl::Themes::TThemedScrollBar FVertScrollBarSliderState;
	System::Types::TRect FVertScrollBarSliderTrackRect;
	System::Types::TRect FVertScrollBarUpButtonRect;
	Vcl::Themes::TThemedScrollBar FVertScrollBarUpButtonState;
	TVclStyleScrollBarWindow* FVertScrollBarWindow;
	MESSAGE void __fastcall CMUpdateVclStyleScrollbars(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMKeyUp(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMNCLButtonDown(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCMouseMove(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMNCLButtonUp(Winapi::Messages::TWMMouse &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMVScroll(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMHScroll(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMCaptureChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMNCLButtonDblClk(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall WMSize(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMMove(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMPosChanged(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall CalcScrollBarsRect(void);
	virtual void __fastcall DrawHorzScrollBar(HDC DC);
	virtual void __fastcall DrawVertScrollBar(HDC DC);
	System::Types::TRect __fastcall GetHorzScrollBarSliderRect(void);
	System::Types::TRect __fastcall GetVertScrollBarSliderRect(void);
	virtual void __fastcall MouseLeave(void);
	virtual void __fastcall PaintScrollBars(void);
	bool __fastcall PointInTreeHeader(const System::Types::TPoint &P);
	void __fastcall UpdateScrollBarWindow(void);
	
public:
	__fastcall virtual TVclStyleScrollBarsHook(Vcl::Controls::TWinControl* AControl);
	__fastcall virtual ~TVclStyleScrollBarsHook(void);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TBaseVirtualTree : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	TVTHeader* FHeader;
	TVirtualNode *FRoot;
	unsigned FDefaultNodeHeight;
	unsigned FIndent;
	TCustomVirtualTreeOptions* FOptions;
	unsigned FUpdateCount;
	unsigned FSynchUpdateCount;
	int FNodeDataSize;
	TVirtualTreeStates FStates;
	TVirtualNode *FLastSelected;
	TVirtualNode *FFocusedNode;
	TColumnIndex FEditColumn;
	TColumnIndex FFocusedColumn;
	System::Types::TPoint FHeightTrackPoint;
	TVirtualNode *FHeightTrackNode;
	TColumnIndex FHeightTrackColumn;
	TScrollDirections FScrollDirections;
	TChangeReason FLastStructureChangeReason;
	TVirtualNode *FLastStructureChangeNode;
	TVirtualNode *FLastChangedNode;
	TVirtualNode *FCurrentHotNode;
	TColumnIndex FCurrentHotColumn;
	bool FHotNodeButtonHit;
	System::Types::TRect FLastSelRect;
	System::Types::TRect FNewSelRect;
	System::Uitypes::TCursor FHotCursor;
	THintAnimationType FAnimationType;
	TVTHintMode FHintMode;
	TVTHintData FHintData;
	unsigned FChangeDelay;
	unsigned FEditDelay;
	TCache FPositionCache;
	unsigned FVisibleCount;
	unsigned FStartIndex;
	TNodeArray FSelection;
	int FSelectionCount;
	bool FSelectionLocked;
	TVirtualNode *FRangeAnchor;
	TVirtualNode *FCheckNode;
	TCheckState FPendingCheckState;
	unsigned FCheckPropagationCount;
	int FLastSelectionLevel;
	System::Classes::TShiftState FDrawSelShiftState;
	_di_IVTEditLink FEditLink;
	TNodeArray FTempNodeCache;
	unsigned FTempNodeCount;
	Vcl::Graphics::TPicture* FBackground;
	int FMargin;
	int FTextMargin;
	int FBackgroundOffsetX;
	int FBackgroundOffsetY;
	unsigned FAnimationDuration;
	bool FWantTabs;
	TVTNodeAlignment FNodeAlignment;
	System::Types::TRect FHeaderRect;
	System::Types::TRect FLastHintRect;
	System::Types::TRect FUpdateRect;
	System::UnicodeString FEmptyListMessage;
	Vcl::Graphics::TBitmap* FPlusBM;
	Vcl::Graphics::TBitmap* FMinusBM;
	Vcl::Graphics::TBitmap* FHotPlusBM;
	Vcl::Graphics::TBitmap* FHotMinusBM;
	Vcl::Imglist::TCustomImageList* FImages;
	Vcl::Imglist::TCustomImageList* FStateImages;
	Vcl::Imglist::TCustomImageList* FCustomCheckImages;
	TCheckImageKind FCheckImageKind;
	Vcl::Imglist::TCustomImageList* FCheckImages;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	Vcl::Imglist::TChangeLink* FStateChangeLink;
	Vcl::Imglist::TChangeLink* FCustomCheckChangeLink;
	System::Classes::TNotifyEvent FOldFontChange;
	TVTColors* FColors;
	TVTButtonStyle FButtonStyle;
	TVTButtonFillMode FButtonFillMode;
	TVTLineStyle FLineStyle;
	TVTLineMode FLineMode;
	HBRUSH FDottedBrush;
	unsigned FSelectionCurveRadius;
	System::Byte FSelectionBlendFactor;
	TVTDrawSelectionMode FDrawSelectionMode;
	System::Classes::TAlignment FAlignment;
	TVTDragImageKind FDragImageKind;
	TDragOperations FDragOperations;
	int FDragThreshold;
	_di_IVTDragManager FDragManager;
	TVirtualNode *FDropTargetNode;
	TDropMode FLastDropMode;
	TNodeArray FDragSelection;
	int FLastDragEffect;
	TVTDragType FDragType;
	TVTDragImage* FDragImage;
	int FDragWidth;
	int FDragHeight;
	TClipboardFormats* FClipboardFormats;
	TVirtualNode *FLastVCLDragTarget;
	int FVCLDragEffect;
	TScrollBarOptions* FScrollBarOptions;
	TAutoScrollInterval FAutoScrollInterval;
	unsigned FAutoScrollDelay;
	unsigned FAutoExpandDelay;
	int FOffsetX;
	int FOffsetY;
	int FEffectiveOffsetX;
	unsigned FRangeX;
	unsigned FRangeY;
	unsigned FBottomSpace;
	TVTNodeAttachMode FDefaultPasteMode;
	TNodeArray FSingletonNodeArray;
	unsigned FDragScrollStart;
	TVTIncrementalSearch FIncrementalSearch;
	unsigned FSearchTimeout;
	System::UnicodeString FSearchBuffer;
	TVirtualNode *FLastSearchNode;
	TVTSearchDirection FSearchDirection;
	TVTSearchStart FSearchStart;
	unsigned FTotalInternalDataSize;
	HWND FPanningWindow;
	HICON FPanningCursor;
	Vcl::Graphics::TBitmap* FPanningImage;
	System::Types::TPoint FLastClickPos;
	unsigned FOperationCount;
	bool FOperationCanceled;
	bool FChangingTheme;
	TVirtualNode *FNextNodeToSelect;
	_di_IAccessible FAccessible;
	_di_IAccessible FAccessibleItem;
	System::UnicodeString FAccessibleName;
	TVTNodeExportEvent FOnBeforeNodeExport;
	TVTNodeExportEvent FOnNodeExport;
	TVTNodeExportEvent FOnAfterNodeExport;
	TVTColumnExportEvent FOnBeforeColumnExport;
	TVTColumnExportEvent FOnColumnExport;
	TVTColumnExportEvent FOnAfterColumnExport;
	TVTTreeExportEvent FOnBeforeTreeExport;
	TVTTreeExportEvent FOnAfterTreeExport;
	TVTTreeExportEvent FOnBeforeHeaderExport;
	TVTTreeExportEvent FOnAfterHeaderExport;
	TVTChangeEvent FOnChange;
	TVTStructureChangeEvent FOnStructureChange;
	TVTInitChildrenEvent FOnInitChildren;
	TVTInitNodeEvent FOnInitNode;
	TVTFreeNodeEvent FOnFreeNode;
	TVTGetImageEvent FOnGetImage;
	TVTGetImageExEvent FOnGetImageEx;
	TVTGetImageTextEvent FOnGetImageText;
	TVTHotNodeChangeEvent FOnHotChange;
	TVTChangingEvent FOnExpanding;
	TVTChangingEvent FOnCollapsing;
	TVTCheckChangingEvent FOnChecking;
	TVTChangeEvent FOnExpanded;
	TVTChangeEvent FOnCollapsed;
	TVTChangeEvent FOnChecked;
	TVTChangeEvent FOnResetNode;
	TVTNodeMovingEvent FOnNodeMoving;
	TVTNodeMovedEvent FOnNodeMoved;
	TVTNodeCopyingEvent FOnNodeCopying;
	TVTNodeClickEvent FOnNodeClick;
	TVTNodeClickEvent FOnNodeDblClick;
	TVTCanSplitterResizeNodeEvent FOnCanSplitterResizeNode;
	TVTNodeHeightTrackingEvent FOnNodeHeightTracking;
	TVTNodeHeightDblClickResizeEvent FOnNodeHeightDblClickResize;
	TVTNodeCopiedEvent FOnNodeCopied;
	TVTEditChangingEvent FOnEditing;
	TVTEditCancelEvent FOnEditCancelled;
	TVTEditChangeEvent FOnEdited;
	TVTFocusChangingEvent FOnFocusChanging;
	TVTFocusChangeEvent FOnFocusChanged;
	TVTAddToSelectionEvent FOnAddToSelection;
	TVTRemoveFromSelectionEvent FOnRemoveFromSelection;
	TVTPopupEvent FOnGetPopupMenu;
	TVTHelpContextEvent FOnGetHelpContext;
	TVTCreateEditorEvent FOnCreateEditor;
	TVTSaveNodeEvent FOnLoadNode;
	TVTSaveNodeEvent FOnSaveNode;
	TVTSaveTreeEvent FOnLoadTree;
	TVTSaveTreeEvent FOnSaveTree;
	TVTAfterAutoFitColumnEvent FOnAfterAutoFitColumn;
	TVTAfterAutoFitColumnsEvent FOnAfterAutoFitColumns;
	TVTBeforeAutoFitColumnsEvent FOnBeforeAutoFitColumns;
	TVTBeforeAutoFitColumnEvent FOnBeforeAutoFitColumn;
	TVTHeaderClickEvent FOnHeaderClick;
	TVTHeaderClickEvent FOnHeaderDblClick;
	TVTAfterHeaderHeightTrackingEvent FOnAfterHeaderHeightTracking;
	TVTBeforeHeaderHeightTrackingEvent FOnBeforeHeaderHeightTracking;
	TVTHeaderHeightTrackingEvent FOnHeaderHeightTracking;
	TVTHeaderHeightDblClickResizeEvent FOnHeaderHeightDblClickResize;
	TVTHeaderMouseEvent FOnHeaderMouseDown;
	TVTHeaderMouseEvent FOnHeaderMouseUp;
	TVTHeaderMouseMoveEvent FOnHeaderMouseMove;
	TVTAfterGetMaxColumnWidthEvent FOnAfterGetMaxColumnWidth;
	TVTBeforeGetMaxColumnWidthEvent FOnBeforeGetMaxColumnWidth;
	TVTColumnClickEvent FOnColumnClick;
	TVTColumnDblClickEvent FOnColumnDblClick;
	TVTHeaderNotifyEvent FOnColumnResize;
	TVTColumnWidthDblClickResizeEvent FOnColumnWidthDblClickResize;
	TVTAfterColumnWidthTrackingEvent FOnAfterColumnWidthTracking;
	TVTBeforeColumnWidthTrackingEvent FOnBeforeColumnWidthTracking;
	TVTColumnWidthTrackingEvent FOnColumnWidthTracking;
	TVTGetHeaderCursorEvent FOnGetHeaderCursor;
	TVTCanSplitterResizeColumnEvent FOnCanSplitterResizeColumn;
	TVTCanSplitterResizeHeaderEvent FOnCanSplitterResizeHeader;
	TVTPaintEvent FOnAfterPaint;
	TVTPaintEvent FOnBeforePaint;
	TVTAfterItemPaintEvent FOnAfterItemPaint;
	TVTBeforeItemPaintEvent FOnBeforeItemPaint;
	TVTBeforeItemEraseEvent FOnBeforeItemErase;
	TVTAfterItemEraseEvent FOnAfterItemErase;
	TVTAfterCellPaintEvent FOnAfterCellPaint;
	TVTBeforeCellPaintEvent FOnBeforeCellPaint;
	TVTHeaderPaintEvent FOnHeaderDraw;
	TVTHeaderPaintQueryElementsEvent FOnHeaderDrawQueryElements;
	TVTAdvancedHeaderPaintEvent FOnAdvancedHeaderDraw;
	TVTGetLineStyleEvent FOnGetLineStyle;
	TVTBackgroundPaintEvent FOnPaintBackground;
	TVTMeasureItemEvent FOnMeasureItem;
	TVTCreateDragManagerEvent FOnCreateDragManager;
	TVTCreateDataObjectEvent FOnCreateDataObject;
	TVTDragAllowedEvent FOnDragAllowed;
	TVTDragOverEvent FOnDragOver;
	TVTDragDropEvent FOnDragDrop;
	TVTHeaderDraggedEvent FOnHeaderDragged;
	TVTHeaderDraggedOutEvent FOnHeaderDraggedOut;
	TVTHeaderDraggingEvent FOnHeaderDragging;
	TVTRenderOLEDataEvent FOnRenderOLEData;
	TVTGetUserClipboardFormatsEvent FOnGetUserClipboardFormats;
	TVTGetNodeDataSizeEvent FOnGetNodeDataSize;
	TVTBeforeDrawLineImageEvent FOnBeforeDrawLineImage;
	TVTKeyActionEvent FOnKeyAction;
	TVTScrollEvent FOnScroll;
	TVTUpdatingEvent FOnUpdating;
	TVTGetCursorEvent FOnGetCursor;
	TVTStateChangeEvent FOnStateChange;
	TVTGetCellIsEmptyEvent FOnGetCellIsEmpty;
	TVTScrollBarShowEvent FOnShowScrollBar;
	TVTCompareEvent FOnCompareNodes;
	TVTDrawHintEvent FOnDrawHint;
	TVTGetHintSizeEvent FOnGetHintSize;
	TVTHintKindEvent FOnGetHintKind;
	TVTIncrementalSearchEvent FOnIncrementalSearch;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	TVTOperationEvent FOnStartOperation;
	TVTOperationEvent FOnEndOperation;
	bool FVclStyleEnabled;
	MESSAGE void __fastcall CMStyleChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMParentDoubleBufferedChange(Winapi::Messages::TMessage &Message);
	void __fastcall AdjustCoordinatesByIndent(TVTPaintInfo &PaintInfo, int Indent);
	void __fastcall AdjustTotalCount(PVirtualNode Node, int Value, bool relative = false);
	void __fastcall AdjustTotalHeight(PVirtualNode Node, int Value, bool relative = false);
	int __fastcall CalculateCacheEntryCount(void);
	void __fastcall CalculateVerticalAlignments(bool ShowImages, bool ShowStateImages, PVirtualNode Node, int &VAlign, int &VButtonAlign);
	bool __fastcall ChangeCheckState(PVirtualNode Node, TCheckState Value);
	bool __fastcall CollectSelectedNodesLTR(int MainColumn, int NodeLeft, int NodeRight, System::Classes::TAlignment Alignment, const System::Types::TRect &OldRect, const System::Types::TRect &NewRect);
	bool __fastcall CollectSelectedNodesRTL(int MainColumn, int NodeLeft, int NodeRight, System::Classes::TAlignment Alignment, const System::Types::TRect &OldRect, const System::Types::TRect &NewRect);
	void __fastcall ClearNodeBackground(const TVTPaintInfo &PaintInfo, bool UseBackground, bool Floating, const System::Types::TRect &R);
	int __fastcall CompareNodePositions(PVirtualNode Node1, PVirtualNode Node2, bool ConsiderChildrenAbove = false);
	void __fastcall DrawLineImage(const TVTPaintInfo &PaintInfo, int X, int Y, int H, int VAlign, TVTLineType Style, bool Reverse);
	PVirtualNode __fastcall FindInPositionCache(PVirtualNode Node, unsigned &CurrentPos)/* overload */;
	PVirtualNode __fastcall FindInPositionCache(unsigned Position, unsigned &CurrentPos)/* overload */;
	void __fastcall FixupTotalCount(PVirtualNode Node);
	void __fastcall FixupTotalHeight(PVirtualNode Node);
	PVirtualNode __fastcall GetBottomNode(void);
	int __fastcall GetCheckedCount(void);
	TCheckState __fastcall GetCheckState(PVirtualNode Node);
	TCheckType __fastcall GetCheckType(PVirtualNode Node);
	unsigned __fastcall GetChildCount(PVirtualNode Node);
	bool __fastcall GetChildrenInitialized(PVirtualNode Node);
	int __fastcall GetCutCopyCount(void);
	bool __fastcall GetDisabled(PVirtualNode Node);
	_di_IVTDragManager __fastcall GetDragManager(void);
	bool __fastcall GetExpanded(PVirtualNode Node);
	bool __fastcall GetFiltered(PVirtualNode Node);
	bool __fastcall GetFullyVisible(PVirtualNode Node);
	bool __fastcall GetHasChildren(PVirtualNode Node);
	bool __fastcall GetMultiline(PVirtualNode Node);
	unsigned __fastcall GetNodeHeight(PVirtualNode Node);
	PVirtualNode __fastcall GetNodeParent(PVirtualNode Node);
	System::Types::TPoint __fastcall GetOffsetXY(void);
	unsigned __fastcall GetRootNodeCount(void);
	bool __fastcall GetSelected(PVirtualNode Node);
	PVirtualNode __fastcall GetTopNode(void);
	unsigned __fastcall GetTotalCount(void);
	System::Byte __fastcall GetVerticalAlignment(PVirtualNode Node);
	bool __fastcall GetVisible(PVirtualNode Node);
	bool __fastcall GetVisiblePath(PVirtualNode Node);
	void __fastcall HandleClickSelection(PVirtualNode LastFocused, PVirtualNode NewNode, System::Classes::TShiftState Shift, bool DragPending);
	bool __fastcall HandleDrawSelection(int X, int Y);
	bool __fastcall HasVisibleNextSibling(PVirtualNode Node);
	bool __fastcall HasVisiblePreviousSibling(PVirtualNode Node);
	void __fastcall ImageListChange(System::TObject* Sender);
	void __fastcall InitializeFirstColumnValues(TVTPaintInfo &PaintInfo);
	void __fastcall InitRootNode(unsigned OldSize = (unsigned)(0x0));
	void __fastcall InterruptValidation(void);
	bool __fastcall IsFirstVisibleChild(PVirtualNode Parent, PVirtualNode Node);
	bool __fastcall IsLastVisibleChild(PVirtualNode Parent, PVirtualNode Node);
	PVirtualNode __fastcall MakeNewNode(void);
	int __fastcall PackArray(const TNodeArray TheArray, int Count);
	void __fastcall PrepareBitmaps(bool NeedButtons, bool NeedLines);
	void __fastcall ReadOldOptions(System::Classes::TReader* Reader);
	void __fastcall SetAlignment(const System::Classes::TAlignment Value);
	void __fastcall SetAnimationDuration(const unsigned Value);
	void __fastcall SetBackground(Vcl::Graphics::TPicture* const Value);
	void __fastcall SetBackgroundOffset(const int Index, const int Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetBottomNode(PVirtualNode Node);
	void __fastcall SetBottomSpace(const unsigned Value);
	void __fastcall SetButtonFillMode(const TVTButtonFillMode Value);
	void __fastcall SetButtonStyle(const TVTButtonStyle Value);
	void __fastcall SetCheckImageKind(TCheckImageKind Value);
	void __fastcall SetCheckState(PVirtualNode Node, TCheckState Value);
	void __fastcall SetCheckType(PVirtualNode Node, TCheckType Value);
	void __fastcall SetChildCount(PVirtualNode Node, unsigned NewChildCount);
	void __fastcall SetClipboardFormats(TClipboardFormats* const Value);
	void __fastcall SetColors(TVTColors* const Value);
	void __fastcall SetCustomCheckImages(Vcl::Imglist::TCustomImageList* const Value);
	void __fastcall SetDefaultNodeHeight(unsigned Value);
	void __fastcall SetDisabled(PVirtualNode Node, bool Value);
	void __fastcall SetEmptyListMessage(const System::UnicodeString Value);
	void __fastcall SetExpanded(PVirtualNode Node, bool Value);
	void __fastcall SetFocusedColumn(TColumnIndex Value);
	void __fastcall SetFocusedNode(PVirtualNode Value);
	void __fastcall SetFullyVisible(PVirtualNode Node, bool Value);
	void __fastcall SetHasChildren(PVirtualNode Node, bool Value);
	void __fastcall SetHeader(TVTHeader* const Value);
	void __fastcall SetFiltered(PVirtualNode Node, bool Value);
	void __fastcall SetImages(Vcl::Imglist::TCustomImageList* const Value);
	void __fastcall SetIndent(unsigned Value);
	void __fastcall SetLineMode(const TVTLineMode Value);
	void __fastcall SetLineStyle(const TVTLineStyle Value);
	void __fastcall SetMargin(int Value);
	void __fastcall SetMultiline(PVirtualNode Node, const bool Value);
	void __fastcall SetNodeAlignment(const TVTNodeAlignment Value);
	void __fastcall SetNodeDataSize(int Value);
	void __fastcall SetNodeHeight(PVirtualNode Node, unsigned Value);
	void __fastcall SetNodeParent(PVirtualNode Node, const PVirtualNode Value);
	void __fastcall SetOffsetX(const int Value);
	void __fastcall SetOffsetXY(const System::Types::TPoint &Value);
	void __fastcall SetOffsetY(const int Value);
	void __fastcall SetOptions(TCustomVirtualTreeOptions* const Value);
	void __fastcall SetRootNodeCount(unsigned Value);
	void __fastcall SetScrollBarOptions(TScrollBarOptions* Value);
	void __fastcall SetSearchOption(const TVTIncrementalSearch Value);
	void __fastcall SetSelected(PVirtualNode Node, bool Value);
	void __fastcall SetSelectionCurveRadius(const unsigned Value);
	void __fastcall SetStateImages(Vcl::Imglist::TCustomImageList* const Value);
	void __fastcall SetTextMargin(int Value);
	void __fastcall SetTopNode(PVirtualNode Node);
	void __fastcall SetUpdateState(bool Updating);
	void __fastcall SetVerticalAlignment(PVirtualNode Node, System::Byte Value);
	HIDESBASE void __fastcall SetVisible(PVirtualNode Node, bool Value);
	void __fastcall SetVisiblePath(PVirtualNode Node, bool Value);
	void __fastcall StaticBackground(Vcl::Graphics::TBitmap* Source, Vcl::Graphics::TCanvas* Target, const System::Types::TPoint &OffsetPosition, const System::Types::TRect &R);
	void __fastcall StopTimer(int ID);
	void __fastcall SetWindowTheme(System::UnicodeString Theme);
	void __fastcall TileBackground(Vcl::Graphics::TBitmap* Source, Vcl::Graphics::TCanvas* Target, const System::Types::TPoint &Offset, const System::Types::TRect &R);
	bool __fastcall ToggleCallback(int Step, int StepSize, void * Data);
	MESSAGE void __fastcall CMColorChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMBiDiModeChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMBorderChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMDenySubclassing(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDrag(Vcl::Controls::TCMDrag &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Vcl::Controls::TCMHintShow &Message);
	MESSAGE void __fastcall CMHintShowPause(Vcl::Forms::TCMHintShowPause &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Vcl::Controls::TCMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall TVMGetItem(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall TVMGetItemRect(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall TVMGetNextItem(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMChangeState(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMContextMenu(Winapi::Messages::TWMContextMenu &Message);
	MESSAGE void __fastcall WMCopy(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMCut(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMEnable(Winapi::Messages::TWMEnable &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMGetObject(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMScroll &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyUp(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonUp(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMMButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMMButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMMButtonUp(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCDestroy(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TWMNCHitTest &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TWMNCPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	MESSAGE void __fastcall WMPaste(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMPrint(Winapi::Messages::TWMPrint &Message);
	HIDESBASE MESSAGE void __fastcall WMPrintClient(Winapi::Messages::TWMPrint &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	MESSAGE void __fastcall WMTimer(Winapi::Messages::TWMTimer &Message);
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMScroll &Message);
	unsigned __fastcall GetRangeX(void);
	bool __fastcall GetDoubleBuffered(void);
	HIDESBASE void __fastcall SetDoubleBuffered(const bool Value);
	void __fastcall ChangeTreeStatesAsync(TChangeStates EnterStates, TChangeStates LeaveStates);
	bool __fastcall GetIsSeBorderInStyleElement(void);
	
protected:
	bool FFontChanged;
	virtual void __fastcall AutoScale(void);
	virtual void __fastcall AddToSelection(PVirtualNode Node)/* overload */;
	virtual void __fastcall AddToSelection(const TNodeArray NewItems, int NewLength, bool ForceInsert = false)/* overload */;
	virtual void __fastcall AdjustImageBorder(Vcl::Imglist::TCustomImageList* Images, System::Classes::TBiDiMode BidiMode, int VAlign, System::Types::TRect &R, TVTImageInfo &ImageInfo);
	virtual void __fastcall AdjustPaintCellRect(TVTPaintInfo &PaintInfo, TColumnIndex &NextNonEmpty);
	virtual void __fastcall AdjustPanningCursor(int X, int Y);
	virtual void __fastcall AdviseChangeEvent(bool StructureChange, PVirtualNode Node, TChangeReason Reason);
	virtual unsigned __fastcall AllocateInternalDataArea(unsigned Size);
	virtual void __fastcall Animate(unsigned Steps, unsigned Duration, TVTAnimationCallback Callback, void * Data);
	virtual bool __fastcall CalculateSelectionRect(int X, int Y);
	virtual bool __fastcall CanAutoScroll(void);
	virtual bool __fastcall CanShowDragImage(void);
	bool __fastcall CanSplitterResizeNode(const System::Types::TPoint &P, PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall Change(PVirtualNode Node);
	DYNAMIC void __fastcall ChangeScale(int M, int D);
	virtual bool __fastcall CheckParentCheckState(PVirtualNode Node, TCheckState NewCheckState);
	virtual void __fastcall ClearTempCache(void);
	virtual bool __fastcall ColumnIsEmpty(PVirtualNode Node, TColumnIndex Column);
	virtual int __fastcall ComputeRTLOffset(bool ExcludeScrollBar = false);
	virtual int __fastcall CountLevelDifference(PVirtualNode Node1, PVirtualNode Node2);
	virtual unsigned __fastcall CountVisibleChildren(PVirtualNode Node);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual TDropMode __fastcall DetermineDropMode(const System::Types::TPoint &P, THitInfo &HitInfo, System::Types::TRect &NodeRect);
	virtual void __fastcall DetermineHiddenChildrenFlag(PVirtualNode Node);
	virtual void __fastcall DetermineHiddenChildrenFlagAllNodes(void);
	virtual void __fastcall DetermineHitPositionLTR(THitInfo &HitInfo, int Offset, int Right, System::Classes::TAlignment Alignment);
	virtual void __fastcall DetermineHitPositionRTL(THitInfo &HitInfo, int Offset, int Right, System::Classes::TAlignment Alignment);
	virtual int __fastcall DetermineLineImageAndSelectLevel(PVirtualNode Node, TLineImage &LineImage);
	virtual TCheckState __fastcall DetermineNextCheckState(TCheckType CheckType, TCheckState CheckState);
	virtual TScrollDirections __fastcall DetermineScrollDirections(int X, int Y);
	virtual void __fastcall DoAdvancedHeaderDraw(THeaderPaintInfo &PaintInfo, const THeaderPaintElements Elements);
	virtual void __fastcall DoAfterCellPaint(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, const System::Types::TRect &CellRect);
	virtual void __fastcall DoAfterItemErase(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, const System::Types::TRect &ItemRect);
	virtual void __fastcall DoAfterItemPaint(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, const System::Types::TRect &ItemRect);
	virtual void __fastcall DoAfterPaint(Vcl::Graphics::TCanvas* Canvas);
	virtual void __fastcall DoAutoScroll(int X, int Y);
	virtual bool __fastcall DoBeforeDrag(PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall DoBeforeCellPaint(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, TVTCellPaintMode CellPaintMode, const System::Types::TRect &CellRect, System::Types::TRect &ContentRect);
	virtual void __fastcall DoBeforeItemErase(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, const System::Types::TRect &ItemRect, System::Uitypes::TColor &Color, TItemEraseAction &EraseAction);
	virtual bool __fastcall DoBeforeItemPaint(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, const System::Types::TRect &ItemRect);
	virtual void __fastcall DoBeforePaint(Vcl::Graphics::TCanvas* Canvas);
	virtual bool __fastcall DoCancelEdit(void);
	virtual void __fastcall DoCanEdit(PVirtualNode Node, TColumnIndex Column, bool &Allowed);
	virtual void __fastcall DoCanSplitterResizeNode(const System::Types::TPoint &P, PVirtualNode Node, TColumnIndex Column, bool &Allowed);
	virtual void __fastcall DoChange(PVirtualNode Node);
	virtual void __fastcall DoCheckClick(PVirtualNode Node, TCheckState NewCheckState);
	virtual void __fastcall DoChecked(PVirtualNode Node);
	virtual bool __fastcall DoChecking(PVirtualNode Node, TCheckState &NewCheckState);
	virtual void __fastcall DoCollapsed(PVirtualNode Node);
	virtual bool __fastcall DoCollapsing(PVirtualNode Node);
	virtual void __fastcall DoColumnClick(TColumnIndex Column, System::Classes::TShiftState Shift);
	virtual void __fastcall DoColumnDblClick(TColumnIndex Column, System::Classes::TShiftState Shift);
	virtual void __fastcall DoColumnResize(TColumnIndex Column);
	virtual int __fastcall DoCompare(PVirtualNode Node1, PVirtualNode Node2, TColumnIndex Column);
	virtual _di_IDataObject __fastcall DoCreateDataObject(void);
	virtual _di_IVTDragManager __fastcall DoCreateDragManager(void);
	virtual _di_IVTEditLink __fastcall DoCreateEditor(PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall DoDragging(const System::Types::TPoint &P);
	virtual void __fastcall DoDragExpand(void);
	virtual void __fastcall DoBeforeDrawLineImage(PVirtualNode Node, int Level, int &XPos);
	virtual bool __fastcall DoDragOver(System::TObject* Source, System::Classes::TShiftState Shift, System::Uitypes::TDragState State, const System::Types::TPoint &Pt, TDropMode Mode, int &Effect);
	virtual void __fastcall DoDragDrop(System::TObject* Source, _di_IDataObject DataObject, TFormatArray Formats, System::Classes::TShiftState Shift, const System::Types::TPoint &Pt, int &Effect, TDropMode Mode);
	void __fastcall DoDrawHint(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, const System::Types::TRect &R, TColumnIndex Column);
	virtual void __fastcall DoEdit(void);
	DYNAMIC void __fastcall DoEndDrag(System::TObject* Target, int X, int Y);
	virtual bool __fastcall DoEndEdit(void);
	virtual void __fastcall DoEndOperation(TVTOperationKind OperationKind);
	DYNAMIC void __fastcall DoEnter(void);
	virtual void __fastcall DoExpanded(PVirtualNode Node);
	virtual bool __fastcall DoExpanding(PVirtualNode Node);
	virtual void __fastcall DoFocusChange(PVirtualNode Node, TColumnIndex Column);
	virtual bool __fastcall DoFocusChanging(PVirtualNode OldNode, PVirtualNode NewNode, TColumnIndex OldColumn, TColumnIndex NewColumn);
	virtual void __fastcall DoFocusNode(PVirtualNode Node, bool Ask);
	virtual void __fastcall DoFreeNode(PVirtualNode Node);
	virtual THintAnimationType __fastcall DoGetAnimationType(void);
	virtual System::Types::TPoint __fastcall DoGetCellContentMargin(PVirtualNode Node, TColumnIndex Column, TVTCellContentMarginType CellContentMarginType = (TVTCellContentMarginType)(0x0), Vcl::Graphics::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual void __fastcall DoGetCursor(System::Uitypes::TCursor &Cursor);
	virtual void __fastcall DoGetHeaderCursor(HICON &Cursor);
	virtual void __fastcall DoGetHintSize(PVirtualNode Node, TColumnIndex Column, System::Types::TRect &R);
	void __fastcall DoGetHintKind(PVirtualNode Node, TColumnIndex Column, TVTHintKind &Kind);
	virtual Vcl::Imglist::TCustomImageList* __fastcall DoGetImageIndex(PVirtualNode Node, TVTImageKind Kind, TColumnIndex Column, bool &Ghosted, int &Index);
	virtual void __fastcall DoGetImageText(PVirtualNode Node, TVTImageKind Kind, TColumnIndex Column, System::UnicodeString &ImageText);
	virtual void __fastcall DoGetLineStyle(void * &Bits);
	virtual System::UnicodeString __fastcall DoGetNodeHint(PVirtualNode Node, TColumnIndex Column, TVTTooltipLineBreakStyle &LineBreakStyle);
	virtual System::UnicodeString __fastcall DoGetNodeTooltip(PVirtualNode Node, TColumnIndex Column, TVTTooltipLineBreakStyle &LineBreakStyle);
	virtual int __fastcall DoGetNodeExtraWidth(PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual int __fastcall DoGetNodeWidth(PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual Vcl::Menus::TPopupMenu* __fastcall DoGetPopupMenu(PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &Position);
	virtual void __fastcall DoGetUserClipboardFormats(TFormatEtcArray &Formats);
	virtual void __fastcall DoHeaderClick(const TVTHeaderHitInfo &HitInfo);
	virtual void __fastcall DoHeaderDblClick(const TVTHeaderHitInfo &HitInfo);
	virtual void __fastcall DoHeaderDragged(TColumnIndex Column, TColumnPosition OldPosition);
	virtual void __fastcall DoHeaderDraggedOut(TColumnIndex Column, const System::Types::TPoint &DropPosition);
	virtual bool __fastcall DoHeaderDragging(TColumnIndex Column);
	virtual void __fastcall DoHeaderDraw(Vcl::Graphics::TCanvas* Canvas, TVirtualTreeColumn* Column, const System::Types::TRect &R, bool Hover, bool Pressed, TVTDropMarkMode DropMark);
	virtual void __fastcall DoHeaderDrawQueryElements(THeaderPaintInfo &PaintInfo, THeaderPaintElements &Elements);
	virtual void __fastcall DoHeaderMouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall DoHeaderMouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall DoHeaderMouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall DoHotChange(PVirtualNode Old, PVirtualNode New);
	virtual int __fastcall DoIncrementalSearch(PVirtualNode Node, const System::UnicodeString Text);
	virtual bool __fastcall DoInitChildren(PVirtualNode Node, unsigned &ChildCount);
	virtual void __fastcall DoInitNode(PVirtualNode Parent, PVirtualNode Node, TVirtualNodeInitStates &InitStates);
	virtual bool __fastcall DoKeyAction(System::Word &CharCode, System::Classes::TShiftState &Shift);
	virtual void __fastcall DoLoadUserData(PVirtualNode Node, System::Classes::TStream* Stream);
	virtual void __fastcall DoMeasureItem(Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, int &NodeHeight);
	virtual void __fastcall DoMouseEnter(void);
	virtual void __fastcall DoMouseLeave(void);
	virtual void __fastcall DoNodeCopied(PVirtualNode Node);
	virtual bool __fastcall DoNodeCopying(PVirtualNode Node, PVirtualNode NewParent);
	virtual void __fastcall DoNodeClick(const THitInfo &HitInfo);
	virtual void __fastcall DoNodeDblClick(const THitInfo &HitInfo);
	virtual bool __fastcall DoNodeHeightDblClickResize(PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, const System::Types::TPoint &P);
	virtual bool __fastcall DoNodeHeightTracking(PVirtualNode Node, TColumnIndex Column, System::Classes::TShiftState Shift, System::Types::TPoint &TrackPoint, const System::Types::TPoint &P);
	virtual void __fastcall DoNodeMoved(PVirtualNode Node);
	virtual bool __fastcall DoNodeMoving(PVirtualNode Node, PVirtualNode NewParent);
	virtual bool __fastcall DoPaintBackground(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual void __fastcall DoPaintDropMark(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, const System::Types::TRect &R);
	virtual void __fastcall DoPaintNode(TVTPaintInfo &PaintInfo);
	virtual void __fastcall DoPopupMenu(PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &Position);
	virtual void __fastcall DoRemoveFromSelection(PVirtualNode Node);
	virtual HRESULT __fastcall DoRenderOLEData(const tagFORMATETC &FormatEtcIn, /* out */ tagSTGMEDIUM &Medium, bool ForClipboard);
	virtual void __fastcall DoReset(PVirtualNode Node);
	virtual void __fastcall DoSaveUserData(PVirtualNode Node, System::Classes::TStream* Stream);
	virtual void __fastcall DoScroll(int DeltaX, int DeltaY);
	virtual bool __fastcall DoSetOffsetXY(const System::Types::TPoint &Value, TScrollUpdateOptions Options, System::Types::PRect ClipRect = (System::Types::PRect)(0x0));
	virtual void __fastcall DoShowScrollBar(int Bar, bool Show);
	DYNAMIC void __fastcall DoStartDrag(Vcl::Controls::TDragObject* &DragObject);
	virtual void __fastcall DoStartOperation(TVTOperationKind OperationKind);
	virtual void __fastcall DoStateChange(const TVirtualTreeStates &Enter, const TVirtualTreeStates &Leave = TVirtualTreeStates() );
	virtual void __fastcall DoStructureChange(PVirtualNode Node, TChangeReason Reason);
	virtual void __fastcall DoTimerScroll(void);
	virtual void __fastcall DoUpdating(TVTUpdateState State);
	virtual bool __fastcall DoValidateCache(void);
	virtual void __fastcall DragAndDrop(unsigned AllowedEffects, _di_IDataObject DataObject, int &DragEffect);
	DYNAMIC void __fastcall DragCanceled(void);
	HIDESBASE virtual HRESULT __fastcall DragDrop(const _di_IDataObject DataObject, int KeyState, const System::Types::TPoint &Pt, int &Effect);
	virtual HRESULT __fastcall DragEnter(int KeyState, const System::Types::TPoint &Pt, int &Effect);
	virtual void __fastcall DragFinished(void);
	virtual void __fastcall DragLeave(void);
	HIDESBASE virtual HRESULT __fastcall DragOver(System::TObject* Source, int KeyState, System::Uitypes::TDragState DragState, const System::Types::TPoint &Pt, int &Effect);
	virtual void __fastcall DrawDottedHLine(const TVTPaintInfo &PaintInfo, int Left, int Right, int Top);
	virtual void __fastcall DrawDottedVLine(const TVTPaintInfo &PaintInfo, int Top, int Bottom, int Left, bool UseSelectedBkColor = false);
	void __fastcall EndOperation(TVTOperationKind OperationKind);
	virtual void __fastcall EnsureNodeFocused(void);
	virtual bool __fastcall FindNodeInSelection(PVirtualNode P, int &Index, int LowBound, int HighBound);
	virtual void __fastcall FinishChunkHeader(System::Classes::TStream* Stream, int StartPos, int EndPos);
	HIDESBASE virtual void __fastcall FontChanged(System::TObject* AFont);
	virtual System::Types::TSize __fastcall GetBorderDimensions(void);
	virtual int __fastcall GetCheckImage(PVirtualNode Node, TCheckType ImgCheckType = (TCheckType)(0x0), TCheckState ImgCheckState = (TCheckState)(0x0), bool ImgEnabled = true);
	__classmethod virtual Vcl::Imglist::TCustomImageList* __fastcall GetCheckImageListFor(TCheckImageKind Kind);
	virtual TVirtualTreeColumnClass __fastcall GetColumnClass(void);
	virtual TVTHintKind __fastcall GetDefaultHintKind(void);
	virtual TVTHeaderClass __fastcall GetHeaderClass(void);
	virtual Vcl::Controls::THintWindowClass __fastcall GetHintWindowClass(void);
	virtual void __fastcall GetImageIndex(TVTPaintInfo &Info, TVTImageKind Kind, TVTImageInfoIndex InfoIndex, Vcl::Imglist::TCustomImageList* DefaultImages);
	virtual System::Types::TSize __fastcall GetNodeImageSize(PVirtualNode Node);
	virtual unsigned __fastcall GetMaxRightExtend(void);
	virtual void __fastcall GetNativeClipboardFormats(TFormatEtcArray &Formats);
	bool __fastcall GetOperationCanceled(void);
	virtual TTreeOptionsClass __fastcall GetOptionsClass(void);
	virtual TBaseVirtualTree* __fastcall GetTreeFromDataObject(const _di_IDataObject DataObject);
	virtual void __fastcall HandleHotTrack(int X, int Y);
	virtual void __fastcall HandleIncrementalSearch(System::Word CharCode);
	virtual void __fastcall HandleMouseDblClick(Winapi::Messages::TWMMouse &Message, const THitInfo &HitInfo);
	virtual void __fastcall HandleMouseDown(Winapi::Messages::TWMMouse &Message, THitInfo &HitInfo);
	virtual void __fastcall HandleMouseUp(Winapi::Messages::TWMMouse &Message, const THitInfo &HitInfo);
	virtual bool __fastcall HasImage(PVirtualNode Node, TVTImageKind Kind, TColumnIndex Column);
	virtual bool __fastcall HasPopupMenu(PVirtualNode Node, TColumnIndex Column, const System::Types::TPoint &Pos);
	virtual void __fastcall InitChildren(PVirtualNode Node);
	virtual void __fastcall InitNode(PVirtualNode Node);
	virtual void __fastcall InternalAddFromStream(System::Classes::TStream* Stream, int Version, PVirtualNode Node);
	bool __fastcall InternalAddToSelection(PVirtualNode Node, bool ForceInsert)/* overload */;
	bool __fastcall InternalAddToSelection(const TNodeArray NewItems, int NewLength, bool ForceInsert)/* overload */;
	virtual void __fastcall InternalCacheNode(PVirtualNode Node);
	virtual void __fastcall InternalClearSelection(void);
	virtual void __fastcall InternalConnectNode(PVirtualNode Node, PVirtualNode Destination, TBaseVirtualTree* Target, TVTNodeAttachMode Mode);
	void * __fastcall InternalData(PVirtualNode Node);
	virtual void __fastcall InternalDisconnectNode(PVirtualNode Node, bool KeepFocus, bool Reindex = true);
	virtual void __fastcall InternalRemoveFromSelection(PVirtualNode Node);
	void __fastcall InvalidateCache(void);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall MainColumnChanged(void);
	virtual void __fastcall MarkCutCopyNodes(void);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall OriginalWMNCPaint(HDC DC);
	virtual void __fastcall Paint(void);
	virtual void __fastcall PaintCheckImage(Vcl::Graphics::TCanvas* Canvas, const TVTImageInfo &ImageInfo, bool Selected);
	virtual void __fastcall PaintImage(TVTPaintInfo &PaintInfo, TVTImageInfoIndex ImageInfoIndex, bool DoOverlay);
	virtual void __fastcall PaintNodeButton(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, const System::Types::TRect &R, int ButtonX, int ButtonY, System::Classes::TBiDiMode BidiMode);
	virtual void __fastcall PaintTreeLines(const TVTPaintInfo &PaintInfo, int VAlignment, int IndentSize, TLineImage LineImage);
	virtual void __fastcall PaintSelectionRectangle(Vcl::Graphics::TCanvas* Target, int WindowOrgX, const System::Types::TRect &SelectionRect, const System::Types::TRect &TargetRect);
	virtual void __fastcall PanningWindowProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall PrepareCell(TVTPaintInfo &PaintInfo, int WindowOrgX, int MaxWidth);
	virtual bool __fastcall ReadChunk(System::Classes::TStream* Stream, int Version, PVirtualNode Node, int ChunkType, int ChunkSize);
	virtual void __fastcall ReadNode(System::Classes::TStream* Stream, int Version, PVirtualNode Node);
	virtual void __fastcall RedirectFontChangeEvent(Vcl::Graphics::TCanvas* Canvas);
	virtual void __fastcall RemoveFromSelection(PVirtualNode Node);
	virtual void __fastcall UpdateNextNodeToSelect(PVirtualNode Node);
	virtual HRESULT __fastcall RenderOLEData(const tagFORMATETC &FormatEtcIn, /* out */ tagSTGMEDIUM &Medium, bool ForClipboard);
	virtual void __fastcall ResetRangeAnchor(void);
	virtual void __fastcall RestoreFontChangeEvent(Vcl::Graphics::TCanvas* Canvas);
	virtual void __fastcall SelectNodes(PVirtualNode StartNode, PVirtualNode EndNode, bool AddOnly);
	virtual void __fastcall SetFocusedNodeAndColumn(PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall SkipNode(System::Classes::TStream* Stream);
	void __fastcall StartOperation(TVTOperationKind OperationKind);
	virtual void __fastcall StartWheelPanning(const System::Types::TPoint &Position);
	virtual void __fastcall StopWheelPanning(void);
	virtual void __fastcall StructureChange(PVirtualNode Node, TChangeReason Reason);
	virtual int __fastcall SuggestDropEffect(System::TObject* Source, System::Classes::TShiftState Shift, const System::Types::TPoint &Pt, int AllowedEffects);
	virtual void __fastcall ToggleSelection(PVirtualNode StartNode, PVirtualNode EndNode);
	virtual void __fastcall UnselectNodes(PVirtualNode StartNode, PVirtualNode EndNode);
	void __fastcall UpdateColumnCheckState(TVirtualTreeColumn* Col);
	virtual void __fastcall UpdateDesigner(void);
	virtual void __fastcall UpdateEditBounds(void);
	virtual void __fastcall UpdateHeaderRect(void);
	virtual void __fastcall UpdateStyleElements(void);
	virtual void __fastcall UpdateWindowAndDragImage(TBaseVirtualTree* const Tree, const System::Types::TRect &TreeRect, bool UpdateNCArea, bool ReshowDragImage);
	virtual void __fastcall ValidateCache(void);
	virtual void __fastcall ValidateNodeDataSize(int &Size);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall WriteChunks(System::Classes::TStream* Stream, PVirtualNode Node);
	virtual void __fastcall WriteNode(System::Classes::TStream* Stream, PVirtualNode Node);
	void __fastcall VclStyleChanged(void);
	__property bool VclStyleEnabled = {read=FVclStyleEnabled, nodefault};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property unsigned AnimationDuration = {read=FAnimationDuration, write=SetAnimationDuration, default=200};
	__property unsigned AutoExpandDelay = {read=FAutoExpandDelay, write=FAutoExpandDelay, default=1000};
	__property unsigned AutoScrollDelay = {read=FAutoScrollDelay, write=FAutoScrollDelay, default=1000};
	__property TAutoScrollInterval AutoScrollInterval = {read=FAutoScrollInterval, write=FAutoScrollInterval, default=1};
	__property Vcl::Graphics::TPicture* Background = {read=FBackground, write=SetBackground};
	__property int BackgroundOffsetX = {read=FBackgroundOffsetX, write=SetBackgroundOffset, index=0, default=0};
	__property int BackgroundOffsetY = {read=FBackgroundOffsetY, write=SetBackgroundOffset, index=1, default=0};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property unsigned BottomSpace = {read=FBottomSpace, write=SetBottomSpace, default=0};
	__property TVTButtonFillMode ButtonFillMode = {read=FButtonFillMode, write=SetButtonFillMode, default=0};
	__property TVTButtonStyle ButtonStyle = {read=FButtonStyle, write=SetButtonStyle, default=0};
	__property unsigned ChangeDelay = {read=FChangeDelay, write=FChangeDelay, default=0};
	__property TCheckImageKind CheckImageKind = {read=FCheckImageKind, write=SetCheckImageKind, default=8};
	__property TClipboardFormats* ClipboardFormats = {read=FClipboardFormats, write=SetClipboardFormats};
	__property TVTColors* Colors = {read=FColors, write=SetColors};
	__property Vcl::Imglist::TCustomImageList* CustomCheckImages = {read=FCustomCheckImages, write=SetCustomCheckImages};
	__property TVTHintKind DefaultHintKind = {read=GetDefaultHintKind, nodefault};
	__property unsigned DefaultNodeHeight = {read=FDefaultNodeHeight, write=SetDefaultNodeHeight, default=18};
	__property TVTNodeAttachMode DefaultPasteMode = {read=FDefaultPasteMode, write=FDefaultPasteMode, default=4};
	__property int DragHeight = {read=FDragHeight, write=FDragHeight, default=350};
	__property TVTDragImageKind DragImageKind = {read=FDragImageKind, write=FDragImageKind, default=0};
	__property TDragOperations DragOperations = {read=FDragOperations, write=FDragOperations, default=3};
	__property TNodeArray DragSelection = {read=FDragSelection};
	__property int LastDragEffect = {read=FLastDragEffect, nodefault};
	__property TVTDragType DragType = {read=FDragType, write=FDragType, default=0};
	__property int DragWidth = {read=FDragWidth, write=FDragWidth, default=200};
	__property TVTDrawSelectionMode DrawSelectionMode = {read=FDrawSelectionMode, write=FDrawSelectionMode, default=0};
	__property TColumnIndex EditColumn = {read=FEditColumn, write=FEditColumn, nodefault};
	__property unsigned EditDelay = {read=FEditDelay, write=FEditDelay, default=1000};
	__property int EffectiveOffsetX = {read=FEffectiveOffsetX, nodefault};
	__property TVTHeader* Header = {read=FHeader, write=SetHeader};
	__property System::Types::TRect HeaderRect = {read=FHeaderRect};
	__property THintAnimationType HintAnimation = {read=FAnimationType, write=FAnimationType, default=3};
	__property TVTHintMode HintMode = {read=FHintMode, write=FHintMode, default=0};
	__property TVTHintData HintData = {read=FHintData, write=FHintData};
	__property System::Uitypes::TCursor HotCursor = {read=FHotCursor, write=FHotCursor, default=0};
	__property Vcl::Imglist::TCustomImageList* Images = {read=FImages, write=SetImages};
	__property TVTIncrementalSearch IncrementalSearch = {read=FIncrementalSearch, write=SetSearchOption, default=1};
	__property TVTSearchDirection IncrementalSearchDirection = {read=FSearchDirection, write=FSearchDirection, default=0};
	__property TVTSearchStart IncrementalSearchStart = {read=FSearchStart, write=FSearchStart, default=2};
	__property unsigned IncrementalSearchTimeout = {read=FSearchTimeout, write=FSearchTimeout, default=1000};
	__property unsigned Indent = {read=FIndent, write=SetIndent, default=18};
	__property bool IsSeBorderInStyleElement = {read=GetIsSeBorderInStyleElement, nodefault};
	__property System::Types::TPoint LastClickPos = {read=FLastClickPos, write=FLastClickPos};
	__property TDropMode LastDropMode = {read=FLastDropMode, write=FLastDropMode, nodefault};
	__property System::Types::TRect LastHintRect = {read=FLastHintRect, write=FLastHintRect};
	__property TVTLineMode LineMode = {read=FLineMode, write=SetLineMode, default=0};
	__property TVTLineStyle LineStyle = {read=FLineStyle, write=SetLineStyle, default=1};
	__property int Margin = {read=FMargin, write=SetMargin, default=4};
	__property PVirtualNode NextNodeToSelect = {read=FNextNodeToSelect};
	__property TVTNodeAlignment NodeAlignment = {read=FNodeAlignment, write=SetNodeAlignment, default=2};
	__property int NodeDataSize = {read=FNodeDataSize, write=SetNodeDataSize, default=-1};
	__property bool OperationCanceled = {read=GetOperationCanceled, nodefault};
	__property Vcl::Graphics::TBitmap* HotMinusBM = {read=FHotMinusBM};
	__property Vcl::Graphics::TBitmap* HotPlusBM = {read=FHotPlusBM};
	__property Vcl::Graphics::TBitmap* MinusBM = {read=FMinusBM};
	__property Vcl::Graphics::TBitmap* PlusBM = {read=FPlusBM};
	__property unsigned RangeX = {read=GetRangeX, nodefault};
	__property unsigned RangeY = {read=FRangeY, nodefault};
	__property unsigned RootNodeCount = {read=GetRootNodeCount, write=SetRootNodeCount, default=0};
	__property TScrollBarOptions* ScrollBarOptions = {read=FScrollBarOptions, write=SetScrollBarOptions};
	__property System::Byte SelectionBlendFactor = {read=FSelectionBlendFactor, write=FSelectionBlendFactor, default=128};
	__property unsigned SelectionCurveRadius = {read=FSelectionCurveRadius, write=SetSelectionCurveRadius, default=0};
	__property Vcl::Imglist::TCustomImageList* StateImages = {read=FStateImages, write=SetStateImages};
	__property int TextMargin = {read=FTextMargin, write=SetTextMargin, default=4};
	__property unsigned TotalInternalDataSize = {read=FTotalInternalDataSize, nodefault};
	__property TCustomVirtualTreeOptions* TreeOptions = {read=FOptions, write=SetOptions};
	__property bool WantTabs = {read=FWantTabs, write=FWantTabs, default=0};
	__property TVTAddToSelectionEvent OnAddToSelection = {read=FOnAddToSelection, write=FOnAddToSelection};
	__property TVTAdvancedHeaderPaintEvent OnAdvancedHeaderDraw = {read=FOnAdvancedHeaderDraw, write=FOnAdvancedHeaderDraw};
	__property TVTAfterAutoFitColumnEvent OnAfterAutoFitColumn = {read=FOnAfterAutoFitColumn, write=FOnAfterAutoFitColumn};
	__property TVTAfterAutoFitColumnsEvent OnAfterAutoFitColumns = {read=FOnAfterAutoFitColumns, write=FOnAfterAutoFitColumns};
	__property TVTAfterCellPaintEvent OnAfterCellPaint = {read=FOnAfterCellPaint, write=FOnAfterCellPaint};
	__property TVTColumnExportEvent OnAfterColumnExport = {read=FOnAfterColumnExport, write=FOnAfterColumnExport};
	__property TVTAfterColumnWidthTrackingEvent OnAfterColumnWidthTracking = {read=FOnAfterColumnWidthTracking, write=FOnAfterColumnWidthTracking};
	__property TVTAfterGetMaxColumnWidthEvent OnAfterGetMaxColumnWidth = {read=FOnAfterGetMaxColumnWidth, write=FOnAfterGetMaxColumnWidth};
	__property TVTTreeExportEvent OnAfterHeaderExport = {read=FOnAfterHeaderExport, write=FOnAfterHeaderExport};
	__property TVTAfterHeaderHeightTrackingEvent OnAfterHeaderHeightTracking = {read=FOnAfterHeaderHeightTracking, write=FOnAfterHeaderHeightTracking};
	__property TVTAfterItemEraseEvent OnAfterItemErase = {read=FOnAfterItemErase, write=FOnAfterItemErase};
	__property TVTAfterItemPaintEvent OnAfterItemPaint = {read=FOnAfterItemPaint, write=FOnAfterItemPaint};
	__property TVTNodeExportEvent OnAfterNodeExport = {read=FOnAfterNodeExport, write=FOnAfterNodeExport};
	__property TVTPaintEvent OnAfterPaint = {read=FOnAfterPaint, write=FOnAfterPaint};
	__property TVTTreeExportEvent OnAfterTreeExport = {read=FOnAfterTreeExport, write=FOnAfterTreeExport};
	__property TVTBeforeAutoFitColumnEvent OnBeforeAutoFitColumn = {read=FOnBeforeAutoFitColumn, write=FOnBeforeAutoFitColumn};
	__property TVTBeforeAutoFitColumnsEvent OnBeforeAutoFitColumns = {read=FOnBeforeAutoFitColumns, write=FOnBeforeAutoFitColumns};
	__property TVTBeforeCellPaintEvent OnBeforeCellPaint = {read=FOnBeforeCellPaint, write=FOnBeforeCellPaint};
	__property TVTColumnExportEvent OnBeforeColumnExport = {read=FOnBeforeColumnExport, write=FOnBeforeColumnExport};
	__property TVTBeforeColumnWidthTrackingEvent OnBeforeColumnWidthTracking = {read=FOnBeforeColumnWidthTracking, write=FOnBeforeColumnWidthTracking};
	__property TVTBeforeDrawLineImageEvent OnBeforeDrawTreeLine = {read=FOnBeforeDrawLineImage, write=FOnBeforeDrawLineImage};
	__property TVTBeforeGetMaxColumnWidthEvent OnBeforeGetMaxColumnWidth = {read=FOnBeforeGetMaxColumnWidth, write=FOnBeforeGetMaxColumnWidth};
	__property TVTTreeExportEvent OnBeforeHeaderExport = {read=FOnBeforeHeaderExport, write=FOnBeforeHeaderExport};
	__property TVTBeforeHeaderHeightTrackingEvent OnBeforeHeaderHeightTracking = {read=FOnBeforeHeaderHeightTracking, write=FOnBeforeHeaderHeightTracking};
	__property TVTBeforeItemEraseEvent OnBeforeItemErase = {read=FOnBeforeItemErase, write=FOnBeforeItemErase};
	__property TVTBeforeItemPaintEvent OnBeforeItemPaint = {read=FOnBeforeItemPaint, write=FOnBeforeItemPaint};
	__property TVTNodeExportEvent OnBeforeNodeExport = {read=FOnBeforeNodeExport, write=FOnBeforeNodeExport};
	__property TVTPaintEvent OnBeforePaint = {read=FOnBeforePaint, write=FOnBeforePaint};
	__property TVTTreeExportEvent OnBeforeTreeExport = {read=FOnBeforeTreeExport, write=FOnBeforeTreeExport};
	__property TVTCanSplitterResizeColumnEvent OnCanSplitterResizeColumn = {read=FOnCanSplitterResizeColumn, write=FOnCanSplitterResizeColumn};
	__property TVTCanSplitterResizeHeaderEvent OnCanSplitterResizeHeader = {read=FOnCanSplitterResizeHeader, write=FOnCanSplitterResizeHeader};
	__property TVTCanSplitterResizeNodeEvent OnCanSplitterResizeNode = {read=FOnCanSplitterResizeNode, write=FOnCanSplitterResizeNode};
	__property TVTChangeEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TVTChangeEvent OnChecked = {read=FOnChecked, write=FOnChecked};
	__property TVTCheckChangingEvent OnChecking = {read=FOnChecking, write=FOnChecking};
	__property TVTChangeEvent OnCollapsed = {read=FOnCollapsed, write=FOnCollapsed};
	__property TVTChangingEvent OnCollapsing = {read=FOnCollapsing, write=FOnCollapsing};
	__property TVTColumnClickEvent OnColumnClick = {read=FOnColumnClick, write=FOnColumnClick};
	__property TVTColumnDblClickEvent OnColumnDblClick = {read=FOnColumnDblClick, write=FOnColumnDblClick};
	__property TVTColumnExportEvent OnColumnExport = {read=FOnColumnExport, write=FOnColumnExport};
	__property TVTHeaderNotifyEvent OnColumnResize = {read=FOnColumnResize, write=FOnColumnResize};
	__property TVTColumnWidthDblClickResizeEvent OnColumnWidthDblClickResize = {read=FOnColumnWidthDblClickResize, write=FOnColumnWidthDblClickResize};
	__property TVTColumnWidthTrackingEvent OnColumnWidthTracking = {read=FOnColumnWidthTracking, write=FOnColumnWidthTracking};
	__property TVTCompareEvent OnCompareNodes = {read=FOnCompareNodes, write=FOnCompareNodes};
	__property TVTCreateDataObjectEvent OnCreateDataObject = {read=FOnCreateDataObject, write=FOnCreateDataObject};
	__property TVTCreateDragManagerEvent OnCreateDragManager = {read=FOnCreateDragManager, write=FOnCreateDragManager};
	__property TVTCreateEditorEvent OnCreateEditor = {read=FOnCreateEditor, write=FOnCreateEditor};
	__property TVTDragAllowedEvent OnDragAllowed = {read=FOnDragAllowed, write=FOnDragAllowed};
	__property TVTDragOverEvent OnDragOver = {read=FOnDragOver, write=FOnDragOver};
	__property TVTDragDropEvent OnDragDrop = {read=FOnDragDrop, write=FOnDragDrop};
	__property TVTDrawHintEvent OnDrawHint = {read=FOnDrawHint, write=FOnDrawHint};
	__property TVTEditCancelEvent OnEditCancelled = {read=FOnEditCancelled, write=FOnEditCancelled};
	__property TVTEditChangingEvent OnEditing = {read=FOnEditing, write=FOnEditing};
	__property TVTEditChangeEvent OnEdited = {read=FOnEdited, write=FOnEdited};
	__property TVTOperationEvent OnEndOperation = {read=FOnEndOperation, write=FOnEndOperation};
	__property TVTChangeEvent OnExpanded = {read=FOnExpanded, write=FOnExpanded};
	__property TVTChangingEvent OnExpanding = {read=FOnExpanding, write=FOnExpanding};
	__property TVTFocusChangeEvent OnFocusChanged = {read=FOnFocusChanged, write=FOnFocusChanged};
	__property TVTFocusChangingEvent OnFocusChanging = {read=FOnFocusChanging, write=FOnFocusChanging};
	__property TVTFreeNodeEvent OnFreeNode = {read=FOnFreeNode, write=FOnFreeNode};
	__property TVTGetCellIsEmptyEvent OnGetCellIsEmpty = {read=FOnGetCellIsEmpty, write=FOnGetCellIsEmpty};
	__property TVTGetCursorEvent OnGetCursor = {read=FOnGetCursor, write=FOnGetCursor};
	__property TVTGetHeaderCursorEvent OnGetHeaderCursor = {read=FOnGetHeaderCursor, write=FOnGetHeaderCursor};
	__property TVTHelpContextEvent OnGetHelpContext = {read=FOnGetHelpContext, write=FOnGetHelpContext};
	__property TVTGetHintSizeEvent OnGetHintSize = {read=FOnGetHintSize, write=FOnGetHintSize};
	__property TVTHintKindEvent OnGetHintKind = {read=FOnGetHintKind, write=FOnGetHintKind};
	__property TVTGetImageEvent OnGetImageIndex = {read=FOnGetImage, write=FOnGetImage};
	__property TVTGetImageExEvent OnGetImageIndexEx = {read=FOnGetImageEx, write=FOnGetImageEx};
	__property TVTGetImageTextEvent OnGetImageText = {read=FOnGetImageText, write=FOnGetImageText};
	__property TVTGetLineStyleEvent OnGetLineStyle = {read=FOnGetLineStyle, write=FOnGetLineStyle};
	__property TVTGetNodeDataSizeEvent OnGetNodeDataSize = {read=FOnGetNodeDataSize, write=FOnGetNodeDataSize};
	__property TVTPopupEvent OnGetPopupMenu = {read=FOnGetPopupMenu, write=FOnGetPopupMenu};
	__property TVTGetUserClipboardFormatsEvent OnGetUserClipboardFormats = {read=FOnGetUserClipboardFormats, write=FOnGetUserClipboardFormats};
	__property TVTHeaderClickEvent OnHeaderClick = {read=FOnHeaderClick, write=FOnHeaderClick};
	__property TVTHeaderClickEvent OnHeaderDblClick = {read=FOnHeaderDblClick, write=FOnHeaderDblClick};
	__property TVTHeaderDraggedEvent OnHeaderDragged = {read=FOnHeaderDragged, write=FOnHeaderDragged};
	__property TVTHeaderDraggedOutEvent OnHeaderDraggedOut = {read=FOnHeaderDraggedOut, write=FOnHeaderDraggedOut};
	__property TVTHeaderDraggingEvent OnHeaderDragging = {read=FOnHeaderDragging, write=FOnHeaderDragging};
	__property TVTHeaderPaintEvent OnHeaderDraw = {read=FOnHeaderDraw, write=FOnHeaderDraw};
	__property TVTHeaderPaintQueryElementsEvent OnHeaderDrawQueryElements = {read=FOnHeaderDrawQueryElements, write=FOnHeaderDrawQueryElements};
	__property TVTHeaderHeightTrackingEvent OnHeaderHeightTracking = {read=FOnHeaderHeightTracking, write=FOnHeaderHeightTracking};
	__property TVTHeaderHeightDblClickResizeEvent OnHeaderHeightDblClickResize = {read=FOnHeaderHeightDblClickResize, write=FOnHeaderHeightDblClickResize};
	__property TVTHeaderMouseEvent OnHeaderMouseDown = {read=FOnHeaderMouseDown, write=FOnHeaderMouseDown};
	__property TVTHeaderMouseMoveEvent OnHeaderMouseMove = {read=FOnHeaderMouseMove, write=FOnHeaderMouseMove};
	__property TVTHeaderMouseEvent OnHeaderMouseUp = {read=FOnHeaderMouseUp, write=FOnHeaderMouseUp};
	__property TVTHotNodeChangeEvent OnHotChange = {read=FOnHotChange, write=FOnHotChange};
	__property TVTIncrementalSearchEvent OnIncrementalSearch = {read=FOnIncrementalSearch, write=FOnIncrementalSearch};
	__property TVTInitChildrenEvent OnInitChildren = {read=FOnInitChildren, write=FOnInitChildren};
	__property TVTInitNodeEvent OnInitNode = {read=FOnInitNode, write=FOnInitNode};
	__property TVTKeyActionEvent OnKeyAction = {read=FOnKeyAction, write=FOnKeyAction};
	__property TVTSaveNodeEvent OnLoadNode = {read=FOnLoadNode, write=FOnLoadNode};
	__property TVTSaveTreeEvent OnLoadTree = {read=FOnLoadTree, write=FOnLoadTree};
	__property TVTMeasureItemEvent OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property TVTNodeClickEvent OnNodeClick = {read=FOnNodeClick, write=FOnNodeClick};
	__property TVTNodeCopiedEvent OnNodeCopied = {read=FOnNodeCopied, write=FOnNodeCopied};
	__property TVTNodeCopyingEvent OnNodeCopying = {read=FOnNodeCopying, write=FOnNodeCopying};
	__property TVTNodeClickEvent OnNodeDblClick = {read=FOnNodeDblClick, write=FOnNodeDblClick};
	__property TVTNodeExportEvent OnNodeExport = {read=FOnNodeExport, write=FOnNodeExport};
	__property TVTNodeHeightTrackingEvent OnNodeHeightTracking = {read=FOnNodeHeightTracking, write=FOnNodeHeightTracking};
	__property TVTNodeHeightDblClickResizeEvent OnNodeHeightDblClickResize = {read=FOnNodeHeightDblClickResize, write=FOnNodeHeightDblClickResize};
	__property TVTNodeMovedEvent OnNodeMoved = {read=FOnNodeMoved, write=FOnNodeMoved};
	__property TVTNodeMovingEvent OnNodeMoving = {read=FOnNodeMoving, write=FOnNodeMoving};
	__property TVTBackgroundPaintEvent OnPaintBackground = {read=FOnPaintBackground, write=FOnPaintBackground};
	__property TVTRemoveFromSelectionEvent OnRemoveFromSelection = {read=FOnRemoveFromSelection, write=FOnRemoveFromSelection};
	__property TVTRenderOLEDataEvent OnRenderOLEData = {read=FOnRenderOLEData, write=FOnRenderOLEData};
	__property TVTChangeEvent OnResetNode = {read=FOnResetNode, write=FOnResetNode};
	__property TVTSaveNodeEvent OnSaveNode = {read=FOnSaveNode, write=FOnSaveNode};
	__property TVTSaveTreeEvent OnSaveTree = {read=FOnSaveTree, write=FOnSaveTree};
	__property TVTScrollEvent OnScroll = {read=FOnScroll, write=FOnScroll};
	__property TVTScrollBarShowEvent OnShowScrollBar = {read=FOnShowScrollBar, write=FOnShowScrollBar};
	__property TVTOperationEvent OnStartOperation = {read=FOnStartOperation, write=FOnStartOperation};
	__property TVTStateChangeEvent OnStateChange = {read=FOnStateChange, write=FOnStateChange};
	__property TVTStructureChangeEvent OnStructureChange = {read=FOnStructureChange, write=FOnStructureChange};
	__property TVTUpdatingEvent OnUpdating = {read=FOnUpdating, write=FOnUpdating};
	
public:
	__fastcall virtual TBaseVirtualTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TBaseVirtualTree(void);
	unsigned __fastcall AbsoluteIndex(PVirtualNode Node);
	virtual PVirtualNode __fastcall AddChild(PVirtualNode Parent, void * UserData = (void *)(0x0));
	void __fastcall AddFromStream(System::Classes::TStream* Stream, PVirtualNode TargetNode);
	virtual void __fastcall AfterConstruction(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	HIDESBASE void __fastcall BeginDrag(bool Immediate, int Threshold = 0xffffffff);
	void __fastcall BeginSynch(void);
	virtual void __fastcall BeginUpdate(void);
	void __fastcall CancelCutOrCopy(void);
	bool __fastcall CancelEditNode(void);
	void __fastcall CancelOperation(void);
	virtual bool __fastcall CanEdit(PVirtualNode Node, TColumnIndex Column);
	DYNAMIC bool __fastcall CanFocus(void);
	virtual void __fastcall Clear(void);
	void __fastcall ClearChecked(void);
	void __fastcall ClearSelection(void);
	PVirtualNode __fastcall CopyTo(PVirtualNode Source, TBaseVirtualTree* Tree, TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	PVirtualNode __fastcall CopyTo(PVirtualNode Source, PVirtualNode Target, TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	virtual void __fastcall CopyToClipboard(void);
	virtual void __fastcall CutToClipboard(void);
	void __fastcall DeleteChildren(PVirtualNode Node, bool ResetHasChildren = false);
	void __fastcall DeleteNode(PVirtualNode Node, bool Reindex = true);
	virtual void __fastcall DeleteSelectedNodes(void);
	HIDESBASE bool __fastcall Dragging(void);
	virtual bool __fastcall EditNode(PVirtualNode Node, TColumnIndex Column);
	bool __fastcall EndEditNode(void);
	void __fastcall EndSynch(void);
	virtual void __fastcall EndUpdate(void);
	virtual void __fastcall EnsureNodeSelected(void);
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	void __fastcall FinishCutOrCopy(void);
	void __fastcall FlushClipboard(void);
	virtual void __fastcall FullCollapse(PVirtualNode Node = (PVirtualNode)(0x0));
	virtual void __fastcall FullExpand(PVirtualNode Node = (PVirtualNode)(0x0));
	DYNAMIC System::Classes::TAlignment __fastcall GetControlsAlignment(void);
	System::Types::TRect __fastcall GetDisplayRect(PVirtualNode Node, TColumnIndex Column, bool TextOnly, bool Unclipped = false, bool ApplyCellContentMargin = false);
	bool __fastcall GetEffectivelyFiltered(PVirtualNode Node);
	bool __fastcall GetEffectivelyVisible(PVirtualNode Node);
	PVirtualNode __fastcall GetFirst(bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetFirstChecked(TCheckState State = (TCheckState)(0x2), bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetFirstChild(PVirtualNode Node);
	PVirtualNode __fastcall GetFirstChildNoInit(PVirtualNode Node);
	PVirtualNode __fastcall GetFirstCutCopy(bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetFirstInitialized(bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetFirstLeaf(void);
	PVirtualNode __fastcall GetFirstLevel(unsigned NodeLevel);
	PVirtualNode __fastcall GetFirstNoInit(bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetFirstSelected(bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetFirstVisible(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetFirstVisibleChild(PVirtualNode Node, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetFirstVisibleChildNoInit(PVirtualNode Node, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetFirstVisibleNoInit(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	virtual void __fastcall GetHitTestInfoAt(int X, int Y, bool Relative, THitInfo &HitInfo);
	PVirtualNode __fastcall GetLast(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetLastInitialized(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetLastNoInit(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetLastChild(PVirtualNode Node);
	PVirtualNode __fastcall GetLastChildNoInit(PVirtualNode Node);
	PVirtualNode __fastcall GetLastVisible(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetLastVisibleChild(PVirtualNode Node, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetLastVisibleChildNoInit(PVirtualNode Node, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetLastVisibleNoInit(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	virtual int __fastcall GetMaxColumnWidth(TColumnIndex Column, bool UseSmartColumnWidth = false);
	PVirtualNode __fastcall GetNext(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetNextChecked(PVirtualNode Node, TCheckState State = (TCheckState)(0x2), bool ConsiderChildrenAbove = false)/* overload */;
	PVirtualNode __fastcall GetNextChecked(PVirtualNode Node, bool ConsiderChildrenAbove)/* overload */;
	PVirtualNode __fastcall GetNextCutCopy(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetNextInitialized(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetNextLeaf(PVirtualNode Node);
	PVirtualNode __fastcall GetNextLevel(PVirtualNode Node, unsigned NodeLevel);
	PVirtualNode __fastcall GetNextNoInit(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetNextSelected(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetNextSibling(PVirtualNode Node);
	PVirtualNode __fastcall GetNextSiblingNoInit(PVirtualNode Node);
	PVirtualNode __fastcall GetNextVisible(PVirtualNode Node, bool ConsiderChildrenAbove = true);
	PVirtualNode __fastcall GetNextVisibleNoInit(PVirtualNode Node, bool ConsiderChildrenAbove = true);
	PVirtualNode __fastcall GetNextVisibleSibling(PVirtualNode Node, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetNextVisibleSiblingNoInit(PVirtualNode Node, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetNodeAt(const System::Types::TPoint &P)/* overload */;
	PVirtualNode __fastcall GetNodeAt(int X, int Y)/* overload */;
	PVirtualNode __fastcall GetNodeAt(int X, int Y, bool Relative, int &NodeTop)/* overload */;
	void * __fastcall GetNodeData(PVirtualNode Node);
	unsigned __fastcall GetNodeLevel(PVirtualNode Node);
	PVirtualNode __fastcall GetPrevious(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetPreviousChecked(PVirtualNode Node, TCheckState State = (TCheckState)(0x2), bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetPreviousCutCopy(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetPreviousInitialized(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetPreviousLeaf(PVirtualNode Node);
	PVirtualNode __fastcall GetPreviousLevel(PVirtualNode Node, unsigned NodeLevel);
	PVirtualNode __fastcall GetPreviousNoInit(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetPreviousSelected(PVirtualNode Node, bool ConsiderChildrenAbove = false);
	PVirtualNode __fastcall GetPreviousSibling(PVirtualNode Node);
	PVirtualNode __fastcall GetPreviousSiblingNoInit(PVirtualNode Node);
	PVirtualNode __fastcall GetPreviousVisible(PVirtualNode Node, bool ConsiderChildrenAbove = true);
	PVirtualNode __fastcall GetPreviousVisibleNoInit(PVirtualNode Node, bool ConsiderChildrenAbove = true);
	PVirtualNode __fastcall GetPreviousVisibleSibling(PVirtualNode Node, bool IncludeFiltered = false);
	PVirtualNode __fastcall GetPreviousVisibleSiblingNoInit(PVirtualNode Node, bool IncludeFiltered = false);
	TNodeArray __fastcall GetSortedCutCopySet(bool Resolve);
	TNodeArray __fastcall GetSortedSelection(bool Resolve);
	virtual void __fastcall GetTextInfo(PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TFont* const AFont, System::Types::TRect &R, System::UnicodeString &Text);
	System::Types::TRect __fastcall GetTreeRect(void);
	PVirtualNode __fastcall GetVisibleParent(PVirtualNode Node, bool IncludeFiltered = false);
	bool __fastcall HasAsParent(PVirtualNode Node, PVirtualNode PotentialParent);
	PVirtualNode __fastcall InsertNode(PVirtualNode Node, TVTNodeAttachMode Mode, void * UserData = (void *)(0x0));
	void __fastcall InvalidateChildren(PVirtualNode Node, bool Recursive);
	void __fastcall InvalidateColumn(TColumnIndex Column);
	virtual System::Types::TRect __fastcall InvalidateNode(PVirtualNode Node);
	void __fastcall InvalidateToBottom(PVirtualNode Node);
	void __fastcall InvertSelection(bool VisibleOnly);
	bool __fastcall IsEditing(void);
	bool __fastcall IsMouseSelecting(void);
	bool __fastcall IsEmpty(void);
	PVirtualNode __fastcall IterateSubtree(PVirtualNode Node, _di_TVTGetNodeProc Callback, void * Data, TVirtualNodeStates Filter = TVirtualNodeStates() , bool DoInit = false, bool ChildNodesOnly = false);
	virtual void __fastcall LoadFromFile(const System::Sysutils::TFileName FileName);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall MeasureItemHeight(Vcl::Graphics::TCanvas* const Canvas, PVirtualNode Node);
	void __fastcall MoveTo(PVirtualNode Source, PVirtualNode Target, TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	void __fastcall MoveTo(PVirtualNode Node, TBaseVirtualTree* Tree, TVTNodeAttachMode Mode, bool ChildrenOnly)/* overload */;
	virtual void __fastcall PaintTree(Vcl::Graphics::TCanvas* TargetCanvas, const System::Types::TRect &Window, const System::Types::TPoint &Target, TVTInternalPaintOptions PaintOptions, Vcl::Graphics::TPixelFormat PixelFormat = (Vcl::Graphics::TPixelFormat)(0x0));
	virtual bool __fastcall PasteFromClipboard(void);
	void __fastcall PrepareDragImage(const System::Types::TPoint &HotSpot, const _di_IDataObject DataObject);
	void __fastcall Print(Vcl::Printers::TPrinter* Printer, bool PrintHeader);
	bool __fastcall ProcessDrop(_di_IDataObject DataObject, PVirtualNode TargetNode, int &Effect, TVTNodeAttachMode Mode);
	bool __fastcall ProcessOLEData(TBaseVirtualTree* Source, _di_IDataObject DataObject, PVirtualNode TargetNode, TVTNodeAttachMode Mode, bool Optimized);
	void __fastcall RepaintNode(PVirtualNode Node);
	virtual void __fastcall ReinitChildren(PVirtualNode Node, bool Recursive);
	virtual void __fastcall ReinitNode(PVirtualNode Node, bool Recursive);
	virtual void __fastcall ResetNode(PVirtualNode Node);
	void __fastcall SaveToFile(const System::Sysutils::TFileName FileName);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream, PVirtualNode Node = (PVirtualNode)(0x0));
	bool __fastcall ScrollIntoView(PVirtualNode Node, bool Center, bool Horizontally = false)/* overload */;
	bool __fastcall ScrollIntoView(TColumnIndex Column, bool Center)/* overload */;
	void __fastcall SelectAll(bool VisibleOnly);
	virtual void __fastcall Sort(PVirtualNode Node, TColumnIndex Column, TSortDirection Direction, bool DoInit = true);
	virtual void __fastcall SortTree(TColumnIndex Column, TSortDirection Direction, bool DoInit = true);
	void __fastcall ToggleNode(PVirtualNode Node);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	void __fastcall UpdateHorizontalRange(void);
	void __fastcall UpdateHorizontalScrollBar(bool DoRepaint);
	void __fastcall UpdateRanges(void);
	virtual void __fastcall UpdateScrollBars(bool DoRepaint);
	void __fastcall UpdateVerticalRange(void);
	void __fastcall UpdateVerticalScrollBar(bool DoRepaint);
	HIDESBASE bool __fastcall UseRightToLeftReading(void);
	void __fastcall ValidateChildren(PVirtualNode Node, bool Recursive);
	void __fastcall ValidateNode(PVirtualNode Node, bool Recursive);
	TVTVirtualNodeEnumeration __fastcall Nodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall CheckedNodes(TCheckState State = (TCheckState)(0x2), bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall ChildNodes(PVirtualNode Node);
	TVTVirtualNodeEnumeration __fastcall CutCopyNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall InitializedNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall LeafNodes(void);
	TVTVirtualNodeEnumeration __fastcall LevelNodes(unsigned NodeLevel);
	TVTVirtualNodeEnumeration __fastcall NoInitNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall SelectedNodes(bool ConsiderChildrenAbove = false);
	TVTVirtualNodeEnumeration __fastcall VisibleNodes(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	TVTVirtualNodeEnumeration __fastcall VisibleChildNodes(PVirtualNode Node, bool IncludeFiltered = false);
	TVTVirtualNodeEnumeration __fastcall VisibleChildNoInitNodes(PVirtualNode Node, bool IncludeFiltered = false);
	TVTVirtualNodeEnumeration __fastcall VisibleNoInitNodes(PVirtualNode Node = (PVirtualNode)(0x0), bool ConsiderChildrenAbove = true, bool IncludeFiltered = false);
	__property _di_IAccessible Accessible = {read=FAccessible, write=FAccessible};
	__property _di_IAccessible AccessibleItem = {read=FAccessibleItem, write=FAccessibleItem};
	__property System::UnicodeString AccessibleName = {read=FAccessibleName, write=FAccessibleName};
	__property PVirtualNode BottomNode = {read=GetBottomNode, write=SetBottomNode};
	__property int CheckedCount = {read=GetCheckedCount, nodefault};
	__property Vcl::Imglist::TCustomImageList* CheckImages = {read=FCheckImages};
	__property TCheckState CheckState[PVirtualNode Node] = {read=GetCheckState, write=SetCheckState};
	__property TCheckType CheckType[PVirtualNode Node] = {read=GetCheckType, write=SetCheckType};
	__property unsigned ChildCount[PVirtualNode Node] = {read=GetChildCount, write=SetChildCount};
	__property bool ChildrenInitialized[PVirtualNode Node] = {read=GetChildrenInitialized};
	__property int CutCopyCount = {read=GetCutCopyCount, nodefault};
	__property TVTDragImage* DragImage = {read=FDragImage};
	__property _di_IVTDragManager DragManager = {read=GetDragManager};
	__property PVirtualNode DropTargetNode = {read=FDropTargetNode, write=FDropTargetNode};
	__property _di_IVTEditLink EditLink = {read=FEditLink};
	__property System::UnicodeString EmptyListMessage = {read=FEmptyListMessage, write=SetEmptyListMessage};
	__property bool Expanded[PVirtualNode Node] = {read=GetExpanded, write=SetExpanded};
	__property TColumnIndex FocusedColumn = {read=FFocusedColumn, write=SetFocusedColumn, default=-2};
	__property PVirtualNode FocusedNode = {read=FFocusedNode, write=SetFocusedNode};
	__property Font;
	__property bool FullyVisible[PVirtualNode Node] = {read=GetFullyVisible, write=SetFullyVisible};
	__property bool HasChildren[PVirtualNode Node] = {read=GetHasChildren, write=SetHasChildren};
	__property PVirtualNode HotNode = {read=FCurrentHotNode};
	__property bool IsDisabled[PVirtualNode Node] = {read=GetDisabled, write=SetDisabled};
	__property bool IsEffectivelyFiltered[PVirtualNode Node] = {read=GetEffectivelyFiltered};
	__property bool IsEffectivelyVisible[PVirtualNode Node] = {read=GetEffectivelyVisible};
	__property bool IsFiltered[PVirtualNode Node] = {read=GetFiltered, write=SetFiltered};
	__property bool IsVisible[PVirtualNode Node] = {read=GetVisible, write=SetVisible};
	__property bool MultiLine[PVirtualNode Node] = {read=GetMultiline, write=SetMultiline};
	__property unsigned NodeHeight[PVirtualNode Node] = {read=GetNodeHeight, write=SetNodeHeight};
	__property PVirtualNode NodeParent[PVirtualNode Node] = {read=GetNodeParent, write=SetNodeParent};
	__property int OffsetX = {read=FOffsetX, write=SetOffsetX, nodefault};
	__property System::Types::TPoint OffsetXY = {read=GetOffsetXY, write=SetOffsetXY};
	__property int OffsetY = {read=FOffsetY, write=SetOffsetY, nodefault};
	__property unsigned OperationCount = {read=FOperationCount, nodefault};
	__property PVirtualNode RootNode = {read=FRoot};
	__property System::UnicodeString SearchBuffer = {read=FSearchBuffer};
	__property bool Selected[PVirtualNode Node] = {read=GetSelected, write=SetSelected};
	__property bool SelectionLocked = {read=FSelectionLocked, write=FSelectionLocked, nodefault};
	__property unsigned TotalCount = {read=GetTotalCount, nodefault};
	__property TVirtualTreeStates TreeStates = {read=FStates, write=FStates};
	__property int SelectedCount = {read=FSelectionCount, nodefault};
	__property PVirtualNode TopNode = {read=GetTopNode, write=SetTopNode};
	__property System::Byte VerticalAlignment[PVirtualNode Node] = {read=GetVerticalAlignment, write=SetVerticalAlignment};
	__property unsigned VisibleCount = {read=FVisibleCount, nodefault};
	__property bool VisiblePath[PVirtualNode Node] = {read=GetVisiblePath, write=SetVisiblePath};
	__property unsigned UpdateCount = {read=FUpdateCount, nodefault};
	__property bool DoubleBuffered = {read=GetDoubleBuffered, write=SetDoubleBuffered, default=1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TBaseVirtualTree(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TVTStringOption : unsigned char { toSaveCaptions, toShowStaticText, toAutoAcceptEditChange };

typedef System::Set<TVTStringOption, TVTStringOption::toSaveCaptions, TVTStringOption::toAutoAcceptEditChange> TVTStringOptions;

class DELPHICLASS TCustomStringTreeOptions;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCustomStringTreeOptions : public TCustomVirtualTreeOptions
{
	typedef TCustomVirtualTreeOptions inherited;
	
private:
	TVTStringOptions FStringOptions;
	void __fastcall SetStringOptions(const TVTStringOptions Value);
	
protected:
	__property TVTStringOptions StringOptions = {read=FStringOptions, write=SetStringOptions, default=5};
	
public:
	__fastcall virtual TCustomStringTreeOptions(TBaseVirtualTree* AOwner);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TCustomStringTreeOptions(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TStringTreeOptions;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringTreeOptions : public TCustomStringTreeOptions
{
	typedef TCustomStringTreeOptions inherited;
	
__published:
	__property AnimationOptions = {default=0};
	__property AutoOptions = {default=1369};
	__property ExportMode = {default=0};
	__property MiscOptions = {default=16809};
	__property PaintOptions = {default=7008};
	__property SelectionOptions = {default=0};
	__property StringOptions = {default=5};
public:
	/* TCustomStringTreeOptions.Create */ inline __fastcall virtual TStringTreeOptions(TBaseVirtualTree* AOwner) : TCustomStringTreeOptions(AOwner) { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TStringTreeOptions(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TVTEdit;
class DELPHICLASS TStringEditLink;
class PASCALIMPLEMENTATION TVTEdit : public Vcl::Stdctrls::TCustomEdit
{
	typedef Vcl::Stdctrls::TCustomEdit inherited;
	
private:
	MESSAGE void __fastcall CMAutoAdjust(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMRelease(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	
protected:
	_di_IVTEditLink FRefLink;
	TStringEditLink* FLink;
	virtual void __fastcall AutoAdjustSize(void);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	
public:
	__fastcall TVTEdit(TStringEditLink* Link);
	virtual void __fastcall Release(void);
	__property AutoSelect = {default=1};
	__property AutoSize = {default=1};
	__property BorderStyle = {default=1};
	__property CharCase = {default=0};
	__property HideSelection = {default=1};
	__property MaxLength = {default=0};
	__property OEMConvert = {default=0};
	__property PasswordChar = {default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TVTEdit(HWND ParentWindow) : Vcl::Stdctrls::TCustomEdit(ParentWindow) { }
	/* TWinControl.Destroy */ inline __fastcall virtual ~TVTEdit(void) { }
	
};


class DELPHICLASS TCustomVirtualStringTree;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStringEditLink : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TVTEdit* FEdit;
	
protected:
	TCustomVirtualStringTree* FTree;
	TVirtualNode *FNode;
	TColumnIndex FColumn;
	System::Classes::TAlignment FAlignment;
	System::Types::TRect FTextBounds;
	bool FStopping;
	void __fastcall SetEdit(TVTEdit* const Value);
	
public:
	__fastcall virtual TStringEditLink(void);
	__fastcall virtual ~TStringEditLink(void);
	__property PVirtualNode Node = {read=FNode};
	__property TColumnIndex Column = {read=FColumn, nodefault};
	virtual bool __stdcall BeginEdit(void);
	virtual bool __stdcall CancelEdit(void);
	__property TVTEdit* Edit = {read=FEdit, write=SetEdit};
	virtual bool __stdcall EndEdit(void);
	virtual System::Types::TRect __stdcall GetBounds(void);
	virtual bool __stdcall PrepareEdit(TBaseVirtualTree* Tree, PVirtualNode Node, TColumnIndex Column);
	virtual void __stdcall ProcessMessage(Winapi::Messages::TMessage &Message);
	virtual void __stdcall SetBounds(const System::Types::TRect R);
private:
	void *__IVTEditLink;	// IVTEditLink 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {2BE3EAFA-5ACB-45B4-9D9A-B58BCC496E17}
	operator _di_IVTEditLink()
	{
		_di_IVTEditLink intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator IVTEditLink*(void) { return (IVTEditLink*)&__IVTEditLink; }
	#endif
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TVSTTextType : unsigned char { ttNormal, ttStatic };

enum DECLSPEC_DENUM TVSTTextSourceType : unsigned char { tstAll, tstInitialized, tstSelected, tstCutCopySet, tstVisible, tstChecked };

typedef void __fastcall (__closure *TVTPaintText)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* const TargetCanvas, PVirtualNode Node, TColumnIndex Column, TVSTTextType TextType);

typedef void __fastcall (__closure *TVSTGetTextEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, TVSTTextType TextType, System::UnicodeString &CellText);

typedef void __fastcall (__closure *TVSTGetHintEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, TVTTooltipLineBreakStyle &LineBreakStyle, System::UnicodeString &HintText);

typedef void __fastcall (__closure *TVSTNewTextEvent)(TBaseVirtualTree* Sender, PVirtualNode Node, TColumnIndex Column, System::UnicodeString NewText);

typedef void __fastcall (__closure *TVSTShortenStringEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, TColumnIndex Column, const System::UnicodeString S, int TextSpace, System::UnicodeString &Result, bool &Done);

typedef void __fastcall (__closure *TVTMeasureTextEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, TColumnIndex Column, const System::UnicodeString Text, int &Extent);

typedef void __fastcall (__closure *TVTDrawTextEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* TargetCanvas, PVirtualNode Node, TColumnIndex Column, const System::UnicodeString Text, const System::Types::TRect &CellRect, bool &DefaultDraw);

class PASCALIMPLEMENTATION TCustomVirtualStringTree : public TBaseVirtualTree
{
	typedef TBaseVirtualTree inherited;
	
private:
	System::UnicodeString FDefaultText;
	int FTextHeight;
	int FEllipsisWidth;
	unsigned FInternalDataOffset;
	TVTPaintText FOnPaintText;
	TVSTGetTextEvent FOnGetText;
	TVSTGetHintEvent FOnGetHint;
	TVSTNewTextEvent FOnNewText;
	TVSTShortenStringEvent FOnShortenString;
	TVTMeasureTextEvent FOnMeasureTextWidth;
	TVTMeasureTextEvent FOnMeasureTextHeight;
	TVTDrawTextEvent FOnDrawText;
	System::UnicodeString __fastcall GetImageText(PVirtualNode Node, TVTImageKind Kind, TColumnIndex Column);
	void __fastcall GetRenderStartValues(TVSTTextSourceType Source, PVirtualNode &Node, TGetNextNodeProc &NextNodeProc);
	TCustomStringTreeOptions* __fastcall GetOptions(void);
	System::UnicodeString __fastcall GetStaticText(PVirtualNode Node, TColumnIndex Column);
	HIDESBASE System::UnicodeString __fastcall GetText(PVirtualNode Node, TColumnIndex Column);
	void __fastcall ReadText(System::Classes::TReader* Reader);
	void __fastcall SetDefaultText(const System::UnicodeString Value);
	HIDESBASE void __fastcall SetOptions(TCustomStringTreeOptions* const Value);
	HIDESBASE void __fastcall SetText(PVirtualNode Node, TColumnIndex Column, const System::UnicodeString Value);
	void __fastcall WriteText(System::Classes::TWriter* Writer);
	MESSAGE void __fastcall WMSetFont(Winapi::Messages::TWMSetFont &Msg);
	void __fastcall GetDataFromGrid(System::Classes::TStringList* const AStrings, const bool IncludeHeading = true);
	
protected:
	System::Classes::TStringList* FPreviouslySelected;
	void __fastcall InitializeTextProperties(TVTPaintInfo &PaintInfo);
	virtual void __fastcall PaintNormalText(TVTPaintInfo &PaintInfo, int TextOutFlags, System::UnicodeString Text);
	virtual void __fastcall PaintStaticText(const TVTPaintInfo &PaintInfo, int TextOutFlags, const System::UnicodeString Text);
	virtual void __fastcall AdjustPaintCellRect(TVTPaintInfo &PaintInfo, TColumnIndex &NextNonEmpty);
	bool __fastcall CanExportNode(PVirtualNode Node);
	virtual int __fastcall CalculateStaticTextWidth(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, System::UnicodeString Text);
	virtual int __fastcall CalculateTextWidth(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, const System::UnicodeString Text);
	virtual bool __fastcall ColumnIsEmpty(PVirtualNode Node, TColumnIndex Column);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual _di_IVTEditLink __fastcall DoCreateEditor(PVirtualNode Node, TColumnIndex Column);
	virtual System::UnicodeString __fastcall DoGetNodeHint(PVirtualNode Node, TColumnIndex Column, TVTTooltipLineBreakStyle &LineBreakStyle);
	virtual System::UnicodeString __fastcall DoGetNodeTooltip(PVirtualNode Node, TColumnIndex Column, TVTTooltipLineBreakStyle &LineBreakStyle);
	virtual int __fastcall DoGetNodeExtraWidth(PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual int __fastcall DoGetNodeWidth(PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual void __fastcall DoGetText(PVirtualNode Node, TColumnIndex Column, TVSTTextType TextType, System::UnicodeString &Text);
	virtual int __fastcall DoIncrementalSearch(PVirtualNode Node, const System::UnicodeString Text);
	virtual void __fastcall DoNewText(PVirtualNode Node, TColumnIndex Column, System::UnicodeString Text);
	virtual void __fastcall DoPaintNode(TVTPaintInfo &PaintInfo);
	virtual void __fastcall DoPaintText(PVirtualNode Node, Vcl::Graphics::TCanvas* const Canvas, TColumnIndex Column, TVSTTextType TextType);
	virtual System::UnicodeString __fastcall DoShortenString(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, const System::UnicodeString S, int Width, int EllipsisWidth = 0x0);
	virtual void __fastcall DoTextDrawing(TVTPaintInfo &PaintInfo, const System::UnicodeString Text, const System::Types::TRect &CellRect, unsigned DrawFormat);
	virtual System::Types::TSize __fastcall DoTextMeasuring(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, const System::UnicodeString Text);
	virtual TTreeOptionsClass __fastcall GetOptionsClass(void);
	HIDESBASE void * __fastcall InternalData(PVirtualNode Node);
	virtual void __fastcall MainColumnChanged(void);
	virtual bool __fastcall ReadChunk(System::Classes::TStream* Stream, int Version, PVirtualNode Node, int ChunkType, int ChunkSize);
	void __fastcall ReadOldStringOptions(System::Classes::TReader* Reader);
	virtual HRESULT __fastcall RenderOLEData(const tagFORMATETC &FormatEtcIn, /* out */ tagSTGMEDIUM &Medium, bool ForClipboard);
	virtual void __fastcall WriteChunks(System::Classes::TStream* Stream, PVirtualNode Node);
	__property System::UnicodeString DefaultText = {read=FDefaultText, write=SetDefaultText, stored=false};
	__property int EllipsisWidth = {read=FEllipsisWidth, nodefault};
	__property TCustomStringTreeOptions* TreeOptions = {read=GetOptions, write=SetOptions};
	__property TVSTGetHintEvent OnGetHint = {read=FOnGetHint, write=FOnGetHint};
	__property TVSTGetTextEvent OnGetText = {read=FOnGetText, write=FOnGetText};
	__property TVSTNewTextEvent OnNewText = {read=FOnNewText, write=FOnNewText};
	__property TVTPaintText OnPaintText = {read=FOnPaintText, write=FOnPaintText};
	__property TVSTShortenStringEvent OnShortenString = {read=FOnShortenString, write=FOnShortenString};
	__property TVTMeasureTextEvent OnMeasureTextWidth = {read=FOnMeasureTextWidth, write=FOnMeasureTextWidth};
	__property TVTMeasureTextEvent OnMeasureTextHeight = {read=FOnMeasureTextHeight, write=FOnMeasureTextHeight};
	__property TVTDrawTextEvent OnDrawText = {read=FOnDrawText, write=FOnDrawText};
	
public:
	__fastcall virtual TCustomVirtualStringTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomVirtualStringTree(void);
	virtual PVirtualNode __fastcall AddChild(PVirtualNode Parent, void * UserData = (void *)(0x0));
	virtual int __fastcall ComputeNodeHeight(Vcl::Graphics::TCanvas* Canvas, PVirtualNode Node, TColumnIndex Column, System::UnicodeString S = System::UnicodeString());
	NativeUInt __fastcall ContentToClipboard(System::Word Format, TVSTTextSourceType Source);
	void __fastcall ContentToCustom(TVSTTextSourceType Source);
	System::RawByteString __fastcall ContentToHTML(TVSTTextSourceType Source, System::UnicodeString Caption = System::UnicodeString());
	System::RawByteString __fastcall ContentToRTF(TVSTTextSourceType Source);
	System::AnsiString __fastcall ContentToText(TVSTTextSourceType Source, System::WideChar Separator)/* overload */;
	System::AnsiString __fastcall ContentToText(TVSTTextSourceType Source, const System::AnsiString Separator)/* overload */;
	System::UnicodeString __fastcall ContentToUnicode(TVSTTextSourceType Source, System::WideChar Separator)/* overload */;
	System::UnicodeString __fastcall ContentToUnicode(TVSTTextSourceType Source, const System::UnicodeString Separator)/* overload */;
	virtual void __fastcall GetTextInfo(PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TFont* const AFont, System::Types::TRect &R, System::UnicodeString &Text);
	virtual System::Types::TRect __fastcall InvalidateNode(PVirtualNode Node);
	System::UnicodeString __fastcall Path(PVirtualNode Node, TColumnIndex Column, TVSTTextType TextType, System::WideChar Delimiter);
	virtual void __fastcall ReinitNode(PVirtualNode Node, bool Recursive);
	virtual void __fastcall AddToSelection(PVirtualNode Node)/* overload */;
	virtual void __fastcall RemoveFromSelection(PVirtualNode Node);
	bool __fastcall SaveToCSVFile(const System::Sysutils::TFileName FileNameWithPath, const bool IncludeHeading);
	__property System::UnicodeString ImageText[PVirtualNode Node][TVTImageKind Kind][TColumnIndex Column] = {read=GetImageText};
	__property System::UnicodeString StaticText[PVirtualNode Node][TColumnIndex Column] = {read=GetStaticText};
	__property System::UnicodeString Text[PVirtualNode Node][TColumnIndex Column] = {read=GetText, write=SetText};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomVirtualStringTree(HWND ParentWindow) : TBaseVirtualTree(ParentWindow) { }
	
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  AddToSelection(const TNodeArray NewItems, int NewLength, bool ForceInsert = false){ TBaseVirtualTree::AddToSelection(NewItems, NewLength, ForceInsert); }
	
};


class DELPHICLASS TVirtualStringTree;
class PASCALIMPLEMENTATION TVirtualStringTree : public TCustomVirtualStringTree
{
	typedef TCustomVirtualStringTree inherited;
	
private:
	HIDESBASE TStringTreeOptions* __fastcall GetOptions(void);
	HIDESBASE void __fastcall SetOptions(TStringTreeOptions* const Value);
	
protected:
	virtual TTreeOptionsClass __fastcall GetOptionsClass(void);
	
private:
	// __classmethod void __fastcall Create@();
	
public:
	__property Canvas;
	__property RangeX;
	__property LastDragEffect;
	
__published:
	__property AccessibleName = {default=0};
	__property Action;
	__property Align = {default=0};
	__property Alignment = {default=0};
	__property Anchors = {default=3};
	__property AnimationDuration = {default=200};
	__property AutoExpandDelay = {default=1000};
	__property AutoScrollDelay = {default=1000};
	__property AutoScrollInterval = {default=1};
	__property Background;
	__property BackgroundOffsetX = {index=0, default=0};
	__property BackgroundOffsetY = {index=1, default=0};
	__property BiDiMode;
	__property BevelEdges = {default=15};
	__property BevelInner = {index=0, default=2};
	__property BevelOuter = {index=1, default=1};
	__property BevelKind = {default=0};
	__property BevelWidth = {default=1};
	__property BorderStyle = {default=1};
	__property BottomSpace = {default=0};
	__property ButtonFillMode = {default=0};
	__property ButtonStyle = {default=0};
	__property BorderWidth = {default=0};
	__property ChangeDelay = {default=0};
	__property CheckImageKind = {default=8};
	__property ClipboardFormats;
	__property Color = {default=-16777211};
	__property Colors;
	__property Constraints;
	__property Ctl3D;
	__property CustomCheckImages;
	__property DefaultNodeHeight = {default=18};
	__property DefaultPasteMode = {default=4};
	__property DefaultText = {default=0};
	__property DragCursor = {default=-12};
	__property DragHeight = {default=350};
	__property DragKind = {default=0};
	__property DragImageKind = {default=0};
	__property DragMode = {default=0};
	__property DragOperations = {default=3};
	__property DragType = {default=0};
	__property DragWidth = {default=200};
	__property DrawSelectionMode = {default=0};
	__property EditDelay = {default=1000};
	__property EmptyListMessage = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property Header;
	__property HintAnimation = {default=3};
	__property HintMode = {default=0};
	__property HotCursor = {default=0};
	__property Images;
	__property IncrementalSearch = {default=1};
	__property IncrementalSearchDirection = {default=0};
	__property IncrementalSearchStart = {default=2};
	__property IncrementalSearchTimeout = {default=1000};
	__property Indent = {default=18};
	__property LineMode = {default=0};
	__property LineStyle = {default=1};
	__property Margin = {default=4};
	__property NodeAlignment = {default=2};
	__property NodeDataSize = {default=-1};
	__property OperationCanceled;
	__property ParentBiDiMode = {default=1};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property RootNodeCount = {default=0};
	__property ScrollBarOptions;
	__property SelectionBlendFactor = {default=128};
	__property SelectionCurveRadius = {default=0};
	__property ShowHint;
	__property StateImages;
	__property StyleElements = {default=7};
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TextMargin = {default=4};
	__property TStringTreeOptions* TreeOptions = {read=GetOptions, write=SetOptions};
	__property Visible = {default=1};
	__property WantTabs = {default=0};
	__property OnAddToSelection;
	__property OnAdvancedHeaderDraw;
	__property OnAfterAutoFitColumn;
	__property OnAfterAutoFitColumns;
	__property OnAfterCellPaint;
	__property OnAfterColumnExport;
	__property OnAfterColumnWidthTracking;
	__property OnAfterGetMaxColumnWidth;
	__property OnAfterHeaderExport;
	__property OnAfterHeaderHeightTracking;
	__property OnAfterItemErase;
	__property OnAfterItemPaint;
	__property OnAfterNodeExport;
	__property OnAfterPaint;
	__property OnAfterTreeExport;
	__property OnBeforeAutoFitColumn;
	__property OnBeforeAutoFitColumns;
	__property OnBeforeCellPaint;
	__property OnBeforeColumnExport;
	__property OnBeforeColumnWidthTracking;
	__property OnBeforeDrawTreeLine;
	__property OnBeforeGetMaxColumnWidth;
	__property OnBeforeHeaderExport;
	__property OnBeforeHeaderHeightTracking;
	__property OnBeforeItemErase;
	__property OnBeforeItemPaint;
	__property OnBeforeNodeExport;
	__property OnBeforePaint;
	__property OnBeforeTreeExport;
	__property OnCanSplitterResizeColumn;
	__property OnCanSplitterResizeHeader;
	__property OnCanSplitterResizeNode;
	__property OnChange;
	__property OnChecked;
	__property OnChecking;
	__property OnClick;
	__property OnCollapsed;
	__property OnCollapsing;
	__property OnColumnClick;
	__property OnColumnDblClick;
	__property OnColumnExport;
	__property OnColumnResize;
	__property OnColumnWidthDblClickResize;
	__property OnColumnWidthTracking;
	__property OnCompareNodes;
	__property OnContextPopup;
	__property OnCreateDataObject;
	__property OnCreateDragManager;
	__property OnCreateEditor;
	__property OnDblClick;
	__property OnDragAllowed;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDrawHint;
	__property OnDrawText;
	__property OnEditCancelled;
	__property OnEdited;
	__property OnEditing;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEndOperation;
	__property OnEnter;
	__property OnExit;
	__property OnExpanded;
	__property OnExpanding;
	__property OnFocusChanged;
	__property OnFocusChanging;
	__property OnFreeNode;
	__property OnGetCellIsEmpty;
	__property OnGetCursor;
	__property OnGetHeaderCursor;
	__property OnGetText;
	__property OnPaintText;
	__property OnGetHelpContext;
	__property OnGetHintKind;
	__property OnGetHintSize;
	__property OnGetImageIndex;
	__property OnGetImageIndexEx;
	__property OnGetImageText;
	__property OnGetHint;
	__property OnGetLineStyle;
	__property OnGetNodeDataSize;
	__property OnGetPopupMenu;
	__property OnGetUserClipboardFormats;
	__property OnHeaderClick;
	__property OnHeaderDblClick;
	__property OnHeaderDragged;
	__property OnHeaderDraggedOut;
	__property OnHeaderDragging;
	__property OnHeaderDraw;
	__property OnHeaderDrawQueryElements;
	__property OnHeaderHeightDblClickResize;
	__property OnHeaderHeightTracking;
	__property OnHeaderMouseDown;
	__property OnHeaderMouseMove;
	__property OnHeaderMouseUp;
	__property OnHotChange;
	__property OnIncrementalSearch;
	__property OnInitChildren;
	__property OnInitNode;
	__property OnKeyAction;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnLoadNode;
	__property OnLoadTree;
	__property OnMeasureItem;
	__property OnMeasureTextWidth;
	__property OnMeasureTextHeight;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnNewText;
	__property OnNodeClick;
	__property OnNodeCopied;
	__property OnNodeCopying;
	__property OnNodeDblClick;
	__property OnNodeExport;
	__property OnNodeHeightDblClickResize;
	__property OnNodeHeightTracking;
	__property OnNodeMoved;
	__property OnNodeMoving;
	__property OnPaintBackground;
	__property OnRemoveFromSelection;
	__property OnRenderOLEData;
	__property OnResetNode;
	__property OnResize;
	__property OnSaveNode;
	__property OnSaveTree;
	__property OnScroll;
	__property OnShortenString;
	__property OnShowScrollBar;
	__property OnStartDock;
	__property OnStartDrag;
	__property OnStartOperation;
	__property OnStateChange;
	__property OnStructureChange;
	__property OnUpdating;
	__property OnCanResize;
	__property OnGesture;
	__property Touch;
	
private:
	// __classmethod void __fastcall Destroy@();
public:
	/* TCustomVirtualStringTree.Create */ inline __fastcall virtual TVirtualStringTree(System::Classes::TComponent* AOwner) : TCustomVirtualStringTree(AOwner) { }
	/* TCustomVirtualStringTree.Destroy */ inline __fastcall virtual ~TVirtualStringTree(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualStringTree(HWND ParentWindow) : TCustomVirtualStringTree(ParentWindow) { }
	
};


typedef void __fastcall (__closure *TVTDrawNodeEvent)(TBaseVirtualTree* Sender, const TVTPaintInfo &PaintInfo);

typedef void __fastcall (__closure *TVTGetCellContentMarginEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* HintCanvas, PVirtualNode Node, TColumnIndex Column, TVTCellContentMarginType CellContentMarginType, System::Types::TPoint &CellContentMargin);

typedef void __fastcall (__closure *TVTGetNodeWidthEvent)(TBaseVirtualTree* Sender, Vcl::Graphics::TCanvas* HintCanvas, PVirtualNode Node, TColumnIndex Column, int &NodeWidth);

class DELPHICLASS TCustomVirtualDrawTree;
class PASCALIMPLEMENTATION TCustomVirtualDrawTree : public TBaseVirtualTree
{
	typedef TBaseVirtualTree inherited;
	
private:
	TVTDrawNodeEvent FOnDrawNode;
	TVTGetCellContentMarginEvent FOnGetCellContentMargin;
	TVTGetNodeWidthEvent FOnGetNodeWidth;
	
protected:
	virtual System::Types::TPoint __fastcall DoGetCellContentMargin(PVirtualNode Node, TColumnIndex Column, TVTCellContentMarginType CellContentMarginType = (TVTCellContentMarginType)(0x0), Vcl::Graphics::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual int __fastcall DoGetNodeWidth(PVirtualNode Node, TColumnIndex Column, Vcl::Graphics::TCanvas* Canvas = (Vcl::Graphics::TCanvas*)(0x0));
	virtual void __fastcall DoPaintNode(TVTPaintInfo &PaintInfo);
	virtual TVTHintKind __fastcall GetDefaultHintKind(void);
	__property TVTDrawNodeEvent OnDrawNode = {read=FOnDrawNode, write=FOnDrawNode};
	__property TVTGetCellContentMarginEvent OnGetCellContentMargin = {read=FOnGetCellContentMargin, write=FOnGetCellContentMargin};
	__property TVTGetNodeWidthEvent OnGetNodeWidth = {read=FOnGetNodeWidth, write=FOnGetNodeWidth};
public:
	/* TBaseVirtualTree.Create */ inline __fastcall virtual TCustomVirtualDrawTree(System::Classes::TComponent* AOwner) : TBaseVirtualTree(AOwner) { }
	/* TBaseVirtualTree.Destroy */ inline __fastcall virtual ~TCustomVirtualDrawTree(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomVirtualDrawTree(HWND ParentWindow) : TBaseVirtualTree(ParentWindow) { }
	
};


class DELPHICLASS TVirtualDrawTree;
class PASCALIMPLEMENTATION TVirtualDrawTree : public TCustomVirtualDrawTree
{
	typedef TCustomVirtualDrawTree inherited;
	
private:
	TVirtualTreeOptions* __fastcall GetOptions(void);
	HIDESBASE void __fastcall SetOptions(TVirtualTreeOptions* const Value);
	
protected:
	virtual TTreeOptionsClass __fastcall GetOptionsClass(void);
	
private:
	// __classmethod void __fastcall Create@();
	
public:
	__property Canvas;
	__property LastDragEffect;
	
__published:
	__property Action;
	__property Align = {default=0};
	__property Alignment = {default=0};
	__property Anchors = {default=3};
	__property AnimationDuration = {default=200};
	__property AutoExpandDelay = {default=1000};
	__property AutoScrollDelay = {default=1000};
	__property AutoScrollInterval = {default=1};
	__property Background;
	__property BackgroundOffsetX = {index=0, default=0};
	__property BackgroundOffsetY = {index=1, default=0};
	__property BiDiMode;
	__property BevelEdges = {default=15};
	__property BevelInner = {index=0, default=2};
	__property BevelOuter = {index=1, default=1};
	__property BevelKind = {default=0};
	__property BevelWidth = {default=1};
	__property BorderStyle = {default=1};
	__property BottomSpace = {default=0};
	__property ButtonFillMode = {default=0};
	__property ButtonStyle = {default=0};
	__property BorderWidth = {default=0};
	__property ChangeDelay = {default=0};
	__property CheckImageKind = {default=8};
	__property ClipboardFormats;
	__property Color = {default=-16777211};
	__property Colors;
	__property Constraints;
	__property Ctl3D;
	__property CustomCheckImages;
	__property DefaultNodeHeight = {default=18};
	__property DefaultPasteMode = {default=4};
	__property DragCursor = {default=-12};
	__property DragHeight = {default=350};
	__property DragKind = {default=0};
	__property DragImageKind = {default=0};
	__property DragMode = {default=0};
	__property DragOperations = {default=3};
	__property DragType = {default=0};
	__property DragWidth = {default=200};
	__property DrawSelectionMode = {default=0};
	__property EditDelay = {default=1000};
	__property Enabled = {default=1};
	__property Font;
	__property Header;
	__property HintAnimation = {default=3};
	__property HintMode = {default=0};
	__property HotCursor = {default=0};
	__property Images;
	__property IncrementalSearch = {default=1};
	__property IncrementalSearchDirection = {default=0};
	__property IncrementalSearchStart = {default=2};
	__property IncrementalSearchTimeout = {default=1000};
	__property Indent = {default=18};
	__property LineMode = {default=0};
	__property LineStyle = {default=1};
	__property Margin = {default=4};
	__property NodeAlignment = {default=2};
	__property NodeDataSize = {default=-1};
	__property OperationCanceled;
	__property ParentBiDiMode = {default=1};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property RootNodeCount = {default=0};
	__property ScrollBarOptions;
	__property SelectionBlendFactor = {default=128};
	__property SelectionCurveRadius = {default=0};
	__property ShowHint;
	__property StateImages;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property TextMargin = {default=4};
	__property TVirtualTreeOptions* TreeOptions = {read=GetOptions, write=SetOptions};
	__property Visible = {default=1};
	__property WantTabs = {default=0};
	__property OnAddToSelection;
	__property OnAdvancedHeaderDraw;
	__property OnAfterAutoFitColumn;
	__property OnAfterAutoFitColumns;
	__property OnAfterCellPaint;
	__property OnAfterColumnExport;
	__property OnAfterColumnWidthTracking;
	__property OnAfterGetMaxColumnWidth;
	__property OnAfterHeaderExport;
	__property OnAfterHeaderHeightTracking;
	__property OnAfterItemErase;
	__property OnAfterItemPaint;
	__property OnAfterNodeExport;
	__property OnAfterPaint;
	__property OnAfterTreeExport;
	__property OnBeforeAutoFitColumn;
	__property OnBeforeAutoFitColumns;
	__property OnBeforeCellPaint;
	__property OnBeforeColumnExport;
	__property OnBeforeColumnWidthTracking;
	__property OnBeforeDrawTreeLine;
	__property OnBeforeGetMaxColumnWidth;
	__property OnBeforeHeaderExport;
	__property OnBeforeHeaderHeightTracking;
	__property OnBeforeItemErase;
	__property OnBeforeItemPaint;
	__property OnBeforeNodeExport;
	__property OnBeforePaint;
	__property OnBeforeTreeExport;
	__property OnCanSplitterResizeColumn;
	__property OnCanSplitterResizeHeader;
	__property OnCanSplitterResizeNode;
	__property OnChange;
	__property OnChecked;
	__property OnChecking;
	__property OnClick;
	__property OnCollapsed;
	__property OnCollapsing;
	__property OnColumnClick;
	__property OnColumnDblClick;
	__property OnColumnExport;
	__property OnColumnResize;
	__property OnColumnWidthDblClickResize;
	__property OnColumnWidthTracking;
	__property OnCompareNodes;
	__property OnContextPopup;
	__property OnCreateDataObject;
	__property OnCreateDragManager;
	__property OnCreateEditor;
	__property OnDblClick;
	__property OnDragAllowed;
	__property OnDragOver;
	__property OnDragDrop;
	__property OnDrawHint;
	__property OnDrawNode;
	__property OnEdited;
	__property OnEditing;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEndOperation;
	__property OnEnter;
	__property OnExit;
	__property OnExpanded;
	__property OnExpanding;
	__property OnFocusChanged;
	__property OnFocusChanging;
	__property OnFreeNode;
	__property OnGetCellIsEmpty;
	__property OnGetCursor;
	__property OnGetHeaderCursor;
	__property OnGetHelpContext;
	__property OnGetHintKind;
	__property OnGetHintSize;
	__property OnGetImageIndex;
	__property OnGetImageIndexEx;
	__property OnGetLineStyle;
	__property OnGetNodeDataSize;
	__property OnGetNodeWidth;
	__property OnGetPopupMenu;
	__property OnGetUserClipboardFormats;
	__property OnHeaderClick;
	__property OnHeaderDblClick;
	__property OnHeaderDragged;
	__property OnHeaderDraggedOut;
	__property OnHeaderDragging;
	__property OnHeaderDraw;
	__property OnHeaderDrawQueryElements;
	__property OnHeaderHeightTracking;
	__property OnHeaderHeightDblClickResize;
	__property OnHeaderMouseDown;
	__property OnHeaderMouseMove;
	__property OnHeaderMouseUp;
	__property OnHotChange;
	__property OnIncrementalSearch;
	__property OnInitChildren;
	__property OnInitNode;
	__property OnKeyAction;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnLoadNode;
	__property OnLoadTree;
	__property OnMeasureItem;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnNodeClick;
	__property OnNodeCopied;
	__property OnNodeCopying;
	__property OnNodeDblClick;
	__property OnNodeExport;
	__property OnNodeHeightTracking;
	__property OnNodeHeightDblClickResize;
	__property OnNodeMoved;
	__property OnNodeMoving;
	__property OnPaintBackground;
	__property OnRemoveFromSelection;
	__property OnRenderOLEData;
	__property OnResetNode;
	__property OnResize;
	__property OnSaveNode;
	__property OnSaveTree;
	__property OnScroll;
	__property OnShowScrollBar;
	__property OnStartDock;
	__property OnStartDrag;
	__property OnStartOperation;
	__property OnStateChange;
	__property OnStructureChange;
	__property OnUpdating;
	__property OnCanResize;
	__property OnGesture;
	__property Touch;
	__property StyleElements = {default=7};
	
private:
	// __classmethod void __fastcall Destroy@();
public:
	/* TBaseVirtualTree.Create */ inline __fastcall virtual TVirtualDrawTree(System::Classes::TComponent* AOwner) : TCustomVirtualDrawTree(AOwner) { }
	/* TBaseVirtualTree.Destroy */ inline __fastcall virtual ~TVirtualDrawTree(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TVirtualDrawTree(HWND ParentWindow) : TCustomVirtualDrawTree(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TBlendMode : unsigned char { bmConstantAlpha, bmPerPixelAlpha, bmMasterAlpha, bmConstantAlphaAndColor };

//-- var, const, procedure ---------------------------------------------------
#define VTVersion L"6.0.0"
static const System::Int8 VTTreeStreamVersion = System::Int8(0x2);
static const System::Int8 VTHeaderStreamVersion = System::Int8(0x6);
static const System::Word CacheThreshold = System::Word(0x7d0);
static const System::Byte FadeAnimationStepCount = System::Byte(0xff);
static const System::Int8 ShadowSize = System::Int8(0x5);
static const System::Int8 NoColumn = System::Int8(-1);
static const System::Int8 InvalidColumn = System::Int8(-2);
static const System::Int8 ckEmpty = System::Int8(0x0);
static const System::Int8 ckRadioUncheckedNormal = System::Int8(0x1);
static const System::Int8 ckRadioUncheckedHot = System::Int8(0x2);
static const System::Int8 ckRadioUncheckedPressed = System::Int8(0x3);
static const System::Int8 ckRadioUncheckedDisabled = System::Int8(0x4);
static const System::Int8 ckRadioCheckedNormal = System::Int8(0x5);
static const System::Int8 ckRadioCheckedHot = System::Int8(0x6);
static const System::Int8 ckRadioCheckedPressed = System::Int8(0x7);
static const System::Int8 ckRadioCheckedDisabled = System::Int8(0x8);
static const System::Int8 ckCheckUncheckedNormal = System::Int8(0x9);
static const System::Int8 ckCheckUncheckedHot = System::Int8(0xa);
static const System::Int8 ckCheckUncheckedPressed = System::Int8(0xb);
static const System::Int8 ckCheckUncheckedDisabled = System::Int8(0xc);
static const System::Int8 ckCheckCheckedNormal = System::Int8(0xd);
static const System::Int8 ckCheckCheckedHot = System::Int8(0xe);
static const System::Int8 ckCheckCheckedPressed = System::Int8(0xf);
static const System::Int8 ckCheckCheckedDisabled = System::Int8(0x10);
static const System::Int8 ckCheckMixedNormal = System::Int8(0x11);
static const System::Int8 ckCheckMixedHot = System::Int8(0x12);
static const System::Int8 ckCheckMixedPressed = System::Int8(0x13);
static const System::Int8 ckCheckMixedDisabled = System::Int8(0x14);
static const System::Int8 ckButtonNormal = System::Int8(0x15);
static const System::Int8 ckButtonHot = System::Int8(0x16);
static const System::Int8 ckButtonPressed = System::Int8(0x17);
static const System::Int8 ckButtonDisabled = System::Int8(0x18);
static const System::Int8 ExpandTimer = System::Int8(0x1);
static const System::Int8 EditTimer = System::Int8(0x2);
static const System::Int8 HeaderTimer = System::Int8(0x3);
static const System::Int8 ScrollTimer = System::Int8(0x4);
static const System::Int8 ChangeTimer = System::Int8(0x5);
static const System::Int8 StructureChangeTimer = System::Int8(0x6);
static const System::Int8 SearchTimer = System::Int8(0x7);
static const System::Int8 ThemeChangedTimer = System::Int8(0x8);
static const System::Word ThemeChangedTimerDelay = System::Word(0x1f4);
static const System::Word WM_CHANGESTATE = System::Word(0x8020);
static const System::Word CM_DENYSUBCLASSING = System::Word(0xce3);
static const System::Word CM_AUTOADJUST = System::Word(0xce8);
static const System::Word CM_UPDATE_VCLSTYLE_SCROLLBARS = System::Word(0xd15);
#define CFSTR_VIRTUALTREE L"Virtual Tree Data"
#define CFSTR_VTREFERENCE L"Virtual Tree Reference"
#define CFSTR_HTML L"HTML Format"
#define CFSTR_RTF L"Rich Text Format"
#define CFSTR_RTFNOOBJS L"Rich Text Format Without Objects"
#define CFSTR_CSV L"CSV"
extern DELPHI_PACKAGE GUID IID_IDropTargetHelper;
extern DELPHI_PACKAGE GUID IID_IDragSourceHelper;
extern DELPHI_PACKAGE GUID IID_IDropTarget;
static const System::Word hcTFEditLinkIsNil = System::Word(0x7d0);
static const System::Word hcTFWrongMoveError = System::Word(0x7d1);
static const System::Word hcTFWrongStreamFormat = System::Word(0x7d2);
static const System::Word hcTFWrongStreamVersion = System::Word(0x7d3);
static const System::Word hcTFStreamTooSmall = System::Word(0x7d4);
static const System::Word hcTFCorruptStream1 = System::Word(0x7d5);
static const System::Word hcTFCorruptStream2 = System::Word(0x7d6);
static const System::Word hcTFClipboardFailed = System::Word(0x7d7);
static const System::Word hcTFCannotSetUserData = System::Word(0x7d8);
static const System::Uitypes::TCursor crHeaderSplit = System::Uitypes::TCursor(63);
static const System::Uitypes::TCursor crVertSplit = System::Uitypes::TCursor(62);
static const System::Int8 UtilityImageSize = System::Int8(0x10);
extern DELPHI_PACKAGE System::Word CF_VIRTUALTREE;
extern DELPHI_PACKAGE System::Word CF_VTREFERENCE;
extern DELPHI_PACKAGE System::Word CF_VRTF;
extern DELPHI_PACKAGE System::Word CF_VRTFNOOBJS;
extern DELPHI_PACKAGE System::Word CF_HTML;
extern DELPHI_PACKAGE System::Word CF_CSV;
extern DELPHI_PACKAGE bool MMXAvailable;
extern DELPHI_PACKAGE bool IsWinVistaOrAbove;
#define DefaultPaintOptions (System::Set<TVTPaintOption, TVTPaintOption::toHideFocusRect, TVTPaintOption::toShowFilteredNodes>() << TVTPaintOption::toShowButtons << TVTPaintOption::toShowDropmark << TVTPaintOption::toShowRoot << TVTPaintOption::toShowTreeLines << TVTPaintOption::toThemeAware << TVTPaintOption::toUseBlendedImages )
#define DefaultAnimationOptions EMPTYSET
#define DefaultAutoOptions (System::Set<TVTAutoOption, TVTAutoOption::toAutoDropExpand, TVTAutoOption::toAutoBidiColumnOrdering>() << TVTAutoOption::toAutoDropExpand << TVTAutoOption::toAutoScrollOnExpand << TVTAutoOption::toAutoSort << TVTAutoOption::toAutoTristateTracking << TVTAutoOption::toAutoDeleteMovedNodes << TVTAutoOption::toAutoChangeScale )
#define DefaultSelectionOptions EMPTYSET
#define DefaultMiscOptions (System::Set<TVTMiscOption, TVTMiscOption::toAcceptOLEDrop, TVTMiscOption::toReverseFullExpandHotKey>() << TVTMiscOption::toAcceptOLEDrop << TVTMiscOption::toFullRepaintOnResize << TVTMiscOption::toInitOnSave << TVTMiscOption::toToggleOnDblClick << TVTMiscOption::toWheelPanning << TVTMiscOption::toEditOnClick )
#define DefaultColumnOptions (System::Set<TVTColumnOption, TVTColumnOption::coAllowClick, TVTColumnOption::coEditable>() << TVTColumnOption::coAllowClick << TVTColumnOption::coDraggable << TVTColumnOption::coEnabled << TVTColumnOption::coParentBidiMode << TVTColumnOption::coParentColor << TVTColumnOption::coResizable << TVTColumnOption::coShowDropMark << TVTColumnOption::coVisible << TVTColumnOption::coAllowFocus << TVTColumnOption::coEditable )
#define DefaultStringOptions (System::Set<TVTStringOption, TVTStringOption::toSaveCaptions, TVTStringOption::toAutoAcceptEditChange>() << TVTStringOption::toSaveCaptions << TVTStringOption::toAutoAcceptEditChange )
extern DELPHI_PACKAGE void __fastcall EnumerateVTClipboardFormats(TVirtualTreeClass TreeClass, System::Classes::TStrings* const List)/* overload */;
extern DELPHI_PACKAGE void __fastcall EnumerateVTClipboardFormats(TVirtualTreeClass TreeClass, TFormatEtcArray &Formats)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetVTClipboardFormatDescription(System::Word AFormat);
extern DELPHI_PACKAGE void __fastcall RegisterVTClipboardFormat(System::Word AFormat, TVirtualTreeClass TreeClass, unsigned Priority)/* overload */;
extern DELPHI_PACKAGE System::Word __fastcall RegisterVTClipboardFormat(System::UnicodeString Description, TVirtualTreeClass TreeClass, unsigned Priority, int tymed = 0x1, Winapi::Activex::PDVTargetDevice ptd = (Winapi::Activex::PDVTargetDevice)(0x0), int dwAspect = 0x1, int lindex = 0xffffffff)/* overload */;
extern DELPHI_PACKAGE Vcl::Imglist::TCustomImageList* __fastcall GetUtilityImages(void);
extern DELPHI_PACKAGE void __fastcall ShowError(System::UnicodeString Msg, int HelpContext);
extern DELPHI_PACKAGE TBaseVirtualTree* __fastcall TreeFromNode(PVirtualNode Node);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ShortenString(HDC DC, const System::UnicodeString S, int Width, int EllipsisWidth = 0x0);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WrapString(HDC DC, const System::UnicodeString S, const System::Types::TRect &Bounds, bool RTL, unsigned DrawFormat);
extern DELPHI_PACKAGE void __fastcall GetStringDrawRect(HDC DC, const System::UnicodeString S, System::Types::TRect &Bounds, unsigned DrawFormat);
extern DELPHI_PACKAGE void __fastcall AlphaBlend(HDC Source, HDC Destination, const System::Types::TRect &R, const System::Types::TPoint &Target, TBlendMode Mode, int ConstantAlpha, int Bias);
extern DELPHI_PACKAGE void __fastcall PrtStretchDrawDIB(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &DestRect, Vcl::Graphics::TBitmap* ABitmap);
}	/* namespace Virtualtrees */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VIRTUALTREES)
using namespace Virtualtrees;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VirtualtreesHPP
