//---------------------------------------------------------------------------

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "VirtualTrees.hpp"
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TVirtualStringTree *VST;
	TButton *ClearButton;
	TButton *CloseButton;
	TButton *Button1;
	TButton *AddOneButton;
	TEdit *Edit1;
	TLabel *Label1;
	void __fastcall AddButtonClick(TObject *Sender);
	void __fastcall ClearButtonClick(TObject *Sender);
	void __fastcall CloseButtonClick(TObject *Sender);
	void __fastcall VSTFreeNode(TBaseVirtualTree *Sender, PVirtualNode Node);
	void __fastcall VSTInitNode(TBaseVirtualTree *Sender, PVirtualNode ParentNode, PVirtualNode Node,
          TVirtualNodeInitStates &InitialStates);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall VSTGetText(TBaseVirtualTree *Sender, PVirtualNode Node, TColumnIndex Column,
          TVSTTextType TextType, UnicodeString &Text);

private:	// User declarations
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
