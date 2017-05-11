// This project is recoded in C++ Builder in order to diagnose bugs related to
// C++ Builder building and linking.
// Demonstration project for TVirtualStringTree to generally show how to get started.

//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "VirtualTrees"
#pragma resource "*.dfm"
TMainForm *MainForm;

// This is a very simple record we use to store data in the nodes.
// Since the application is responsible to manage all data including the node's caption
// this record can be considered as minimal requirement in all VT applications.
// Extend it to whatever your application needs.
typedef struct tagTMyRec {
   UnicodeString Caption;
} TMyRec, *PMyRec;


//---------------------------------------------------------------------------

void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  // Let the tree know how much data space we need.
  VST->NodeDataSize = sizeof(TMyRec);

  // Set an initial number of nodes.
  VST->RootNodeCount = 20;
}

//---------------------------------------------------------------------------

void __fastcall TMainForm::ClearButtonClick(TObject *Sender)
{

  Cardinal Start;
  Screen->Cursor = crHourGlass;

  try {
	Start = GetTickCount();
	VST->Clear();
	Label1->Caption = Format("Last operation duration: %d ms", ARRAYOFCONST((GetTickCount() - Start)));
  }
  __finally {
	Screen->Cursor = crDefault;
  }
}

//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TMainForm::AddButtonClick(TObject *Sender)
{

  Cardinal Count, Start;

  // Add some nodes to the treeview.
  Screen->Cursor = crHourGlass;
  try {
	Start = GetTickCount();
	switch (((TButton *)Sender)->Tag) {
	  case 0: // add to root
		Count = StrToInt(Edit1->Text);
		VST->RootNodeCount = VST->RootNodeCount + Count;
		break;
	  case 1: // add as child
		if (VST->FocusedNode != NULL) {
		  Count = StrToInt(Edit1->Text);
		  VST->ChildCount[VST->FocusedNode] = VST->ChildCount[VST->FocusedNode] + Count;
		  VST->Expanded[VST->FocusedNode] = True;
		  VST->InvalidateToBottom(VST->FocusedNode);
		}
		break;
	}
	Label1->Caption = Format("Last operation duration: %d ms", ARRAYOFCONST((GetTickCount() - Start)));
  }
  __finally {
	Screen->Cursor = crDefault;
  }

}

//---------------------------------------------------------------------------

void __fastcall TMainForm::VSTGetText(TBaseVirtualTree *Sender, PVirtualNode Node,
		  TColumnIndex Column, TVSTTextType TextType, UnicodeString &Text)

{
  PMyRec Data;
  Data = (PMyRec) Sender->GetNodeData(Node);
  if (Data != NULL)
	Text = Data->Caption;
}

//---------------------------------------------------------------------------

void __fastcall TMainForm::VSTFreeNode(TBaseVirtualTree *Sender, PVirtualNode Node)

{
  PMyRec Data;
  Data = (PMyRec) Sender->GetNodeData(Node);

  // If your Data contains fields, they must be freed as necessary.
  // If the Data contains strings, they must be set to empty strings.
  // In Delphi, we took help of Finalize(Data^) to do that in one call.
  // But there is no such thing in C++ Builder. However there is a trick
  // as shown below.
  Data->~tagTMyRec(); //equivalent of Finalize in C++ Builder
}

//---------------------------------------------------------------------------

void __fastcall TMainForm::VSTInitNode(TBaseVirtualTree *Sender, PVirtualNode ParentNode,
		  PVirtualNode Node, TVirtualNodeInitStates &InitialStates)

{
  PMyRec Data;
  Data = (PMyRec) Sender->GetNodeData(Node);
  // Construct a node caption. This event is triggered once for each node but
  // appears asynchronously, which means when the node is displayed not when it is added.
  Data->Caption = Format("Level %d, Index %d", ARRAYOFCONST((VST->GetNodeLevel(Node), Node->Index)));
}

//---------------------------------------------------------------------------

void __fastcall TMainForm::CloseButtonClick(TObject *Sender)
{
  Close();
}



