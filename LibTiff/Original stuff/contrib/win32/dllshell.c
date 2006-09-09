dllshell.c  ---------------------------------------------------------------
#define  STRICT
#include <windows.h>
#pragma hdrstop

#pragma argsused

/* DLL has an entry point LibMain || DllEntryPoint and an exit point WEP. */

#if defined(__FLAT__)

BOOL WINAPI DllEntryPoint(HINSTANCE hinstDll, DWORD fdwRreason,
		LPVOID plvReserved)
{
	 return 1;   /* Indicate that the DLL was initialized successfully. */
}

#else /* not flat model  */

int FAR PASCAL LibMain(HINSTANCE hInstance, WORD wDataSegment, WORD wHeapSize,
		LPSTR lpszCmdLine)
{
/* The startup code for the DLL initializes the local heap(if there is one)
	with a call to LocalInit which locks the data segment. */

	 if ( wHeapSize != 0 )
		  UnlockData( 0 );
	 return 1;   /* Indicate that the DLL was initialized successfully. */
}

#endif /* __FLAT */

#pragma argsused

int FAR PASCAL WEP ( int bSystemExit )
{
	 return 1;
}
