//
//  Little cms
//  Copyright (C) 1998-2002 Marti Maria
//
// THIS SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
// EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
// WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
//
// IN NO EVENT SHALL MARTI MARIA BE LIABLE FOR ANY SPECIAL, INCIDENTAL,
// INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
// OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
// WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
// LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
// OF THIS SOFTWARE.
//
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

// Named color support

#include "lcms.h"


LPcmsNAMEDCOLORLIST  cdecl cmsAllocNamedColorList(void);
void				 cdecl cmsFreeNamedColorList(LPcmsNAMEDCOLORLIST List);
BOOL				 cdecl cmsAppendNamedColor(cmsHTRANSFORM xform, const char* Name, WORD PCS[3], WORD Colorant[MAXCHANNELS]);


// ---------------------------------------------------------------------------------

static
BOOL GrowNamedColorList(LPcmsNAMEDCOLORLIST v, int ByElements)
{
	LPcmsNAMEDCOLOR NewList;
	int NewElements;
	
	if (ByElements > v ->Allocated) {

		if (v ->Allocated == 0)
			NewElements = 64;	// Initial guess
		else
			NewElements = v ->Allocated;

		while (ByElements > NewElements)
				NewElements *= 2;
		
		NewList = (LPcmsNAMEDCOLOR) malloc(sizeof(cmsNAMEDCOLOR) * NewElements);

		if (NewList == NULL) {
			cmsSignalError(LCMS_ERRC_ABORTED, "Out of memory reallocating named color list");
			return FALSE;
		}
		else {

			if (v -> List) {
				CopyMemory(NewList, v -> List, v ->nColors* sizeof(cmsNAMEDCOLOR));
				free(v -> List);				
			}

			v -> List = NewList;
			v -> Allocated = NewElements;
			return TRUE;
		}
	}

	return TRUE;
}


LPcmsNAMEDCOLORLIST cmsAllocNamedColorList(void)
{
	LPcmsNAMEDCOLORLIST v = (LPcmsNAMEDCOLORLIST) malloc(sizeof(cmsNAMEDCOLORLIST));

	if (v == NULL) {
		cmsSignalError(LCMS_ERRC_ABORTED, "Out of memory creating named color list");
		return NULL;
	}

	ZeroMemory(v, sizeof(cmsNAMEDCOLORLIST));

	v ->nColors   = 0;
	v ->Allocated = 0;	
	v ->Prefix[0] = 0;
	v ->Suffix[0] = 0;	
	v -> List	  = NULL; 
	
	
	return v;
}

void cmsFreeNamedColorList(LPcmsNAMEDCOLORLIST v)
{
	if (v == NULL) {
		cmsSignalError(LCMS_ERRC_RECOVERABLE, "Couldn't free a NULL named color list");
		return;
	}
				
	if (v -> List) free(v->List);
	free(v);
}	




BOOL cmsAppendNamedColor(cmsHTRANSFORM xform, const char* Name, WORD PCS[3], WORD Colorant[MAXCHANNELS])
{
	_LPcmsTRANSFORM v = (_LPcmsTRANSFORM) xform;
	LPcmsNAMEDCOLORLIST List = v ->NamedColorList;
	int i;

	if (List == NULL) return FALSE;
	if (!GrowNamedColorList(List, List ->nColors + 1)) return FALSE;
	
	for (i=0; i < MAXCHANNELS; i++)
		List ->List[List ->nColors].DeviceColorant[i] = Colorant[i];

	for (i=0; i < 3; i++)
		List ->List[List ->nColors].PCS[i] = PCS[i];

	strncpy(List ->List[List ->nColors].Name, Name, MAX_PATH-1);

	List ->nColors++;
	return TRUE;
}


// Returns named color count 

int LCMSEXPORT cmsNamedColorCount(cmsHTRANSFORM xform)
{
	 _LPcmsTRANSFORM v = (_LPcmsTRANSFORM) xform;

	 if (v ->NamedColorList == NULL) return 0;
	 return v ->NamedColorList ->nColors;
}


BOOL LCMSEXPORT cmsNamedColorInfo(cmsHTRANSFORM xform, int nColor, char* Name, char* Prefix, char* Suffix)
{
	_LPcmsTRANSFORM v = (_LPcmsTRANSFORM) xform;


	 if (v ->NamedColorList == NULL) return FALSE;

	 if (nColor < 0 || nColor >= cmsNamedColorCount(xform)) return FALSE;

	 if (Name) strncpy(Name, v ->NamedColorList->List[nColor].Name, 31);
	 if (Prefix) strncpy(Name, v ->NamedColorList->Prefix, 31);
	 if (Suffix) strncpy(Name, v ->NamedColorList->Suffix, 31);

	 return TRUE;
}


int  LCMSEXPORT cmsNamedColorIndex(cmsHTRANSFORM xform, const char* Name)
{
	_LPcmsTRANSFORM v = (_LPcmsTRANSFORM) xform;	
	int i, n;

		 if (v ->NamedColorList == NULL) return -1;

		n = cmsNamedColorCount(xform);
		for (i=0; i < n; i++) {
			if (stricmp(Name,  v ->NamedColorList->List[i].Name) == 0)
					return i;
		}

		return -1;
}
