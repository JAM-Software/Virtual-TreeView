//
//  Little cms
//  Copyright (C) 1998-2003 Marti Maria
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

// Version 1.10


// Ugly-- swig doesn't like VC6 __declspec and so, so force to non-windows
// (this has no significance at all)

#ifndef NON_WINDOWS
#define NON_WINDOWS	1
#endif

/* File lcms.i */
%module lcms
%{
#include "lcms.h"
%}


#define register
%varargs(char* args) cmsSignalError;

%ignore USE_FLOAT;
%ignore USE_C;
%ignore USE_ASSEMBLER;
%ignore USE_TRILINEAR;
%ignore USE_TETRAHEDRAL;
%ignore LCMS_DLL;
%ignore LCMS_DLL_BUILD;
%ignore USE_BIG_ENDIAN;
%ignore USE_INT64;
%ignore USE_CUSTOM_SWAB;
%ignore M_PI;
%ignore LOGE;
%ignore LCMS_APIONLY;
%ignore MAX_PATH;
%ignore TRUE;
%ignore FALSE;
%ignore _cmsSAMPLER;
%ignore SAMPLEDCURVE;
%ignore LPSAMPLEDCURVE;
%ignore cmsDoTransform;

%rename (cmsSaveProfile) _cmsSaveProfile;

/*
* Some typemaps
*/

/*
*	Gamma triple. Must be specified as tuple.
*/

%typemap(in) LPGAMMATABLE[3](LPGAMMATABLE gamma[3]) {  
  
  PyObject* tmp[3];

  if (PyTuple_Check($input)) {
    if (!PyArg_ParseTuple($input,"OOO",&tmp[0],&tmp[1],&tmp[2])) {
      PyErr_SetString(PyExc_TypeError,"gamma must have 3 elements");
      return NULL;
    }
	
	if ((SWIG_ConvertPtr(tmp[0], (void **) &gamma[0], SWIGTYPE_LPGAMMATABLE,1)) == -1) return NULL;
	if ((SWIG_ConvertPtr(tmp[1], (void **) &gamma[1], SWIGTYPE_LPGAMMATABLE,1)) == -1) return NULL;
	if ((SWIG_ConvertPtr(tmp[2], (void **) &gamma[2], SWIGTYPE_LPGAMMATABLE,1)) == -1) return NULL;

    $1 = gamma;

  } else {
    PyErr_SetString(PyExc_TypeError,"expected a tuple.");
    return NULL;
  }
}




/*
*	1-Color containers
*/

%inline %{
typedef struct {

	WORD w[MAXCHANNELS];
		
	} COLORW;

typedef COLORW* LPCOLORW;

typedef struct {

	BYTE b[MAXCHANNELS];
	
	} COLORB;

typedef COLORB* LPCOLORB;
%}

/*
*  Turn off all symbols of lcms.h except pure API
*/

#define  LCMS_APIONLY   1
%include "lcms.h"


/*
* Usefull for debug
*/

extern BOOL _cmsSaveProfile(cmsHPROFILE hProfile, const char* FileName);


/*
** Extend our basic types
*/


%extend COLORB {
		COLORB() {		                
				LPCOLORB v;
                v = (LPCOLORB) malloc(sizeof(COLORB));
				ZeroMemory(v, sizeof(COLORB));
                return v;
        }
        ~COLORB() {
                free(self);
        }
		const char* __repr__() {                
                return "Color 8 bps";
        }

		WORD __getitem__(int key) {
                return self -> b[key];
        }
        void __setitem__(int key, int val) {
                self -> b[key] = (BYTE) val;
        }
};


%extend COLORW {
		COLORW() {		                
				LPCOLORW v;
                v = (LPCOLORW) malloc(sizeof(COLORW));
				ZeroMemory(v, sizeof(COLORW));
                return v;
        }
        ~COLORB() {
                free(self);
        }
		const char* __repr__() {                
                return "Color 16 bps";
        }

		WORD __getitem__(int key) {
                return self -> w[key];
        }
        void __setitem__(int key, int val) {
                self -> w[key] = (WORD) val;
        }
};



%extend cmsCIEXYZ {
        cmsCIEXYZ(double x=0, double y=0, double z=0) {
                LPcmsCIEXYZ v;
                v = (LPcmsCIEXYZ) malloc(sizeof(cmsCIEXYZ));
                v->X = x;
                v->Y = y;
                v->Z = z;
                return v;
        }
        ~cmsCIEXYZ() {
                free(self);
        }
        const char* __repr__() {
                static char Buffer[256];

                sprintf(Buffer, "XYZ [%g, %g, %g]", self->X,self->Y,self->Z);
                return Buffer;
        }
};

%extend cmsCIExyY {
        cmsCIExyY(double x=0, double y=0, double Y=0) {
                LPcmsCIExyY v;
                v = (LPcmsCIExyY) malloc(sizeof(cmsCIExyY));
                v->x = x;
                v->y = y;
                v->Y = Y;
                return v;
        }
        ~cmsCIExyY() {
                free(self);
        }
        const char* __repr__() {
                static char Buffer[256];

                sprintf(Buffer, "xyY [%g, %g, %g]", self->x,self->y,self->Y);
                return Buffer;
        }
};

%extend cmsCIELab {
        cmsCIELab(double L=0, double a=0, double b=0) {
                LPcmsCIELab v;
                v = (LPcmsCIELab) malloc(sizeof(cmsCIELab));
                v->L = L;
                v->a = a;
                v->b = b;
                return v;
        }
        ~cmsCIELab() {
                free(self);
        }
        const char* __repr__() {
                static char Buffer[256];

                sprintf(Buffer, "Lab [%g, %g, %g]", self->L,self->a,self->b);
                return Buffer;
        }
        int __cmp__(cmsCIELab* a) {
                return cmsDeltaE(self, a) > 0.0;
        }

};

%extend cmsCIELCh {
        cmsCIELCh(double L=0, double C=0, double h=0) {
                LPcmsCIELCh v;
                v = (LPcmsCIELCh) malloc(sizeof(cmsCIELCh));
                v->L = L;
                v->C = C;
                v->h = h;
                return v;
        }
        ~cmsCIELCh() {
                free(self);
        }
        const char* __repr__() {
                static char Buffer[256];

                sprintf(Buffer, "LCh [%g, %g, %g]", self->L,self->C,self->h);
                return Buffer;
        }
};


%extend cmsJCh {
        cmsJCh(double J=0, double C=0, double h=0) {
                LPcmsJCh v;
                v = (LPcmsJCh) malloc(sizeof(cmsJCh));
                v->J = J;
                v->C = C;
                v->h = h;
                return v;
        }
        ~cmsJCh() {
                free(self);
        }
        const char* __repr__() {
                static char Buffer[256];

                sprintf(Buffer, "CIECAM JCh [%g, %g, %g]", self->J,self->C,self->h);
                return Buffer;
        }
};

%extend GAMMATABLE {
        GAMMATABLE(double Gamma, int nEntries=256) {
                return Gamma <= 0 ? cmsAllocGamma(nEntries) : cmsBuildGamma(nEntries, Gamma);
        }
        ~GAMMATABLE() {
                cmsFreeGamma(self);
        }

        const char* __repr__() {

				static char Buffer[256];

                sprintf(Buffer, "Gamma Table of %d entries [estimated gamma %g]", self ->nEntries, cmsEstimateGamma(self));
				return Buffer;
        }
        WORD __getitem__(int key) {
                return self -> GammaTable[key];
        }
        void __setitem__(int key, WORD val) {
                self -> GammaTable[key] = val;
        }

};


%extend cmsCIExyYTRIPLE {
        cmsCIExyYTRIPLE(cmsCIExyY* Red, cmsCIExyY* Green, cmsCIExyY* Blue) {
                LPcmsCIExyYTRIPLE v;
                v = (LPcmsCIExyYTRIPLE) malloc(sizeof(cmsCIExyYTRIPLE));
                CopyMemory(&v->Red,   Red,   sizeof(cmsCIExyY));
				CopyMemory(&v->Green, Green, sizeof(cmsCIExyY));
				CopyMemory(&v->Blue,  Blue,  sizeof(cmsCIExyY));
                return v;
        }
        ~cmsCIExyYTRIPLE() {
                free(self);
        }
        const char* __repr__() {
                
                return "xyY Triple";                
        }
};


%extend cmsCIEXYZTRIPLE {
        cmsCIEXYZTRIPLE(cmsCIEXYZ* Red, cmsCIEXYZ* Green, cmsCIEXYZ* Blue) {
                LPcmsCIEXYZTRIPLE v;
                v = (LPcmsCIEXYZTRIPLE) malloc(sizeof(cmsCIEXYZTRIPLE));
                CopyMemory(&v->Red,   Red,   sizeof(cmsCIExyY));
				CopyMemory(&v->Green, Green, sizeof(cmsCIExyY));
				CopyMemory(&v->Blue,  Blue,  sizeof(cmsCIExyY));
                return v;
        }
        ~cmsCIEXYZTRIPLE() {
                free(self);
        }
        const char* __repr__() {
                
                return "xyY Triple";                
        }
};




%extend cmsViewingConditions {
        cmsViewingConditions(cmsCIEXYZ* WhitePoint, double Yb, double La, int surround, double D_value) {
                LPcmsViewingConditions v;
                v = (LPcmsViewingConditions) malloc(sizeof(cmsViewingConditions));
				CopyMemory(&v -> whitePoint, WhitePoint, sizeof(cmsCIEXYZ));
				v ->Yb = Yb;
				v ->La = La;
				v -> surround = surround;
				v -> D_value = D_value;
                return v;
        }
        ~cmsViewingConditions() {
                free(self);
        }
        const char* __repr__() {
                
                return "CIECAM97s viewing conditions";                
        }
};



%extend VEC3 {
        VEC3(double vx=0, double vy=0, double vz=0) {
                LPVEC3 v;
                v = (LPVEC3) malloc(sizeof(VEC3));
                v->n[VX] =vx;
                v->n[VY] =vy;
                v->n[VZ] =vz;
                return v;

        }
        ~VEC3() {
                free(self);
        }
        double __getitem__(int key) {
                return self -> n[key];
        }
        void __setitem__(int key, double val) {
                self -> n[key] = val;
        }
        const char* __repr__() {
                static char Buffer[256];

                sprintf(Buffer, "VEC3 [%g, %g, %g]", self->n[VX],self->n[VY],self->n[VZ]);
                return Buffer;
        }

};


class icTagSignature {
};

%extend icTagSignature {
	icTagSignature(unsigned int n) {
		icTagSignature* v =  (icTagSignature*) malloc(sizeof(icTagSignature));
		*v = (icTagSignature) n;
		return v;
	}

	~icTagSignature() {
                free(self);
    }
};


// ----------------------------------------------------------------------- TODO


%inline %{
class StrPointer {
};
%}

%extend StrPointer {
	StrPointer(char* s) {
		return (StrPointer*) s;
	}
};
