//
//  Little cms
//  Copyright (C) 1998-2001 Marti Maria
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

#include "lcms.h"
#include <stdarg.h>

// xgetopt() interface -----------------------------------------------------

extern int   xoptind;
extern char *xoptarg;
extern int   xopterr;
extern char  SW;
int    cdecl xgetopt(int argc, char *argv[], char *optionS);

// ------------------------------------------------------------------------

static char* Description = "Devicelink profile";
static int Intent = INTENT_PERCEPTUAL;
static char *cOutProf = "devicelink.icm";
static int PrecalcMode = 1;


static
void FatalError(const char *frm, ...)
{
       va_list args;

       va_start(args, frm);
       vfprintf(stderr, frm, args);
       va_end(args);

       exit(1);
}

// The toggles stuff

static
void HandleSwitches(int argc, char *argv[])
{
       int s;
      
       while ((s = xgetopt(argc,argv,"O:o:T:t:D:d:C:c:")) != EOF) {

       switch (s){

     
       case 'd':
       case 'D':
            Description = xoptarg;
            break;

       case 'o':
       case 'O':
           cOutProf = xoptarg;
            break;


       case 't':
       case 'T':
            Intent = atoi(xoptarg);
            if (Intent > 3) Intent = 3;
            if (Intent < 0) Intent = 0;
            break;
     
        case 'c':
        case 'C':
            PrecalcMode = atoi(xoptarg);
            if (PrecalcMode < 0 || PrecalcMode > 2)
                    FatalError("ERROR: Unknown precalc mode '%d'", PrecalcMode);
            break;

            

  default:

       FatalError("Unknown option - run without args to see valid ones.\n");
    }       
    }
}

static
void Help(void)
{
    
     fprintf(stderr, "\nLinks two or more profiles into a single devicelink profile.\n");     
     fprintf(stderr, "Colorspaces must be paired except Lab/XYZ, that can be interchanged.\n");
     fprintf(stderr, "\n");     
     fprintf(stderr, "usage: icclink [flags] <profiles>\n\n");
     fprintf(stderr, "flags:\n\n");         
     fprintf(stderr, "%co<profile> - Output devicelink profile. [defaults to 'devicelink.icm']\n", SW);        
     fprintf(stderr, "%ct<0,1,2,3> - Intent (0=Perceptual, 1=Colorimetric, 2=Saturation, 3=Absolute)\n", SW);    
     fprintf(stderr, "%cc<0,1,2> - Precission (0=LowRes, 1=Normal, 2=Hi-res) [defaults to 1]\n", SW);     
     fprintf(stderr, "%cd<description> - description text (quotes can be used)\n", SW);      
     fprintf(stderr, "\n");
     fprintf(stderr, "This program is intended to be a demo of the little cms\n"
                     "engine. Both lcms and this program are freeware. You can\n"
                     "obtain both in source code at http://www.littlecms.com\n"
                     "For suggestions, comments, bug reports etc. send mail to\n"
                     "marti@littlecms.com\n\n");
     exit(0);
}


static
cmsHPROFILE OpenProfile(const char* File)
{
    cmsHPROFILE h;

       if (!File) 
            return cmsCreate_sRGBProfile();    
       
       if (stricmp(File, "*Lab") == 0)
                return cmsCreateLabProfile(NULL);
       
       if (stricmp(File, "*XYZ") == 0)
                return cmsCreateXYZProfile();
         
       if (stricmp(File, "*srgb") == 0)
                return cmsCreate_sRGBProfile();

       if (stricmp(File, "*Gray22") == 0) {
           LPGAMMATABLE Gamma = cmsBuildGamma(256, 2.2);
           cmsHPROFILE hProfile = cmsCreateGrayProfile(cmsD50_xyY(), Gamma);
           cmsFreeGamma(Gamma);
           return hProfile;

       }

       h = cmsOpenProfileFromFile(File, "r");

       if (cmsGetDeviceClass(h) == icSigNamedColorClass)
            FatalError("ERROR: Cannot make devicelink of named color profiles!");

       return h;
}


int main(int argc, char *argv[])
{
    int i, nargs;
    cmsHPROFILE Profiles[256];
    cmsHPROFILE hProfile;
    DWORD dwFlags = 0;
    cmsHTRANSFORM hTransform;

    fprintf(stderr, "little cms device link generator - v1.0\n");

     HandleSwitches(argc, argv);

     nargs = (argc - xoptind);
     if (nargs < 2)
                Help(); 
     
     if (nargs > 255)
            FatalError("ERROR: Holy profile! what are you trying to do with so many profiles?");


     for (i=0; i < nargs; i++) {
         Profiles[i] = OpenProfile(argv[i + xoptind]);
     }

    
     switch (PrecalcMode) {
            
        case 0: dwFlags |= cmsFLAGS_LOWRESPRECALC; break;
        case 2: dwFlags |= cmsFLAGS_HIGHRESPRECALC; break;
        case 1: break;

        default: FatalError("ERROR: Unknown precalculation mode '%d'", PrecalcMode);
     }

     hTransform = cmsCreateMultiprofileTransform(Profiles, nargs, 0, 0, Intent, dwFlags);
     if (hTransform) {

           
        hProfile =  cmsTransform2DeviceLink(hTransform, dwFlags);
        cmsAddTag(hProfile, icSigProfileDescriptionTag, Description);
        cmsAddTag(hProfile, icSigCopyrightTag, "Generated by littlecms icclink. No copyright, use freely");

        if (_cmsSaveProfile(hProfile, cOutProf)) 
                fprintf(stderr, "Ok");
        else 
                fprintf(stderr, "Error saving file!");

        cmsCloseProfile(hProfile);

     }

     cmsDeleteTransform(hTransform);

     for (i=0; i < nargs; i++) {
         cmsCloseProfile(Profiles[i]);
     }

            
      return 0;     
}
