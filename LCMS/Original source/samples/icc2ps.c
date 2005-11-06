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

static char *cInProf = NULL;
static char *cOutProf = NULL;
static int Intent = INTENT_PERCEPTUAL;
static FILE* OutFile;


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
      
       while ((s = xgetopt(argc,argv,"I:i:O:o:T:t:")) != EOF) {

       switch (s){

     
       case 'i':
       case 'I':
            cInProf = xoptarg;
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
     
     

  default:

       FatalError("Unknown option - run without args to see valid ones.\n");
    }       
    }
}

static
void Help(void)
{
     fprintf(stderr, "\nGenerates PostScript CRD or CSA from ICC profiles.\n");
     fprintf(stderr, "If no file is given, output goes to stdout.\n");
     fprintf(stderr, "You can specify by %ci or %co the desired profile(s).\n", SW, SW);
     fprintf(stderr, "Devicelink profiles are accepted as long as input\n");
     fprintf(stderr, "for CRD / output for CSA were XYZ or Lab.\n\n");    

     fprintf(stderr, "usage: icc2ps [flags] [output file]\n\n");

     fprintf(stderr, "flags:\n\n");
     
     fprintf(stderr, "%ci<profile> - Input profile: Generates Color Space Array (CSA)\n", SW);
     fprintf(stderr, "%co<profile> - Output profile: Generates Color Rendering Dictionary (CRD)\n", SW);   
     
     fprintf(stderr, "%ct<0,1,2,3> - Intent (0=Perceptual, 1=Colorimetric, 2=Saturation, 3=Absolute)\n", SW);    
          
     fprintf(stderr, "\n");
     fprintf(stderr, "This program is intended to be a demo of the little cms\n"
                     "engine. Both lcms and this program are freeware. You can\n"
                     "obtain both in source code at http://www.littlecms.com\n"
                     "For suggestions, comments, bug reports etc. send mail to\n"
                     "marti@littlecms.com\n\n");
     exit(0);
}


static
void GenerateCSA(void)
{
    cmsHPROFILE hProfile = cmsOpenProfileFromFile(cInProf, "r");
    size_t n;
    char* Buffer;

    n = cmsGetPostScriptCSA(hProfile, Intent, NULL, 0);
    if (n == 0) return;

    Buffer = (char*) malloc(n + 1);
    cmsGetPostScriptCSA(hProfile, Intent, Buffer, n);
    Buffer[n] = 0;

    fprintf(OutFile, "%s", Buffer); 
    
    free(Buffer);
    cmsCloseProfile(hProfile);
}


static
void GenerateCRD(void)
{
    cmsHPROFILE hProfile = cmsOpenProfileFromFile(cOutProf, "r");
    size_t n;
    char* Buffer;

    n = cmsGetPostScriptCRD(hProfile, Intent, NULL, 0);
    if (n == 0) return;

    Buffer = (char*) malloc(n + 1);
    cmsGetPostScriptCRD(hProfile, Intent, Buffer, n);
    Buffer[n] = 0;

    fprintf(OutFile, "%s", Buffer);         
    free(Buffer);
    cmsCloseProfile(hProfile);
}



int main(int argc, char *argv[])
{
    int nargs;

    fprintf(stderr, "little cms PostScript converter - v1.1\n");

     HandleSwitches(argc, argv);

     nargs = (argc - xoptind);
     if (nargs != 0 && nargs != 1)
                Help();            
    
     if (nargs == 0) 
            OutFile = stdout;
     else
            OutFile = fopen(argv[xoptind], "wt");
        

     if (cInProf == NULL && cOutProf == NULL)
                Help();

    
      if (cInProf != NULL)
            GenerateCSA();
          
      if (cOutProf != NULL)
            GenerateCRD();
        
      if (nargs == 1) {
          fclose(OutFile);
      }

      fprintf(stderr, "Ok\n");
      return 0;     
}


