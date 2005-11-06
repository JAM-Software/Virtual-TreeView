//
//  Little cms
//  Copyright (C) 1998-2000 Marti Maria
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


// As a rule, only the functions visibles from API can signal
// errors.
void cdecl cmsSignalError(int ErrorCode, const char *ErrorText, ...);
int  LCMSEXPORT cmsErrorAction(int lAbort);


// ******************************************************************

static int nDoAbort = LCMS_ERROR_ABORT;


int LCMSEXPORT cmsErrorAction(int nAction)
{
       int nOld = nDoAbort;    
       nDoAbort = nAction;

       return nOld;
}


// Default error handler

// ml: Use the Delphi Abort method (silent exception) to return flow to caller.
extern __pascal void Abort(void);

void cmsSignalError(int ErrorCode, const char *ErrorText, ...)
{
       va_list args;

      
       if (nDoAbort == LCMS_ERROR_IGNORE) return;

        va_start(args, ErrorText);

// ml: Have to undefine the __CONSOLE__ macro for BCB as it seems to be defined by default for lib projects.
#undef __CONSOLE__

#if defined( __CONSOLE__ ) || defined( NON_WINDOWS )

              fprintf(stderr, "lcms: Error #%d; ", ErrorCode);
              vfprintf(stderr, ErrorText, args);
              fprintf(stderr, "\n");
              va_end(args);

              if (nDoAbort == LCMS_ERROR_ABORT) exit(1);
#else
              {
              char Buffer1[1024];
              char Buffer2[256];

              sprintf(Buffer1, "Error #%x; ", ErrorCode);
              vsprintf(Buffer2, ErrorText, args);
              strcat(Buffer1, Buffer2);
              MessageBox(NULL, Buffer1, "Little cms",
                                          MB_OK|MB_ICONSTOP|MB_TASKMODAL);
              va_end(args);

              if (nDoAbort == LCMS_ERROR_ABORT) {

#ifdef __BORLANDC__
                // ml: Use SEH to signal errors instead just blowing up the application.
                Abort();
                    //_cexit();
#else
                  FatalAppExit(0, "lcms is terminating application");
#endif
              }

              }
#endif
}
