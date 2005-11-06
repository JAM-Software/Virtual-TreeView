/*
//
//  Little cms sources filter utility
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


// A simple filter to strip CR and ^Z, and replace //-like comments to old style

*/

#include <stdio.h>
#include <io.h>


static int ch;
static int IsEof;
static FILE *In, *Out;


static
void NextCh(void)
{
	if (IsEof)
   	ch = 0;
   else {
   		do {

         	ch = getc(In);

            } while (ch == '\r' || ch == '\032');  /* Ignore CR and ^Z */

         IsEof = (ch == EOF);
   		if (IsEof) ch = 0;
   }
}


static
void Translate(void)
{
         IsEof = 0;
         while (!IsEof) {

	         NextCh();
            switch (ch) {

            case '/':
                  NextCh();
                  if (ch == '/') {  /* Found comment */

                        NextCh();

                         /* Cleanup white spaces */
                        while (ch == ' ' && !IsEof)
                              NextCh();

                        if (ch == '\n' && !IsEof) /* Comment is empty */
                              break;
                        
                         /* Comment contains something */

                         putc('/', Out);
                         putc('*', Out);
                         putc(' ', Out);

                         while (ch != '\n' && !IsEof) {

                                  putc(ch, Out);
                                  NextCh();
                         }
                         putc(' ', Out);
                         putc('*', Out);
                         putc('/', Out);
                  }
                  else
                  	putc('/', Out);
                  break;


            default:;
            }

            if (ch != 0)
            		putc(ch, Out);
         }
}


int main(int argc, char *argv[])
{

         if (argc != 3)
         {
              fprintf(stderr, "Usage: %s infile outfile\n", argv[0]);
              return 1;

         }

         if (access(argv[2], 0) == 0)
         {
              fprintf(stderr, "%s already exist, please erase manually\n", argv[2]);
              return 1;
         }

         In = fopen(argv[1], "rb");
         if (!In) { perror(argv[1]); return 1;};

         Out = fopen(argv[2], "wb");
         if (!Out) { perror(argv[2]); return 2;};

         Translate();

         fclose(In); fclose(Out);

         return 0;
}

