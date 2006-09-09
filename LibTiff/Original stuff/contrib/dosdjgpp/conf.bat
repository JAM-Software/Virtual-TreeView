@echo off
rem copy the Makefiles for libtiff for DJGPP2 into proper locations
rem we assume to be in $(top)/contrib/dosdjgpp/

copy Makefile.top ..\..\Makefile
copy Makefile.lib ..\..\libtiff\Makefile
copy port.h ..\..\libtiff\port.h
copy Makefile.too* ..\..\tools\Makefile

echo all set for building the library. Now just do make
cd ..\..
