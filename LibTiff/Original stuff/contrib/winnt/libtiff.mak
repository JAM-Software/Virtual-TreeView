# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (ALPHA) Static Library" 0x0604
# TARGTYPE "Macintosh Static Library" 0x0304
# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug" && "$(CFG)" !=\
 "Macintosh Release" && "$(CFG)" != "Macintosh Debug" && "$(CFG)" != "APXrel" &&\
 "$(CFG)" != "APXdeb"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "libtiff.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "Macintosh Release" (based on "Macintosh Static Library")
!MESSAGE "Macintosh Debug" (based on "Macintosh Static Library")
!MESSAGE "APXrel" (based on "Win32 (ALPHA) Static Library")
!MESSAGE "APXdeb" (based on "Win32 (ALPHA) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Release"

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : $(OUTDIR)/""libtiff.lib"" $(OUTDIR)/""libtiff.bsc""

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /YX /O2 /I "." /I ".." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
CPP_PROJ=/nologo /MT /W3 /GX /YX /O2 /I "." /I ".." /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /FR$(INTDIR)/ /Fp$(OUTDIR)/"libtiff.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libtiff.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"tif_jpeg.sbr" \
	$(INTDIR)/"tif_dirinfo.sbr" \
	$(INTDIR)/"tif_win32.sbr" \
	$(INTDIR)/"tif_flush.sbr" \
	$(INTDIR)/"tif_thunder.sbr" \
	$(INTDIR)/"tif_compress.sbr" \
	$(INTDIR)/"tif_print.sbr" \
	$(INTDIR)/"tif_dirread.sbr" \
	$(INTDIR)/"tif_getimage.sbr" \
	$(INTDIR)/"tif_fax3.sbr" \
	$(INTDIR)/"tif_version.sbr" \
	$(INTDIR)/"tif_codec.sbr" \
	$(INTDIR)/"tif_dir.sbr" \
	$(INTDIR)/"tif_predict.sbr" \
	$(INTDIR)/"tif_close.sbr" \
	$(INTDIR)/"tif_dumpmode.sbr" \
	$(INTDIR)/"tif_aux.sbr" \
	$(INTDIR)/"tif_error.sbr" \
	$(INTDIR)/"tif_lzw.sbr" \
	$(INTDIR)/"tif_zip.sbr" \
	$(INTDIR)/"tif_read.sbr" \
	$(INTDIR)/"tif_packbits.sbr" \
	$(INTDIR)/"tif_swab.sbr" \
	$(INTDIR)/"tif_dirwrite.sbr" \
	$(INTDIR)/"tif_open.sbr" \
	$(INTDIR)/"tif_warning.sbr" \
	$(INTDIR)/"tif_tile.sbr" \
	$(INTDIR)/"tif_strip.sbr" \
	$(INTDIR)/"tif_next.sbr" \
	$(INTDIR)/"tif_write.sbr" \
	$(INTDIR)/"fax3sm.sbr"

$(OUTDIR)/"libtiff.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"libtiff.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/"tif_jpeg.obj" \
	$(INTDIR)/"tif_dirinfo.obj" \
	$(INTDIR)/"tif_win32.obj" \
	$(INTDIR)/"tif_flush.obj" \
	$(INTDIR)/"tif_thunder.obj" \
	$(INTDIR)/"tif_compress.obj" \
	$(INTDIR)/"tif_print.obj" \
	$(INTDIR)/"tif_dirread.obj" \
	$(INTDIR)/"tif_getimage.obj" \
	$(INTDIR)/"tif_fax3.obj" \
	$(INTDIR)/"tif_version.obj" \
	$(INTDIR)/"tif_codec.obj" \
	$(INTDIR)/"tif_dir.obj" \
	$(INTDIR)/"tif_predict.obj" \
	$(INTDIR)/"tif_close.obj" \
	$(INTDIR)/"tif_dumpmode.obj" \
	$(INTDIR)/"tif_aux.obj" \
	$(INTDIR)/"tif_error.obj" \
	$(INTDIR)/"tif_lzw.obj" \
	$(INTDIR)/"tif_zip.obj" \
	$(INTDIR)/"tif_read.obj" \
	$(INTDIR)/"tif_packbits.obj" \
	$(INTDIR)/"tif_swab.obj" \
	$(INTDIR)/"tif_dirwrite.obj" \
	$(INTDIR)/"tif_open.obj" \
	$(INTDIR)/"tif_warning.obj" \
	$(INTDIR)/"tif_tile.obj" \
	$(INTDIR)/"tif_strip.obj" \
	$(INTDIR)/"tif_next.obj" \
	$(INTDIR)/"tif_write.obj" \
	$(INTDIR)/"fax3sm.obj"

$(OUTDIR)/"libtiff.lib" : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : $(OUTDIR)/""dlibtiff.lib"" $(OUTDIR)/""libtiff.bsc""

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /Z7 /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /Z7 /YX /Od /I "." /I ".." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FAs /FR /c
CPP_PROJ=/nologo /MT /W3 /GX /Z7 /YX /Od /I "." /I ".." /D "WIN32" /D "_DEBUG"\
 /D "_WINDOWS" /FAs /Fa$(INTDIR)/ /FR$(INTDIR)/ /Fp$(OUTDIR)/"libtiff.pch"\
 /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinDebug/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libtiff.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"tif_jpeg.sbr" \
	$(INTDIR)/"tif_dirinfo.sbr" \
	$(INTDIR)/"tif_win32.sbr" \
	$(INTDIR)/"tif_flush.sbr" \
	$(INTDIR)/"tif_thunder.sbr" \
	$(INTDIR)/"tif_compress.sbr" \
	$(INTDIR)/"tif_print.sbr" \
	$(INTDIR)/"tif_dirread.sbr" \
	$(INTDIR)/"tif_getimage.sbr" \
	$(INTDIR)/"tif_fax3.sbr" \
	$(INTDIR)/"tif_version.sbr" \
	$(INTDIR)/"tif_codec.sbr" \
	$(INTDIR)/"tif_dir.sbr" \
	$(INTDIR)/"tif_predict.sbr" \
	$(INTDIR)/"tif_close.sbr" \
	$(INTDIR)/"tif_dumpmode.sbr" \
	$(INTDIR)/"tif_aux.sbr" \
	$(INTDIR)/"tif_error.sbr" \
	$(INTDIR)/"tif_lzw.sbr" \
	$(INTDIR)/"tif_zip.sbr" \
	$(INTDIR)/"tif_read.sbr" \
	$(INTDIR)/"tif_packbits.sbr" \
	$(INTDIR)/"tif_swab.sbr" \
	$(INTDIR)/"tif_dirwrite.sbr" \
	$(INTDIR)/"tif_open.sbr" \
	$(INTDIR)/"tif_warning.sbr" \
	$(INTDIR)/"tif_tile.sbr" \
	$(INTDIR)/"tif_strip.sbr" \
	$(INTDIR)/"tif_next.sbr" \
	$(INTDIR)/"tif_write.sbr" \
	$(INTDIR)/"fax3sm.sbr"

$(OUTDIR)/"libtiff.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO /OUT:"WinDebug\dlibtiff.lib"
LIB32_FLAGS=/NOLOGO /OUT:"WinDebug\dlibtiff.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/"tif_jpeg.obj" \
	$(INTDIR)/"tif_dirinfo.obj" \
	$(INTDIR)/"tif_win32.obj" \
	$(INTDIR)/"tif_flush.obj" \
	$(INTDIR)/"tif_thunder.obj" \
	$(INTDIR)/"tif_compress.obj" \
	$(INTDIR)/"tif_print.obj" \
	$(INTDIR)/"tif_dirread.obj" \
	$(INTDIR)/"tif_getimage.obj" \
	$(INTDIR)/"tif_fax3.obj" \
	$(INTDIR)/"tif_version.obj" \
	$(INTDIR)/"tif_codec.obj" \
	$(INTDIR)/"tif_dir.obj" \
	$(INTDIR)/"tif_predict.obj" \
	$(INTDIR)/"tif_close.obj" \
	$(INTDIR)/"tif_dumpmode.obj" \
	$(INTDIR)/"tif_aux.obj" \
	$(INTDIR)/"tif_error.obj" \
	$(INTDIR)/"tif_lzw.obj" \
	$(INTDIR)/"tif_zip.obj" \
	$(INTDIR)/"tif_read.obj" \
	$(INTDIR)/"tif_packbits.obj" \
	$(INTDIR)/"tif_swab.obj" \
	$(INTDIR)/"tif_dirwrite.obj" \
	$(INTDIR)/"tif_open.obj" \
	$(INTDIR)/"tif_warning.obj" \
	$(INTDIR)/"tif_tile.obj" \
	$(INTDIR)/"tif_strip.obj" \
	$(INTDIR)/"tif_next.obj" \
	$(INTDIR)/"tif_write.obj" \
	$(INTDIR)/"fax3sm.obj"

$(OUTDIR)/"dlibtiff.lib" : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Macintosh Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "MacRel"
# PROP BASE Intermediate_Dir "MacRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "MacRel"
# PROP Intermediate_Dir "MacRel"
OUTDIR=.\MacRel
INTDIR=.\MacRel

ALL : $(OUTDIR)/""libtiff.lib"" $(OUTDIR)/""libtiff.bsc""

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

CPP=cl.exe
# ADD BASE CPP /nologo /AL /W3 /GX /YX /O2 /D "_WINDOWS" /D "_MAC" /D "_68K_" /D "NDEBUG" /FR /c
# ADD CPP /nologo /AL /W3 /GX /YX /O2 /D "_WINDOWS" /D "_MAC" /D "_68K_" /D "NDEBUG" /FR /c
CPP_PROJ=/nologo /AL /W3 /GX /YX /O2 /D "_WINDOWS" /D "_MAC" /D "_68K_" /D\
 "NDEBUG" /FR$(INTDIR)/ /Fp$(OUTDIR)/"libtiff.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\MacRel/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libtiff.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"tif_jpeg.sbr" \
	$(INTDIR)/"tif_dirinfo.sbr" \
	$(INTDIR)/"tif_win32.sbr" \
	$(INTDIR)/"tif_flush.sbr" \
	$(INTDIR)/"tif_thunder.sbr" \
	$(INTDIR)/"tif_compress.sbr" \
	$(INTDIR)/"tif_print.sbr" \
	$(INTDIR)/"tif_dirread.sbr" \
	$(INTDIR)/"tif_getimage.sbr" \
	$(INTDIR)/"tif_fax3.sbr" \
	$(INTDIR)/"tif_version.sbr" \
	$(INTDIR)/"tif_codec.sbr" \
	$(INTDIR)/"tif_dir.sbr" \
	$(INTDIR)/"tif_predict.sbr" \
	$(INTDIR)/"tif_close.sbr" \
	$(INTDIR)/"tif_dumpmode.sbr" \
	$(INTDIR)/"tif_aux.sbr" \
	$(INTDIR)/"tif_error.sbr" \
	$(INTDIR)/"tif_lzw.sbr" \
	$(INTDIR)/"tif_zip.sbr" \
	$(INTDIR)/"tif_read.sbr" \
	$(INTDIR)/"tif_packbits.sbr" \
	$(INTDIR)/"tif_swab.sbr" \
	$(INTDIR)/"tif_dirwrite.sbr" \
	$(INTDIR)/"tif_open.sbr" \
	$(INTDIR)/"tif_warning.sbr" \
	$(INTDIR)/"tif_tile.sbr" \
	$(INTDIR)/"tif_strip.sbr" \
	$(INTDIR)/"tif_next.sbr" \
	$(INTDIR)/"tif_write.sbr" \
	$(INTDIR)/"fax3sm.sbr"

$(OUTDIR)/"libtiff.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"libtiff.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/"tif_jpeg.obj" \
	$(INTDIR)/"tif_dirinfo.obj" \
	$(INTDIR)/"tif_win32.obj" \
	$(INTDIR)/"tif_flush.obj" \
	$(INTDIR)/"tif_thunder.obj" \
	$(INTDIR)/"tif_compress.obj" \
	$(INTDIR)/"tif_print.obj" \
	$(INTDIR)/"tif_dirread.obj" \
	$(INTDIR)/"tif_getimage.obj" \
	$(INTDIR)/"tif_fax3.obj" \
	$(INTDIR)/"tif_version.obj" \
	$(INTDIR)/"tif_codec.obj" \
	$(INTDIR)/"tif_dir.obj" \
	$(INTDIR)/"tif_predict.obj" \
	$(INTDIR)/"tif_close.obj" \
	$(INTDIR)/"tif_dumpmode.obj" \
	$(INTDIR)/"tif_aux.obj" \
	$(INTDIR)/"tif_error.obj" \
	$(INTDIR)/"tif_lzw.obj" \
	$(INTDIR)/"tif_zip.obj" \
	$(INTDIR)/"tif_read.obj" \
	$(INTDIR)/"tif_packbits.obj" \
	$(INTDIR)/"tif_swab.obj" \
	$(INTDIR)/"tif_dirwrite.obj" \
	$(INTDIR)/"tif_open.obj" \
	$(INTDIR)/"tif_warning.obj" \
	$(INTDIR)/"tif_tile.obj" \
	$(INTDIR)/"tif_strip.obj" \
	$(INTDIR)/"tif_next.obj" \
	$(INTDIR)/"tif_write.obj" \
	$(INTDIR)/"fax3sm.obj"

$(OUTDIR)/"libtiff.lib" : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Macintosh Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "MacDebug"
# PROP BASE Intermediate_Dir "MacDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "MacDebug"
# PROP Intermediate_Dir "MacDebug"
OUTDIR=.\MacDebug
INTDIR=.\MacDebug

ALL : $(OUTDIR)/""libtiff.lib"" $(OUTDIR)/""libtiff.bsc""

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

CPP=cl.exe
# ADD BASE CPP /nologo /AL /Q68m /W3 /GX /Z7 /YX /Od /D "_WINDOWS" /D "_MAC" /D "_68K_" /D "_DEBUG" /FR /c
# ADD CPP /nologo /AL /Q68m /W3 /GX /Z7 /YX /Od /D "_WINDOWS" /D "_MAC" /D "_68K_" /D "_DEBUG" /FR /c
CPP_PROJ=/nologo /AL /Q68m /W3 /GX /Z7 /YX /Od /D "_WINDOWS" /D "_MAC" /D\
 "_68K_" /D "_DEBUG" /FR$(INTDIR)/ /Fp$(OUTDIR)/"libtiff.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\MacDebug/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libtiff.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"tif_jpeg.sbr" \
	$(INTDIR)/"tif_dirinfo.sbr" \
	$(INTDIR)/"tif_win32.sbr" \
	$(INTDIR)/"tif_flush.sbr" \
	$(INTDIR)/"tif_thunder.sbr" \
	$(INTDIR)/"tif_compress.sbr" \
	$(INTDIR)/"tif_print.sbr" \
	$(INTDIR)/"tif_dirread.sbr" \
	$(INTDIR)/"tif_getimage.sbr" \
	$(INTDIR)/"tif_fax3.sbr" \
	$(INTDIR)/"tif_version.sbr" \
	$(INTDIR)/"tif_codec.sbr" \
	$(INTDIR)/"tif_dir.sbr" \
	$(INTDIR)/"tif_predict.sbr" \
	$(INTDIR)/"tif_close.sbr" \
	$(INTDIR)/"tif_dumpmode.sbr" \
	$(INTDIR)/"tif_aux.sbr" \
	$(INTDIR)/"tif_error.sbr" \
	$(INTDIR)/"tif_lzw.sbr" \
	$(INTDIR)/"tif_zip.sbr" \
	$(INTDIR)/"tif_read.sbr" \
	$(INTDIR)/"tif_packbits.sbr" \
	$(INTDIR)/"tif_swab.sbr" \
	$(INTDIR)/"tif_dirwrite.sbr" \
	$(INTDIR)/"tif_open.sbr" \
	$(INTDIR)/"tif_warning.sbr" \
	$(INTDIR)/"tif_tile.sbr" \
	$(INTDIR)/"tif_strip.sbr" \
	$(INTDIR)/"tif_next.sbr" \
	$(INTDIR)/"tif_write.sbr" \
	$(INTDIR)/"fax3sm.sbr"

$(OUTDIR)/"libtiff.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"libtiff.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/"tif_jpeg.obj" \
	$(INTDIR)/"tif_dirinfo.obj" \
	$(INTDIR)/"tif_win32.obj" \
	$(INTDIR)/"tif_flush.obj" \
	$(INTDIR)/"tif_thunder.obj" \
	$(INTDIR)/"tif_compress.obj" \
	$(INTDIR)/"tif_print.obj" \
	$(INTDIR)/"tif_dirread.obj" \
	$(INTDIR)/"tif_getimage.obj" \
	$(INTDIR)/"tif_fax3.obj" \
	$(INTDIR)/"tif_version.obj" \
	$(INTDIR)/"tif_codec.obj" \
	$(INTDIR)/"tif_dir.obj" \
	$(INTDIR)/"tif_predict.obj" \
	$(INTDIR)/"tif_close.obj" \
	$(INTDIR)/"tif_dumpmode.obj" \
	$(INTDIR)/"tif_aux.obj" \
	$(INTDIR)/"tif_error.obj" \
	$(INTDIR)/"tif_lzw.obj" \
	$(INTDIR)/"tif_zip.obj" \
	$(INTDIR)/"tif_read.obj" \
	$(INTDIR)/"tif_packbits.obj" \
	$(INTDIR)/"tif_swab.obj" \
	$(INTDIR)/"tif_dirwrite.obj" \
	$(INTDIR)/"tif_open.obj" \
	$(INTDIR)/"tif_warning.obj" \
	$(INTDIR)/"tif_tile.obj" \
	$(INTDIR)/"tif_strip.obj" \
	$(INTDIR)/"tif_next.obj" \
	$(INTDIR)/"tif_write.obj" \
	$(INTDIR)/"fax3sm.obj"

$(OUTDIR)/"libtiff.lib" : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "APXrel"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "APXrel"
# PROP BASE Intermediate_Dir "APXrel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "APXrel"
# PROP Intermediate_Dir "APXrel"
OUTDIR=.\APXrel
INTDIR=.\APXrel

ALL : $(OUTDIR)/"libtiff.lib" $(OUTDIR)/"libtiff.bsc"

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

CPP=cl.exe
# ADD BASE CPP /nologo /ML /Gt0 /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /Gt0 /W3 /GX /YX /O2 /I ".." /I "." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
CPP_PROJ=/nologo /MT /Gt0 /W3 /GX /YX /O2 /I ".." /I "." /D "WIN32" /D "NDEBUG"\
 /D "_WINDOWS" /FR$(INTDIR)/ /Fp$(OUTDIR)/"libtiff.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\APXrel/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libtiff.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"tif_jpeg.sbr" \
	$(INTDIR)/"tif_dirinfo.sbr" \
	$(INTDIR)/"tif_win32.sbr" \
	$(INTDIR)/"tif_flush.sbr" \
	$(INTDIR)/"tif_thunder.sbr" \
	$(INTDIR)/"tif_compress.sbr" \
	$(INTDIR)/"tif_print.sbr" \
	$(INTDIR)/"tif_dirread.sbr" \
	$(INTDIR)/"tif_getimage.sbr" \
	$(INTDIR)/"tif_fax3.sbr" \
	$(INTDIR)/"tif_version.sbr" \
	$(INTDIR)/"tif_codec.sbr" \
	$(INTDIR)/"tif_dir.sbr" \
	$(INTDIR)/"tif_predict.sbr" \
	$(INTDIR)/"tif_close.sbr" \
	$(INTDIR)/"tif_dumpmode.sbr" \
	$(INTDIR)/"tif_aux.sbr" \
	$(INTDIR)/"tif_error.sbr" \
	$(INTDIR)/"tif_lzw.sbr" \
	$(INTDIR)/"tif_zip.sbr" \
	$(INTDIR)/"tif_read.sbr" \
	$(INTDIR)/"tif_packbits.sbr" \
	$(INTDIR)/"tif_swab.sbr" \
	$(INTDIR)/"tif_dirwrite.sbr" \
	$(INTDIR)/"tif_open.sbr" \
	$(INTDIR)/"tif_warning.sbr" \
	$(INTDIR)/"tif_tile.sbr" \
	$(INTDIR)/"tif_strip.sbr" \
	$(INTDIR)/"tif_next.sbr" \
	$(INTDIR)/"tif_write.sbr" \
	$(INTDIR)/"fax3sm.sbr"

$(OUTDIR)/"libtiff.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO
LIB32_FLAGS=/NOLOGO /OUT:$(OUTDIR)\"libtiff.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/"tif_jpeg.obj" \
	$(INTDIR)/"tif_dirinfo.obj" \
	$(INTDIR)/"tif_win32.obj" \
	$(INTDIR)/"tif_flush.obj" \
	$(INTDIR)/"tif_thunder.obj" \
	$(INTDIR)/"tif_compress.obj" \
	$(INTDIR)/"tif_print.obj" \
	$(INTDIR)/"tif_dirread.obj" \
	$(INTDIR)/"tif_getimage.obj" \
	$(INTDIR)/"tif_fax3.obj" \
	$(INTDIR)/"tif_version.obj" \
	$(INTDIR)/"tif_codec.obj" \
	$(INTDIR)/"tif_dir.obj" \
	$(INTDIR)/"tif_predict.obj" \
	$(INTDIR)/"tif_close.obj" \
	$(INTDIR)/"tif_dumpmode.obj" \
	$(INTDIR)/"tif_aux.obj" \
	$(INTDIR)/"tif_error.obj" \
	$(INTDIR)/"tif_lzw.obj" \
	$(INTDIR)/"tif_zip.obj" \
	$(INTDIR)/"tif_read.obj" \
	$(INTDIR)/"tif_packbits.obj" \
	$(INTDIR)/"tif_swab.obj" \
	$(INTDIR)/"tif_dirwrite.obj" \
	$(INTDIR)/"tif_open.obj" \
	$(INTDIR)/"tif_warning.obj" \
	$(INTDIR)/"tif_tile.obj" \
	$(INTDIR)/"tif_strip.obj" \
	$(INTDIR)/"tif_next.obj" \
	$(INTDIR)/"tif_write.obj" \
	$(INTDIR)/"fax3sm.obj"

$(OUTDIR)/"libtiff.lib" : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "APXdeb"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "APXdeb"
# PROP BASE Intermediate_Dir "APXdeb"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "APXdeb"
# PROP Intermediate_Dir "APXdeb"
OUTDIR=.\APXdeb
INTDIR=.\APXdeb

ALL : $(OUTDIR)/"dlibtiff.lib" $(OUTDIR)/"libtiff.bsc"

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

CPP=cl.exe
# ADD BASE CPP /nologo /ML /Gt0 /W3 /GX /Z7 /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /Gt0 /W3 /GX /Z7 /YX /Od /I ".." /I "." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
CPP_PROJ=/nologo /MT /Gt0 /W3 /GX /Z7 /YX /Od /I ".." /I "." /D "WIN32" /D\
 "_DEBUG" /D "_WINDOWS" /FR$(INTDIR)/ /Fp$(OUTDIR)/"libtiff.pch" /Fo$(INTDIR)/\
 /c 
CPP_OBJS=.\APXdeb/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libtiff.bsc" 
BSC32_SBRS= \
	$(INTDIR)/"tif_jpeg.sbr" \
	$(INTDIR)/"tif_dirinfo.sbr" \
	$(INTDIR)/"tif_win32.sbr" \
	$(INTDIR)/"tif_flush.sbr" \
	$(INTDIR)/"tif_thunder.sbr" \
	$(INTDIR)/"tif_compress.sbr" \
	$(INTDIR)/"tif_print.sbr" \
	$(INTDIR)/"tif_dirread.sbr" \
	$(INTDIR)/"tif_getimage.sbr" \
	$(INTDIR)/"tif_fax3.sbr" \
	$(INTDIR)/"tif_version.sbr" \
	$(INTDIR)/"tif_codec.sbr" \
	$(INTDIR)/"tif_dir.sbr" \
	$(INTDIR)/"tif_predict.sbr" \
	$(INTDIR)/"tif_close.sbr" \
	$(INTDIR)/"tif_dumpmode.sbr" \
	$(INTDIR)/"tif_aux.sbr" \
	$(INTDIR)/"tif_error.sbr" \
	$(INTDIR)/"tif_lzw.sbr" \
	$(INTDIR)/"tif_zip.sbr" \
	$(INTDIR)/"tif_read.sbr" \
	$(INTDIR)/"tif_packbits.sbr" \
	$(INTDIR)/"tif_swab.sbr" \
	$(INTDIR)/"tif_dirwrite.sbr" \
	$(INTDIR)/"tif_open.sbr" \
	$(INTDIR)/"tif_warning.sbr" \
	$(INTDIR)/"tif_tile.sbr" \
	$(INTDIR)/"tif_strip.sbr" \
	$(INTDIR)/"tif_next.sbr" \
	$(INTDIR)/"tif_write.sbr" \
	$(INTDIR)/"fax3sm.sbr"

$(OUTDIR)/"libtiff.bsc" : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LIB32=lib.exe
# ADD BASE LIB32 /NOLOGO
# ADD LIB32 /NOLOGO /OUT:"APXdeb\dlibtiff.lib"
LIB32_FLAGS=/NOLOGO /OUT:"APXdeb\dlibtiff.lib" 
DEF_FLAGS=
DEF_FILE=
LIB32_OBJS= \
	$(INTDIR)/"tif_jpeg.obj" \
	$(INTDIR)/"tif_dirinfo.obj" \
	$(INTDIR)/"tif_win32.obj" \
	$(INTDIR)/"tif_flush.obj" \
	$(INTDIR)/"tif_thunder.obj" \
	$(INTDIR)/"tif_compress.obj" \
	$(INTDIR)/"tif_print.obj" \
	$(INTDIR)/"tif_dirread.obj" \
	$(INTDIR)/"tif_getimage.obj" \
	$(INTDIR)/"tif_fax3.obj" \
	$(INTDIR)/"tif_version.obj" \
	$(INTDIR)/"tif_codec.obj" \
	$(INTDIR)/"tif_dir.obj" \
	$(INTDIR)/"tif_predict.obj" \
	$(INTDIR)/"tif_close.obj" \
	$(INTDIR)/"tif_dumpmode.obj" \
	$(INTDIR)/"tif_aux.obj" \
	$(INTDIR)/"tif_error.obj" \
	$(INTDIR)/"tif_lzw.obj" \
	$(INTDIR)/"tif_zip.obj" \
	$(INTDIR)/"tif_read.obj" \
	$(INTDIR)/"tif_packbits.obj" \
	$(INTDIR)/"tif_swab.obj" \
	$(INTDIR)/"tif_dirwrite.obj" \
	$(INTDIR)/"tif_open.obj" \
	$(INTDIR)/"tif_warning.obj" \
	$(INTDIR)/"tif_tile.obj" \
	$(INTDIR)/"tif_strip.obj" \
	$(INTDIR)/"tif_next.obj" \
	$(INTDIR)/"tif_write.obj" \
	$(INTDIR)/"fax3sm.obj"

$(OUTDIR)/"dlibtiff.lib" : $(OUTDIR)  $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_jpeg.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_jpeg.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_jpeg.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_jpeg.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_jpeg.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_jpeg.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_jpeg.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_dirinfo.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_dirinfo.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_dirinfo.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_dirinfo.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_dirinfo.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_dirinfo.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_dirinfo.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_win32.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_win32.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_win32.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_win32.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_win32.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_win32.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_win32.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_flush.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_flush.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_flush.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_flush.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_flush.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_flush.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_flush.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_thunder.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_thunder.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_thunder.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_thunder.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_thunder.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_thunder.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_thunder.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_compress.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_compress.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_compress.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_compress.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_compress.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_compress.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_compress.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_print.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_print.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_dirread.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_dirread.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_dirread.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_dirread.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_dirread.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_dirread.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_dirread.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_getimage.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_getimage.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_getimage.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_getimage.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_getimage.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_getimage.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_getimage.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_fax3.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_fax3.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_fax3.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_fax3.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_fax3.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_fax3.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_fax3.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_version.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_version.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_codec.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_codec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_codec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_codec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_codec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_codec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_codec.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_dir.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_dir.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_dir.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_dir.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_dir.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_dir.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_dir.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_predict.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_predict.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_predict.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_predict.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_predict.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_predict.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_predict.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_close.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_close.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_close.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_close.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_close.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_close.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_close.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_dumpmode.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_dumpmode.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_dumpmode.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_dumpmode.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_dumpmode.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_dumpmode.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_dumpmode.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_aux.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_aux.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_aux.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_aux.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_aux.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_aux.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_aux.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_error.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_error.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_lzw.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_lzw.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_lzw.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_lzw.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_lzw.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_lzw.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_lzw.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_zip.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_zip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_zip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_zip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_zip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_zip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_zip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_read.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_read.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_read.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_read.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_read.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_read.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_read.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_packbits.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_packbits.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_packbits.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_packbits.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_packbits.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_packbits.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_packbits.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_swab.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_swab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_swab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_swab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_swab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_swab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_swab.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_dirwrite.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_dirwrite.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_dirwrite.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_dirwrite.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_dirwrite.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_dirwrite.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_dirwrite.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_open.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_open.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_open.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_open.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_open.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_open.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_open.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_warning.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_warning.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_warning.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_warning.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_warning.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_warning.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_warning.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\libtiff.def
# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_tile.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_tile.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_tile.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_tile.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_tile.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_tile.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_tile.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_strip.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_strip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_strip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_strip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_strip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_strip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_strip.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_next.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_next.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_next.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_next.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_next.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_next.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_next.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\tiff\tiff-v3.4beta024\libtiff\tif_write.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"tif_write.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"tif_write.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"tif_write.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"tif_write.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"tif_write.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"tif_write.obj" :  $(SOURCE)  $(INTDIR)
   $(CPP) $(CPP_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\fax3sm.c
DEP_FAX3S=\
	"\tiff\tiff-v3.4beta024\libtiff\tiff.h"\
	"\tiff\tiff-v3.4beta024\libtiff\tif_fax3.h"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"fax3sm.obj" :  $(SOURCE)  $(DEP_FAX3S) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"fax3sm.obj" :  $(SOURCE)  $(DEP_FAX3S) $(INTDIR)

!ELSEIF  "$(CFG)" == "Macintosh Release"

$(INTDIR)/"fax3sm.obj" :  $(SOURCE)  $(DEP_FAX3S) $(INTDIR)

!ELSEIF  "$(CFG)" == "Macintosh Debug"

$(INTDIR)/"fax3sm.obj" :  $(SOURCE)  $(DEP_FAX3S) $(INTDIR)

!ELSEIF  "$(CFG)" == "APXrel"

$(INTDIR)/"fax3sm.obj" :  $(SOURCE)  $(DEP_FAX3S) $(INTDIR)

!ELSEIF  "$(CFG)" == "APXdeb"

$(INTDIR)/"fax3sm.obj" :  $(SOURCE)  $(DEP_FAX3S) $(INTDIR)

!ENDIF 

# End Source File
# End Group
# End Project
################################################################################
