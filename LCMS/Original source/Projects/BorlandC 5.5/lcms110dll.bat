@echo off
echo.
echo This will build the littlecms DLL using Borland C 5.5 compiler.
echo.
echo Press Ctrl-C to abort, or
pause
bcc32 @lcms110dll.lst
if errorlevel 0 tlink32 @lcms110dll.cmd
if errorlevel 0 brc32 -fe..\..\bin\lcms.dll lcms.rc
del *.obj
del *.res
echo Done!
								
					
