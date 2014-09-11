@ECHO OFF
REM This small batch file is intended to create a source code release file of the VirtualTreeView as ZIP archive
REM It expects the ZIP.EXE from the InfoZip project V3.0 or higher to be in the system's search path
REM Download e.g. from: ftp://ftp.info-zip.org/pub/infozip/win32/
REM 
ZIP -9 -r .\VirtualTreeView.zip  INSTALL.txt Changes.txt Source Design Packages Demos Contributions Help\VirtualTreeview.chm -i *.pas -i *.dpk -i *.bpk -i *.bpg -i *.bdsproj -i -i *.groupproj *.dproj -i *.cbproj -i *.hlp -i *.rc *.res -i *.cfg -i *.dpr -i *.dof -i *.bpr -i *.dfm -i *.hpp -i *.cpp -i *.inc -i *.dcr -i *.chm -i *.png -i *.js -i *.txt -i *.bmp -i *.uni
ECHO Source code zip archive "VirtualTreeView.zip" created. 
ECHO !!!Please ensure that the const TVTVersion is correct!!!
ECHO !!!Please add version number to ZIP file name!!!
ECHO !!!Please create SVN tag!!!
pause