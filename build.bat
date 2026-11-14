@echo off
rem Builds HeaderShots.exe. Pass the Virtual TreeView Source folder as first parameter.
set VT_SOURCE=%~1
if "%VT_SOURCE%"=="" set VT_SOURCE=..\Virtual-TreeView\Source
call "%BDS%\bin\rsvars.bat" 2>nul
if "%BDS%"=="" call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
cd /d "%~dp0"
brcc32 HeaderShotsManifest.rc
dcc32 -B -Q -NSSystem;Vcl;Winapi;System.Win;Vcl.Imaging -U"%VT_SOURCE%" -R"%VT_SOURCE%" -NUdcu -E. HeaderShots.dpr
