@echo off

if %builder_start%=="" goto :EOF

echo Building solution...

%msb%\MSBuild.exe %sln_file% >%msb_log%
if errorlevel 1 set msb_fails=True
if not "%msb_fails%"=="" goto :fail

:succ

echo Solution has been built!
goto :EOF

:fail

echo Error occured during the building.........FAIL

