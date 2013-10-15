@echo off

if %builder_start%=="" goto :EOF

echo Building solution...

%msb%\MSBuild.exe %sln_file%>%msb_log%

if errorlevel 1 goto :fail

:succ

echo Solution has been built!
goto :EOF

:fail

set msb_failed=true

