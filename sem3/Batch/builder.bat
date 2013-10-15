@echo off

set builder_start=TRUE
set conf=.builder

call %conf%\settings.bat
call %conf%\cleanup.bat

call %conf%\git_clone.bat
if /I "%git_failed%"=="true" call %conf%\git_failed.bat & goto :end

call %conf%\msb_build.bat
if /I "%msb_failed%"=="true" call %conf%\msb_failed.bat & goto :end

call %conf%\check.bat

:end

call %conf%\email.bat
call %conf%\rm_temp.bat

echo Done.
pause