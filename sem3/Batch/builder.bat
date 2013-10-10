@echo off

set builder_start=TRUE
set conf=.builderconf

call %conf%\settings.bat
call %conf%\cleanup.bat

call %conf%\fetch_repo.bat
if not "%git_fails%"=="" call %conf%\git_failed.bat & goto :end

call %conf%\build_sln.bat
if not "%msb_fails%"=="" call %conf%\msb_failed.bat & goto :end

call %conf%\check.bat

:end

call %conf%\email.bat
call %conf%\rm_temp.bat

echo Done.
pause