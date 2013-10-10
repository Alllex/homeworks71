@echo off

if %builder_start%=="" goto :EOF

echo Git failed.............................FAIL

echo %git_fail_head% >%msg_file%
echo. >>%msg_file%
echo -----%git_fail_head2%----- >>%msg_file%
type %git_err% >>%msg_file%