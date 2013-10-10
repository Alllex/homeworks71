@echo off

if %builder_start%=="" goto :EOF

echo MSBuild failed..............................FAIL

echo %fail_build_head% >%msg_file%
echo. >>%msg_file%
echo -----%fail_build_head2%----- >>%msg_file%
type %msb_log% >>%msg_file%