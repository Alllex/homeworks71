@echo off

if "%builder_start%"=="" goto :EOF

echo Git failed...............................FAIL

if exist %email_attached_files% echo ,>%email_attached_files% 
echo %git_errors%>>%email_attached_files%

set email_extra_subject=%git_fail_result%