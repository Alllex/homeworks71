@echo off

if "%builder_start%"=="" goto :EOF

echo MSBuild failed..............................FAIL

if exist %email_attached_files% echo ,>%email_attached_files% 
echo %msb_log%>%email_attached_files%

set email_extra_subject=%msb_fail_result%