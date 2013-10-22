@echo off

if "%builder_start%"=="" goto :EOF

set repo_name=GeometryVis
set git_repo_url=https://github.com/Alllex/%repo_name%.git
set git_fail_result=GIT CLONE FAILED
set git_errors=git_errors.log
set git_failed=false

set msb=%WINDIR%\Microsoft.NET\Framework\v4.0.30319
set sln_name=GeometryVis
set sln_file=%repo_name%\%sln_name%.sln

set build_folder=%repo_name%\GeometryVis\bin\Debug
set binaries=%conf%\binaries.txt
set my_log=my_log.log

set msb_log=msbuild.log
set msb_succ_result=SUCCESS BUILD
set msb_fail_result=BUILDING FAILED
set msb_failed=false

set email_from=s63alex@mail.ru
set email_server=smtp.mail.ru
set email_subject=Auto-building solution: \"%sln_name%\";

set email_log=email_sending.log
set email_list=%conf%\email_list.txt
set email_attached_files=attached_files.txt
