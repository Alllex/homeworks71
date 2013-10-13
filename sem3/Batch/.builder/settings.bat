@echo off

if %builder_start%=="" goto :EOF

set repo_name=GeometryVis
set git_repo=https://github.com/Alllex/%repo_name%.git
set "git_fail_head=Repository clonning failed"
set "git_fail_head2=Git error logs"
set git_err=git.err

set msb=%WINDIR%\Microsoft.NET\Framework\v4.0.30319
set sln_name=GeometryVis
set sln_file=%repo_name%\%sln_name%.sln
set msb_log=msbuild.log
set "succ_build_msg=Success build of solution: "%sln_name%"!"
set "fail_build_head=Sorry, building of "%sln_name%" solution has failed..."
set "fail_build_head2=MSBuild logs"
set build_folder=%repo_name%\GeometryVis\bin\Debug
set binaries=%conf%\binaries.txt

set msg_file=build_msg.txt
set email_to=alllex.semin@gmail.com
set email_from=s63alex@mail.ru
set email_server=smtp.mail.ru
set email_subject="Auto notifying: Solution \"%sln_name%\" building result"
set email_log=email_sending.log
set email_list=%conf%\email_list.txt
