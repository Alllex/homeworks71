@echo off

if "%builder_start%"=="" goto :EOF

echo Removing temp files...

if exist %git_errors% del %git_errors%
if exist %msb_log% del %msb_log%
if exist %email_log% del %email_log%
if exist %my_log% del %my_log%
if exist %email_attached_files% del %email_attached_files%