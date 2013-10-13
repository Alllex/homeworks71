@echo off

if %builder_start%=="" goto :EOF

echo Removing temp files...

if exist %git_err% del %git_err%
if exist %msb_log% del %msb_log%
if exist %msg_file% del %msg_file%
if exist %email_log% del %email_log%