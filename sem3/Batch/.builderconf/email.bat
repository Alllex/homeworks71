@echo off

if %builder_start%=="" goto :EOF

echo Sending email with building result...

blat %msg_file% -tf %email_list% -server %email_server% -f %email_from% -subject %email_subject% >%email_log% 

echo Email(s) has been sent


