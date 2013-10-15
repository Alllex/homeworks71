@echo off

if %builder_start%=="" goto :EOF

echo Sending email with building result...

if not exist %email_attached_files% (
    blat -body " " -tf %email_list% -server %email_server% -f %email_from% -subject "%email_subject% [%email_extra_subject%]" >%email_log%
) else (
    blat -body " " -tf %email_list% -server %email_server% -f %email_from% -subject "%email_subject% [%email_extra_subject%]" -atf %email_attached_files% >%email_log%
)

echo Email(s) has been sent


