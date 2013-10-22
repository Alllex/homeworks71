@echo off

if "%builder_start%"=="" goto :EOF

echo Checking for correct building...

for /F "tokens=*" %%f in (%binaries%) do (
	if not exist "%build_folder%\%%f" (
		set file_missed="%%f"
		goto :fail
	)
)

goto :succ

:succ

echo Success build!
set email_extra_subject=%msb_succ_result%
goto :EOF

:fail

echo Building failed...............................FAIL
set email_extra_subject=%msb_fail_result%
echo File %file_missed% is missed after building.>%my_log%
if exist %email_attached_files% echo ,>%email_attached_files% 
echo %my_log%>>%email_attached_files%
