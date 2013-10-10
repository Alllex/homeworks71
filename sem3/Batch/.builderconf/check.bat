@echo off

if %builder_start%=="" goto :EOF

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
echo %succ_build_msg% >%msg_file%
goto :EOF

:fail

echo Building failed...............................FAIL
echo %fail_build_head% >%msg_file%
echo File %file_missed% is missed after building. >>%msg_file%
