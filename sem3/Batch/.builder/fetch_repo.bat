@echo off

if %builder_start%=="" goto :EOF

echo Cloning repository...

git clone --branch master %git_repo% >nul 2>%git_err%

if errorlevel 1 goto :fail

:succ

echo Repository has been cloned
goto :EOF

:fail

set git_fails=True
echo Repository clonning failed................FAIL
