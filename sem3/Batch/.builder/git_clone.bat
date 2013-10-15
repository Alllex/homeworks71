@echo off

if %builder_start%=="" goto :EOF

echo Cloning repository...

git clone --branch master %git_repo_url% >nul 2>%git_errors%

if errorlevel 1 goto :fail

:succ

goto :EOF

:fail

set git_failed=true
