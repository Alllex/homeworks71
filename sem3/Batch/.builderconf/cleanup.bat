@echo off

if %builder_start%=="" goto :EOF

echo Deleting old solution folder...

if not %repo_name%=="" if exist %repo_name% rmdir /q /s %repo_name%

echo Folder has been successfully deleted