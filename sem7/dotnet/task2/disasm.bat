@ECHO OFF

SET ILDASM="C:\Program Files (x86)\Microsoft SDKs\Windows\v10.0A\bin\NETFX 4.6 Tools\ildasm.exe"
SET CLASS_NAME=SmartCalc
SET SOURSE_NAME=%CLASS_NAME%.il
SET EXEC_NAME=%CLASS_NAME%.exe

%ILDASM% %EXEC_NAME% /text > %SOURSE_NAME%

IF ERRORLEVEL 1 GOTO :FAIL
ECHO Successfully disassembled to %SOURSE_NAME%
GOTO :EOF

:FAIL
ECHO Disassembly has failed...