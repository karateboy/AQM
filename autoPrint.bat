@ECHO OFF

:: �ݦC�L�� PDF ��Ƨ�
SET NOTIFICATION="notification"
SET EXE="printExcel"
:: ���o Excel �ɮצW�١A����C�L�B�h���ɮ� (�۰��л\����)
FOR %%f IN (%NOTIFICATION%\*.xlsx) DO ECHO %%f && %EXE% %~dp0\%%f && Del /S %%f

ECHO �C�L����