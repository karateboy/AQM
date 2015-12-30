@ECHO OFF

:: 待列印的 PDF 資料夾
SET NOTIFICATION="notification"
SET EXE="printExcel"
:: 取得 Excel 檔案名稱，執行列印、搬移檔案 (自動覆蓋舊檔)
FOR %%f IN (%NOTIFICATION%\*.xlsx) DO ECHO %%f && %EXE% %~dp0\%%f && Del /S %%f

ECHO 列印完畢