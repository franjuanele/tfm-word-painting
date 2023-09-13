@echo off
::Exporta un MusicXML a pdf con MuseScore
echo ##############################
echo ##### MUSESCORE EXPORTER #####
echo ##############################
echo.
echo.
::Cambiad el path de MuseScore al correspondiente en vuestro ordenador
set musescorepath=D:\MuseScore\bin\MuseScore4.exe
set xmlpath=%~1
set pdfpath=%~2

if "%~3"=="svg" (
	%musescorepath% -T 0 -o %pdfpath% %xmlpath%)

if "%~3"=="" (
	%musescorepath% -o %pdfpath% %xmlpath%)
::ping 127.0.0.1 -n 2 >nul
exit /b 0