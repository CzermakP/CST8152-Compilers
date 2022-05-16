:: -----------------------------------------------------------------------
:: SCRIPT A3 - CST8152 - W21
:: -----------------------------------------------------------------------
::
:: COMPILERS COURSE - README ---------------------------------------------
::
:: CONTAINS THE CONSTANTS YOU *NEED* TO CHANGE ACCORDING TO YOUR PROJECT
::
:: PRJPATH: PATH OF YOUR PROJECT
:: FILPATH: WHERE ARE THE FILES TO COPY TO PROJECT FOLDER
:: DEBPATH: DEBUG FOLDER - (MOST OF TIME %PRJPATH%\Debug)
:: INPPATH: WHERE ARE THE INPUTS (.pls) FILES (TIP: PUT AT DEBUG FOLDER)
:: OUTPATH: WHERE ARE THE OUTPUTS (TIP: SAME DEBUG FOLDER)
:: PRJNAME: NAME OF THE BINARY FILE CREATED BY THE PROJECT
:: BINNAME: NAME OF THE DEFAULT BINARY NAME (EX: "parser.exe")
:: CMPNAME: NAME OF THE OS TOOL USED TO COMPARE FILES (Windows: "fc.exe")
:: OUTNAME: NAME OF THE OUTPUT FILE THAT (Ex: "output.txt")
:: SUFINPU: SUFIX NAME FOR INPUT FILES (OUR ".pls" FILES)
:: SUFOUTP: SUFIX NAME FOR GENERATED OUTPUTS (COMMONLY THE ".seva")
:: SUFOUTS: SUFIX NAME FOR STANDARD OUTPUTS (OUR ".sout" FILES).
::
:: END OF README SESSION

@echo off
set PRJPATH=C:\Users\paulo\source\repos\Compilers
set FILPATH=.
set DEBPATH=%PRJPATH%\Debug
set INPPATH=%DEBPATH%
set OUTPATH=%DEBPATH%
set PRJNAME=Compilers.exe
set BINNAME=parser.exe
set CMPNAME=fc.exe
set OUTNAME=output.txt
set SUFINPU=.pls
set SUFOUTP=.seva
set SUFOUTS=.sout
cls

::
:: A3 SCRIPT - MENU ------------------------------------------------------
::

:menu
echo:
cls
echo ---------------------------------------------------------------------
echo - Main Menu (A3 - W21) ----------------------------------------------
echo ---------------------------------------------------------------------
echo 1: Help - Show configuration
echo 2: Clean environment
echo 3: Execute all
echo 4: Run standard tests
echo 5: Run additional tests
echo 6: Show output
echo 0: Exit
echo:
set /p option="Menu options (select one): " 

if "%option%" == "1" goto con3
if "%option%" == "2" goto cle3
if "%option%" == "3" goto all3
if "%option%" == "4" goto bas3
if "%option%" == "5" goto adv3
if "%option%" == "6" goto out3
if "%option%" == "0" goto exi3

::
:: A3 SCRIPT - SHOW CONF -------------------------------------------------
::

:con3
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo -  A3: SHOW CONFIGURATION                                           -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo MENU OPTIONS:
echo 1: Help (this option): Show details and configuration.
echo 2: Copy files to project: From FILPATH to PRJPATH.
echo 3: Clean environment: Remove outputs, BINNAME, OUTNAME.
echo 4: List files: Show outputs (SUFOUTP), BINNAME, OUTNAME.
echo 5: Execute all: Standard and additional tests.
echo 6: Run standard tests: Required for working program.
echo 7: Run additional tests: Required for complete marks.
echo 8: Show output: Show OUTNAME.
echo 9: Copy files from project: From PRJPATH to FILPATH.
echo 0: Exit.
echo:
echo CURRENT CONFIGURATION:
echo PRJPATH = %PRJPATH%
echo FILPATH = %FILPATH%
echo DEBPATH = %DEBPATH%
echo INPPATH = %INPPATH%
echo OUTPATH = %OUTPATH%
echo PRJNAME = %PRJNAME%
echo BINNAME = %BINNAME%
echo CMPNAME = %CMPNAME%
echo OUTNAME = %OUTNAME%
echo SUFINPU = %SUFINPU%
echo SUFOUTP = %SUFOUTP%
echo SUFOUTS = %SUFOUTS%
echo:
echo NOTE: Change variables to adjust for your configuration.
echo:
pause
goto menu

::
:: A3 SCRIPT - CLEAN FILES (AFTER REBUILD SOLUTION)  ---------------------
::

:cle3
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - A3: PREPARE FOR EXECUTION                                         -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "del *%SUFOUTP%"
	del *%SUFOUTP%
echo "del %BINNAME%"
	del %BINNAME%
echo "copy %PRJNAME% %BINNAME%"
	copy %PRJNAME% %BINNAME%
echo "del %PRJNAME%"
	del %PRJNAME%
echo "del %OUTNAME%"
	del %OUTNAME%
echo:
pause
goto menu

::
:: A3 SCRIPT - SHOW CONTENT OF OUTPUT  -----------------------------------
::

:out3
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - A3: SHOW CONTENT OF OUTPUT FILE                                   -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "type %OUTNAME%"
	type %OUTNAME%
echo:
pause
goto menu

::
:: A3 SCRIPT - EXECUTE ALL TESTS -----------------------------------------
::

:all3
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - A3: EXECUTE ALL TESTS                                             -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "echo > %OUTNAME%"
	echo > %OUTNAME%
echo "echo STANDARD TESTS... >> %OUTNAME%"
	echo STANDARD TESTS... >> %OUTNAME%
echo "%BINNAME% a3e%SUFINPU% > a3e%SUFOUTP%"
	%BINNAME% a3e%SUFINPU% > a3e%SUFOUTP%
echo "%CMPNAME% /B a3e%SUFOUTS% a3e%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3e%SUFOUTS% a3e%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3r1%SUFINPU% > a3r1%SUFOUTP%"
	%BINNAME% a3r1%SUFINPU% > a3r1%SUFOUTP%
echo "%CMPNAME% /B a3r1%SUFOUTS% a3r1%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3r1%SUFOUTS% a3r1%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3r2%SUFINPU% > a3r2%SUFOUTP%"
	%BINNAME% a3r2%SUFINPU% > a3r2%SUFOUTP%
echo "%CMPNAME% /B a3r2%SUFOUTS% a3r2%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3r2%SUFOUTS% a3r2%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3r3%SUFINPU% > a3r3%SUFOUTP%"
	%BINNAME% a3r3%SUFINPU% > a3r3%SUFOUTP%
echo "%CMPNAME% /B a3r3%SUFOUTS% a3r3%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3r3%SUFOUTS% a3r3%SUFOUTP% >> %OUTNAME%
echo "echo ADDITIONAL TESTS... >> %OUTNAME%"
	echo ADDITIONAL TESTS... >> %OUTNAME%	
echo "%BINNAME% a3w1%SUFINPU% > a3w1%SUFOUTP%"
	%BINNAME% a3w1%SUFINPU% > a3w1%SUFOUTP%
echo "%CMPNAME% /B a3w1%SUFOUTS% a3w1%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w1%SUFOUTS% a3w1%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w2%SUFINPU% > a3w2%SUFOUTP%"
	%BINNAME% a3w2%SUFINPU% > a3w2%SUFOUTP%
echo "%CMPNAME% /B a3w2%SUFOUTS% a3w2%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w2%SUFOUTS% a3w2%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w3%SUFINPU% > a3w3%SUFOUTP%"
	%BINNAME% a3w3%SUFINPU% > a3w3%SUFOUTP%
echo "%CMPNAME% /B a3w3%SUFOUTS% a3w3%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w3%SUFOUTS% a3w3%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w4%SUFINPU% > a3w4%SUFOUTP%"
	%BINNAME% a3w4%SUFINPU% > a3w4%SUFOUTP%
echo "%CMPNAME% /B a3w4%SUFOUTS% a3w4%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w4%SUFOUTS% a3w4%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w5%SUFINPU% > a3w5%SUFOUTP%"
	%BINNAME% a3w5%SUFINPU% > a3w5%SUFOUTP%
echo "%CMPNAME% /B a3w5%SUFOUTS% a3w5%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w5%SUFOUTS% a3w5%SUFOUTP% >> %OUTNAME%
echo:
pause
goto menu

::
:: A3 SCRIPT - EXECUTE ONLY STANDARD TESTS  ------------------------------
::

:bas3
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - A3: EXECUTE STANDARD TESTS                                        -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "echo > %OUTNAME%"
	echo > %OUTNAME%
echo "echo STANDARD TESTS... >> %OUTNAME%"
	echo STANDARD TESTS... >> %OUTNAME%
echo "%BINNAME% a3e%SUFINPU% > a3e%SUFOUTP%"
	%BINNAME% a3e%SUFINPU% > a3e%SUFOUTP%
echo "%CMPNAME% /B a3e%SUFOUTS% a3e%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3e%SUFOUTS% a3e%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3r1%SUFINPU% > a3r1%SUFOUTP%"
	%BINNAME% a3r1%SUFINPU% > a3r1%SUFOUTP%
echo "%CMPNAME% /B a3r1%SUFOUTS% a3r1%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3r1%SUFOUTS% a3r1%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3r2%SUFINPU% > a3r2%SUFOUTP%"
	%BINNAME% a3r2%SUFINPU% > a3r2%SUFOUTP%
echo "%CMPNAME% /B a3r2%SUFOUTS% a3r2%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3r2%SUFOUTS% a3r2%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3r3%SUFINPU% > a3r3%SUFOUTP%"
	%BINNAME% a3r3%SUFINPU% > arw3%SUFOUTP%
echo "%CMPNAME% /B a3r3%SUFOUTS% a3r3%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3r3%SUFOUTS% a3r3%SUFOUTP% >> %OUTNAME%
echo:
pause
goto menu

::
:: A3 SCRIPT - EXECUTE ADDITIONAL TESTS  ---------------------------------
::

:adv3
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - A3: EXECUTE ADDITIONAL TESTS                                      -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "echo > %OUTNAME%"
	echo > %OUTNAME%
echo "echo ADDITIONAL TESTS... >> %OUTNAME%"
	echo ADDITIONAL TESTS... >> %OUTNAME%	
echo "%BINNAME% a3w1%SUFINPU% > a3w1%SUFOUTP%"
	%BINNAME% a3w1%SUFINPU% > a3w1%SUFOUTP%
echo "%CMPNAME% /B a3w1%SUFOUTS% a3w1%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w1%SUFOUTS% a3w1%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w2%SUFINPU% > a3w2%SUFOUTP%"
	%BINNAME% a3w2%SUFINPU% > a3w2%SUFOUTP%
echo "%CMPNAME% /B a3w2%SUFOUTS% a3w2%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w2%SUFOUTS% a3w2%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w3%SUFINPU% > a3w3%SUFOUTP%"
	%BINNAME% a3w3%SUFINPU% > a3w3%SUFOUTP%
echo "%CMPNAME% /B a3w3%SUFOUTS% a3w3%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w3%SUFOUTS% a3w3%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w4%SUFINPU% > a3w4%SUFOUTP%"
	%BINNAME% a3w4%SUFINPU% > a3w4%SUFOUTP%
echo "%CMPNAME% /B a3w4%SUFOUTS% a3w4%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w4%SUFOUTS% a3w4%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a3w5%SUFINPU% > a3w5%SUFOUTP%"
	%BINNAME% a3w5%SUFINPU% > a3w5%SUFOUTP%
echo "%CMPNAME% /B a3w5%SUFOUTS% a3w5%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a3w5%SUFOUTS% a3w5%SUFOUTP% >> %OUTNAME%
echo:
pause
goto menu

::
:: A3 SCRIPT - TERMINATE EXECUTION  --------------------------------------
::

:exi3
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - A3: TERMINATE EXECUTION                                           -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
set PRJPATH = ""
set FILPATH = ""
set DEBPATH = ""
set INPPATH = ""
set OUTPATH = ""
set PRJNAME = ""
set BINNAME = ""
set CMPNAME = ""
set OUTNAME = ""
set SUFOUTP = ""
set SUFOUTS = ""
echo ---------------------------------------------------------------------
echo - Batch End (A3 - W21) ----------------------------------------------
echo ---------------------------------------------------------------------
