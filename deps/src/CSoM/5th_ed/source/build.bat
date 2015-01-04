echo off

rem  ******EDIT THE NEXT TWO LINES IF NECESSARY***********

set ED5=c:\5th_ed
set G95=c:\g95

rem  *****************************************************

set FEL=%ED5%\source\library
set PATH=%G95%\bin

rem  BUILD LIBRARIES**************************************

cd %FEL%\geom
del *.a *.mod
g95 -c *.f03
ar -r geomlib.a *.o
del *.o
cd %FEL%\main
del *.a *.mod
g95 -c *.f03
ar -r mainlib.a *.o
del *.o

rem  *****************************************************

rem  BUILD runs.exe **************************************

cd %ED5%\source
g95 runs.f03 -o runs.exe
copy runs.exe %ED5%\source\chap04
copy runs.exe %ED5%\source\chap05
copy runs.exe %ED5%\source\chap06
copy runs.exe %ED5%\source\chap07
copy runs.exe %ED5%\source\chap08
copy runs.exe %ED5%\source\chap09
copy runs.exe %ED5%\source\chap10
copy runs.exe %ED5%\source\chap11
del runs.exe

rem  *****************************************************

rem  BUILD run5.bat  *************************************

echo echo off>>run5.bat
echo set ED5=%ED5%>>run5.bat
echo set G95=%G95%>>run5.bat
echo set FEL=%%ED5%%\source\library>>run5.bat
echo set PATH=%%G95%%\BIN>>run5.bat
echo g95 %%1.f03 -o %%1.exe %%FEL%%\arpack\arpacklib.a ^^>>run5.bat
echo -I %%FEL%%\main -I %%FEL%%\geom ^^>>run5.bat
echo %%FEL%%\main\mainlib.a %%FEL%%\geom\geomlib.a>>run5.bat
echo %%1 %%2>>run5.bat
echo del %%1.exe>>run5.bat

rem  ******************************************************

copy run5.bat %ED5%\source\chap04
copy run5.bat %ED5%\source\chap05
copy run5.bat %ED5%\source\chap06
copy run5.bat %ED5%\source\chap07
copy run5.bat %ED5%\source\chap08
copy run5.bat %ED5%\source\chap09
copy run5.bat %ED5%\source\chap10
copy run5.bat %ED5%\source\chap11
del run5.bat
