REM use -mwindows to create a Windows application (with WinMain)
c:\mingw\bin\g++ -c -o test.o test.cc
c:\mingw\bin\g++ -o test.exe test.o Reportman.a
c:\mingw\bin\g++ -c -o testqt.o testqt.cc
c:\mingw\bin\g++ -o testqt.exe testqt.o Reportman.a
