REM use -mwindows to create a Windows application (with WinMain)
c:\mingw\bin\g++ -c -o testwin.o testwin.cc
c:\mingw\bin\g++ -o testwin.exe testwin.o Reportman.a
c:\mingw\bin\g++ -c -o testqt.o testqt.cc
c:\mingw\bin\g++ -o testqt.exe testqt.o Reportman.a
c:\mingw\bin\g++ -c -o testcliwin.o testcliwin.cc
c:\mingw\bin\g++ -o testcliwin.exe testcliwin.o Reportman.a

