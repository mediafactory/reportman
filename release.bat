webactivex\generatecab
set REPORTMANNETDIR=c:\prog\toni\reportmannet
copy %REPORTMANNETDIR%\output\*.dll repman\net
copy %REPORTMANNETDIR%\output2\*.dll repman\net2
copy %REPORTMANNETDIR%\output\*.exe repman\net
copy %REPORTMANNETDIR%\output2\*.exe repman\net2
del repman\net\fbembed.dll
del repman\net2\fbembed.dll
