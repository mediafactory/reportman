#Enable this setting if you have problems starting the application it will use libqt system libraries
#export CLX_USE_LIBQT=true
#Enable this setting if you have not defined the LANG variable in your system
#export LANG=en_US
export LD_LIBRARY_PATH=:$PWD:$LD_LIBRARY_PATH
#Bug fix in some distros need LC_NUMERIC en_US or print will not work
#That is Kylix print bugfix
#export LC_NUMERIC=en_US
./printreptopdf $1 $2 $3