kylix_bin=/opt/kylix2/bin
drivers_path=../../drivers/linux

install:
	cp $(kylix_bin)/libqtintf-6.5-qt2.3.so .
	cp $(kylix_bin)/libqt.so.2 .
	cp $(drivers_path)/libsqlib.so.1 .
	cp $(drivers_path)/libsqlmy.so.1 .
	cp $(drivers_path)/libsqlmy23.so.1 .
	cp $(drivers_path)/libsqlpg.so.1.0 .
	cp $(drivers_path)/libdbxoodbc.so .
	cp ../../repman/repmand .
	cp ../../repman/dbxdrivers .
	cp ../../repman/dbxconnections .
	cp ../../repman/utils/metaprint/metaprint .
	cp ../../repman/utils/metaview/metaview .
	cp ../../repman/utils/printrep/printrep .
	cp ../../repman/utils/reptotxt/reptotxt .
	cp ../../repman/utils/txttorep/txttorep .
	tar c repmand dbxdrivers dbxconnections metaprint metaview printrep reptotxt txttorep repmand repmand.sh README *.so *.so.1 *.so.2 *.so.1.0 > repmand.i386.tar
	gzip repmand.i386.tar
clean:
	-rm -f *.so *.so.1 *.so.2 *.so.1.0
	-rm -f repmand
	-rm -f dbxconnections dbxdrivers metaprint metaview printrep
	-rm -f reptotxt txttorep
	-rm -f *.gz
