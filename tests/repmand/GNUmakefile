kylix_bin=/opt/kylix2/bin
drivers_path=../../drivers/linux

install:
	cp $(kylix_bin)/libqtintf-6.5-qt2.3.so .
	cp $(kylix_bin)/libqt.so.2 .
	cp $(drivers_path)/libsqlib.so.1 .
	cp $(drivers_path)/libsqlmy.so.1 .
	cp $(drivers_path)/libsqlmy23.so.1 .
	cp $(drivers_path)/libdbxoodbc.so .
	cp ../../repman/repmand .
	cp ../../repman/dbxdrivers .
	cp ../../repman/dbxconnections .
	-cp ../../repman/utils/metaprint/metaprint .
	-cp ../../repman/utils/metaview/metaview .
	-cp ../../repman/utils/printrep/printrep .
	-cp ../../repman/utils/reptotxt/reptotxt .
	-cp ../../repman/utils/txttorep/txttorep .
clean:
	-rm -f *.so *.so.1 *.so.2
	-rm -f repmand
	-rm -f dbxconnections dbxdrivers metaprint metaview printrep
	-rm -f reptotxt txttorep
