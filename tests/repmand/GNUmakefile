kylix_bin=/opt/kylix2/bin

install:
	cp $(kylix_bin)/libqtintf-6.5-qt2.3.so .
	cp $(kylix_bin)/libqt.so.2 .
	cp $(kylix_bin)/libsqlib.so.1 .
	cp $(kylix_bin)/libsqlmy.so.1 .
	cp $(kylix_bin)/libsqlmy23.so.1 .
	cp ../../repman/repmand .
	cp ../../repman/dbxdrivers .
	cp ../../repman/dbxconnections .
	-cp ../../repman/utils/metaprint/metaprint .
	-cp ../../repman/utils/metaview/metaview .
	-cp ../../repman/utils/printrep/printrep .
clean:
	-rm -f *.so *.so.1 *.so.2
	-rm -f repmand
	-rm -f dbxconnections dbxdrivers metaprint metaview printrep
