kylix_bin=/opt/kylix2/bin
drivers_path=../../drivers/linux

install:
	cp $(kylix_bin)/libqtintf-6.5-qt2.3.so .
	cp $(kylix_bin)/libqt.so.2 .
	cp $(kylix_bin)/libmidas.so.1 .
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
	cp ../../repman/utils/printreptopdf/printreptopdf .
	cp ../../repman/reportmanres.es .
	cp ../../repman/reportmanres.en .
	cp ../../repman/qttranslation/linux/qt_* .
#	cp ../../repman/repmand.es .
	-cp ../clxreport/Project1 .
	cp ../../repman/repsamples/sample4.rep .
	cp ../clxreport/biolife.cds .
	-mkdir doc
	cp ../../doc/doc/*.html doc
	mkdir doc/images
	cp ../../doc/doc/images/*.jpg doc/images
#	include repmand.es when linux translation done
	tar c doc sample4.rep biolife.cds repmand dbxdrivers dbxconnections metaprint metaview printrep printreptopdf reptotxt txttorep repmand repmand.sh README *.so *.so.1 *.so.2 *.so.1.0 qt_* > repmand-1RC1.i386.tar
	gzip repmand-1RC2.i386.tar
clean:
	-rm -f *.so *.so.1 *.so.2 *.so.1.0
	-rm -f repmand
	-rm -f dbxconnections dbxdrivers metaprint metaview printrep printreptopdf
	-rm -f reptotxt txttorep
	-rm -f *.gz
	-rm -f *.rep
	-rm -f *.cds
	-rm -Rf doc
	-rm -f *.rpmf
	-rm -f *.tar
	-rm -f *.pdf
	-rm -f qt_*
	-rm -f repmand.es
	-rm -f reportmanres*
