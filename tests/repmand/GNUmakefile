#kylix_bin=/home/toni/kylix3/bin
kylix_bin=/opt/kylix3/bin
drivers_path=../../drivers/linux

install:
	cp $(kylix_bin)/libqtintf-6.9-qt2.3.so .
	cp $(kylix_bin)/libborqt-6.9-qt2.3.so .
	cp $(kylix_bin)/libmidas.so.1 .
	cp $(drivers_path)/libsqldb2.so .
	cp $(drivers_path)/libsqlib.so .
	cp $(drivers_path)/libsqlinf.so .
	cp $(drivers_path)/libsqlmy.so.1 .
	cp $(drivers_path)/libsqlmy23.so .
	cp $(drivers_path)/libsqlpg.so .
	cp $(drivers_path)/libsqlora.so .
	cp $(drivers_path)/libdbxoodbc.so .
	cp $(drivers_path)/dbxres.en.1 .
	cp ../../repman/repmand .
	cp ../../repman/dbxdrivers .
	cp ../../repman/dbxconnections .
	cp ../../repman/utils/metaprint/metaprint .
	cp ../../repman/utils/metaview/metaview .
	cp ../../repman/utils/printrep/printrep .
	cp ../../repman/utils/reptotxt/reptotxt .
	cp ../../repman/utils/txttorep/txttorep .
	cp ../../server/app/reportserverapp .
	cp ../../server/app/reportservercon .
	cp ../../server/config/repserverconfig .
	cp ../../repman/utils/printreptopdf/printreptopdf .
	cp ../../repman/reportmanres.es .
	cp ../../repman/reportmanres.en .
	cp ../../repman/qttranslation/linux/qt_* .
	cp ../../LICENSE.TXT .
#	cp ../../repman/repmand.es .
	-cp ../clxreport/Project1 .
	cp ../../repman/repsamples/sample4.rep .
	cp ../clxreport/biolife.cds .
	-mkdir doc
	cp ../../doc/doc/*.html doc
	mkdir doc/images
	cp ../../doc/doc/images/*.jpg doc/images
#	include repmand.es when linux translation done
	tar c doc LICENSE.TXT sample4.rep biolife.cds reportmanres.en reportmanres.es repmand reportserverapp reportserverapp.sh reportservercon reportservercon.sh repserverconfig repserverconfig.sh dbxdrivers dbxconnections metaprint metaview metaview.sh printrep printreptopdf reptotxt txttorep repmand repmand.sh README *.so *.so.1 qt_* dbxres.en.1 > repmand-1_1.i386.tar
	gzip repmand-1_1.i386.tar
clean:
	-rm -f *.so *.so.1 *.so.2 *.so.1.0
	-rm -f repmand
	-rm -f dbxconnections dbxdrivers metaprint metaview printrep printreptopdf
	-rm -f reptotxt txttorep repserverconfig reportservercon
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
	-rm -f LICENSE.TXT
	-rm -f dbxres.en.*
	-rm -f reportserverapp
