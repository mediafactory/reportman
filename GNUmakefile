kylix_bin=/opt/kylix2/bin
compile=$(kylix_bin)/dcc


all: packages repman

repman:
	cd repman
	$(comile) repmand.dpr
	cd ..



clean:
	-rm -Rf *.dcu repman/*.dcu
	-rm -Rf *.dpu repman/*.dpu
	-rm -Rf *.~* repman/*.~*
	-rm -Rf *.exe repman/*.exe
	-rm -Rf *.o repman/*.o
	-rm -Rf *.dcp repman/*.dcp
	-rm -Rf *.ow repman/*.ow
	-rm -Rf *.rst repman/*.rst
	-rm -Rf repman/utils/metaview/*.dcu
	-rm -Rf repman/utils/metaview/*.dpu
	-rm -Rf repman/utils/metaview/*.~*
	-rm -Rf repman/utils/metaprint/*.~*
	-rm -Rf repman/utils/metaprint/*.dcu
	-rm -Rf repman/utils/metaprint/*.dpu
	-rm -Rf repman/utils/metaview/*.~*
	-rm -Rf repman/utils/reptotxt/*.dcu
	-rm -Rf repman/utils/reptotxt/*.dpu
	-rm -Rf repman/utils/reptotxt/*.~*
	-rm -Rf repman/utils/printrep/*.~*
	-rm -Rf repman/utils/printrep/*.dcu
	-rm -Rf repman/meta.rpmf


	-rm tests/eval/project1
	-rm tests/metafiles/metafile
	-rm tests/objinsp/project1
	-rm tests/params/project1
	-rm tests/qpainter/project2
	-rm tests/ruler/project1
	-rm repman/utils/reptotxt/reptotxt
	-rm repman/utils/txttorep/txttorep
	-rm repman/utils/printrep/printrep
	-rm repman/utils/metaprint/metaprint
	-rm repman/utils/metaview/metaview
	-rm tests/repmand/*.so tests/repmand/*.so.1 tests/repmand/*.so.2
	-rm tests/repmand/repmand
	-rm tests/repmand/dbxdrivers
	-rm tests/repmand/dbxconnections
	-rm tests/repmand/metaview
	-rm tests/repmand/metaprint
	-rm tests/repmand/printrep
	-rm tests/repmand/Project1
	-rm tests/repmand/sample4.rep
	-rm tests/repmand/biolife.cds
	-rm tests/repmand/*.0
	-rm tests/repmand/*.tar
	-rm tests/repmand/*.gz
	-rm tests/repmand/meta.rpmf
	-rm tests/repmand/reptotxt
	-rm tests/repmand/txttorep
	-rm tests/clxreport/Project1
	-rm tests/clxreport/*.dcu
	-rm tests/clxreport/*.~*
	-rm tests/clxreport/Project2
real_clean: clean
	-rm -Rf  *.bpl
	-rm -Rf  *.so
packages:
	$(compile) rppack.dpk
	$(compile) rppackv.dpk
