kylix_bin=/opt/kylix2/bin
compile=$(kylix_bin)/dcc


all: packages repman

repman:
	cd repman
	$(comile) repmand.dpr
	cd ..



clean:
	-rm -Rf *.dcu
	-rm -Rf *.dpu
	-rm -Rf *.~*
	-rm -Rf *.exe
	-rm -Rf *.o
	-rm -Rf *.dcp
	-rm -Rf *.ow
	-rm -Rf *.rst

	-rm tests/eval/project1
	-rm tests/metafiles/metafile
	-rm tests/objinsp/project1
	-rm tests/params/project1
	-rm tests/qpainter/project2
	-rm tests/ruler/project1
	-rm repman/utils/reptotxt/reptotxt
	-rm repman/utils/txttorep/txttorep
	-rm repman/utils/printrep/printrep
	-rm tests/repmand/*.so tests/repmand/*.so.1 tests/repmand/*.so.2
	-rm tests/repmand/repmand
real_clean: clean
	-rm -Rf  *.bpl
	-rm -Rf  *.so
packages:
	$(compile) rppack.dpk
	$(compile) rppackv.dpk
