kylix_bin=/opt/kylix2/bin
compile=$(kylix_bin)/dcc

clean:
	-rm -Rf *.dcu
	-rm -Rf *.dpu
	-rm -Rf *.~*
	-rm -Rf *.exe
	-rm -Rf *.o
	-rm -Rf *.dcp

	-rm eval/project1
	-rm tests/metafiles/metafile
	-rm tests/objinsp/project1
	-rm tests/params/project1
	-rm tests/qpainter/project2
	-rm tests/ruler/project1
	-rm repman/utils/reptotxt/reptotxt
	-rm repman/utils/txttorep/txttorep
	-rm repman/utils/printrep/printrep
real_clean: clean
	-rm -Rf  *.bpl
	-rm -Rf  *.so
packages:
	$(compile) rppackv.dpk
