all: test testqt
	
test: test.cc libreportmanapi.so
	g++ -o test test.cc -L. libreportmanapi.so

testqt: testqt.cc bplreportmanapiqt.so
	g++ -o testqt testqt.cc -L. bplreportmanapiqt.so libborqt-6.9-qt2.3.so bplvisualclx.so.6.9 bplrppack.so bplrppackv.so bpldataclx.so.6.9 bplrtl.so.6.9 libborunwind.so.6.0

clean:
	rm -f test
	rm -f testqt pp.pdf
