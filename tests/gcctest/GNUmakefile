all: test testqtlin
	
test: test.cc
	g++ -o test test.cc -L. libreportmanapi.so

testqtlin: testqtlin.cc libreportmanapiqt.so
	g++ -o testqtlin testqtlin.cc -L. -L/usr/X11R6/lib libreportmanapiqt.so libborqt-6.9-qt2.3.so libborunwind.so.6.0

clean:
	rm -f test
	rm -f testqt pp.pdf
