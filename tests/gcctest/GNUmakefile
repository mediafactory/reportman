all: test testqt
	
test: test.cc
	g++ -o test test.cc -L. libreportmanapi.so

testqt: testqt.cc
	g++ -o testqt testqt.cc -L. libreportmanapiqt.so libborqt-6.9-qt2.3.so

clean:
	rm -f test
	rm -f testqt pp.pdf
