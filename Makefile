
DELPHIPATH=c:\prog\delphi6
COMPILE=dcc32 -U$(DELPHIPATH)\projects\bpl

all: packages repman

repman:
        cd repman
        $(COMPILE) repmand.dpr
        cd ..

packages: clxpackages rtlpackages

rtlpackages:
        $(COMPILE) rppack_del.dpk
clxpackages:
        $(COMPILE) rppackv_del.dpk


clean:
        -del /s *.dcu
        -del /s *.dpu
        -del /s *.~*
        -del /s *.o
        -del /s *.exe
        -del /s *.dcp
        -del /s *.ow
        -del /s *.ppw
        -del /s *.rst

        -del tests\eval\project1
        -del tests\metafiles\metafile
        -del tests\objinsp\project1
        -del tests\params\project1
        -del tests\qpainter\project2
        -del tests\ruler\project1
        -del repman\utils\reptotxt\reptotxt
        -del repman\utils\txttorep\txttorep
        -del repman\utils\printrep\printrep
        -del repman\utils\metaprint\metaprint
        -del repman\utils\metaview\metaview
real_clean:      clean
        -del /s *.bpl
        -del /s *.so
