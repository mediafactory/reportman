;DELPHIPATH=c:\Archivos de programa\Borland\Delphi7
DELPHIPATH=c:\prog\Delphi7
REPORTMANPATH=c:\prog\toni\cvsroot\reportman\reportman;$(DELPHIPATH)\OCX\Servers
PACKAGESPATH="$(DELPHIPATH)\Projects\Bpl"
COMPILE="$(DELPHIPATH)\bin\dcc32" -LN$(PACKAGESPATH) -LE$(PACKAGESPATH) -U"$(REPORTMANPATH);$(DELPHIPATH)\projects\bpl" -I"$(REPORTMANPATH)"
all: clean packages reportman

reportman: reportmanutils reportmanserver reportmanutilsxp reportmanserverxp


webx:   clean
        cd webactivex
        $(COMPILE)  -DFORWEBAX WebReportManX.dpr
        generatecab
        cd ..


reportmanutils:
        cd repman
        $(COMPILE) repmand.dpr
        cd utils\reptotxt
        $(COMPILE) reptotxt.dpr
        cd ..
        cd txttorep
        $(COMPILE) txttorep.dpr
        cd ..
        cd printreptopdf
        $(COMPILE) printreptopdf.dpr
        cd ..
        cd printrep
        $(COMPILE) printrep.dpr
        cd ..
        cd metaview
        $(COMPILE) metaview.dpr
        cd ..
        cd metaprint
        $(COMPILE) metaprint.dpr
        cd ..
        cd startup
        $(COMPILE) startup.dpr
        cd ..
        cd unixtodos
        $(COMPILE) unixtodos.dpr
        cd ..

        cd ..
        cd ..

reportmanserver:
        cd server
        cd app
        $(COMPILE) reportserverapp.dpr
        $(COMPILE) reportservercon.dpr
        cd ..
        cd config
        $(COMPILE) repserverconfig.dpr
        cd ..
        cd web
        $(COMPILE) repwebexe.dpr
        cd ..
        cd ..


designerxp:
        cd repman
        $(COMPILE) repmandxp.dpr
        cd ..

reportmanutilsxp: designerxp
        cd repman
        $(COMPILE) repmandxp.dpr

        cd utils
        cd printrep
        $(COMPILE) printrepxp.dpr
        cd ..
        cd metaview
        $(COMPILE) metaviewxp.dpr
        cd ..
        cd metaprint
        $(COMPILE) metaprintxp.dpr
        cd ..
        cd rptranslator
        $(COMPILE) rptranslate.dpr
        cd ..


        cd ..
        cd ..

        cd activex
        $(COMPILE) Reportman.dpr
        cd ..

reportmanserverxp:
        cd server
        cd app
        $(COMPILE) reportserverappxp.dpr
        cd ..
        cd config
        $(COMPILE) repserverconfigxp.dpr
        cd ..
        cd service
        $(COMPILE) repserverservice.dpr
        $(COMPILE) repserviceinstall.dpr
        cd ..
        cd web
        $(COMPILE) repwebserver.dpr
        cd ..
        cd ..

packages: rtlpackages vclpackages clxpackages vcldesignpackages designpackages

rtlpackages:
        $(COMPILE) rppack_del.dpk
vclpackages:
        $(COMPILE) rppackvcl_del.dpk
clxpackages:
        $(COMPILE) rppackv_del.dpk
vcldesignpackages:
        $(COMPILE) rppackdesignvcl_del.dpk
designpackages:
        $(COMPILE) rppackdesign_del.dpk


clean:
        -del /s *.dcu
        -del /s *.dpu
        -del /s *.~*
        -del /s *.o
        -del /s *.exe
        -del /s repman\*.dll
        -del /s server\*.bin
        -del /s server\*.dll
        -del /s *.dcp
        -del /s *.ow
        -del /s *.ppw
        -del /s *.rst
        -del *.hpp
	  copy builderclx\antivcl.hpp .	
        -del /s *.obj
        -del /s *.tds
        -del /s *.ocx
        -del *.bpl
        -del *.bpi
        -del *.lib




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
        -del /s /q install\Output
        -rmdir install\Output
real_clean:      clean
        -del /s *.bpl
        -del /s *.so
