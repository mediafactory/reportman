//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("rppack_builder4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("rpreg.pas");
USEUNIT("rppdfreport.pas");
USERES("rppdfreport.dcr");
USEUNIT("rpvclreport.pas");
USERES("rpvclreport.dcr");
USEPACKAGE("Vclmid40.bpi");
USEPACKAGE("Vcldb40.bpi");
USEUNIT("rpalias.pas");
USERES("rpalias.dcr");
USEUNIT("rpeval.pas");
USERES("rpeval.dcr");
USEUNIT("rplastsav.pas");
USERES("rplastsav.dcr");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------