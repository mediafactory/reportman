library reportmanapiqt;

// Report Manager standard C API
// This file is for Linux, the
// library for Windows is
// ReportMan.ocx
// This library depend
// on X Server running but can
// print and preview, if you need only generate pdfs or metafiles
// you can use reportmanapi lib

{$I rpconf.inc}
{$E so}

uses
  SysUtils,
  Classes,
  rpnotlibrary in 'rpnotlibrary.pas',
  rpdllutil in '../rpdllutil.pas',
  rpmdconsts in '../rpmdconsts.pas',
  rppdfdriver in '../rppdfdriver.pas',
  rpdllutilqt in '../rpdllutilqt.pas';

exports
 rp_open,
 rp_execute,
 rp_close,
 rp_lasterror,
 rp_print,
 rp_preview;

begin
end.


