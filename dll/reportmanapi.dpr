library reportmanapi;

// Report Manager standard C API
// This file is for Linux, the
// library for Windows is
// ReportMan.ocx
// This library does not depend
// on X Server running but can not
// print, only generate pdfs o metafiles
// you can use reportmanapiqt for print and preview

{$I rpconf.inc}

uses
  SysUtils,
  Classes,
  rpdllutil in '..\rpdllutil.pas',
  rpmdconsts in '..\rpmdconsts.pas',
  rppdfdriver in '..\rppdfdriver.pas';

{$R *.RES}
exports
 rp_open,
 rp_execute,
 rp_close,
 rp_lasterror;

begin
end.


