unit rpaspserver;

interface

uses
  Classes,ComObj, ActiveX, AspTlb, ReportMan_TLB, StdVcl,
  rpreport,rppdfdriver;

type
  TReportmanXAServer = class(TASPMTSObject, IReportmanXAServer)
  protected
    procedure GetPDF(const Report: IReportReport; Compressed: WordBool);
      safecall;
  end;

implementation

uses ComServ;

procedure TReportmanXAServer.GetPDF(const Report: IReportReport;
  Compressed: WordBool);
var
 areport:TRpReport;
 memstream:TMemoryStream;
 astring:String;
begin
 areport:=TRpReport(Report.VCLReport);
 memstream:=TMemoryStream.Create;
 try
  rppdfdriver.PrintReportPDFStream(areport,'',false,true,
   1,99999,1,memstream,Compressed,false);
  memstream.Seek(0,soFromBeginning);
//  Response.Content:='Executed, size:'+IntToStr(astream.size);
  Response.Clear;
  Response.ContentType := 'application/pdf';
  SetLength(astring,memstream.size);
  memstream.Read(astring[1],memstream.size);
//  Response.Write(astring);
  Response.BinaryWrite(astring);
 finally
  memstream.free;
 end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TReportmanXAServer, Class_ReportmanXAServer,
    ciMultiInstance, tmApartment);
end.
