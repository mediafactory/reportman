unit rpaxreportreport;

interface

uses
  ComObj, ActiveX, Reportman_TLB, StdVcl,rpreport,
  rpaxreportparameters;

type
  TReportReport = class(TAutoObject, IReportReport)
  private
    FReportParameters:TReportParameters;
  protected
    function Get_Params: ReportParameters; safecall;
    function Get_VCLReport: PChar; safecall;
    { Protected declarations }
  public
   FReport:TRpReport;
  end;

implementation

uses ComServ;

function TReportReport.Get_Params: ReportParameters;
begin
 if Not Assigned(FReportParameters) then
 begin
  FReportParameters:=TReportParameters.Create;
  FReportParameters.FReport:=FReport;
 end;
 Result:=FReportParameters;
 Result._Addref;
end;


function TReportReport.Get_VCLReport: PChar;
begin
 Result:=PChar(FReport);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TReportReport, Class_ReportReport,
    ciInternal, tmSingle);
end.
