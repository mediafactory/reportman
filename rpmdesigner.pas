{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmdesigner                                     }
{       TRpDesigner: A Component to call report designer}
{       can be used in windows and linux                }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpmdesigner;

interface

uses Classes,SysUtils,rpconsts,rpreport;

type
 // Stream: You can set the stream you want to write the report
 // report: Internal report component if you want to modify something
 // handled: You don't want TRpDesigner save anything (you saved it for example)
 TRpSaveEvent=procedure (var Stream:TStream;report:TRpReport;
  var handled:boolean) of object;

 TRpDesigner=class(TComponent)
  private
   FFilename:string;
   FReadOnly:boolean;
   FReport:TRpReport;
   FModal:boolean;
   FOnSave:TRpSaveEvent;
   procedure CheckLoaded;
  public
   constructor Create(AOwner:TComponent);override;
   procedure LoadFromStream(stream:TStream);
   procedure LoadFromFile(AFilename:string);
   function Execute:boolean;
   procedure SaveToStream(stream:TStream);
  published
   property Filename:string read FFilename write FFilename;
   property ReadOnly:boolean read FReadonly write FReadOnly default false;
   property OnSave:TRpSaveEvent read FOnSave write FOnSave;
   property Modal:boolean read FModal write FModal default true;
  end;

implementation

constructor TRpDesigner.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 FModal:=true;
end;

procedure TRpDesigner.LoadFromStream(stream:TStream);
begin
 if Assigned(FReport)then
 begin
  FReport.free;
  FReport:=nil;
 end;
 FReport:=TRpReport.Create(Self);
 try
  FReport.LoadFromStream(Stream);
 except
  FReport.Free;
  FReport:=nil;
  raise;
 end;
end;

procedure TRpDesigner.LoadFromFile(AFilename:string);
begin
 if Assigned(FReport)then
 begin
  FReport.free;
  FReport:=nil;
 end;
 FReport:=TRpReport.Create(Self);
 try
  FReport.LoadFromFile(AFileName);
 except
  FReport.Free;
  FReport:=nil;
  raise;
 end;
end;

procedure TRpDesigner.CheckLoaded;
begin
 // Loads the report
 if Assigned(FReport) then
  exit;
 if Length(FFilename)<1 then
  Raise Exception.Create(SRpNoFilename);
 LoadFromFile(FFilename);
end;

function TRpDesigner.Execute:boolean;
var
 handled:boolean;
 stream:TStream;
begin
 CheckLoaded;
 // Creates the form and on close do the save event

 if FReadonly then
  exit;
 handled:=false;
 stream:=nil;
 if Assigned(FOnSave) then
 begin
  FOnSave(stream,Freport,handled);
 end;
 if not handled then
 begin
  if Not Assigned(stream) then
  begin
   if Length(FFileName)>0 then
   begin
    FReport.SaveToFile(FFilename);
   end
   else
   begin
    Raise Exception.Create(SRpNoStreamToSaveReport);
   end;
  end;
 end;
end;

procedure TRpDesigner.SaveToStream(stream:TStream);
begin
 CheckLoaded;
 FReport.SaveToStream(Stream);
end;

end.
