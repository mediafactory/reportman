unit rpactivexreport;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,
  rpvclreport,Graphics;


const
 AX_CONSWIDTH=50;
 AX_CONSHEIGHT=20;

type
  TRpActiveXReport = class(TCustomControl)
  private
    { Private declarations }
    FVCLReport:TVCLReport;
    FFilename:String;
    FPreview:boolean;
    procedure SetFilename(Value:string);
    procedure SetPreview(Value:boolean);
  protected
    { Protected declarations }
    procedure Paint;override;
  public
   procedure Execute;
   procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
   constructor Create(AOwner:TComponent);override;
   { Public declarations }
  published
    { Published declarations }
    property Filename:string read FFilename write SetFilename;
    property Preview:boolean read FPreview write SetPreview default true;
  end;


implementation

procedure TRpActiveXReport.Paint;
begin
 Canvas.Brush.Color:=clWhite;
 Canvas.Pen.Color:=clBlack;
 Canvas.Rectangle(ClientRect);
 Canvas.TextOut(2,2,'AXReport');
end;

procedure TRpActiveXReport.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
 AWidth:=AX_CONSWIDTH;
 AHeight:=AX_CONSHEIGHT;
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

constructor TRpActiveXReport.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);

 Width:=AX_CONSWIDTH;
 Height:=AX_CONSHEIGHT;
 FVCLReport:=TVCLReport.Create(Self);
end;

procedure TRpActiveXReport.SetFilename(Value:string);
begin
 FVCLReport.Filename:=Value;
 FFilename:=Value;
end;

procedure TRpActiveXReport.SetPreview(Value:boolean);
begin
 FVCLReport.Preview:=Value;
 FPreview:=Value;
end;

procedure TRpActiveXReport.Execute;
begin
 FVCLReport.Execute;
end;

end.

