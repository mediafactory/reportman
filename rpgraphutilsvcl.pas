{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpgraphutilsvcl                                 }
{       Graphic routines used by report manager         }
{                                                       }
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
unit rpgraphutilsvcl;

interface

{$I rpconf.inc}

uses
  windows,
  SysUtils,Classes,Types,rptranslator,
  Graphics, Forms,Buttons, ExtCtrls,
  Controls, StdCtrls,ImgList,ComCtrls,
  rpmdconsts,rpmunits;


type
  TMessageButton = (smbOK, smbCancel, smbYes, smbNo, smbAbort, smbRetry, smbIgnore);
  TMessageButtons = set of TMessageButton;
  TMessageStyle = (smsInformation, smsWarning, smsCritical);

  TFRpMessageDlgVCL = class(TForm)
    Panel1: TPanel;
    BCancel: TButton;
    BOk: TButton;
    BYes: TButton;
    BNo: TButton;
    LMessage: TLabel;
    BAbort: TButton;
    BRetry: TButton;
    BIgnore: TButton;
    EInput: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BYesClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
   Buttonpressed,EscapeButton:TMessageButton;
  public
    { Public declarations }
  end;

procedure DrawGrid(Canvas:TCanvas;XWidth,XHeight,PixelsWidth,PixelsHeight:integer;Color:TColor;lines:boolean;XOffset,YOffset:integer);
function twipstopixels(ATwips:integer):integer;
function pixelstotwips(apixels:integer):integer;
function FontStyleToInteger(fontstyle:TFontStyles):integer;
function IntegerToFontStyle(intfontstyle:integer):TFontStyles;
function IntegerFontStyleToString(intfontstyle:integer):String;
function AlignToGrid(Value:integer;scale:integer):integer;
function AlignToGridPixels(Value:integer;scaletwips:integer):integer;
function RpMessageBox(const Text: WideString; const Caption: WideString = '';
  Buttons: TMessageButtons = [smbOK]; Style: TMessageStyle = smsInformation;
  Default: TMessageButton = smbOK; Escape: TMessageButton = smbCancel): TMessageButton;
function RpInputBox(const ACaption, APrompt, ADefault:WideString ):WideString;
procedure FillTreeView(path:String;Nodes:TTreeNodes;Node:TTreeNode;pattern:string);

implementation

{$R *.dfm}


{$IFDEF MSWINDOWS}
const
  kernel = 'kernel32.dll';
  OldLocaleOverrideKey = 'Software\Borland\Delphi\Locales';
  NewLocaleOverrideKey = 'Software\Borland\Locales';


function RegOpenKeyEx(hKey: LongWord; lpSubKey: PChar; ulOptions,
  samDesired: LongWord; var phkResult: LongWord): Longint; stdcall;
  external advapi32 name 'RegOpenKeyExA';
function RegQueryValueEx(hKey: LongWord; lpValueName: PChar;
  lpReserved: Pointer; lpType: Pointer; lpData: PChar; lpcbData: Pointer): Integer; stdcall;
  external advapi32 name 'RegQueryValueExA';
{$ENDIF}

function AlignToGrid(Value:integer;scale:integer):integer;
var
 rest:integer;
begin
 Result:=Value div scale;
 rest:=Value mod scale;
 Result:=scale*Result;
 if rest>(scale div 2) then
  Result:=Result+scale;
end;

function AlignToGridPixels(Value:integer;scaletwips:integer):integer;
begin
 Value:=pixelstotwips(Value);
 Value:=AlignToGrid(Value,scaletwips);
 Result:=twipstopixels(Value);
end;


function LogicalPointToDevicePoint(origin,destination,value:TPoint):TPoint;
begin
 Result:=value;
 value.X:=Round(value.X*(destination.X/origin.X));
 value.Y:=Round(value.Y*(destination.Y/origin.Y));
 Result:=Value;
end;

procedure DrawGrid(Canvas:TCanvas;XWidth,XHeight,PixelsWidth,PixelsHeight:integer;Color:TColor;lines:boolean;XOffset,YOffset:integer);
var
 pixelsperinchx,pixelsperinchy:integer;
 rect:TRect;
 han:HDC;
// hanbrush:QBrushH;
 windowwidth,windowheight:integer;
 x,y:integer;
 pixelwidth:integer;
// pixelheight:integer;
 xof,yof:integer;
 oldmapmode:integer;
 origin,destination,avalue:TPoint;
begin
 if XHeight<=0 then
  exit;
 if XWidth<=0 then
  exit;

 Rect.Left:=0;
 Rect.Top:=0;
 Rect.Right:=PixelsWidth+XOffset;
 Rect.Bottom:=PixelsHeight+YOffset;

 if Screen.PixelsPerInch<=0 then
  exit;
 pixelsperinchx:=Screen.PixelsPerInch;
 pixelsperinchy:=Screen.PixelsPerInch;
 xof:=Round(XOffset/pixelsperinchx*TWIPS_PER_INCHESS);
 yof:=Round(YOffset/pixelsperinchy*TWIPS_PER_INCHESS);
 windowwidth:=Round(TWIPS_PER_INCHESS*(rect.right+XOffset)/pixelsperinchx);
 windowheight:=Round(TWIPS_PER_INCHESS*(rect.bottom+YOffset)/pixelsperinchy);
 Canvas.Pen.Color:=Color;
 Canvas.Brush.Color:=Color;
 han:=Canvas.Handle;
// hanbrush:=Canvas.Brush.handle;

// pixelwidth:=Round(TWIPS_PER_INCHESS/pixelsperinchx)+1;
// pixelheight:=Round(TWIPS_PER_INCHESS/pixelsperinchy)+1;

 origin.X:=windowwidth;
 origin.Y:=windowheight;
 destination.X:=rect.Right;
 destination.Y:=rect.Bottom;
 // Draws the grid
 // Painting of the ruler
 oldmapmode:=SetMapMode(han,MM_TEXT);
 // Get the pixels per inch
 try
  if lines then
  begin
   x:=xof+XWidth;
   y:=yof+Xheight;
   while ((x<windowwidth) or (y<windowheight)) do
   begin
    if (x<windowwidth) then
    begin
     avalue.X:=x;
     avalue.Y:=yof;
     avalue:=LogicalPointToDevicePoint(origin,destination,avalue);
     Canvas.MoveTo(avalue.X,avalue.Y);
     avalue.X:=x;
     avalue.Y:=windowheight;
     avalue:=LogicalPointToDevicePoint(origin,destination,avalue);
     Canvas.LineTo(avalue.X,avalue.Y);
      x:=x+XWidth;
    end;
    if (y<windowheight) then
    begin
     avalue.X:=xof;
     avalue.Y:=y;
     avalue:=LogicalPointToDevicePoint(origin,destination,avalue);
     Canvas.MoveTo(avalue.X,avalue.Y);
     avalue.X:=windowwidth;
     avalue.Y:=y;
     avalue:=LogicalPointToDevicePoint(origin,destination,avalue);
     Canvas.LineTo(avalue.X,avalue.Y);
     y:=y+XHeight;
    end;
   end;
  end
  else
  begin
   x:=xof+XWidth;
   while (x<windowwidth) do
   begin
//    avalue.X:=x;
//    avalue.Y:=x+pixelwidth;
//    avalue:=LogicalPointToDevicePoint(origin,destination,avalue);

//    rect.Left:=avalue.X;
//    rect.Right:=avalue.Y;
    y:=yof+XHeight;
    while y<windowheight do
    begin
     avalue.X:=x;
     avalue.Y:=y;
//     avalue.X:=y;
//     avalue.Y:=y+pixelheight;
     avalue:=LogicalPointToDevicePoint(origin,destination,avalue);
//     rect.Top:=avalue.X;
//     rect.Bottom:=avalue.Y;
//     Canvas.FillRect(rect);
     SetPixel(han,avalue.X,avalue.Y,Color);
     y:=y+XHeight;
    end;
    x:=x+XWidth;
   end;
  end;
 finally
  SetMapMode(han,oldmapmode);
 end;
end;


function twipstopixels(ATwips:integer):integer;
begin
 Result:=Round((ATwips/TWIPS_PER_INCHESS)*Screen.PixelsPerInch);
end;

function pixelstotwips(apixels:integer):integer;
begin
 Result:=Round((APixels/Screen.PixelsPerInch)*TWIPS_PER_INCHESS);
end;

function FontStyleToInteger(fontstyle:TFontStyles):integer;
begin
 Result:=0;
 if (fsBold in fontstyle) then
  Result:=Result or 1;
 if (fsItalic in fontstyle) then
  Result:=Result or (1 shl 1);
 if (fsUnderline in fontstyle) then
  Result:=Result or (1 shl 2);
 if (fsStrikeOut in fontstyle) then
  Result:=Result or (1 shl 3);
end;

function IntegerToFontStyle(intfontstyle:integer):TFontStyles;
begin
 Result:=[];
 if (intfontstyle and 1)>0 then
  include(Result,fsBold);
 if (intfontstyle and (1 shl 1))>0 then
  include(Result,fsItalic);
 if (intfontstyle and (1 shl 2))>0 then
  include(Result,fsUnderline);
 if (intfontstyle and (1 shl 3))>0 then
  include(Result,fsStrikeOut);
end;

function IntegerFontStyleToString(intfontstyle:integer):String;
begin
 Result:='[';
 if (intfontstyle and 1)>0 then
  Result:=Result+SRpBold+',';
 if (intfontstyle and (1 shl 1))>0 then
  Result:=Result+SRpItalic+',';
 if (intfontstyle and (1 shl 2))>0 then
  Result:=Result+SRpUnderline+',';
 if (intfontstyle and (1 shl 3))>0 then
  Result:=Result+SRpStrikeOut+',';
 if Length(Result)>1 then
  Result:=Copy(REsult,1,Length(Result)-1);
 Result:=Result+']';
end;


function FindQtLocaleFile:string;
{$IFDEF LINUX}
var
 LangCode,P:PChar;
 I:Integer;
 afilename:String;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  FileName: array[0..260] of Char;
  Key: LongWord;
  LocaleName, LocaleOverride: array[0..4] of Char;
  Size: Integer;
  P: PChar;
  afilename:string;

  function FindBS(Current: PChar): PChar;
  begin
    Result := Current;
    while (Result^ <> #0) and (Result^ <> '\') do
      Result := CharNext(Result);
  end;

  function ToLongPath(AFileName: PChar): PChar;
  var
    CurrBS, NextBS: PChar;
     L: Integer;
     Handle:Integer;
    FindData: TWin32FindData;
    Buffer: array[0..260] of Char;
    GetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
      cchBuffer: Integer): Integer stdcall;
  begin
{$R-}
    Result := AFileName;
    Handle := GetModuleHandle(kernel);
    if Handle <> 0 then
    begin
      @GetLongPathName := GetProcAddress(Handle, 'GetLongPathNameA');
      if Assigned(GetLongPathName) and
         (GetLongPathName(AFileName, Buffer, SizeOf(Buffer)) <> 0) then
      begin
        lstrcpy(AFileName, Buffer);
        Exit;
      end;
    end;

    if AFileName[0] = '\' then
    begin
      if AFileName[1] <> '\' then Exit;
      CurrBS := FindBS(AFileName + 2);  // skip server name
      if CurrBS^ = #0 then Exit;
      CurrBS := FindBS(CurrBS + 1);     // skip share name
      if CurrBS^ = #0 then Exit;
    end else
      CurrBS := AFileName + 2;          // skip drive name

    L := CurrBS - AFileName;
    lstrcpyn(Buffer, AFileName, L + 1);
    while CurrBS^ <> #0 do
    begin
      NextBS := FindBS(CurrBS + 1);
      if L + (NextBS - CurrBS) + 1 > SizeOf(Buffer) then Exit;
      lstrcpyn(Buffer + L, CurrBS, (NextBS - CurrBS) + 1);

      Handle := FindFirstFile(Buffer, FindData);
      if (Handle = -1) then Exit;
      windows.FindClose(Handle);

      if L + 1 + lstrlen(FindData.cFileName) + 1 > SizeOf(Buffer) then Exit;
      Buffer[L] := '\';
      lstrcpy(Buffer + L + 1, FindData.cFileName);
      Inc(L, lstrlen(FindData.cFileName) + 1);
      CurrBS := NextBS;
    end;
    lstrcpy(AFileName, Buffer);
{$R+}
  end;
{$ENDIF}
begin
{$IFDEF LINUX}
 afilename:='qt';
 LangCode := getenv('LANG');
 if (LangCode = nil) or (LangCode^ = #0) then
  Exit;
 // look for modulename.en_US
 P := LangCode;
 while P^ in ['a'..'z', 'A'..'Z', '_'] do
  Inc(P);
 if P = LangCode then
  Result := afilename
 else
 begin
//  Result := afilename + '.' + Copy(LangCode, 1, P - LangCode);
  Result:='qt_'+Copy(LangCode, 1, P - LangCode)+'.qm';
  if not FileExists(Result) then
  begin
   Result := afilename + '.' + Copy(LangCode, 1, P - LangCode);
   // look for modulename.en    (ignoring country code and suffixes)
   I := Length(Result);
   while (I > 0) and not (Result[I] in ['.', '_']) do
    Dec(I);
   if (I-1 = Length(Result)) or (I-1 < Length(afilename)) then
    Exit;
   SetLength(Result, I-1);
   Result:='qt_'+Copy(ExtractFileExt(Result),2,255)+'.qm';

   if not FileExists(Result) then
   begin
    Result:=afilename;
    Exit;
   end;
  end;
 end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result:=afilename;
  afilename:='qt.exe';
  StrCopy(Filename,Pchar(afilename));
  LocaleOverride[0] := #0;
  if (RegOpenKeyEx(HKEY_CURRENT_USER, NewLocaleOverrideKey, 0, KEY_ALL_ACCESS, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OldLocaleOverrideKey, 0, KEY_ALL_ACCESS, Key) = 0) then
  try
    Size := SizeOf(LocaleOverride);
    if RegQueryValueEx(Key, ToLongPath(FileName), nil, nil, LocaleOverride, @Size) <> 0 then
      RegQueryValueEx(Key, '', nil, nil, LocaleOverride, @Size);
  finally
    RegCloseKey(Key);
  end;
  GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
  Result := '';
  if (FileName[0] <> #0) and ((LocaleName[0] <> #0) or (LocaleOverride[0] <> #0)) then
  begin
    P := PChar(@FileName) + lstrlen(FileName);
    while (P^ <> '.') and (P <> @FileName) do Dec(P);
    if P <> @FileName then
    begin
      Inc(P);
      // First look for a locale registry override
      if LocaleOverride[0] <> #0 then
      begin
        lstrcpy(P, LocaleOverride);
        afilename:='qt_'+Copy(ExtractFileExt(StrPas(FileName)),2,255)+'.qm';
        if FileExists(aFileName) then
         Result := aFileName;
      end;
      if (Result ='') and (LocaleName[0] <> #0) then
      begin
        // Then look for a potential language/country translation
        lstrcpy(P, LocaleName);
        afilename:='qt_'+Copy(ExtractFileExt(StrPas(FileName)),2,255)+'.qm';
        if FileExists(aFileName) then
         Result := aFileName;
        if Result = '' then
        begin
          // Finally look for a language only translation
          LocaleName[2] := #0;
          lstrcpy(P, LocaleName);
          afilename:='qt_'+Copy(ExtractFileExt(StrPas(FileName)),2,255)+'.qm';
          if FileExists(aFileName) then
           Result := aFileName;
        end;
      end;
    end;
  end;
{$ENDIF}
end;



function RpMessageBox(const Text: WideString; const Caption: WideString = '';
  Buttons: TMessageButtons = [smbOK]; Style: TMessageStyle = smsInformation;
  Default: TMessageButton = smbOK; Escape: TMessageButton = smbCancel): TMessageButton;
var
 dia:TFRpMessageDlgVCL;
begin
 dia:=TFRpMessageDlgVCL.Create(Application);
 try
  dia.LMessage.Caption:=Text;
  if smbOK in Buttons then
   dia.BOK.Visible:=true;
  if smbCancel in Buttons then
   dia.BCancel.Visible:=true;
  if smbYes in Buttons then
   dia.BYes.Visible:=true;
  if smbNo in Buttons then
   dia.BNo.Visible:=true;
  if smbAbort in Buttons then
   dia.BAbort.Visible:=true;
  if smbRetry in Buttons then
   dia.BRetry.Visible:=true;
  if smbIgnore in Buttons then
   dia.BIgnore.Visible:=true;
  dia.EscapeButton:=Escape;
  case Default of
   smbOK:
    begin
     dia.BOk.Default:=True;
     dia.ActiveControl:=dia.BOK;
    end;
   smbCancel:
    begin
     dia.BCancel.Default:=True;
     dia.ActiveControl:=dia.BCancel;
    end;
   smbYes:
    begin
     dia.BYes.Default:=True;
     dia.ActiveControl:=dia.BYes;
    end;
   smbNo:
    begin
     dia.BNo.Default:=True;
     dia.ActiveControl:=dia.BNo;
    end;
   smbRetry:
    begin
     dia.BRetry.Default:=True;
     dia.ActiveControl:=dia.BRetry;
    end;
   smbIgnore:
    begin
     dia.BIgnore.Default:=True;
     dia.ActiveControl:=dia.BIgnore;
    end;
   smbAbort:
    begin
     dia.BIgnore.Default:=True;
     dia.ActiveControl:=dia.BAbort;
    end;
  end;
  case Style of
   smsInformation:
    begin
     dia.Caption:=SRpInformation;
    end;
   smsWarning:
    begin
     dia.Caption:=SRpWarning;
    end;
   smsCritical:
    begin
     dia.Caption:=SRpCritical;
    end;
  end;
  dia.ShowModal;
  Result:=dia.Buttonpressed;
 finally
  dia.free;
 end;
end;

procedure TFRpMessageDlgVCL.FormCreate(Sender: TObject);
begin
 Buttonpressed:=smbCancel;
 EscapeButton:=smbCancel;
 BYes.Tag:=integer(smbYes);
 BNo.Tag:=integer(smbNo);
 BOk.Tag:=integer(smbOk);
 BCancel.Tag:=integer(smbCancel);
 BRetry.Tag:=integer(smbRetry);
 BIgnore.Tag:=integer(smbIgnore);
 BIgnore.Tag:=integer(smbIgnore);
 BYes.Caption:=SRpYes;
 BNo.Caption:=SRpNo;
 BOk.Caption:=SRpOk;
 BCancel.Caption:=SRpCancel;
 BIgnore.Caption:=SRpIgnore;
 BAbort.Caption:=SRpAbort;
 BRetry.Caption:=SRpRetry;
end;

procedure TFRpMessageDlgVCL.BYesClick(Sender: TObject);
begin
 ButtonPressed:=TMessageButton((Sender As TButton).Tag);
 Close;
end;

procedure TFRpMessageDlgVCL.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key=VK_ESCAPE then
 begin
  Buttonpressed:=EscapeButton;
  Close;
 end;
end;

function RpInputBox(const ACaption, APrompt, ADefault:WideString ):WideString;
var
 dia:TFRpMessageDlgVCL;
begin
 dia:=TFRpMessageDlgVCL.Create(Application);
 try
  dia.LMessage.Caption:=APrompt;
  dia.EInput.Visible:=true;
  dia.EInput.Text:=ADefault;
  dia.ActiveControl:=dia.EInput;
  dia.LMessage.Alignment:=taLeftJustify;
  dia.Caption:=ACaption;
  dia.BOk.Visible:=True;
  dia.BOk.Default:=True;
  dia.BCancel.Visible:=True;
  dia.showmodal;
  if dia.Buttonpressed=smbOK then
   Result:=dia.EInput.Text
  else
   Result:='';
 finally
  dia.free;
 end;
end;



procedure FillTreeView(path:String;Nodes:TTreeNodes;Node:TTreeNode;pattern:string);
var
 att:integer;
 han:integer;
 F:TSearchRec;
 apath:string;
 anode:TTreeNode;
begin
 Nodes.Clear;

 att:=faReadOnly or faArchive;
 FillChar(F,sizeof(F),0);
 apath:=path+'\'+pattern;
 han:=FindFirst(aPath,Att,F);
 if han=0 then
 begin
  try
   repeat
    if (F.Attr and faDirectory)=0 then
     Nodes.AddChild(Node,F.Name);
   until FindNext(F)<>0;
  finally
   FindClose(F);
  end;
 end;

 att:=faDirectory;
 FillChar(F,sizeof(F),0);
 apath:=path+'\*.*';
 han:=FindFirst(aPath,Att,F);
 if han=0 then
 begin
  try
   repeat
    if (F.Attr and faDirectory)>0 then
    begin
     if ((F.Name<>'.') AND (F.Name<>'..')) then
     begin
      anode:=Nodes.AddChild(Node,F.Name);
      FillTreeView(path+'\'+F.Name,Nodes,aNode,pattern);
     end;
    end;
   until FindNext(F)<>0;
  finally
   FindClose(F);
  end;
 end;
end;


initialization
 if ChangeFileExt(ExtractFileName(UpperCase(Application.ExeName)),'')='REPMANDXP' then
  Application.Title:=TranslateStr(1,Application.Title);
end.
