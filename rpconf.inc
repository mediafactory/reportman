// Database drivers to be used
{$DEFINE USESQLEXPRESS}
{$DEFINE USEIBX}
{$DEFINE USEADO}
{$DEFINE USEBDE}
//{$DEFINE USEIBO}

// May be you want to enablwe TEECHART under Kylix
// but take care it needs an X server running
{$IFDEF MSWINDOWS}
 {$DEFINE USETEECHART}
{$ENDIF}

// Unsupported database drivers in Linux
// To enable IBX under Kylix remove the related UNDEF
// By default is not defined becuase Borland don't provide
// it in default installation
{$IFDEF LINUX}
 {$UNDEF USEIBX}
 {$UNDEF USEADO}
 {$UNDEF USEBDE}
 {$UNDEF USEIBO}
{$ENDIF}

// Windows qtintf.dll has a bug about selecting horz paper
// If horz paper is set and print setup is called seems
// to work ok
{$IFDEF MSWINDOWS}
 {$DEFINE HORZPAPERBUG}
{$ENDIF}



{$DEFINE USEVARIANTS}
{$DEFINE USEBCD}

// Delphi 5  compiler defines
{$IFDEF VER130}
 {$UNDEF USEVARIANTS}
 {$UNDEF USEBCD}
 {$UNDEF USESQLEXPRESS}
 {$DEFINE MSWINDOWS}
 {$DEFINE USEVCL}
{$ENDIF}


//  Builder 4 compiler defines
{$IFDEF VER125}
 {$UNDEF USEVARIANTS}
 {$UNDEF USEBCD}
 {$UNDEF USESQLEXPRESS}
 {$DEFINE MSWINDOWS}
 {$DEFINE USEVCL}
 {$UNDEF USEIBX}
 {$UNDEF USEADO}
 {$DEFINE BUILDER4}
{$ENDIF}

{$IFDEF LINUX}
 {$DEFINE LINUXPRINTBUG}
 {$DEFINE KYLIX2}
{$ENDIF}

{$IFDEF LINUX}
{$IFDEF VER140}
 {$UNDEF KYLIX2}
// Vcl File Filters is a missing feature of CLX Dialogs in kylix 2 and Delphi 6
 {$DEFINE VCLFILEFILTERS}
{$ENDIF}
{$ENDIF}

// Delphi 7
{$IFDEF VER150}
 {$DEFINE VCLFILEFILTERS}
 {$IFDEF MSWINDOWS}
  {$DEFINE BLOBSTREAMBUG}
  {$DEFINE QUERYLINKBUG}
 {$ENDIF}
{$ENDIF}

