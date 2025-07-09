unit uDebugHelper;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils, Classes;

type
  TDebugHelper = class
  private
    class var FLogFile: string;
    class procedure WriteToFile(const AMessage: string);
  public
    class procedure Initialize(const ALogFile: string = '');
    class procedure Finalize;
    class procedure Log(const AMessage: string);
    class procedure LogFmt(const AFormat: string; const Args: array of const);
  end;

procedure DebugLog(const AMessage: string);
procedure DebugLogFmt(const AFormat: string; const Args: array of const);

implementation

class procedure TDebugHelper.Initialize(const ALogFile: string);
begin
  if ALogFile <> '' then
    FLogFile := ALogFile
  else
    FLogFile := ChangeFileExt(ParamStr(0), '.log');
end;

class procedure TDebugHelper.Finalize;
begin
  FLogFile := '';
end;

class procedure TDebugHelper.WriteToFile(const AMessage: string);
var
  F: TextFile;
begin
  if FLogFile = '' then Exit;

  try
    AssignFile(F, FLogFile);
    if FileExists(FLogFile) then
      Append(F)
    else
      Rewrite(F);
    try
      WriteLn(F, AMessage);
    finally
      CloseFile(F);
    end;
  except
    // 忽略文件写入错误
  end;
end;

class procedure TDebugHelper.Log(const AMessage: string);
var
  TimeStr: string;
  FullMessage: string;
begin
  TimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  FullMessage := Format('[%s] %s', [TimeStr, AMessage]);

  OutputDebugString(PChar(FullMessage));
  WriteToFile(FullMessage);
end;

class procedure TDebugHelper.LogFmt(const AFormat: string; const Args: array of const);
begin
  Log(Format(AFormat, Args));
end;

procedure DebugLog(const AMessage: string);
begin
  TDebugHelper.Log(AMessage);
end;

procedure DebugLogFmt(const AFormat: string; const Args: array of const);
begin
  TDebugHelper.LogFmt(AFormat, Args);
end;

initialization
  TDebugHelper.Initialize;

finalization
  TDebugHelper.Finalize;

end.
