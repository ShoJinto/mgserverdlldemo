unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uDebugHelper,
  mgServer;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnBroadcast: TButton;
    EnableWS: TCheckBox;
    EnableTLS: TCheckBox;
    memLog: TMemo;
    TimerPoll: TTimer;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnBroadcastClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerPollTimer(Sender: TObject);
  private
    FHandle: TServerHandle;
    FConfig: TServerConfig;
    FRunning: Boolean;
    FAppPath: string;
    // 保持字符串存活
    FCertFileStr: AnsiString;
    FKeyFileStr: AnsiString;
    FRootDirStr: AnsiString;
  public
  end;

var
  Form1: TForm1;

// 全局回调函数
procedure GlobalHttpCallback(server: TServerHandle; conn_id: Int64; request: PHttpRequest; response: PHttpResponse); stdcall;
procedure GlobalWsCallback(server: TServerHandle; conn_id: Int64;message: PWsMessage); stdcall;


implementation

{$R *.lfm}

procedure GlobalHttpCallback(server: TServerHandle; conn_id: Int64; request: PHttpRequest; response: PHttpResponse); stdcall;
var
  FilePath:AnsiString;
  extra_headers: PAnsiChar;
begin
  FilePath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'static\mgserver.dll';
  form1.memLog.Lines.Add(format('HTTP request (conn_id: %d) method: %s uri: %s body: %s',
  [conn_id, request^.method, request^.uri, request^.body]));
  if StrLComp(request^.uri, '/api/hello', 9) = 0 then
  begin
    response^.status_code := 200;
    response^.headers := 'Content-Type: application/json'#13#10;
    response^.body := '{"status":"ok","message":"Response from server"}';
    response^.body_len := Length(response^.body);
  end
  else if FileExists(FilePath) and (StrLComp(request^.uri, '/mgserver.dll', Length('/mgserver.dll')) = 0) then
  begin
    //extra_headers:='Content-Type: application/octet-stream'#13#10;
    Server_HttpServeFile(server,conn_id,PAnsiChar(FilePath),nil);
  end
  else
  begin
    // 不设置body，自动走静态目录
    response^.body := nil;
    response^.body_len := 0;
  end;

end;

procedure GlobalWsCallback(server: TServerHandle; conn_id: Int64; message: PWsMessage); stdcall;
var
  ret: Integer;
begin
  Form1.memLog.Lines.Add(Format('WebSocket Message: %s', [message^.data]));
  ret := Server_WsSendToOne(server, conn_id, message);
  Form1.memLog.Lines.Add(Format('Callback params: conn_id=%d, len=%d, binary=%d, ret=%d', [conn_id, message^.data_len, message^.binary, ret]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Server_SetLogLevel(1,LOG_LEVEL_DEBUG);
  Server_SetLogTarget(LOG_TARGET_FILE,PAnsiChar('server_debug.log'));
  TDebugHelper.Initialize('server_debug.log');
  FHandle := nil;
  FRunning := False;
  btnStop.Enabled := False;

  // 创建服务器实例
  FHandle := Server_Create;
  DebugLogFmt('Server_Create [%p]',[FHandle]);
  if FHandle = nil then
  begin
    ShowMessage('Failed to create server instance');
    Application.Terminate;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FRunning then
    Server_Stop(FHandle);
  if FHandle <> nil then
    Server_Destroy(FHandle);
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  try
    // 保存字符串引用
    FCertFileStr := 'tfxing.com.crt';
    FKeyFileStr := 'tfxing.com.key';
    FRootDirStr := 'webroot';

    // 初始化配置
    FillChar(FConfig, SizeOf(FConfig), 0);
    FConfig.port := 8000;
    if EnableTLS.Checked then
       FConfig.use_tls := 1 else FConfig.use_tls := 0;
    if EnableWS.Checked then
       FConfig.enable_ws := 1 else FConfig.enable_ws := 0;
    FConfig.cert_file := PAnsiChar(FCertFileStr);
    FConfig.key_file := PAnsiChar(FKeyFileStr);
    FConfig.root_dir := PAnsiChar(FRootDirStr);

    // 设置配置
    if Server_SetConfig(FHandle, @FConfig) <> 0 then
    begin
      ShowMessage('Failed to configure server');
      Exit;
    end;

    // 设置回调
    if Server_SetCallbacks(FHandle, @GlobalHttpCallback, @GlobalWsCallback, Self) <> 0 then
    begin
      ShowMessage('Failed to set callbacks');
      Exit;
    end;

    // 启动服务器
    if Server_Start(FHandle) <> 0 then
    begin
      ShowMessage('Failed to start server');
      Exit;
    end;

    FRunning := True;
    btnStart.Enabled := False;
    btnStop.Enabled := True;
    TimerPoll.Enabled := True;
    memLog.Lines.Add('Server started on port 8000');

  except
    on E: Exception do
    begin
      memLog.Lines.Add('Error: ' + E.Message);
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  TimerPoll.Enabled := False;
  if FHandle <> nil then
    Server_Stop(FHandle);
  FRunning := False;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  memLog.Lines.Add('Server stopped');
end;

procedure TForm1.TimerPollTimer(Sender: TObject);
begin
  try
    Server_Poll(FHandle, 100);
  except
    on E: Exception do
      DebugLogFmt('Poll exception: %s', [E.Message]);
  end;
end;

procedure TForm1.btnBroadcastClick(Sender: TObject);
var
  message: PWsMessage;
  msgContent: string;
begin
  if not FRunning then Exit;

  // 1. 为指针分配内存
  New(message);
  try
    // 2. 安全初始化字符串（假设 data 是 PChar 类型）
    msgContent := 'Hello from Lazarus!';
    message^.data := StrNew(PAnsiChar(msgContent));  // 分配字符串内存并复制内容
    message^.data_len := Length(msgContent);
    message^.binary := 0;

    // 3. 调用广播函数
    Server_WsBroadcast(FHandle, message);
    memLog.Lines.Add('Broadcast message sent');
  finally
    // 4. 释放内存（根据 Server_WsBroadcast 实现确认是否需要）
    StrDispose(message^.data);  // 释放字符串内存
    Dispose(message);           // 释放结构体内存
  end;
end;

end.
