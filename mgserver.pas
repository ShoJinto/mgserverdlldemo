unit mgServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DLLName = 'mgServer.dll';

type
  // 日志级别
  TLogLevel = (
    LOG_LEVEL_NONE = 0,
    LOG_LEVEL_ERROR,
    LOG_LEVEL_WARN,
    LOG_LEVEL_INFO,
    LOG_LEVEL_DEBUG,
    LOG_LEVEL_TRACE
  );

  // 日志目标
  TLogTarget = (
    LOG_TARGET_CONSOLE = 0,
    LOG_TARGET_FILE = 1
  );

  // 服务器句柄类型
  TServerHandle = Pointer;

  // 服务器配置结构体
  {$ALIGN 8}
  {$PACKRECORDS C}
  PServerConfig = ^TServerConfig;
  TServerConfig = record
    port: Integer;
    use_tls: Integer;
    enable_ws: Integer;
    cert_file: PAnsiChar;
    key_file: PAnsiChar;
    root_dir: PAnsiChar;
  end;

   // HTTP请求结构体
  THttpRequest = record
    method    : PAnsiChar;       // "GET", "POST" 等
    uri       : PAnsiChar;       // 请求URI
    uri_len   : SizeUInt;    // URI长度
    headers   : PAnsiChar;       // 可选请求头
    body      : PAnsiChar;       // 可选请求体
    body_len  : SizeUInt;    // 请求体长度
  end;
  PHttpRequest = ^THttpRequest;

  // HTTP响应结构体
  THttpResponse = record
    status_code: Integer;    // HTTP状态码，如200、404等
    headers    : PAnsiChar;      // 额外响应头（可为NULL）
    body       : PAnsiChar;      // 响应内容（可为NULL）
    body_len   : SizeUInt;   // 响应内容长度
  end;
  PHttpResponse = ^THttpResponse;

  // WebSocket消息结构体
  TWsMessage = record
    data       : PAnsiChar;      // 消息内容
    data_len   : SizeUInt;   // 消息长度
    binary     : Integer;    // 1=二进制，0=文本
    // 可扩展更多字段，如opcode、mask等
  end;
  PWsMessage = ^TWsMessage;


  THttpServeOpts = record
    root_dir: PAnsiChar;
    ssi_pattern: PAnsiChar;
    extra_headers: PAnsiChar;
    mime_types: PAnsiChar;
    page404: PAnsiChar;
  end;
  PHttpServeOpts = ^THttpServeOpts;

  // HTTP回调类型
  THttpCallback = procedure(server: TServerHandle; conn_id: Int64; request: PHttpRequest; response: PHttpResponse); stdcall;
  // WS回调类型
  TWsCallback = procedure(server: TServerHandle; conn_id: Int64; message: PWsMessage); stdcall;

  // 设置回调时的函数类型
  TSetCallbacks = function(handle: TServerHandle; http_cb: THttpCallback; ws_cb: TWsCallback; user_data: Pointer): Integer; stdcall;


  // 日志控制函数类型
  TSetLogLevel = procedure(enabled: Integer; level: TLogLevel); stdcall;
  TSetLogTarget = procedure(target: TLogTarget; filename: PAnsiChar); stdcall;

// DLL 函数声明
function Server_Create: TServerHandle; stdcall; external DLLName;
procedure Server_Destroy(handle: TServerHandle); stdcall; external DLLName;
function Server_SetConfig(handle: TServerHandle; const config: PServerConfig): Integer; stdcall; external DLLName;
function Server_SetCallbacks(handle: TServerHandle; http_cb: THttpCallback; ws_cb: TWsCallback; user_data: Pointer): Integer; stdcall; external DLLName;
function Server_Start(handle: TServerHandle): Integer; stdcall; external DLLName;
procedure Server_Stop(handle: TServerHandle); stdcall; external DLLName;
procedure Server_Poll(handle: TServerHandle; timeout_ms: Integer); stdcall; external DLLName;
procedure Server_SetLogLevel(enabled: Integer; level: TLogLevel); stdcall; external DLLName;
procedure Server_SetLogTarget(target: TLogTarget; filename: PAnsiChar); stdcall; external DLLName;
function Server_WsSendToOne(handle: TServerHandle; conn_id: Int64; message: PWsMessage): Integer; stdcall; external DLLName;
function Server_WsBroadcast(handle: TServerHandle; message: PWsMessage): Integer; stdcall; external DLLName;
function Server_HttpReply(handle: TServerHandle; conn_id: Int64; request:PHttpRequest; response:PHttpResponse): Integer; stdcall; external DLLName;
function Server_HttpServeFile(handle: TServerHandle; conn_id: Int64; file_path:PAnsiChar; extra_headers:PAnsiChar): Integer; stdcall; external DLLName;


implementation

end.
