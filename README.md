# MongooseTest 项目

基于Pascal语言开发的Web服务器应用，使用Mongoose嵌入式库实现HTTP服务。
主要是测试[mgserver.dll](https://github.com/shojinto/mgserverdll)

## 功能特性
- 嵌入式HTTP服务器
- 静态文件托管（webroot目录）
- Windows平台DLL依赖集成

## 快速启动
1. 使用Lazarus IDE打开`project1.lpi`
2. 编译并运行项目
3. 访问 http://localhost:8000 

## 项目结构
```
├── lib*.dll          # 运行时依赖库
├── mgserver.pas      # Mongoose服务封装单元
├── unit1.pas         # 主窗体单元
├── webroot/          # 网站根目录
└── static/           # 静态资源目录
```

## 依赖环境
- Lazarus IDE 2.2.6+
- Windows 10/11 64位系统