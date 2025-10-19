# 1. RunnerJarCompatibilityCheckFast - Jar兼容性检查，快速模式，跳过解析代码并写入数据库

## 1.1. 使用 java-callgraph2 的配置参数

未使用

## 1.2. 使用 java-all-call-graph 的配置参数

### 1.2.1. 使用的主要的配置文件参数

以下为各配置参数文件有使用的配置参数

#### 1.2.1.1. _jacg_config/config.properties

|配置参数|说明|
|---|---|
|app.name|当前应用的调用关系写入数据库里的表名后缀|
|db.insert.batch.size|批量写入数据库时每次插入的数量|
|output.dir.flag|生成方法调用链文件的目录名中的标志|
|output.dir.name|生成方法调用链文件的目录名|
|output.root.path|生成方法调用链文件的根目录路径，以"/"或"\\"作为分隔符，末尾是否为分隔符不影响|
|text.to.excel.width.px|将生成的文本文件转换为 Excel 文件时的宽度像素|
|thread.num|并发处理线程数量/数据源连接池数量|

#### 1.2.1.2. _jacg_config/config_db.properties

|配置参数|说明|
|---|---|
|db.driver.name|数据库配置（仅当使用非H2数据库时需要指定），驱动类名|
|db.h2.file.path|H2数据库文件路径（仅当使用H2数据库时需要指定），后缀“.mv.db”支持指定，也支持不指定|
|db.password|数据库配置（仅当使用非H2数据库时需要指定），密码|
|db.table.suffix|数据库表后缀，默认使用空不需要指定|
|db.url|数据库配置（仅当使用非H2数据库时需要指定），URL|
|db.use.h2|是否使用H2数据库，true: 使用，false: 不使用|
|db.username|数据库配置（仅当使用非H2数据库时需要指定），用户名|
|slow.query.row.num|数据库慢查询监控，查询结果数量阈值，查询结果数量大于该值时记录慢查询日志|
|slow.query.switch|数据库慢查询监控开关，若开启，会在应用日志中打印慢查询相关信息，可搜索“出现慢查询”|
|slow.query.time|数据库慢查询监控，时间阈值，单位为毫秒，查询耗时大于该值时记录慢查询日志|

### 1.2.2. 使用的不区分顺序的其他配置参数

未使用

### 1.2.3. 使用的区分顺序的其他配置参数

|配置文件|说明|
|---|---|
|_jacg_jar_compatibility/other_h2_db_path.properties|(作用) 指定检查Jar兼容性时使用的其他H2数据库文件路径|

### 1.2.4. 使用的表达式配置参数

未使用

