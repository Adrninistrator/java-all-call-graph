# 1. RunnerGenJarDiffCalleeGraph-skip_write_db

## 1.1. 使用 java-callgraph2 的配置参数

未使用

## 1.2. 使用 java-all-call-graph 的配置参数

以下为各配置参数文件有使用的配置参数

### 1.2.1. _jacg_config/config.properties

#### 1.2.1.1. app.name

- 参数说明

```
当前应用的调用关系写入数据库里的表名后缀
使用 H2 数据库时，固定为“jacg”
不能使用-作为分隔符，可使用_
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|是|
|当前使用参数值|jar_diff_diff_same|
|参数默认值|jacg|
|参数枚举名|CKE_APP_NAME|

#### 1.2.1.2. call.graph.file.short.mode

- 参数说明

```
生成方法完整调用链文件时，文件名是否使用更短的模式，以避免超过Windows文件系统支持的长度
若是，则文件名仅包含对应方法的HASH+长度；若否，则文件名还会包含方法的唯一类名及方法名
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_CALL_GRAPH_FILE_SHORT_MODE|

#### 1.2.1.3. call.graph.gen.stack.other.forms

- 参数说明

```
使用 FindCallStackTrace 类生成方法调用堆栈文件时，是否生成其他形式的调用堆栈文件，仅当 call.graph.output.detail=0 时支持
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|false|
|参数枚举名|CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS|

#### 1.2.1.4. call.graph.output.detail

- 参数说明

```
生成方法完整调用链文件时的详细程度
0: 最详细 完整类名+方法名+方法参数+返回类型
1: 详细 完整类名+方法名+方法参数
2: 中等 完整类名+方法名
3: 最简单 简单类名（对于同名类展示完整类名）+方法名
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|是|
|当前使用参数值|1|
|参数默认值|1|
|参数枚举名|CKE_CALL_GRAPH_OUTPUT_DETAIL|

#### 1.2.1.5. call.graph.return.in.memory

- 参数说明

```
生成方法完整调用链文件时，是否在内存中返回调用链数据
不能与 call.graph.write.to.file 开关同时设置为false
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_CALL_GRAPH_RETURN_IN_MEMORY|

#### 1.2.1.6. call.graph.write.to.file

- 参数说明

```
生成方法完整调用链文件时，是否将调用链数据写入文件
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CKE_CALL_GRAPH_WRITE_TO_FILE|

#### 1.2.1.7. db.insert.batch.size

- 参数说明

```
批量写入数据库时每次插入的数量
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|是|
|当前使用参数值|1000|
|参数默认值|1000|
|参数枚举名|CKE_DB_INSERT_BATCH_SIZE|

#### 1.2.1.8. el.debug.mode

- 参数说明

```
是否开启表达式执行调试模式，若开启会在应用日志中输出表达式执行时的详细信息
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_EL_DEBUG_MODE|

#### 1.2.1.9. gen.call.graph.depth.limit

- 参数说明

```
生成方法完整调用链文件时，允许生成的方法完整调用链深度限制，默认为0，小于等于0代表不限制
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|否|
|当前使用参数值|0|
|参数默认值|0|
|参数枚举名|CKE_GEN_CALL_GRAPH_DEPTH_LIMIT|

#### 1.2.1.10. gen.call.graph.num.limit

- 参数说明

```
生成方法完整调用链文件时，每个方法允许生成的方法调用数量限制，默认为0，小于等于0代表不限制
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|否|
|当前使用参数值|0|
|参数默认值|0|
|参数枚举名|CKE_GEN_CALL_GRAPH_NUM_LIMIT|

#### 1.2.1.11. output.dir.flag

- 参数说明

```
生成方法完整调用链文件的目录名中的标志
完整目录名使用{app.name}{output.dir.flag}_{当前时间}
默认为空
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值||
|参数默认值||
|参数枚举名|CKE_OUTPUT_DIR_FLAG|

#### 1.2.1.12. output.dir.name

- 参数说明

```
生成方法完整调用链文件的目录名
非空时目录名使用当前参数值
默认为空，使用 output.dir.flag 参数说明的格式
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值||
|参数默认值||
|参数枚举名|CKE_OUTPUT_DIR_NAME|

#### 1.2.1.13. output.root.path

- 参数说明

```
生成方法完整调用链文件的根目录路径，以"/"或"\\"作为分隔符，末尾是否为分隔符不影响
默认为当前目录
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值||
|参数默认值||
|参数枚举名|CKE_OUTPUT_ROOT_PATH|

#### 1.2.1.14. text.to.excel.width.px

- 参数说明

```
将生成的文本文件转换为 Excel 文件时的宽度像素
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|否|
|当前使用参数值|1920|
|参数默认值|1920|
|参数枚举名|CKE_TEXT_TO_EXCEL_WIDTH_PX|

#### 1.2.1.15. thread.num

- 参数说明

```
并发处理线程数量/数据源连接池数量
若超过了需要处理的任务数量，会使用任务数量作为线程数量
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|是|
|当前使用参数值|20|
|参数默认值|20|
|参数枚举名|CKE_THREAD_NUM|

### 1.2.2. _jacg_config/config_db.properties

#### 1.2.2.1. db.driver.name

- 参数说明

```
数据库配置（仅当使用非H2数据库时需要指定），驱动类名
MySQL 使用 com.mysql.cj.jdbc.Driver
PostgreSQL 使用 org.postgresql.Driver
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|是|
|当前使用参数值|com.mysql.cj.jdbc.Driver|
|参数默认值|com.mysql.cj.jdbc.Driver|
|参数枚举名|CDKE_DB_DRIVER_NAME|

#### 1.2.2.2. db.h2.file.path

- 参数说明

```
H2数据库文件路径（仅当使用H2数据库时需要指定），后缀“.mv.db”支持指定，也支持不指定
需要使用绝对路径或相对路径。若指定为相对路径，则需要以 ./ 开头
示例：D:/build/jacg_h2db.mv.db
示例：./build/jacg_h2db.mv.db
示例：D:/build/jacg_h2db
示例：./build/jacg_h2db
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|是|
|当前使用参数值|./build/jacg_h2db_rbc|
|参数默认值|./build/jacg_h2db|
|参数枚举名|CDKE_DB_H2_FILE_PATH|

#### 1.2.2.3. db.password

- 参数说明

```
数据库配置（仅当使用非H2数据库时需要指定），密码
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|是|
|当前使用参数值|123456|
|参数默认值||
|参数枚举名|CDKE_DB_PASSWORD|

#### 1.2.2.4. db.table.suffix

- 参数说明

```
数据库表后缀，默认使用空不需要指定
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值||
|参数默认值||
|参数枚举名|CDKE_DB_TABLE_SUFFIX|

#### 1.2.2.5. db.url

- 参数说明

```
数据库配置（仅当使用非H2数据库时需要指定），URL
使用 MySQL 时，url需要指定 rewriteBatchedStatements=true ，开启批量插入，提高效率，默认未开启
使用 PostgreSQL 时，需要指定通过 currentSchema 指定 schema ，如 jdbc:postgresql://x.x.x.x:5432/database?currentSchema=schema&useUnicode=true&characterEncoding=UTF-8
使用 PostgreSQL 时，假如偶尔出现异常“org.postgresql.util.PSQLException: 尝试连线已失败。”，可以在 URL 中指定“sslmode=disable”以禁用 SSL
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|是|
|当前使用参数值|jdbc:mysql://127.0.0.1:3307/testdb?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true|
|参数默认值|jdbc:mysql://x.x.x.x:3306/database?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true|
|参数枚举名|CDKE_DB_URL|

#### 1.2.2.6. db.use.h2

- 参数说明

```
是否使用H2数据库，true: 使用，false: 不使用
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|是|
|当前使用参数值|false|
|参数默认值|true|
|参数枚举名|CDKE_DB_USE_H2|

#### 1.2.2.7. db.username

- 参数说明

```
数据库配置（仅当使用非H2数据库时需要指定），用户名
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|是|
|当前使用参数值|test|
|参数默认值||
|参数枚举名|CDKE_DB_USERNAME|

#### 1.2.2.8. slow.query.row.num

- 参数说明

```
数据库慢查询监控，查询结果数量阈值，查询结果数量大于该值时记录慢查询日志
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|否|
|当前使用参数值|5000|
|参数默认值|5000|
|参数枚举名|CDKE_SLOW_QUERY_ROW_NUM|

#### 1.2.2.9. slow.query.switch

- 参数说明

```
数据库慢查询监控开关，若开启，会在应用日志中打印慢查询相关信息，可搜索“出现慢查询”
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CDKE_SLOW_QUERY_SWITCH|

#### 1.2.2.10. slow.query.time

- 参数说明

```
数据库慢查询监控，时间阈值，单位为毫秒，查询耗时大于该值时记录慢查询日志
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|否|
|当前使用参数值|200|
|参数默认值|200|
|参数枚举名|CDKE_SLOW_QUERY_TIME|

### 1.2.3. 使用的不区分顺序的其他配置参数

未使用

### 1.2.4. 使用的区分顺序的其他配置参数

未使用

### 1.2.5. 使用的表达式配置参数

#### 1.2.5.1. _jacg_jar_diff/jar_diff_gen_all_call_graph_ignore_callee.av

- 配置文件枚举类名与常量名

ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLEE

- 参数说明

JarDiff获得发生变化的方法的影响范围时（生成向上的方法完整调用链及调用堆栈），指定发生变化的方法中，需要忽略的方法

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|class_name|String|完整类名|a.b.Class1|
|package_name|String|完整包名<br>不会以.结束|a.b|
|simple_class_name|String|简单类名|Class1|
|method_name|String|方法名<br>不包括括号及方法参数|method1|
|method_arg_num|int|方法参数数量|0<br>1|
|full_method|String|完整方法|a.b.Class1:f1()<br>a.b.Class1:f2(int,java.lang.String)|

- 当前使用参数值

```

```

