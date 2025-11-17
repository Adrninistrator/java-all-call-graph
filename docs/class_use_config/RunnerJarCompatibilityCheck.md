# 1. RunnerJarCompatibilityCheck

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
|当前使用参数值|test_rbc|
|参数默认值|jacg|
|参数枚举名|CKE_APP_NAME|

#### 1.2.1.2. db.insert.batch.size

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

#### 1.2.1.3. output.dir.flag

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

#### 1.2.1.4. output.dir.name

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

#### 1.2.1.5. output.root.path

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

#### 1.2.1.6. text.to.excel.width.px

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

#### 1.2.1.7. thread.num

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
|当前使用参数值||
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
|当前使用参数值|jdbc:mysql://x.x.x.x:3306/database?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true|
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
|当前使用参数值|true|
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
|当前使用参数值||
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

#### 1.2.4.1. _jacg_jar_compatibility/other_h2_db_path.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_COMPATIBILITY_OTHER_H2_DB_PATH

- 参数说明

(作用) 指定检查Jar兼容性时使用的其他H2数据库文件路径

(内容) 指定通过 RunnerWriteDbCompatibilityMode 类生成的H2数据库文件，包含JDK等jar文件中的类、方法信息等

(顺序) JDK的jar文件解析生成的H2数据库文件在最后指定

(示例) build/jacg_h2db_tomcat_compatibility_mode.mv.db

(示例) build/jacg_h2db_jdk_compatibility_mode.mv.db

(示例) D:/jacg_h2db_jdk_compatibility_mode.mv.db

(示例) D:/test/build/jar-diff-version-2

- 当前使用参数值

```
```

### 1.2.5. 使用的表达式配置参数

未使用

