# 1. FindCallStackTrace-4er

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

#### 1.2.1.3. call.graph.gen.json.caller

- 参数说明

```
生成向下的方法完整调用链文件时，是否输出JSON格式的方法完整调用链文件
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_CALL_GRAPH_GEN_JSON_CALLER|

#### 1.2.1.4. call.graph.gen.stack.other.forms

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

#### 1.2.1.5. call.graph.output.detail

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
|当前使用参数值|0|
|参数默认值|1|
|参数枚举名|CKE_CALL_GRAPH_OUTPUT_DETAIL|

#### 1.2.1.6. call.graph.return.in.memory

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

#### 1.2.1.7. call.graph.write.to.file

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

#### 1.2.1.8. db.insert.batch.size

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

#### 1.2.1.9. el.debug.mode

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

#### 1.2.1.10. gen.call.graph.depth.limit

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

#### 1.2.1.11. gen.call.graph.num.limit

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

#### 1.2.1.12. ignore.dup.callee.in.one.caller

- 参数说明

```
生成向下的方法完整调用链文件时，在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否忽略
true: 忽略，false: 不忽略
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER|

#### 1.2.1.13. output.dir.flag

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

#### 1.2.1.14. output.dir.name

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

#### 1.2.1.15. output.root.path

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

#### 1.2.1.16. text.to.excel.width.px

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

#### 1.2.1.17. thread.num

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

#### 1.2.3.1. _jacg_gen_all_call_graph/method_class_4caller.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER

- 参数说明

(作用) 生成指定类调用或方法向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定需要生成的类名+方法前缀/代码行号，可指定起始代码行号、结束代码行号

(格式1) {类名}

(格式2) {类名}:{方法名} {起始代码行号}-{结束代码行号}

(格式3) {类名}:{方法名}({参数}) {起始代码行号}-{结束代码行号}

(格式3) {类名}:{方法名}({参数}):{方法返回类型} {起始代码行号}-{结束代码行号}

(格式4) {类名}:{代码行号} {起始代码行号}-{结束代码行号}

(格式说明) 假如仅指定了{类名}，则会处理对应类的所有方法

(格式说明) {类名}可指定简单类名或完整类名；若存在同名类，则需要指定完整类名

(格式说明) 若存在同名方法，则需要指定方法参数以区分

(格式说明) {起始代码行号}-{结束代码行号}为可选参数，若不指定则输出指定的整个方法向下的方法完整调用链；若指定则输出方法指定行号范围内向下的方法完整调用链，即 >= 起始代码行号 且 <= 结束代码行号的范围

(格式说明) {代码行号}可指定某个方法对应的任意代码行号，如C:f1()方法代码起止行号范围为{100,203}，则可指定以上范围的任意数字代表需要处理C:f1()方法

(格式说明) {方法返回类型}需要指定完整类型

(示例)

Test1

com.test.Test1

Test1:func1 139-492

Test1:func1(

Test1:func1(java.lang.String)

com.test.Test1:func1 395-1358

com.test.Test1:func1(

com.test.Test1:func1(java.lang.String)

com.test.Test1:func1(java.lang.String):java.lang.String

Test1:139

Test1:139 139-492

- 当前使用参数值

```
test.callgraph.methodcall.TestMCCaller
```

#### 1.2.3.2. _jacg_gen_all_call_graph/caller_graph_callee_arg_type_polymorphism.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseSetEnum.OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM

- 参数说明

(作用) 生成向下方法完整调用链时，指定哪些方法参数作为被调用对象涉及多态时的类型替换（每行指定一项配置，可指定多行）

(作用) 即对被调用类型使用实际传入的子类类型替换方法参数定义的父类类型

(前提) 使用 java-callgraph2 组件解析方法调用时需要将 parse.method.call.type.value 参数值设置为 true

(限制) 仅支持获取被调用方法被直接调用（没有嵌套多层调用）时的子类类型

(限制) 仅支持调用被调用方法时使用一种子类类型，不支持多种

(格式1) {参数作为被调用对象时需要替换被调用类型的完整方法}={对应的参数序号，从1开始}

(格式2) {参数作为被调用对象时需要替换被调用类型的完整方法}:{方法返回类型}={对应的参数序号，从1开始}

(示例)

a.b.C:f1(int,a.b.Super)=2

a.b.C:f1(int,a.b.Super):java.lang.String=1

- 当前使用参数值

```
```

#### 1.2.3.3. _jacg_business_data_type/business_data_type_show_4er.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER

- 参数说明

生成向下的方法完整调用链时，指定默认支持的业务功能数据需要显示哪些类型。若不指定则不显示

默认支持的业务功能数据类型参考 DefaultBusinessDataTypeEnum 枚举类，supportEr=true的type

method_call_info
method_arg_generics_type
method_return_generics_type
mybatis_mysql_table
mybatis_mysql_write_table

- 当前使用参数值

```
```

### 1.2.4. 使用的区分顺序的其他配置参数

#### 1.2.4.1. _jacg_find_stack_keyword/find_stack_keyword_4er.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER

- 参数说明

生成向下的方法完整调用链文件后，从最顶层调用方法开始向下查找包含指定关键字的方法的调用堆栈时，使用的关键字

每行指定一个关键字，可指定多行

若向下的方法完整调用链文件的某行包含当前配置文件中的某个关键字，则认为找到需要生成调用堆栈的方法

- 当前使用参数值

```
System:
java.lang.Deprecated
```

#### 1.2.4.2. _jacg_extensions/method_annotation_formatter.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER

- 参数说明

定义在生成方法完整调用链时，显示方法注解信息的扩展类完整类名（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.annotation.formatter.AbstractAnnotationFormatter 类的子类

假如需要显示方法上的注解，请将默认的方法注解处理类 DefaultAnnotationFormatter 在最后指定

假如不需要显示方法上的注解，请将当前参数值置为空

- 当前使用参数值

```
com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter
com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter
com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter
```

#### 1.2.4.3. _jacg_extensions/find_stack_keyword_filter.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER

- 参数说明

在此定义用于对完整调用链文件生成调用堆栈时使用的过滤器扩展类完整类名，若未指定则使用配置参数中的关键字（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.extensions.findstackfilter.FindStackKeywordFilterInterface 接口的实现类

- 当前使用参数值

```
```

### 1.2.5. 使用的表达式配置参数

#### 1.2.5.1. _jacg_gen_all_call_graph/gen_call_graph_ignore_method_call.av

- 配置文件枚举类名与常量名

ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL

- 参数说明

指定生成方法完整调用链时是否跳过解析特定的方法调用，支持通过方法调用类型、调用方法或被调用方法等判断

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|method_call_type|String|方法调用类型<br>参考 JavaCG2CallTypeEnum 类|VIR INT SPE STA DYN _SPR_ACT_I _SPR_ACT_C _ACT_I _ACT_C _ITF _BSM _LM _RIR1 _RIR2 _CIC1 _CIC2 _TCID1 _TCID2 _TCWRID1 _TCWRID2 _TSR _SCC _CCS _CCS_SPE _CCS_I _CCID _ICID _MA _MAA ILLEGAL|
|er_class_name|String|调用方完整类名|a.b.Class1|
|er_package_name|String|调用方完整包名<br>不会以.结束|a.b|
|er_simple_class_name|String|调用方简单类名|Class1|
|er_method_name|String|调用方方法名<br>不包括括号及方法参数|method1|
|er_method_arg_num|int|调用方方法参数数量|0<br>1|
|er_full_method|String|调用方完整方法<br>包括括号及方法参数|a.b.Class1:method1(int)|
|ee_class_name|String|被调用方完整类名|a.b.Class1|
|ee_package_name|String|被调用方完整包名<br>不会以.结束|a.b|
|ee_simple_class_name|String|被调用方简单类名|Class1|
|ee_method_name|String|被调用方方法名<br>不包括括号及方法参数|method1|
|ee_method_arg_num|int|被调用方方法参数数量|0<br>1|
|ee_full_method|String|被调用方完整方法<br>包括括号及方法参数|a.b.Class1:method1(int)|
|flags_enum|List<String>|方法调用标志枚举<br>指定 MethodCallFlagsEnum 类的常量名称，代表需要处理包含/不包含哪些标志的方法调用<br>如 MCFE_ER_METHOD_ANNOTATION|MCFE_ER_METHOD_ANNOTATION	调用方法有注解<br>MCFE_EE_METHOD_ANNOTATION	被调用方法有注解<br>MCFE_METHOD_CALL_INFO	存在方法调用信息<br>MCFE_EE_ARGS_WITH_GENERICS_TYPE	被调用方法参数存在泛型类型<br>MCFE_ER_ARGS_WITH_GENERICS_TYPE	调用方法参数存在泛型类型<br>MCFE_EE_MYBATIS_MAPPER	被调用方法为Mybatis Mapper<br>MCFE_EE_MYBATIS_MAPPER_WRITE	被调用方法为Mybatis写数据库的Mapper方法<br>MCFE_EE_BUSINESS_DATA	被调用方法存在业务功能数据<br>MCFE_EE_RETURN_WITH_GENERICS_TYPE	被调用方法返回存在泛型类型<br>MCFE_ER_RETURN_WITH_GENERICS_TYPE	调用方法返回存在泛型类型<br>MCFE_EE_DTO_GET_SET_METHOD	被调用方法属于dto的get/set方法|

- 当前使用参数值

```

```

