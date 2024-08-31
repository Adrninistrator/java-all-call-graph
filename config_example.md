# 1. 说明

当前文件中的配置文件只有基本的说明，各配置文件的详细说明请打开对应的配置文件查看

每个配置参数可以通过配置文件或对应的枚举进行修改，效果相同。通过枚举修改配置参数的方式可参考[https://github.com/Adrninistrator/java-all-call-graph/blob/main/config_example.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/config_example.md)

# 2. 主要的配置信息

## 2.1. _jacg_config/config.properties

- 配置文件枚举类名

ConfigKeyEnum

|参数名称|参数枚举名|参数说明|参数值|
|---|---|---|---|
|app.name|CKE_APP_NAME|当前应用的调用关系写入数据库里的表名后缀|test_rbc|
|call.graph.output.detail|CKE_CALL_GRAPH_OUTPUT_DETAIL|生成调用链时的详细程度，参考 OutputDetailEnum 枚举，0: 最详细，1: 详细，2: 中等，3: 最简单|2|
|thread.num|CKE_THREAD_NUM|并发处理线程数量/数据源连接池数量|20|
|ignore.dup.callee.in.one.caller|CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER|生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否需要忽略|false|
|output.root.path|CKE_OUTPUT_ROOT_PATH|生成调用链文件的根目录路径，以"/"或"\\"作为分隔符，末尾是否为分隔符不影响（默认为当前目录）||
|output.dir.flag|CKE_OUTPUT_DIR_FLAG|生成调用链文件的目录名中的标志，完整目录名使用{app.name}{output.dir.flag}_{当前时间}，默认为空||
|output.dir.name|CKE_OUTPUT_DIR_NAME|生成调用链文件的目录名，非空时目录名使用当前值，为空时使用上一个参数说明的格式||
|db.insert.batch.size|CKE_DB_INSERT_BATCH_SIZE|批量写入数据库时每次插入的数量|1000|
|check.jar.file.updated|CKE_CHECK_JAR_FILE_UPDATED|检查jar包文件是否有更新|true|
|handle.get.set.field.relationship|CKE_HANDLE_GET_SET_FIELD_RELATIONSHIP|处理通过get/set方法关联的字段关联关系|false|
|call.graph.gen.json.caller|CKE_CALL_GRAPH_GEN_JSON_CALLER|生成向下的方法调用链时，是否需要输出JSON格式的内容|false|

## 2.2. _jacg_config/config_db.properties

- 配置文件枚举类名

ConfigDbKeyEnum

|参数名称|参数枚举名|参数说明|参数值|
|---|---|---|---|
|db.use.h2|CDKE_DB_USE_H2|是否使用H2数据库|false|
|db.h2.file.path|CDKE_DB_H2_FILE_PATH|H2数据库文件路径（仅当使用H2数据库时需要指定）|./build/jacg_h2db_rbc|
|db.driver.name|CDKE_DB_DRIVER_NAME|数据库配置（仅当使用非H2数据库时需要指定），驱动类名|com.mysql.cj.jdbc.Driver|
|db.url|CDKE_DB_URL|数据库配置（仅当使用非H2数据库时需要指定），URL|jdbc:mysql://127.0.0.1:3306/xxx?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true|
|db.username|CDKE_DB_USERNAME|数据库配置（仅当使用非H2数据库时需要指定），用户名|test|
|db.password|CDKE_DB_PASSWORD|数据库配置（仅当使用非H2数据库时需要指定），密码|xxx|
|db.table.suffix|CDKE_DB_TABLE_SUFFIX|数据库表后缀||

# 3. 不区分顺序的其他配置信息

## 3.1. _jacg_config/allowed_class_prefix.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX

- 参数说明

将java-callgraph2生成的方法调用关系文件写入数据库时使用的配置，需要处理的类名前缀

- 参数值

```
```

## 3.2. _jacg_config/method_class_4callee.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE

- 参数说明

生成调用指定类/方法的所有向上的方法完整调用链时的配置文件,指定需要生成的类名，或类名+方法前缀/代码行号

- 参数值

```
java.lang.System
test.callgraph.annotation.MethodWithAnnotation
test.callgraph.argument.TestArgument1
test.callgraph.argument.TestArgument2:testNoCaller(
test.callgraph.argument.TestArgument2:testNotExist(
test.callgraph.cyclecall.TestCycleCall1
test.callgraph.methodcall.TestMCCallee:20
test.callgraph.methodcall.TestMCCallee:run(
test.callgraph.methodcall.TestMCCallee:test
test.callgraph.spring.mvc.TestSpringController1:get(
```

## 3.3. _jacg_config/method_class_4caller.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER

- 参数说明

生成指定类/方法调用的所有向下的方法完整调用链时的配置文件，指定需要生成的类名+方法前缀/代码行号，可指定起始代码行号、结束代码行号

- 参数值

```
test.callgraph.annotation.CallMethodWithAnnotation:test1(
test.callgraph.annotation.MethodWithAnnotation
test.callgraph.argument.TestArgument1:test
test.callgraph.argument.TestArgument2:test(
test.callgraph.argument.TestArgument2:testNoCallee(
test.callgraph.argument.TestArgument2:testNotExist(
test.callgraph.argument.TestArgumentGenerics1
test.callgraph.cyclecall.TestCycleCall1
test.callgraph.extendcomplex.ChildClassA1
test.callgraph.extendcomplex.ChildClassA2
test.callgraph.extendcomplex.ChildClassB1
test.callgraph.extendcomplex.ChildClassB2
test.callgraph.extendcomplex.TestExtendComplex
test.callgraph.future.CallableImpl:call(
test.callgraph.interfaces.interfaces.InterfaceSuper1:testSuper1(
test.callgraph.interfaces.interfaces.InterfaceSuper2:testSuper2(
test.callgraph.interfacesgeneric.TestInterfacesGeneric1
test.callgraph.lambda.TestLambda
test.callgraph.methodcall.TestMCCaller:20
test.callgraph.spring.bean.use.complex.TestUseComplexService
test.callgraph.spring.mvc.TestSpringController1
```

## 3.4. _jacg_config/ignore_call_type.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CALL_TYPE

- 参数说明

生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的方法调用类型，指定 JavaCG2CallTypeEnum 枚举中的type

- 参数值

```
```

## 3.5. _jacg_config/ignore_method_type_4caller.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_TYPE_4CALLER

- 参数说明

生成指定类/方法调用的所有向下的方法完整调用链时的配置文件，指定忽略的方法类型，指定 JACGMethodTypeEnum 枚举中的type

- 参数值

```
```

## 3.6. _jacg_config/ignore_class_keyword.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CLASS_KEYWORD

- 参数说明

生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的类名关键字，可指定包名中的关键字，或类名中的关键字

- 参数值

```
```

## 3.7. _jacg_config/ignore_full_method_prefix.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_FULL_METHOD_PREFIX

- 参数说明

生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的完整方法前缀，可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数

- 参数值

```
```

## 3.8. _jacg_config/ignore_method_prefix.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_PREFIX

- 参数说明

生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的方法名前缀

- 参数值

```
```

## 3.9. _jacg_config/include_full_method_prefix.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_INCLUDE_FULL_METHOD_PREFIX

- 参数说明

生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定需要包含的完整方法前缀，优先级低于 allowed_class_prefix.properties、ignore_call_type.properties，优先级高于 ignore_class_keyword.properties、ignore_full_method_prefix.properties、ignore_method_prefix.properties，可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数

- 参数值

```
```

## 3.10. _jacg_business_data_type/business_data_type_show_4ee.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE

- 参数说明

生成向上的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据

- 参数值

```
```

## 3.11. _jacg_business_data_type/business_data_type_show_4er.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER

- 参数说明

生成向下的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据

- 参数值

```
```

## 3.12. _jacg_field_relationship/fr_eq_conversion_method.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFULE_FR_EQ_CONVERSION_METHOD

- 参数说明

在处理通过get/set方法的字段关联关系时使用，指定方法返回值与被调用对象或参数属于等值转换的方法，在java-callgraph2中使用

- 参数值

```
java.lang.Boolean:<init>=1
java.lang.Boolean:parseBoolean=1
java.lang.Boolean:valueOf=1
java.lang.Double:<init>=1
java.lang.Double:parseDouble=1
java.lang.Double:valueOf=1
java.lang.Float:<init>=1
java.lang.Float:parseFloat=1
java.lang.Float:valueOf=1
java.lang.Integer:<init>=1
java.lang.Integer:parseInt=1
java.lang.Integer:valueOf=1
java.lang.Long:<init>=1
java.lang.Long:parseLong=1
java.lang.Long:valueOf=1
java.lang.String:<init>=1
java.lang.String:trim=0
java.lang.String:valueOf=1
java.math.BigDecimal:<init>=1
java.math.BigDecimal:=0
java.math.BigDecimal:toString=0
java.math.BigDecimal:valueOf=1
org.apache.commons.lang.StringUtils:defaultIfBlank=1
org.apache.commons.lang.StringUtils:defaultIfEmpty=1
org.apache.commons.lang.StringUtils:defaultString=1
org.apache.commons.lang.StringUtils:trim=1
org.apache.commons.lang.math.NumberUtils:createBigDecimal=1
org.apache.commons.lang.math.NumberUtils:createBigInteger=1
org.apache.commons.lang.math.NumberUtils:createDouble=1
org.apache.commons.lang.math.NumberUtils:createFloat=1
org.apache.commons.lang.math.NumberUtils:createInteger=1
org.apache.commons.lang.math.NumberUtils:createLong=1
org.apache.commons.lang.math.NumberUtils:createNumber=1
org.apache.commons.lang3.StringUtils:defaultIfBlank=1
org.apache.commons.lang3.StringUtils:defaultIfEmpty=1
org.apache.commons.lang3.StringUtils:defaultString=1
org.apache.commons.lang3.StringUtils:trim=1
org.apache.commons.lang3.math.NumberUtils:createBigDecimal=1
org.apache.commons.lang3.math.NumberUtils:createBigInteger=1
org.apache.commons.lang3.math.NumberUtils:createDouble=1
org.apache.commons.lang3.math.NumberUtils:createFloat=1
org.apache.commons.lang3.math.NumberUtils:createInteger=1
org.apache.commons.lang3.math.NumberUtils:createLong=1
org.apache.commons.lang3.math.NumberUtils:createNumber=1
```

# 4. 区分顺序的其他配置信息

## 4.1. _jacg_config/jar_dir.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_JAR_DIR

- 参数说明

指定需要处理的jar包路径，或保存class、jar文件的目录路径

- 参数值

```
build/libs/test.jar
```

## 4.2. _jacg_find_stack_keyword/find_stack_keyword_4ee.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE

- 参数说明

生成向上的方法完整调用链文件后，再查找到起始方法的调用堆栈时，使用的关键字

- 参数值

```
!entry!
<init>
```

## 4.3. _jacg_find_stack_keyword/find_stack_keyword_4er.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER

- 参数说明

生成向下的方法完整调用链文件后，再查找从起始方法开始的调用堆栈时，使用的关键字

- 参数值

```
System:
java.lang.Deprecated
```

## 4.4. _jacg_extensions/code_parser.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER

- 参数说明

定义用于对代码进行解析的扩展类完整类名

- 参数值

```
```

## 4.5. _jacg_extensions/method_annotation_formatter.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER

- 参数说明

定义处理方法上的注解生成用于显示信息的扩展类完整类名

- 参数值

```
com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter
com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter
com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter
```

## 4.6. _jacg_extensions/manual_add_method_call1.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1

- 参数说明

定义人工添加方法调用关系的扩展类完整类名，处理特定的子类与实现类

- 参数值

```
```

## 4.7. _jacg_extensions/find_stack_keyword_filter.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER

- 参数说明

定义用于对完整调用链文件生成调用堆栈时使用的过滤器扩展类完整类名

- 参数值

```
```

## 4.8. _jacg_jar_diff_callee_graph/jar_diff_dir.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR

- 参数说明

指定新旧两个目录，比较其中的不同版本jar包的方法修改情况，以及新目录中修改方法的影响范围

- 参数值

```
```

