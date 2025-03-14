# 1. 说明

当前文件中的配置文件只有基本的说明，各配置文件的详细说明请打开对应的配置文件查看

每个配置参数可以通过配置文件或对应的枚举进行修改，效果相同

# 2. 主要的配置信息

## 2.1. _jacg_config/config.properties

- 配置文件枚举类名

ConfigKeyEnum

|参数名称|参数枚举名|参数说明|参数值|
|---|---|---|---|
|app.name|CKE_APP_NAME|当前应用的调用关系写入数据库里的表名后缀，分隔符不能使用-，需要使用_|test_rbc|
|thread.num|CKE_THREAD_NUM|并发处理线程数量/数据源连接池数量（若超过了需要处理的任务数量，会使用任务数量作为线程数量）|20|
|db.insert.batch.size|CKE_DB_INSERT_BATCH_SIZE|批量写入数据库时每次插入的数量|1000|
|drop.or.truncate.table|CKE_DROP_OR_TRUNCATE_TABLE|在插入数据库表前，对表执行 DROP(false) 还是 TRUNCATE(true) 操作|false|
|output.root.path|CKE_OUTPUT_ROOT_PATH|生成调用链文件的根目录路径，以"/"或"\\"作为分隔符，末尾是否为分隔符不影响（默认为当前目录）||
|output.dir.flag|CKE_OUTPUT_DIR_FLAG|生成调用链文件的目录名中的标志，完整目录名使用{app.name}{output.dir.flag}_{当前时间}，默认为空||
|output.dir.name|CKE_OUTPUT_DIR_NAME|生成调用链文件的目录名，非空时目录名使用当前值，为空时使用上一个参数说明的格式||
|check.jar.file.updated|CKE_CHECK_JAR_FILE_UPDATED|生成调用链文件时，是否检查jar包文件有更新，若发现jar包文件内容发生变化则不生成，false:不检查，true:检查|true|
|call.graph.output.detail|CKE_CALL_GRAPH_OUTPUT_DETAIL|生成调用链时的详细程度 0: 最详细 完整类名+方法名+方法参数+返回类型 1: 详细 完整类名+方法名+方法参数 2: 中等 完整类名+方法名 3: 最简单 简单类名（对于同名类展示完整类名）+方法名|1|
|ignore.dup.callee.in.one.caller|CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER|生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否需要忽略，true:忽略，false:不忽略|false|
|call.graph.gen.separate.stack|CKE_CALL_GRAPH_GEN_SEPARATE_STACK|生成方法调用链时，是否需要为每个调用堆栈生成独立的文件，仅当 call.graph.output.detail=1 时支持|true|
|call.graph.gen.json.caller|CKE_CALL_GRAPH_GEN_JSON_CALLER|生成向下的方法调用链时，是否需要输出JSON格式的内容|false|
|gen.call.graph.num.limit|CKE_GEN_CALL_GRAPH_NUM_LIMIT|生成调用链文件时，每个方法允许生成的方法调用数量限制，默认为0，小于等于0代表不限制|0|
|call.graph.write.to.file|CKE_CALL_GRAPH_WRITE_TO_FILE|生成调用链时，是否需要将调用链数据写入文件|true|
|call.graph.return.in.memory|CKE_CALL_GRAPH_RETURN_IN_MEMORY|生成调用链时，是否需要在内存中返回调用链数据 不能与 call.graph.write.to.file 开关同时设置为false _jacg_config/method_class_4caller.properties 或 _jacg_config/method_class_4callee.properties 配置文件中只能一个需要处理的方法 设置为 true 时，可通过 gen.call.graph.num.limit 开关设置允许生成的方法调用数量限制|false|
|call.graph.file.short.mode|CKE_CALL_GRAPH_FILE_SHORT_MODE|生成调用链时，文件名是否使用更短的模式，以避免超过Windows文件系统支持的长度 若是，则文件名仅包含对应方法的HASH+长度；若否，则文件名还会包含方法的唯一类名及方法名|false|

## 2.2. _jacg_config/config_db.properties

- 配置文件枚举类名

ConfigDbKeyEnum

|参数名称|参数枚举名|参数说明|参数值|
|---|---|---|---|
|db.use.h2|CDKE_DB_USE_H2|是否使用H2数据库，true: 使用，false: 不使用|false|
|db.h2.file.path|CDKE_DB_H2_FILE_PATH|H2数据库文件路径（仅当使用H2数据库时需要指定），示例：./build/jacg_h2db，不需要指定“.mv.db”|./build/jacg_h2db_rbc|
|db.driver.name|CDKE_DB_DRIVER_NAME|数据库配置（仅当使用非H2数据库时需要指定），驱动类名|com.mysql.cj.jdbc.Driver|
|db.url|CDKE_DB_URL|数据库配置（仅当使用非H2数据库时需要指定），URL 使用MySQL时，url需要指定rewriteBatchedStatements=true，开启批量插入，提高效率，默认未开启|jdbc:mysql://127.0.0.1:-/-?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true|
|db.username|CDKE_DB_USERNAME|数据库配置（仅当使用非H2数据库时需要指定），用户名|-|
|db.password|CDKE_DB_PASSWORD|数据库配置（仅当使用非H2数据库时需要指定），密码|-|
|db.table.suffix|CDKE_DB_TABLE_SUFFIX|数据库表后缀，默认使用空不需要指定||

# 3. 不区分顺序的其他配置信息

## 3.1. _jacg_config/method_class_4callee.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE

- 参数说明

(作用) 生成调用指定类的所有向上的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定需要生成的类名，或类名+方法前缀/代码行号

(格式1) {类名}

(格式2) {类名}:{方法名}

(格式3) {类名}:{方法名+参数}

(格式4) {类名}:{代码行号}

(格式说明) 假如只指定了类名，没有指定方法名或代码行号，则处理指定类的全部方法；假如指定了方法名或代码行号，则处理指定类的对应方法

(格式说明) {类名}可指定简单类名或完整类名；若存在同名类，则需要指定完整类名

(格式说明) {代码行号}可指定某个方法对应的任意代码行号，如C:f1()方法代码起止行号范围为{100,203}，则可指定以上范围的任意数字代表需要处理C:f1()方法

(格式说明) 假如某个方法是接口中未实现的方法或抽象方法，则不支持指定代码行号的方式，需要指定方法前缀

(示例)

Test1

com.test.Test1

Test1:test

Test1:test(

Test1:test(java.lang.String)

Test1:234

- 参数值

```
java.lang.System
test.callgraph.annotation.MethodWithAnnotation
test.callgraph.cyclecall.TestCycleCall1
test.callgraph.empty.TestEmptyClass1
test.callgraph.empty.TestNoMethodClass1
test.callgraph.methodargument.TestArgument1
test.callgraph.methodargument.TestArgument2:testNoCaller(
test.callgraph.methodargument.TestArgument2:testNotExist(
test.callgraph.methodcall.TestMCCallee:20
test.callgraph.methodcall.TestMCCallee:run(
test.callgraph.methodcall.TestMCCallee:test
test.callgraph.spring.mvc.TestSpringController1:get(
```

## 3.2. _jacg_config/method_class_4caller.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER

- 参数说明

(作用) 生成指定类调用的所有向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定需要生成的类名+方法前缀/代码行号，可指定起始代码行号、结束代码行号

(格式1) {类名}

(格式2) {类名}:{方法名} {起始代码行号}-{结束代码行号}

(格式3) {类名}:{方法名+参数} {起始代码行号}-{结束代码行号}

(格式4) {类名}:{代码行号} {起始代码行号}-{结束代码行号}

(格式说明) 假如仅指定了{类名}，则会处理对应类的所有方法

(格式说明) {类名}可指定简单类名或完整类名；若存在同名类，则需要指定完整类名

(格式说明) 若存在同名方法，则需要指定方法参数以区分，或者在o_g4caller_entry_method_ignore_prefix.properties中指定需要忽略的方法前缀

(格式说明) {起始代码行号}-{结束代码行号}为可选参数，若不指定则输出指定的整个方法向下的方法完整调用链；若指定则输出方法指定行号范围内向下的方法完整调用链，即 >= 起始代码行号 且 <= 结束代码行号的范围

(格式说明) {代码行号}可指定某个方法对应的任意代码行号，如C:f1()方法代码起止行号范围为{100,203}，则可指定以上范围的任意数字代表需要处理C:f1()方法

(示例)

Test1

com.test.Test1

Test1:func1 139-492

Test1:func1(

Test1:func1(java.lang.String)

com.test.Test1:func1 395-1358

com.test.Test1:func1(

com.test.Test1:func1(java.lang.String)

Test1:139

Test1:139 139-492

- 参数值

```
test.callgraph.annotation.CallMethodWithAnnotation:test1(
test.callgraph.annotation.MethodWithAnnotation
test.callgraph.cyclecall.TestCycleCall1
test.callgraph.empty.TestEmptyClass1
test.callgraph.empty.TestNoMethodClass1
test.callgraph.extendcomplex.ChildClassA1
test.callgraph.extendcomplex.ChildClassA2
test.callgraph.extendcomplex.ChildClassB1
test.callgraph.extendcomplex.ChildClassB2
test.callgraph.extendcomplex.TestExtendComplex
test.callgraph.future.CallableImpl:call(
test.callgraph.interfaces.interfaces.InterfaceSuper1:testSuper1(
test.callgraph.interfaces.interfaces.InterfaceSuper2:testSuper2(
test.callgraph.interfacesdefault.TestUseInterfaceDefault1
test.callgraph.interfacesgeneric.TestInterfacesGeneric1
test.callgraph.lambda.TestLambda
test.callgraph.methodargument.TestArgument1:test
test.callgraph.methodargument.TestArgument2:test(
test.callgraph.methodargument.TestArgument2:testNoCallee(
test.callgraph.methodargument.TestArgument2:testNotExist(
test.callgraph.methodargument.TestArgumentGenerics1
test.callgraph.methodcall.TestMCCaller:20
test.callgraph.spring.bean.use.complex.TestUseComplexService
test.callgraph.spring.mvc.TestSpringController1
```

## 3.3. _jacg_config/ignore_call_type.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CALL_TYPE

- 参数说明

(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定忽略的方法调用类型

(格式) 指定 JavaCG2CallTypeEnum 枚举中的type

(示例)

_ITF

_SCC

- 参数值

```
```

## 3.4. _jacg_config/ignore_method_type_4caller.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_TYPE_4CALLER

- 参数说明

(作用) 生成指定类/方法调用的所有向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定忽略的方法类型

(格式) 指定 JACGMethodTypeEnum 枚举中的type

(示例)

dto.get.set

- 参数值

```
```

## 3.5. _jacg_config/ignore_class_keyword.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CLASS_KEYWORD

- 参数说明

(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定忽略的类名关键字

(格式) 可指定包名中的关键字，或类名中的关键字

(示例)

.dto.

.entity.

Enum

Constant

- 参数值

```
```

## 3.6. _jacg_config/ignore_full_method_prefix.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_FULL_METHOD_PREFIX

- 参数说明

(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定忽略的完整方法前缀

(格式) 可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数

(示例)

com.test

com.test.Test1

com.test.Test1:func1

com.test.Test1:func1(

com.test.Test1:func1(java.lang.String)

- 参数值

```
```

## 3.7. _jacg_config/ignore_method_prefix.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_PREFIX

- 参数说明

(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定忽略的方法名前缀

(示例)

func1

func1(

func1()

func1(java.lang.String)

toString()

hashCode()

equals(java.lang.Object)

<init>(

<clinit>(

name()

clone()

- 参数值

```
```

## 3.8. _jacg_config/include_full_method_prefix.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFUSE_INCLUDE_FULL_METHOD_PREFIX

- 参数说明

(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）

(内容) 指定需要包含的完整方法前缀

(优先级低于) allowed_class_prefix.properties、ignore_call_type.properties

(优先级高于) ignore_class_keyword.properties、ignore_full_method_prefix.properties、ignore_method_prefix.properties

(格式) 可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数

(示例)

com.test

com.test.Test1

com.test.Test1:func1

com.test.Test1:func1(

com.test.Test1:func1(java.lang.String)

- 参数值

```
```

## 3.9. _jacg_business_data_type/business_data_type_show_4ee.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE

- 参数说明

生成向上的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据

默认的业务功能数据类型参考 DefaultBusinessDataTypeEnum 枚举类，supportEe=true的type

method_call_info

method_arg_generics_type

method_return_generics_type

- 参数值

```
```

## 3.10. _jacg_business_data_type/business_data_type_show_4er.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER

- 参数说明

生成向下的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据

默认的业务功能数据类型参考 DefaultBusinessDataTypeEnum 枚举类，supportEr=true的type

method_call_info

method_arg_generics_type

method_return_generics_type

mybatis_mysql_table

mybatis_mysql_write_table

- 参数值

```
```

# 4. 区分顺序的其他配置信息

## 4.1. _jacg_find_stack_keyword/find_stack_keyword_4ee.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE

- 参数说明

生成向上的方法完整调用链文件后，再查找到起始方法的调用堆栈时，使用的关键字（每行指定一项配置，可指定多行）

- 参数值

```
!entry!
<init>
```

## 4.2. _jacg_find_stack_keyword/find_stack_keyword_4er.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER

- 参数说明

生成向下的方法完整调用链文件后，再查找从起始方法开始的调用堆栈时，使用的关键字（每行指定一项配置，可指定多行）

- 参数值

```
System:
java.lang.Deprecated
```

## 4.3. _jacg_extensions/code_parser.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER

- 参数说明

在此定义用于对代码进行解析的扩展类完整类名（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.javacg2.extensions.codeparser.CodeParserInterface 接口的实现类

- 参数值

```
```

## 4.4. _jacg_extensions/method_annotation_formatter.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER

- 参数说明

在此定义处理方法上的注解生成用于显示信息的扩展类完整类名（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.annotation.formatter.AbstractAnnotationFormatter 类的子类

假如需要显示方法上的注解，请将默认的方法注解处理类 DefaultAnnotationFormatter 在最后指定

假如不需要显示方法上的注解，请只指定不显示方法注解的处理类 HideAnnotationFormatter

com.adrninistrator.jacg.annotation.formatter.HideAnnotationFormatter

- 参数值

```
com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter
com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter
com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter
```

## 4.5. _jacg_extensions/manual_add_method_call1.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1

- 参数说明

在此定义人工添加方法调用关系的扩展类完整类名，处理特定的子类与实现类（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1 类的子类

- 参数值

```
```

## 4.6. _jacg_extensions/find_stack_keyword_filter.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER

- 参数说明

在此定义用于对完整调用链文件生成调用堆栈时使用的过滤器扩展类完整类名，若未指定则使用配置参数中的关键字（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.extensions.findstackfilter.FindStackKeywordFilterInterface 接口的实现类

- 参数值

```
```

## 4.7. _jacg_extensions/javacg2_method_call_extensions.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL

- 参数说明

java-callgraph2 组件在处理方法调用时的扩展类（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.javacg2.extensions.methodcall.JavaCG2MethodCallExtensionInterface 接口的实现类

- 参数值

```
```

## 4.8. _jacg_extensions/jacg_method_call_extensions.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL

- 参数说明

java-all-call-graph 组件在处理方法调用时的扩展类（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.extensions.methodcall.AbstractJACGMethodCallExtension 类的子类

- 参数值

```
```

## 4.9. _jacg_jar_diff_callee_graph/jar_diff_dir.properties

- 配置文件枚举类名与枚举名

OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR

- 参数说明

(作用) 指定新旧两个目录，比较其中的不同版本jar包的方法修改情况，以及新目录中修改方法的影响范围

(内容) 第1行指定旧目录路径，第2行指定新目录路径

(示例) build/libs1

(示例) build/libs2

(示例) D:/test/build/libs1

(示例) D:/test/build/libs2

- 参数值

```
```

