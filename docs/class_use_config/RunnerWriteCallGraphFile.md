# 1. RunnerWriteCallGraphFile

## 1.1. 使用 java-callgraph2 的配置参数

以下为各配置参数文件有使用的配置参数

### 1.1.1. _javacg2_config/config.properties

#### 1.1.1.1. analyse.field.relationship

- 参数说明

```
是否需要分析dto的字段之间通过get/set方法的关联关系，仅当parse.method.call.type.value参数为true时才可以生效
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|false|
|参数枚举名|CKE_ANALYSE_FIELD_RELATIONSHIP|

#### 1.1.1.2. continue.when.error

- 参数说明

```
解析方法出现异常时，是否要继续
若开启后在出现异常时不会抛出异常，会继续执行；若不开启则出现异常时会抛出异常终止处理
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_CONTINUE_WHEN_ERROR|

#### 1.1.1.3. el.debug.mode

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

#### 1.1.1.4. first.parse.init.method.type

- 参数说明

```
处理类的方法前是否需要先解析构造函数以获取非静态字段可能的类型，仅当parse.method.call.type.value参数为true时才可以生效
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CKE_FIRST_PARSE_INIT_METHOD_TYPE|

#### 1.1.1.5. handle.callee.new.raw.actual

- 参数说明

```
解析方法调用时，通过new创建的被调用类型使用原始类型还是实际类型的开关
only_raw: 记录一条方法调用关系，被调用类型使用：原始类型
only_actual: 记录一条方法调用关系，被调用类型使用：实际类型
raw_actual: 记录两条方法调用关系，被调用类型分别使用：原始类型、实际类型
例如 Super1 obj = new Child1(); obj.func1(); ，被调用对象的原始类型为Super1，实际类型为Child1，通过该开关选择被调用对象使用的类型
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值|only_actual|
|参数默认值|only_actual|
|参数枚举名|CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL|

#### 1.1.1.6. handle.callee.spring.bean.raw.actual

- 参数说明

```
解析方法调用时，被调用对象为Spring Bean时（支持字段注入），类型使用原始类型还是实际类型的开关
only_raw: 记录一条方法调用关系，被调用类型使用：原始类型
only_actual: 记录一条方法调用关系，被调用类型使用：实际类型
raw_actual: 记录两条方法调用关系，被调用类型分别使用：原始类型、实际类型
例如Spring Bean字段定义的类型为Super1，实际注入的类型为Child1，被调用对象的原始类型为Super1，实际类型为Child1，通过该开关选择被调用对象使用的类型
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值|only_actual|
|参数默认值|only_actual|
|参数枚举名|CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL|

#### 1.1.1.7. jdk.runtime.major.version

- 参数说明

```
解析的 jar 文件在运行时使用的 JDK 主版本号
例如 8、11、17、21 等
默认为 8 ，代表 JDK8 及以下版本
```

|描述|内容|
|---|---|
|参数类型|Integer|
|参数值是否必填|否|
|当前使用参数值|8|
|参数默认值|8|
|参数枚举名|CKE_JDK_RUNTIME_MAJOR_VERSION|

#### 1.1.1.8. jmod.program.path

- 参数说明

```
指定用于解析 .jmod 文件的jmod程序完整路径
假如指定的 .jmod 文件在JDK的jmods目录中，可找到bin目录的jmod程序，则当前参数可省略
假如无法找到JDK bin目录的jmod程序，则当前参数不能省略
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值||
|参数默认值||
|参数枚举名|CKE_JMOD_PROGRAM_PATH|

#### 1.1.1.9. log.method.spend.time

- 参数说明

```
是否在输出目录生成记录方法分析耗时的文件
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CKE_LOG_METHOD_SPEND_TIME|

#### 1.1.1.10. merge.separate.fat.jar

- 参数说明

```
在合并需要解析的jar文件时，是否合并出一个单独的fat jar。仅包含.class文件，且所有的jar文件都合并到从根目录开始
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_MERGE_SEPARATE_FAT_JAR|

#### 1.1.1.11. output.file.ext

- 参数说明

```
指定生成文件后缀名，需要以“.”开头
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值|.md|
|参数默认值|.txt|
|参数枚举名|CKE_OUTPUT_FILE_EXT|

#### 1.1.1.12. output.root.path

- 参数说明

```
生成文件的根目录，分隔符支持使用/或\，末尾是否为分隔符不影响
生成解析结果文件时，根目录由该参数控制，默认生成到需要解析的jar文件所在目录
在解析jar文件时，若需要合并jar、war文件、目录合并为新的jar文件，所在目录由该参数控制，默认生成到需要解析的第一个文件、目录所在目录
假如需要解析jmod文件，则当前参数必须指定
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值||
|参数默认值||
|参数枚举名|CKE_OUTPUT_ROOT_PATH|

#### 1.1.1.13. parse.jar.compatibility.mode

- 参数说明

```
是否使用Jar兼容性检查模式，仅解析类、方法、字段、类的注解等基础信息
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_PARSE_JAR_COMPATIBILITY_MODE|

#### 1.1.1.14. parse.method.call.type.value

- 参数说明

```
处理方法调用时是否解析被调用对象和参数可能的类型与值
开启后可支持识别多态、Spring Bean等使用的实际类型
例如对于方法调用 A a = new B(); a.func(123); 开启当前开关后可获得对象a类型为B，func方法调用时参数值为123
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CKE_PARSE_METHOD_CALL_TYPE_VALUE|

#### 1.1.1.15. parse.only.class.mode

- 参数说明

```
是否仅解析类及类的注解信息，比 parse.jar.compatibility.mode 优先级高
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_PARSE_ONLY_CLASS_MODE|

### 1.1.2. 使用的不区分顺序的其他配置参数

#### 1.1.2.1. _javacg2_config/fr_eq_conversion_method.properties

- 配置文件枚举类名与常量名

JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD

- 参数说明

(作用) 在分析dto的字段之间通过get/set方法的关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法（每行代表一条记录，支持多行）

(内容) key指定对应的方法，包含{完整类名}:{方法名}

(内容) value指定与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号

(格式) {完整类名}:{方法名}={被调用对象或方法参数序号}

- 当前使用参数值

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

### 1.1.3. 使用的区分顺序的其他配置参数

#### 1.1.3.1. _javacg2_config/jar_dir.properties

- 配置文件枚举类名与常量名

JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR

- 参数说明

(作用) 指定需要解析的jar、war、jmod文件路径，或保存class、jar、war、jmod文件的目录路径（每行代表一条记录，支持多行）

(格式) 路径中的分隔符支持使用/或\，目录最后指定或不指定分隔符均可

(示例) build/

(示例) build/test.jar

(示例) D:/test/build/test.jar

(示例) jdk-21.0.4+7/jmods/java.base.jmod

- 当前使用参数值

```
build/test.jar
```

### 1.1.4. 使用的表达式配置参数

#### 1.1.4.1. _javacg2_merge_file_switch/ignore_jar_in_dir.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_DIR

- 参数说明

指定是否跳过合并目录中的jar文件

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|file_path|String|目录中的文件绝对路径<br>以斜杠/为分隔符|D:/a/b.jar<br>/tmp/a/b.jar|
|file_dir_path|String|目录中的文件所在目录绝对路径<br>以斜杠/为分隔符，不以分隔符结尾|D:/a<br>/tmp/a|
|file_name|String|文件名称|a.class<br>a.xml|

- 当前使用参数值

```

```

#### 1.1.4.2. _javacg2_merge_file_switch/ignore_war_in_dir.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_WAR_IN_DIR

- 参数说明

指定是否跳过合并目录中的war文件

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|file_path|String|目录中的文件绝对路径<br>以斜杠/为分隔符|D:/a/b.jar<br>/tmp/a/b.jar|
|file_dir_path|String|目录中的文件所在目录绝对路径<br>以斜杠/为分隔符，不以分隔符结尾|D:/a<br>/tmp/a|
|file_name|String|文件名称|a.class<br>a.xml|

- 当前使用参数值

```

```

#### 1.1.4.3. _javacg2_merge_file_switch/ignore_class_in_dir.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_DIR

- 参数说明

指定是否跳过合并目录中的class文件

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|file_path|String|目录中的文件绝对路径<br>以斜杠/为分隔符|D:/a/b.jar<br>/tmp/a/b.jar|
|file_dir_path|String|目录中的文件所在目录绝对路径<br>以斜杠/为分隔符，不以分隔符结尾|D:/a<br>/tmp/a|
|file_name|String|文件名称|a.class<br>a.xml|

- 当前使用参数值

```

```

#### 1.1.4.4. _javacg2_merge_file_switch/ignore_other_in_dir.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_DIR

- 参数说明

指定是否跳过合并目录中的非jar、war、class文件

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|file_path|String|目录中的文件绝对路径<br>以斜杠/为分隔符|D:/a/b.jar<br>/tmp/a/b.jar|
|file_dir_path|String|目录中的文件所在目录绝对路径<br>以斜杠/为分隔符，不以分隔符结尾|D:/a<br>/tmp/a|
|file_name|String|文件名称|a.class<br>a.xml|
|file_ext|String|非jar、war、class文件后缀<br>以.开头|.xml<br>.properties|

- 当前使用参数值

```

```

#### 1.1.4.5. _javacg2_merge_file_switch/ignore_jar_in_jar_war.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR

- 参数说明

指定是否跳过合并jar、war中的jar文件

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|file_path|String|jar/war文件中的文件相对路径<br>相对根目录的路径<br>以斜杠/为分隔符，不以分隔符开头|a/b/c.jar<br>a/b/c.xml|
|file_dir_path|String|jar/war文件中的文件所在目录相对路径<br>相对根目录的路径<br>以斜杠/为分隔符，不以分隔符开头或结尾|a<br>a/b|
|file_name|String|文件名称|a.class<br>a.xml|

- 当前使用参数值

```

```

#### 1.1.4.6. _javacg2_merge_file_switch/ignore_class_in_jar_war.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR

- 参数说明

指定是否跳过合并jar、war中的class文件

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|file_path|String|jar/war文件中的文件相对路径<br>相对根目录的路径<br>以斜杠/为分隔符，不以分隔符开头|a/b/c.jar<br>a/b/c.xml|
|file_dir_path|String|jar/war文件中的文件所在目录相对路径<br>相对根目录的路径<br>以斜杠/为分隔符，不以分隔符开头或结尾|a<br>a/b|
|file_name|String|文件名称|a.class<br>a.xml|
|class_file_path|String|jar/war文件中的class文件的相对路径<br>相对根目录，或WEB-INF/classes、BOOT-INF/classes目录的路径<br>以斜杠/为分隔符，不以分隔符开头|a/b/c.class|

- 当前使用参数值

```

```

#### 1.1.4.7. _javacg2_merge_file_switch/ignore_other_in_jar_war.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR

- 参数说明

指定是否跳过合并jar、war中的非jar、war、class文件

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|file_path|String|jar/war文件中的文件相对路径<br>相对根目录的路径<br>以斜杠/为分隔符，不以分隔符开头|a/b/c.jar<br>a/b/c.xml|
|file_dir_path|String|jar/war文件中的文件所在目录相对路径<br>相对根目录的路径<br>以斜杠/为分隔符，不以分隔符开头或结尾|a<br>a/b|
|file_name|String|文件名称|a.class<br>a.xml|
|file_ext|String|非jar、war、class文件后缀<br>以.开头|.xml<br>.properties|

- 当前使用参数值

```

```

#### 1.1.4.8. _javacg2_merge_file_switch/ignore_jar_war_by_class_dir_prefix.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX

- 参数说明

通过class文件对应指定层级的目录路径判断是否跳过合并当前jar、war文件

相当于通过jar、war文件中类的包名控制是否跳过合并当前jar、war文件

以下参数为jar、war文件中的class文件对应指定层级的目录路径集合。在表达式中可通过“include”方法判断集合中是否包含指定元素

集合中的元素类型为字符串，以/作为分隔符，不会以分隔符开头或结尾

例如jar文件中有a1/c1.class、a2/b2/c2.class，则该jar文件中的class文件目录1级路径有a1、a2，2级路径有a2/b2，没有层级大于2级的路径

在使用以下 class_dir_prefix_level_ 参数时，需要以 class_dir_prefix_level_ 开头，后续通过数字指定class文件路径层级

例如 class_dir_prefix_level_3 代表第3级class文件路径集合

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|class_dir_prefix_level_|Set|jar/war文件中的class文件对应指定层级的目录路径集合|集合：('a')<br>集合：('a', 'a/b')|

- 当前使用参数值

```

```

#### 1.1.4.9. _javacg2_parse_class_method_switch/parse_ignore_class.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS

- 参数说明

指定是否跳过解析类

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|class_name|String|完整类名|a.b.Class1|
|package_name|String|完整包名<br>不会以.结束|a.b|
|simple_class_name|String|简单类名|Class1|

- 当前使用参数值

```

```

#### 1.1.4.10. _javacg2_parse_class_method_switch/parse_ignore_method.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD

- 参数说明

指定是否跳过解析方法

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

#### 1.1.4.11. _javacg2_parse_method_call_switch/parse_ignore_method_call.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL

- 参数说明

指定是否跳过解析方法调用，支持通过方法调用类型、调用方法或被调用方法等判断

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

- 当前使用参数值

```

```

#### 1.1.4.12. _javacg2_parse_method_call_switch/parse_ignore_method_call_type_value_caller.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER

- 参数说明

指定解析方法调用被调用对象和参数可能的类型与值需要跳过哪些方法，通过调用方法判断

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|er_class_name|String|调用方完整类名|a.b.Class1|
|er_package_name|String|调用方完整包名<br>不会以.结束|a.b|
|er_simple_class_name|String|调用方简单类名|Class1|
|er_method_name|String|调用方方法名<br>不包括括号及方法参数|method1|
|er_method_arg_num|int|调用方方法参数数量|0<br>1|
|er_full_method|String|调用方完整方法<br>包括括号及方法参数|a.b.Class1:method1(int)|

- 当前使用参数值

```

```

#### 1.1.4.13. _javacg2_handle_xml_switch/handle_ignore_spring_bean_in_xml.av

- 配置文件枚举类名与常量名

JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML

- 参数说明

指定处理XML文件中定义的Spring Bean需要跳过哪些Bean

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|bean_name|String|Spring Bean名称|selectService1|
|class_name|String|Spring Bean类名|test.callgraph.mybatis.service.select.SelectService1|
|profile|String|Spring Bean profile<br>可能为空字符串，可能包含一级或多级，使用半角逗号拼接|dev<br>dev,dev1|

- 当前使用参数值

```

```

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

### 1.2.2. 使用的不区分顺序的其他配置参数

未使用

### 1.2.3. 使用的区分顺序的其他配置参数

#### 1.2.3.1. _jacg_extensions/code_parser.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER

- 参数说明

定义用于对代码进行解析的扩展类完整类名（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.javacg2.extensions.codeparser.CodeParserInterface 接口的实现类

- 当前使用参数值

```
```

#### 1.2.3.2. _jacg_extensions/javacg2_method_call_extensions.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL

- 参数说明

java-callgraph2 组件在处理方法调用时的扩展类（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.javacg2.extensions.methodcall.JavaCG2MethodCallExtensionInterface 接口的实现类

- 当前使用参数值

```
```

### 1.2.4. 使用的表达式配置参数

未使用

