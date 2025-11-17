# 1. RunnerJarCompatibilityCheckFast

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
|当前使用参数值|true|
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
|当前使用参数值|jacg|
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

#### 1.2.1.3. drop.or.truncate.table

- 参数说明

```
在插入数据库表前，对表执行的清理操作 true: DROP，false: TRUNCATE
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_DROP_OR_TRUNCATE_TABLE|

#### 1.2.1.4. el.debug.mode

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

#### 1.2.1.5. output.dir.flag

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

#### 1.2.1.6. output.dir.name

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

#### 1.2.1.7. output.root.path

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

#### 1.2.1.8. parse.spring.aop.info

- 参数说明

```
解析指定的jar文件时，是否需要对Spring AOP相关信息进行解析
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_PARSE_SPRING_AOP_INFO|

#### 1.2.1.9. skip.write.db.when.jar.not.modified

- 参数说明

```
需要解析的jar文件没有变化时是否跳过写数据库操作，true：跳过，false：不跳过
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED|

#### 1.2.1.10. text.to.excel.width.px

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

#### 1.2.1.11. thread.num

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

#### 1.2.4.1. _jacg_extensions/code_parser.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER

- 参数说明

定义用于对代码进行解析的扩展类完整类名（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.javacg2.extensions.codeparser.CodeParserInterface 接口的实现类

- 当前使用参数值

```
```

#### 1.2.4.2. _jacg_extensions/manual_add_method_call1.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1

- 参数说明

在此定义人工添加方法调用关系的扩展类完整类名，处理特定的子类与实现类（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1 类的子类

- 当前使用参数值

```
```

#### 1.2.4.3. _jacg_extensions/javacg2_method_call_extensions.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL

- 参数说明

java-callgraph2 组件在处理方法调用时的扩展类（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.javacg2.extensions.methodcall.JavaCG2MethodCallExtensionInterface 接口的实现类

- 当前使用参数值

```
```

#### 1.2.4.4. _jacg_extensions/jacg_method_call_extensions.properties

- 配置文件枚举类名与常量名

OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL

- 参数说明

java-all-call-graph 组件在处理方法调用时的扩展类（每行指定一项配置，可指定多行）

需要是 com.adrninistrator.jacg.extensions.methodcall.AbstractJACGMethodCallExtension 类的子类

- 当前使用参数值

```
```

#### 1.2.4.5. _jacg_jar_compatibility/other_h2_db_path.properties

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

#### 1.2.5.1. _jacg_jar_compatibility/compatibility_check_ignore_class_reference.av

- 配置文件枚举类名与常量名

ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE

- 参数说明

指定Jar兼容性检查快速模式时是否跳过记录特定的类引用关系

- 允许使用的变量

|变量名称|变量类型|变量描述|变量值示例|
|---|---|---|---|
|er_class_name|String|调用方完整类名|a.b.Class1|
|er_package_name|String|调用方完整包名<br>不会以.结束|a.b|
|er_simple_class_name|String|调用方简单类名|Class1|
|ee_class_name|String|被调用方完整类名|a.b.Class1|
|ee_package_name|String|被调用方完整包名<br>不会以.结束|a.b|
|ee_simple_class_name|String|被调用方简单类名|Class1|

- 当前使用参数值

```

```

