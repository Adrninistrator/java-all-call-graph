# 表达式配置文件说明
当前文件为表达式示例配置文件，使用 aviator 表达式组件，语法与 Java 类似
使用文档可参考 https://www.yuque.com/boyan-avfmj/aviatorscript
每个配置文件的表达式的执行结果类型需要为 boolean ，即结果要么是 true ，要么是 false
通过表达式的执行结果，决定配置文件所对应场景下执行什么操作
配置文件中有说明允许使用的变量信息

# 查看表达式忽略的数据
若表达式用于忽略数据，则被忽略的数据会记录在日志文件中，保存在当前输出目录中，文件名为 el_ignore_data.log

# 支持使用的表达式配置
|表达式作用|表达式配置文件|表达式枚举常量名|
|---|---|---|
|指定生成方法完整调用链时是否跳过解析特定的方法调用，支持通过方法调用类型、调用方法或被调用方法等判断|_jacg_gen_all_call_graph/gen_call_graph_ignore_method_call.av|ElConfigEnum.ECE_GEN_ALL_CALL_GRAPH_IGNORE_METHOD_CALL|
|指定解析Spring AOP影响方法时忽略哪些Spring Bean类，支持指定类名、包名、简单类名|_jacg_spring_aop/spring_aop_ignore_spring_bean_class.av|ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS|
|JarDiff获得发生变化的方法的影响范围时（生成向上的方法完整调用链及调用堆栈），指定发生变化的方法中，需要忽略的方法|_jacg_jar_diff/jar_diff_gen_all_call_graph_ignore_callee.av|ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLEE|
|JarDiff获得发生变化的方法向下的方法完整调用链时，指定发生变化的方法中，需要忽略的方法|_jacg_jar_diff/jar_diff_gen_all_call_graph_ignore_caller.av|ElConfigEnum.ECE_JAR_DIFF_GEN_ALL_CALL_GRAPH_IGNORE_CALLER|
|指定Jar兼容性检查快速模式时是否跳过记录特定的类引用关系|_jacg_jar_compatibility/compatibility_check_ignore_class_reference.av|ElConfigEnum.ECE_COMPATIBILITY_CHECK_IGNORE_CLASS_REFERENCE|


# 表达式示例

字符串比较的表达式示例可参考 _el_example/string_compare.md

不同场景的表达式示例可参考 _el_example 目录中子目录的对应文件

# 表达式调试方式
将配置文件 _jacg_config/config.properties 的 el.debug.mode 参数设置为 true 可以使表达式执行时开启调试模式，会在应用日志中输出表达式执行时的详细信息

# 表达式语法 - aviator 默认支持

## aviator .av 文件中的注释格式
在aviator .av 文件中，需要注释某一行时，需要在行首指定两个井号“##”

## 返回固定值

### true
若表达式配置为“true”，则表达式执行结果固定为 true

### false
若表达式配置为“false”，或未指定表达式，则表达式执行结果固定为 false

## 字符串处理
除判断字符串是否等于指定值外，需要使用 aviator 提供的 string.xxx() 函数对字符串进行判断
字符串常量可以使用单引号包含，如 'abc'

### +
（作用）拼接字符串
（语法）{字符串变量/常量/运算结果} + {字符串变量/常量/运算结果} + ...
（示例）str1 + 'abc'
（示例）'abc' + '123'
（示例）str1 + str2

### ==
（作用）判断字符串类型的变量是否等于指定内容
（语法）{字符串变量/常量/运算结果} == {字符串变量/常量/运算结果}
（示例）str1 == 'abc'
（示例）str1 == str2 + 'abc'

### nil（对应Java中的null）
（作用）判断变量（包括字符串类型）为空，对应Java中的null
（语法）{变量} == nil
（语法）{变量} != nil
（示例）str1 == nil
（示例）str1 != nil

### string.startsWith()
（作用）判断字符串类型的变量是否以指定内容开头
（语法）string.startsWith({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})
（示例）string.startsWith(str1, 'abc')
（示例）string.startsWith(str1, str2 + 'abc')

### string.endsWith()
（作用）判断字符串类型的变量是否以指定内容结尾
（语法）string.endsWith({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})
（示例）string.endsWith(str1, 'abc')
（示例）string.endsWith(str1, str2 + 'abc')

### string.contains()
（作用）判断字符串类型的变量是否包含指定内容
（语法）string.contains({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})
（示例）string.contains(str1, 'abc')
（示例）string.contains(str1, str2 + 'abc')

### string.length()
（作用）获取字符串类型的变量的长度
（语法）string.length({字符串变量/常量/运算结果})
（示例）string.length(str1)
（示例）string.length(str1 + str2)

## 整型处理
整形的判断与 Java 语法相同，可使用比较运算符：==、<、>、<=、>=、!=
（语法）{整型变量名称} {比较运算符} {常量整形值}
（示例）int1 == 1
（示例）int1 != 1
（示例）int1 >= 1

## 逻辑判断
aviator 支持的逻辑判断运算符与 Java 相同

### &&
（作用）判断两个条件是否同时成立，只有两个条件都为 true 时，整体结果才为 true
（语法）{条件1} && {条件2}
（示例）x > 10 && y < 20

### ||
（作用）判断两个条件中是否有一个成立，只要有一个条件为 true，整体结果就为 true
（语法）{条件1} || {条件2}
（示例）x > 10 || y < 20

### !
（作用）取反运算符，用于将条件的结果取反，若条件为 true，则结果为 false；若条件为 false，则结果为 true
（语法）!{条件}
（示例）!(x > 10)

### ()
（作用）用于改变运算顺序，确保按照期望的顺序执行多个逻辑表达式
（语法）({表达式1} {运算符} {表达式2})
（示例）(x > 10 && y < 20) || z == 5

## 集合处理

### include
（作用）判断集合中是否包含指定的元素
（语法）include({集合变量}, {字符串/整型等常量})
（示例）include(set1, 'abc')

### !include
（作用）判断集合中是否不包含指定的元素
（语法）!include({集合变量}, {字符串/整型等常量})
（示例）!include(set1, 'abc')

# 表达式语法 - java-callgraph2 组件扩展支持

## 比较字符串忽略大小写的方法

### string.containsIC()
（作用）判断字符串类型的变量是否包含指定内容，忽略大小写
（语法）string.containsIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})
（示例）string.containsIC(str1, 'abc')

### string.endsWithIC()
（作用）判断字符串类型的变量是否以指定内容结尾，忽略大小写
（语法）string.endsWithIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})
（示例）string.endsWithIC(str1, 'abc')

### string.equalsIC()
（作用）判断字符串类型的变量是否与指定内容相等，忽略大小写
（语法）string.equalsIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})
（示例）string.equalsIC(str1, 'abc')

### string.startsWithIC()
（作用）判断字符串类型的变量是否以指定内容开头，忽略大小写
（语法）string.startsWithIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})
（示例）string.startsWithIC(str1, 'abc')

## 比较字符串支持比较多个的方法

### string.containsAny()
（作用）判断字符串类型的变量是否包含多个指定内容中的任意一个
（语法）string.containsAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)
（示例）string.containsAny(str1, 'abc', 'def', 'ghi')

### string.endsWithAny()
（作用）判断字符串类型的变量是否以多个指定内容中的任意一个结尾
（语法）string.endsWithAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)
（示例）string.endsWithAny(str1, 'abc', 'def', 'ghi')

### string.equalsAny()
（作用）判断字符串类型的变量是否与多个指定内容中的任意一个相等
（语法）string.equalsAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)
（示例）string.equalsAny(str1, 'abc', 'def', 'ghi')

### string.startsWithAny()
（作用）判断字符串类型的变量是否以多个指定内容中的任意一个开头
（语法）string.startsWithAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)
（示例）string.startsWithAny(str1, 'abc', 'def', 'ghi')
