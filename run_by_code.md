# 1. 说明

有时可能需要使用 java-all-call-graph 进行二次开发，调用写入数据库、生成方法完整调用链、生成包含关键字的所有方法到起始方法之间的调用链等方法，可以按照以下说明。

从`0.7.7`版本开始，java-all-call-graph 会尝试读取 jar 包中的配置文件，相关的配置文件可以不释放到项目中，可以通过 Java 代码对配置参数进行设置（进行二次开发时可能需要使用）。

从`0.8.0`版本开始，java-all-call-graph 支持通过 Java 代码调用时，每次执行任务使用独立的配置信息，可支持多个任务并行执行（适用于在 Web 项目中使用 java-all-call-graph 的场景）

# 2. 通过源码启动

通过源码启动时，可以使用测试代码进行功能验证

## 2.1. 编译测试代码

在项目根目录执行以下命令

```
gradlew test_gen_jar
```

## 2.2. 执行相关的类

参考 [通过代码执行的其他示例](run_by_code_example.md)，“通过代码指定配置参数的主要功能示例”部分

## 2.3. 通过 Java 代码对配置参数进行设置

支持通过 Java 代码对配置参数进行设置，可覆盖配置文件中的参数（或仅使用 Java 代码中设置的参数，不使用配置文件中的参数）

以下使用 Java 代码中指定的参数配置，与通过配置文件配置的效果一样

相关的参数定义可参考以下类

```
com.adrninistrator.jacg.common.enums.ConfigKeyEnum
com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum
com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum
com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum
```

以下类用于对配置参数进行设置

```java
com.adrninistrator.jacg.conf.ConfigureWrapper
```

以下类的构造函数支持传入 ConfigureWrapper 对象，以支持每次执行任务使用独立的配置信息，以下方法可支持多个任务并行执行（适用于在 Web 项目中使用 java-all-call-graph 的场景），每次需要创建以下类新的实例

|类|方法|作用|
|---|---|---|
|RunnerWriteCallGraphFile|run()|生成 Java 方法调用关系并写入文件，不写入数据库|
|RunnerWriteDb|run()|生成 Java 方法调用关系并写入数据库|
|RunnerGenAllGraph4Callee|run()|生成调用指定类方法向上的完整调用链|
|RunnerGenAllGraph4Caller|run()|生成指定方法向下完整调用链|
|FindCallStackTrace|find(boolean order4ee)|生成包含关键字的所有方法到起始方法之间的调用链|

RunnerWriteCallGraphFile、RunnerWriteDb 类的带参数构造函数，除了需要指定 ConfigureWrapper 外，还需要指定 java-callgraph2 组件中的配置参数包装类 JavaCG2ConfigureWrapper，使用方式可参考 java-callgraph2 组件文档

## 2.4. 示例

以下可参考`test.run_by_code`包中的测试代码，在`TestRunByCodeBase`类中创建了 ConfigureWrapper 对象，并在该类的子类中使用 ConfigureWrapper 对象调用相关的方法。

可参考`test.runbycode.config.TestConfigGenerator`类中通过代码进行配置的方式

### 2.4.1. 配置文件说明

参考 [通过代码执行的其他示例](run_by_code_example.md)，该文档中有每个配置文件的简单说明，以及对应的枚举信息。每个配置文件的详细说明可打开配置文件查看

通过枚举修改配置的方式与修改配置文件的效果相同

### 2.4.2. 设置_jacg_config/config.properties 配置文件参数

```java
configureWrapper.setConfig(ConfigKeyEnum configKeyEnum, String value);
```

`对于 app.name 参数，在以上方法中会将参数值中的-替换为_`

ConfigKeyEnum 枚举类中定义了_jacg_config/config.properties 配置文件中的参数 key

通过 value 参数指定需要设置的参数值

示例如下：

```java
configureWrapper.setConfig(ConfigKeyEnum.CKE_APPNAME, "test_rbc");
```

### 2.4.3. 设置_jacg_config、_jacg_extensions 目录配置文件参数

```java
configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet);
```

OtherConfigFileUseSetEnum 枚举类中定义了_jacg_config 目录中其他配置文件的文件名，以及_jacg_extensions 目录中的配置文件名

通过 configSet 参数指定需要设置的 Set 类型的参数值

示例如下：

```java
configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX, new HashSet<>(Arrays.asList(
        "test.callgraph.methodcall",
        "test.callgraph.argument",
        "java.")));
```

### 2.4.4. 设置_jacg_find_keyword 目录配置文件参数

```java
configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList);
```

OtherConfigFileUseListEnum 枚举类中定义了_jacg_find_keyword 目录中配置文件的文件名

通过 configList 参数指定需要设置的 List 类型的参数值

示例如下：

```java
configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Arrays.asList("!entry!", "<init>"));
```
