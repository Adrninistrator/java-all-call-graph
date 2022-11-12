# 1. 说明

有时可能需要使用java-all-call-graph进行二次开发，调用写入数据库、生成方法完整调用链、生成包含关键字的所有方法到起始方法之间的调用链等方法，可以按照以下说明。

从`0.7.7`版本开始，java-all-call-graph会尝试读取jar包中的配置文件，相关的配置文件可以不释放到项目中，可以通过Java代码对配置参数进行设置（进行二次开发时可能需要使用）。

从`0.8.0`版本开始，java-all-call-graph支持通过Java代码调用时，每次执行任务使用独立的配置信息，可支持多个任务并行执行（适用于在Web项目中使用java-all-call-graph的场景）

# 2. 二次开发

## 2.1. 通过Java代码对配置参数进行设置

支持通过Java代码对配置参数进行设置，可覆盖配置文件中的参数（或仅使用Java代码中设置的参数，不使用配置文件中的参数）

以下类用于对配置参数进行设置

```java
com.adrninistrator.jacg.conf.ConfigureWrapper
```

以下方法在执行时支持传入ConfigureWrapper对象，支持每次执行任务使用独立的配置信息，可支持多个任务并行执行（适用于在Web项目中使用java-all-call-graph的场景）

|类|方法|作用|
|---|---|---|
|RunnerWriteDb|run(ConfigureWrapper configureWrapper)|生成Java方法调用关系并写入数据库|
|RunnerGenAllGraph4Callee|run(ConfigureWrapper configureWrapper)|生成调用指定类方法向上的完整调用链|
|RunnerGenAllGraph4Caller|run(ConfigureWrapper configureWrapper)|生成指定方法向下完整调用链|
|FindKeywordCallGraph|find(boolean order4ee, ConfigureWrapper configureWrapper)|生成包含关键字的所有方法到起始方法之间的调用链|

## 2.2. 示例

以下可参考`test.run_by_code`包中的测试代码，在`TestRunByCodeBase`类中创建了ConfigureWrapper对象，并在该类的子类中使用ConfigureWrapper对象调用相关的方法。

### 2.2.1. 设置_jacg_config/config.properties配置文件参数

```java
configureWrapper.addConfig(ConfigKeyEnum configKeyEnum, String value);
```

`对于app.name参数，在以上方法中会将参数值中的-替换为_`

ConfigKeyEnum枚举类中定义了_jacg_config/config.properties配置文件中的参数key

通过value参数指定需要设置的参数值

示例如下：

```java
configureWrapper.addConfig(ConfigKeyEnum.CKE_APPNAME, "test_rbc");
```

### 2.2.2. 设置_jacg_config、_jacg_extensions目录配置文件参数

```java
configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet);
```

OtherConfigFileUseSetEnum枚举类中定义了_jacg_config目录中其他配置文件的文件名，以及_jacg_extensions目录中的配置文件名

通过configSet参数指定需要设置的Set类型的参数值

示例如下：

```java
configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX, new HashSet<>(Arrays.asList(
        "test.call_graph.method_call",
        "test.call_graph.argument",
        "java.")));
```

### 2.2.3. 设置_jacg_find_keyword目录配置文件参数

```java
configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList);
```

OtherConfigFileUseListEnum枚举类中定义了_jacg_find_keyword目录中配置文件的文件名

通过configList参数指定需要设置的List类型的参数值

示例如下：

```java
configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Arrays.asList("!entry!", "<init>"));
```
