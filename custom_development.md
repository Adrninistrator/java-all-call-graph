# 1. 说明

有时可能需要使用java-all-call-graph进行二次开发，调用写入数据库、生成方法完整调用链、生成包含关键字的所有方法到起始方法之间的调用链等方法，可以按照以下说明。

从`0.7.7`版本开始，java-all-call-graph会尝试读取jar包中的配置文件，相关的配置文件可以不释放到项目中，可以通过Java代码对配置参数进行设置（进行二次开发时可能需要使用）。

# 2. 二次开发

## 2.1. 通过Java代码对配置参数进行设置

支持通过Java代码对配置参数进行设置，可覆盖配置文件中的参数（或仅使用Java代码中设置的参数，不使用配置文件中的参数）

可通过以下类的方法对配置参数进行设置

```java
com.adrninistrator.jacg.conf.ConfigureWrapper
```

在执行释放到项目中的test.jacg包中的入口类（如TestRunnerWriteDb），或执行jar包中com.adrninistrator.jacg.runner包中的入口类（如RunnerWriteDb）之前，需要先调用ConfigureWrapper类的方法设置配置参数。

以下可参考`test.run_by_code`包中的测试代码，在`TestRunByCodeBase`类中调用了ConfigureWrapper类的方法。

### 2.1.1. 设置~jacg_config/config.properties配置文件参数

```java
ConfigureWrapper.addConfig(ConfigKeyEnum configKeyEnum, String value);
```

`对于app.name参数，在以上方法中会将参数值中的-替换为_`

ConfigKeyEnum枚举类中定义了~jacg_config/config.properties配置文件中的参数key

通过value参数指定需要设置的参数值

示例如下：

```java
ConfigureWrapper.addConfig(ConfigKeyEnum.CKE_APPNAME, "test_rbc");
```

### 2.1.2. 设置~jacg_config、~jacg_extensions目录配置文件参数

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet);
```

OtherConfigFileUseSetEnum枚举类中定义了~jacg_config目录中其他配置文件的文件名，以及~jacg_extensions目录中的配置文件名

通过configSet参数指定需要设置的Set类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX, new HashSet<>(Arrays.asList(
        "test.call_graph.method_call",
        "test.call_graph.argument",
        "java.")));
```

### 2.1.3. 设置~jacg_find_keyword目录配置文件参数

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList);
```

OtherConfigFileUseListEnum枚举类中定义了~jacg_find_keyword目录中配置文件的文件名

通过configList参数指定需要设置的List类型的参数值

示例如下：

```java
ConfigureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Arrays.asList("!entry!", "<init>"));
```


## 2.2. 使用命令行方式执行

以上所述执行方式，需要在IDE中执行，假如需要使用命令行方式执行，可参考以下方法。

在项目根目录执行`gradlew gen_run_jar`命令，生成可以直接执行的jar包，并拷贝相关文件。

在生成的`output_dir`目录中，包含了当前项目生成的jar包、依赖jar包，以及资源文件、启动脚本等，如下所示：

```
~jacg_config
~jacg_extensions
~jacg_find_keyword
~jacg_sql
jar
lib
run.bat
run.sh
```

可选择run.bat或run.sh脚本，以命令行方式执行，脚本中执行的类可为test.jacg包中的类，可选择的类可参考前文内容。

在执行脚本前，需要根据需要修改脚本中执行的类名。
