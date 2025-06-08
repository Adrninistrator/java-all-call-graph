# 1. 快速开始

## 1.1. 引入组件

在使用本工具前，首先需要在对应的项目引入本工具组件的依赖，将其引入到test模块或使用provided类型，可以避免发布到服务器中。

- Gradle

```
testImplementation 'com.github.adrninistrator:java-all-call-graph:xxx'
```

- Maven

```xml
<dependency>
  <groupId>com.github.adrninistrator</groupId>
  <artifactId>java-all-call-graph</artifactId>
  <version>xxx</version>
</dependency>
```

最新版本号可查看[https://search.maven.org/artifact/com.github.adrninistrator/java-all-call-graph](https://search.maven.org/artifact/com.github.adrninistrator/java-all-call-graph)。

`执行以下类时，需要选择classpath对应模块为test`

## 1.2. 释放启动类及配置文件

执行以下类的main()方法：

```
com.adrninistrator.jacg.unzip.UnzipFile
```

执行以上类后，会将java-all-callgraph.jar中保存配置文件及启动类释放到当前Java项目的test模块的resources、java目录中（仅在本地生效，避免发布到服务器中）。

若当前Java项目存在“src/test”或“src/unit.test”目录，则将配置文件与Java文件分别释放在该目录的resources、java目录中；

若当前Java项目不存在以上目录，则将上述文件释放在“_jacg-\[当前时间戳\]”目录中，之后需要手工将对应目录拷贝至test模块对应目录中。

当目标文件不存在时，则会进行释放；若目标文件已存在，则不会覆盖。

## 1.3. 生成Java方法调用关系并写入数据库

修改配置文件`_jacg_config/config.properties`中的参数

执行以下类的main()方法：

```
test.jacg.TestRunnerWriteDb
```

## 1.4. 生成调用指定类方法向上的完整调用链

修改配置文件`_jacg_config/config.properties`、`_jacg_config/o_g4callee_class_name.properties`中的参数

执行以下类的main()方法：

```
test.jacg.TestRunnerGenAllGraph4Callee
```

## 1.5. 生成指定方法向下完整调用链-生成所有的调用链

修改配置文件`_jacg_config/config.properties`、`_jacg_config/o_g4caller_entry_method.properties`、`_jacg_config/o_g4caller_entry_method_ignore_prefix.properties`中的参数

执行以下类的main()方法：

```
test.jacg.TestRunnerGenAllGraph4Caller
```

## 1.6. 生成指定方法向下完整调用链-忽略特定的调用关系

除修改以上配置文件外，还需要修改`_jacg_config/o_g4caller_ignore_class_keyword.properties`、`_jacg_config/o_g4caller_ignore_full_method_prefix.properties`、`_jacg_config/o_g4caller_ignore_method_prefix.properties`中的参数

执行以下类的main()方法：

```
test.jacg.TestRunnerGenAllGraph4CallerSupportIgnore
```
