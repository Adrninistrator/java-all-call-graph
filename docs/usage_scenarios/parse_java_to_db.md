# 1. 对 Java 代码静态分析并支持 SQL 查询

## 1.1. 作用

java-all-call-graph 项目支持对编译后的 Java 代码进行静态分析，将相关信息写入数据库表，之后可使用 SQL 语句查询静态分析获取的结果

java-all-call-graph 项目地址为 [https://github.com/Adrninistrator/java-all-call-graph](https://github.com/Adrninistrator/java-all-call-graph)

支持解析 Java 中的常见数据，包括类、方法、字段、注解、泛型、方法调用、枚举等

支持解析部分常用开发框架信息，包括 MyBatis、Spring AOP、Spring Bean、Spring Controller、Spring Task、Spring 包扫描路径 等

支持解析的完整信息，即解析后会写入的数据库表，可参考 [数据库表定义](../db_tables.md)

## 1.2. 解析 Java 代码实现

在解析 Java 代码时，使用 java-callgraph2 项目 [https://github.com/Adrninistrator/java-callgraph2](https://github.com/Adrninistrator/java-callgraph2)

java-callgraph2 支持对指定的 jar 等文件进行解析，并将结果输入到文本文件

## 1.3. 支持的数据库

### 1.3.1. 数据库类型

java-all-call-graph 项目会读取 java-callgraph2 生成的文本文件，将数据保存在数据库中，支持使用 MySQL、H2、PostgreSQL 数据库（理论上支持其他数据库，但可能需要对 SQL 语句进行调整）

### 1.3.2. 数据库权限要求

所使用的数据库用户需要有 DML 读写权限，及 DDL 权限（需要执行 CREATE TABLE、TRUNCATE TABLE 操作）

### 1.3.3. H2 数据库

`建议使用本地文件形式的 H2 数据库`，可不依赖外部的其他数据库，使用更简单；且经过验证，H2 比 MySQL 数据库的读写速度更快。生成的 H2 数据库中，schema 为“jacg”

H2 数据库使用说明可参考 [https://blog.csdn.net/a82514921/article/details/108029222](https://blog.csdn.net/a82514921/article/details/108029222)

假如选择使用 H2 数据库，则当前步骤执行完毕后，会在日志中打印用于连接 H2 数据库的 JDBC URL，日志内容如下所示：

```
可用于连接 H2 数据库的 JDBC URL:
jdbc:h2:file:D:\test\java-all-call-graph\build\jacg_h2db
```

### 1.3.4. PostgreSQL 数据库

假如需要使用 PostgreSQL 数据库，需要修改配置参数中指定的数据库驱动类名与 URL

URL 中需要指定使用的 PostgreSQL 数据库的 schame 名称，可能需要禁用 SSL

建表语句会自动转换为 PostgreSQL 数据库支持的形式，不需要人工修改

### 1.3.5. Neo4j

支持将数据保存到 Neo4j，可参考 [使用 Neo4j 图数据库](docs/use_neo4j.md)，但仅支持部分功能的数据写入 Neo4j

不建议使用 Neo4j 存储数据，建议使用关系型数据库

## 1.4. 配置参数说明

### 1.4.1. 当前场景使用的配置参数

当前场景使用的配置参数使用说明，可参考 [RunnerWriteDb - 解析代码并将结果写入数据库](../class_use_config/RunnerWriteDb.md)

### 1.4.2. 重要的配置参数

#### 1.4.2.1. java-callgraph2

java-callgraph2 需要使用的重要配置参数是配置文件 _javacg2_config/jar_dir.properties

用于指定需要解析的 jar、war、jmod 文件路径，或保存 class、jar、war、jmod 文件的目录路径

对于使用 IDE 打开的 Java 项目，可以指定保存编译生成的 class 文件目录进行解析。例如使用 IDEA 时，可以指定 out 或目录 out/production/classes

java-callgraph2 支持通过表达式忽略解析部分数据，可参考对应文档使用说明

#### 1.4.2.2. java-all-call-graph

java-all-call-graph 在当前场景需要使用的重要配置参数是配置文件_jacg_config/config_db.properties

指定需要使用的数据库信息

### 1.4.3. 配置参数示例

参考 [配置参数示例](../_jacg_all_config.md)

### 1.4.4. EL 表达式使用通用说明文档

参考 [EL 表达式使用通用说明文档](../../java-all-call-graph/src/main/resources/_el_example/el_usage.md)

### 1.4.5. EL 表达式字符串比较说明文档

参考 [EL 表达式字符串比较说明文档](../../java-all-call-graph/src/main/resources/_el_example/string_compare.md)

## 1.5. 入口类

- 通过配置文件指定配置参数

当通过配置文件指定配置参数时，入口类为 test.jacg.TestRunnerWriteDb

该类会调用下面的类

- 通过代码指定配置参数

通过代码指定配置参数时，入口类为 com.adrninistrator.jacg.runner.RunnerWriteDb

## 1.6. 示例代码

以下为通过代码指定配置参数执行时的示例代码

```java
// 生成 java-callgraph2 使用的配置参数包装类
JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
        TestConfigGenerator.TEST_JAR_PATH);

ConfigureWrapper configureWrapper = new ConfigureWrapper();
Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
```

对应 test.runbycodemain.TestRBC1RunnerWriteDb:$test0WriteDb 方法

以上方法会使用 ConfigureWrapper 对应的默认配置，会使用 H2 数据库

### 1.6.1. 使用 H2 数据库

示例代码如下：

```java
JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
        TestConfigGenerator.TEST_JAR_PATH);

ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
```

对应 test.runbycodemain.db.TestUseH2Db:test 方法

### 1.6.2. 使用 MySQL 数据库

示例代码如下：

```java
JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
        TestConfigGenerator.TEST_JAR_PATH);

ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, com.mysql.cj.jdbc.Driver.class.getName());
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
        "jdbc:mysql://127.0.0.1:3307/testdb?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true");
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
```

对应 test.runbycodemain.db.TestUseMySQL:test 方法

### 1.6.3. 使用 PostgreSQL 数据库

示例代码如下：

```java
JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
        TestConfigGenerator.TEST_JAR_PATH);

ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, org.postgresql.Driver.class.getName());
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
        "jdbc:postgresql://127.0.0.1:5432/testdb?currentSchema=jacg&sslmode=disable&useUnicode=true&characterEncoding=UTF-8");
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
```

对应 test.runbycodemain.db.TestUsePostgreSQL:test 方法

## 1.7. 人工增加方法调用关系

对于某些方法调用关系，不存在对应的方法调用指令，需要人工增加

人工增加方法调用关系的操作发生在 RunnerWriteDb 类对 Java 代码静态分析并将结果写入数据库阶段

### 1.7.1. 根据方法调用参数类型增加调用关系-支持 Apache Commons Chain

#### 1.7.1.1. 支持处理的代码与增加的方法调用关系

当某方法调用 org.apache.commons.chain.impl.ChainBase:addCommand 方法时，增加该方法调用 addCommand 方法参数 1 对象对应类的 execute(org.apache.commons.chain.Context) 方法的调用关系

示例代码如下：

```java
public class TestUseChain1 extends TestSpringBase {

    @Resource(name = TestChainCommandService1.SERVICE_NAME)
    private Command commandService1;

    @Autowired
    @Qualifier(TestChainCommandService2.SERVICE_NAME)
    private Command commandService2;

    @Test
    public void run() throws Exception {
        ChainBase chain = new ChainBase();
        chain.addCommand(new TestChainCommand1());
        chain.addCommand(commandService1);
        chain.addCommand(commandService2);
        Context context = new ContextBase();
        chain.execute(context);
    }
}
```

- chain.addCommand(new TestChainCommand1());

对于该方法，会增加 test.callgraph.chain.use.TestUseChain1:run 方法调用 test.callgraph.chain.define.TestChainCommand1:execute(org.apache.commons.chain.Context) 方法的关系关系

- chain.addCommand(commandService1);

对于该方法，会增加 test.callgraph.chain.use.TestUseChain1:run 方法调用 test.callgraph.chain.define.TestChainCommandService1:execute(org.apache.commons.chain.Context) 方法的关系关系

- chain.addCommand(commandService2);

对于该方法，会增加 test.callgraph.chain.use.TestUseChain1:run 方法调用 test.callgraph.chain.define.TestChainCommandService2:execute(org.apache.commons.chain.Context) 方法的关系关系

#### 1.7.1.2. 使用方式

在调用 java-all-call-graph 项目的 RunnerWriteDb 类对 Java 代码静态分析并将结果写入数据库时，使用的配置参数包装类中，需要进行以下配置：

- _jacg_extensions/javacg2_method_call_extensions.properties

当前配置文件是 java-callgraph2 组件在处理方法调用时的扩展类

对应的枚举常量为 OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL

当前配置文件中需要配置 com.adrninistrator.jacg.extensions.methodcall.JavaCG2ApacheCommonsChainMethodCallExtension 类的完整类名

- _jacg_extensions/jacg_method_call_extensions.properties

当前配置文件是 java-all-call-graph 组件在处理方法调用时的扩展类

对应的枚举常量为 OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL

当前配置文件中需要配置 com.adrninistrator.jacg.extensions.methodcall.JACGApacheCommonsChainMethodCallExtension 类的完整类名

对应代码如下：

```java
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2ApacheCommonsChainMethodCallExtension.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGApacheCommonsChainMethodCallExtension.class.getName());
```

假如在解析 java 代码时有包含 commons-chain 依赖库 jar 文件，则需要通过表达式指定忽略 org.apache.commons.chain.Command:execute() 方法调用实现类方法的调用

#### 1.7.1.3. 代码示例

参考 test.runbycode.extensions.methodcall.apachecommonschain.TestAddMethodCall4ApacheCommonsChain 类

## 1.8. 解析代码并将结果写入文件

假如在解析代码后不需要将结果写入数据库，只需要将结果写入文件，则可通过以下方式实现

### 1.8.1. 当前场景使用的配置参数

当前场景使用的配置参数使用说明，可参考 [RunnerWriteCallGraphFile - 解析代码并将结果写入文件](../class_use_config/RunnerWriteCallGraphFile.md)

#### 1.8.1.1. 重要的配置参数

#### 1.8.1.2. java-callgraph2

同上

#### 1.8.1.3. java-all-call-graph

无

### 1.8.2. 入口类

- 通过配置文件指定配置参数

当通过配置文件指定配置参数时，入口类为 test.jacg.TestRunnerWriteCallGraphFile

该类会调用下面的类

- 通过代码指定配置参数

通过代码指定配置参数时，入口类为 com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile

### 1.8.3. 示例代码

以下为通过代码指定配置参数执行时的示例代码

```java
// 生成 java-callgraph2 使用的配置参数包装类
JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
        TestConfigGenerator.TEST_JAR_PATH);

ConfigureWrapper configureWrapper = new ConfigureWrapper();
Assert.assertTrue(new RunnerWriteCallGraphFile(javaCG2ConfigureWrapper, configureWrapper).run());
```

对应 test.runbycodemain.TestRBCRunnerWriteCallGraphFile#$test0WriteFile 方法
