[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-all-call-graph.svg)]()

[![Apache License 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)]()

# 1. 项目 GitHub 地址

[https://github.com/Adrninistrator/java-all-call-graph](https://github.com/Adrninistrator/java-all-call-graph)

# 2. 使用案例

以下为通过微信及搜索引擎等公共渠道找到的明确提到有使用 java-all-call-graph、java-callgraph2 的技术文章，如果不希望在这里展示，请联系我删除

## 2.1. vivo 千镜——调用图的构建及其在代码安全扫描中的应用

[https://mp.weixin.qq.com/s/pNhYzGoeCqp_zkOcNf0xgg](https://mp.weixin.qq.com/s/pNhYzGoeCqp_zkOcNf0xgg)

有使用的库：java-all-call-graph、java-callgraph2

## 2.2. 自如技术——代码瘦身的设计思想及技术内幕

[https://mp.weixin.qq.com/s/ovzhSasR6QL7L5vlFILDgw](https://mp.weixin.qq.com/s/ovzhSasR6QL7L5vlFILDgw)
 
有使用的库：java-callgraph2

## 2.3. 携程技术——干货 | 携程代码分析平台，快速实现精准测试与应用瘦身

[https://mp.weixin.qq.com/s/p9BcfNqLk2ZDUNcXukqXpg](https://mp.weixin.qq.com/s/p9BcfNqLk2ZDUNcXukqXpg)
 
有使用的库：java-callgraph2

# 3. 当前项目能够做什么

## 3.1. 总体目标

当前项目的目标是，通过 Java 代码对 Java 代码进行自动化静态分析，将 Java 代码中非结构化的信息结构化到关系型数据库中，为 Java 应用的开发、测试、安全等场景提供有价值的信息

## 3.2. 对代码静态分析并支持 SQL 查询

支持对编译后的 Java 代码进行静态分析，将相关信息写入数据库表，可使用 SQL 语句查询

支持解析 Java 中的常见数据，包括类、方法、字段、注解、泛型、方法调用、枚举等

支持解析部分常用开发框架信息，包括 MyBatis、Spring AOP、Spring Bean、Spring Controller、Spring Task 等

详细说明及使用方法见 [对 Java 代码静态分析并支持 SQL 查询](docs/usage_scenarios/parse_java_to_db.md)

## 3.3. 生成方法完整调用链

支持从指定的方法开始，向上或向下生成完整的调用链

详细说明及使用方法见 [静态分析生成 Java 代码方法完整调用链](docs/usage_scenarios/gen_all_call_graph.md)

## 3.4. 生成方法调用堆栈

支持从指定的方法开始，向上或向下生成完整的调用链后，再生成从起始方法到包含指定关键字方法的调用堆栈

详细说明及使用方法见 [静态分析生成 Java 代码方法调用堆栈](docs/usage_scenarios/gen_call_stack.md)

## 3.5. 从方法调用堆栈文件提取关注的信息

支持从指定的方法开始，向上或向下生成完整的调用链后，再生成从起始方法到包含指定关键字方法的调用堆栈。在生成调用链时，支持显示能够反映业务含义的信息，例如方法调用中使用的常量值、参数类型、方法的参数或返回值的泛型类型、方法使用 MyBatis 操作的数据库表名等，支持从方法调用堆栈文件提取关注的信息

详细说明及使用方法见 [从方法调用堆栈文件提取关注的信息](extract_call_stack_with_info.md)

## 3.6. 分析 Java 代码中通过 MyBatis 操作的数据库表名（支持 MySQL 数据库）

支持从指定的方法开始，向上或向下生成完整的调用链时，以及再生成从起始方法到包含指定关键字方法的调用堆栈时，支持获取并展示 MyBatis Mapper 方法操作的数据库表名

[](docs/analyse_use_mybatis_mysql_table.md)

## 3.7. JarDiff 比较 jar 文件版本差异及代码影响范围

支持对 jar 文件的不同版本比较差异，获得生成发生变化的方法，并生成向上或向下的完整调用链，获得代码影响范围

支持根据 MyBatis Mapper XML 文件 sql 语句的变化，获得对应的 MyBatis Mapper 方法，再生成调用链及获得代码影响范围

详细说明及使用方法见 [JarDiff 比较 jar 文件版本差异及代码影响范围](docs/usage_scenarios/jar_diff_call_graph.md)

## 3.8. JDK 及依赖库兼容问题扫描

当 Java 应用升级使用的 JDK 或依赖库版本时，可能出现依赖库中使用了找不到的类、方法、字段的兼容问题，支持对这些问题进行扫描，也支持检查类名完全相关的类

详细说明及使用方法见 [Java 应用 JDK 及依赖库兼容问题扫描工具](docs/usage_scenarios/jar_compatibility_check.md)

## 3.9. 获取方法调用参数类型或值

支持获取方法调用中的被调用对象或参数使用使用的常量值，或参数类型

详细说明及使用方法见 [静态分析获取 Java 代码方法调用参数类型或值](docs/usage_scenarios/method_call_args.md)

## 3.10. 解析字符串拼接

支持对使用字符串常量、枚举常量方法调用返回值等进行字符串拼接的情况进行解析，获取拼接前的各参数内容，及拼接后的字符串值

详细说明及使用方法见 [静态分析解析 Java 代码字符串拼接](docs/usage_scenarios/parse_string_append.md)

# 4. 加群讨论

已创建微信群“Java 静态分析交流@java-all-call-graph”，如果有兴趣可以加群讨论

由于群二维码只有 7 天有效期，请先添加微信，备注“jacg”，再加到群里

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/wechat.png)

由于只有部分业余时间维护，所以相关的讨论和问题不能保证及时回复，请见谅

建议先在 github 提 issue 再讨论，便于整理相关内容

后续有价值且不重复的问题相关内容会整理到文档中

# 5. 让大模型基于项目回答问题

## 5.1. DeepWiki

提问不需要注册

[https://deepwiki.com/Adrninistrator/java-all-call-graph](https://deepwiki.com/Adrninistrator/java-all-call-graph)

通过大模型分析项目代码，可向大模型提出关于项目的问题，包括使用方法等

## 5.2. zread.ai

提问需要注册

[https://zread.ai/Adrninistrator/java-all-call-graph](https://zread.ai/Adrninistrator/java-all-call-graph)

作用同上

# 6. 概念约定

## 6.1. 方法调用关系

方法调用关系是指，调用方法调用被调用方法，包括调用方法与被调用方法的包名、类名、方法名、方法参数、方法返回类型、方法调用类型（区分调用静态方法、非静态方法等）

例如 a.b.c.Caller:f1() 调用 a.b.c.Callee:f2(int,java.lang.String)

方法调用关系可以认为是调用方法与被调用方法组成的二元组

对应写入数据库的 method_call 表

## 6.2. 方法调用信息

方法调用信息是指，方法调用中，被调用对象或参数所使用的常量值，变量类型，变量对应的方法参数序号、其他方法调用的返回值，类的变量等信息

对应写入数据库的 method_call_info 表

## 6.3. 所有方法的调用图

对于某个 Java 项目，所有的方法调用所形成的调用图是一个非常复杂的图，例如以下为一个简单的示例：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/method_call_graph.png)

## 6.4. 方法完整调用链

从某个方法开始的向下的完整调用链，从该方法开始，经过该方法直接调用及间接调用的方法，最后到没有调用其他方法的方法为止

从某个方法开始的向上的完整调用链，从该方法开始，经过直接调用及间接调用该方法的方法，最后到没有被其他方法调用的方法为止

方法完整调用链是树形的结构，如下所示

```java
com.example.Main:main(String[])
├─ com.example.service.UserService:getUserById(Long)
│   ├─ com.example.repository.UserRepository:findById(Long)
│   │   ├─ com.example.mapper.UserMapper:toEntity(ResultSet)
│   │   └─ com.example.validator.IdValidator:validate(Long)
│   └─ com.example.cache.UserCache:get(String)
└─ com.example.util.Logger:info(String)
```

如下图所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/method_call_all.png)

从某个方法向上或向下生成的完整调用链，可以认为是所有方法的调用图中的一个子图（形状类似树，有根节点，但因为可能存在循环调用，因此是图）

## 6.5. 方法调用堆栈

方法调用堆栈是指，在方法完整调用链的基础上，从起始方法到某个调用/被调用方法之间的调用路径

方法调用堆栈是链表结构，如下所示

```java
com.example.Main:main(String[])
└─ com.example.service.UserService:getUserById(Long)
    └─ com.example.repository.UserRepository:findById(Long)
        └─ com.example.mapper.UserMapper:toEntity(ResultSet)

com.example.Main:main(String[])
└─ com.example.util.Logger:info(String)
```

如下图中红色的调用路径所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/method_call_color.png)

方法调用堆栈可以认为是该方法完整调用链中的一条调用路径

# 7. 依赖库关系

相关依赖库之间的关系如下：

java-all-call-graph 依赖 java-callgraph2，用于对 Java 代码进行静态分析

java-callgraph2 依赖 bcel，用于解析 Java 字节码信息

java-all-call-graph 依赖 druid，作为数据源

java-all-call-graph 依赖 mybatis-mysql-table-parser，用于解析 MyBatis XML 中的 MySQL 语句

mybatis-mysql-table-parser 依赖 druid，用于解析 SQL 语句

mybatis-mysql-table-parser 依赖 jdom2，用于解析 XML 文件

java-all-call-graph 依赖 java-text-to-excel，用于根据文本生成 Excel 文件

java-text-to-excel 依赖 fastexcel，用于生成 Excel 文件

如下图所示

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/001_lib_relationship.png)

# 8. 主要流程

## 8.1. 解析代码并写入数据库

`当前项目在执行时，首先需要解析代码并写入数据库`，步骤如下

java-all-call-graph 库的 RunnerWriteDb 类调用 java-callgraph2 库

java-callgraph2 库读取需要解析的 jar 等文件，进行静态分析，将分析结果写入文本文件

RunnerWriteDb 类读取以上生成的文件，写入数据库

以上 RunnerWriteDb 类的使用说明见“当前项目能够做什么”“对代码静态分析并支持 SQL 查询”部分

`当被解析的代码发生变化时，需要重新执行当前步骤，使数据库中写入最新代码对应的数据`

## 8.2. 从数据库读取并生成数据

java-all-call-graph 库的其他类读取数据库数据，按要求生成数据

包括用于生成方法完整调用链、方法调用堆栈等文件的类

以上流程如下图所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/002_main_steps.png)

# 9. 使用说明

## 9.1. 依赖环境

需要使用 JDK8 及以上版本

若需要通过 IDE 打开项目源码运行，建议安装 Gradle 管理依赖库

## 9.2. 配置参数

### 9.2.1. 支持的参数配置方式

```
通过配置文件指定
通过代码指定
```

以上两种方式的效果是相同的，每个配置文件及配置参数在代码中都存在对应项

### 9.2.2. 支持的配置参数格式

支持以下三种格式的配置参数

#### 9.2.2.1. Map 格式-key value 形式的参数

当前项目中各个参数的对应两种枚举，分别是与数据库无关的配置参数、与数据库有关的配置参数

```
com.adrninistrator.jacg.conf.enums.ConfigKeyEnum
com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum
```

以上枚举对应的配置文件分别如下

```
_jacg_config/config.properties
_jacg_config/config_db.properties
```

每个枚举常量代表一个参数，参数为键值对形式，每个参数指定唯一的值

#### 9.2.2.2. List 格式-区分顺序的参数

当前项目中的对应枚举为 com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值区分顺序，可指定多个值

#### 9.2.2.3. Set 格式-不区分顺序的参数

当前项目中的对应枚举为 com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值不区分顺序，可指定多个值

#### 9.2.2.4. EL 表达式

当前项目中的对应枚举为 com.adrninistrator.jacg.el.enums.ElConfigEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值需要指定 EL 表达式，整个参数值是一个表达式

### 9.2.3. 配置参数示例

参考 [配置参数示例](docs/_jacg_all_config.md)

### 9.2.4. EL 表达式使用通用说明文档

参考 [EL 表达式使用通用说明文档](java-all-call-graph/src/main/resources/_el_example/el_usage.md)

### 9.2.5. EL 表达式字符串比较说明文档

参考 [EL 表达式字符串比较说明文档](java-all-call-graph/src/main/resources/_el_example/string_compare.md)

### 9.2.6. 通过代码指定配置参数

在代码中使用 com.adrninistrator.jacg.conf.ConfigureWrapper 类可以指定配置参数

以下为 ConfigureWrapper 用于指定配置参数的方法

|方法名称|方法作用|
|---|---|
|setMainConfig|设置 key value 形式的参数|
|setOtherConfigList|设置区分顺序的参数|
|setOtherConfigSet|设置不区分顺序的参数|
|setElConfigText|设置 EL 表达式|

## 9.3. 运行方式

不同场景下需要运行的类，参考“当前项目能够做什么”部分的对应文档

### 9.3.1. 支持的运行方式

```
通过 IDE 打开项目源码运行
在其他项目中引用当前项目的库运行
使用项目源码构建后运行
```

### 9.3.2. 各种运行方式支持的参数配置方式

|运行方式|支持的参数配置方式|
|---|---|
|通过 IDE 打开项目源码运行|通过配置文件指定<br>通过代码指定|
|在其他项目中引用当前项目的库运行|通过配置文件指定<br>通过代码指定|
|使用项目源码构建后运行|通过配置文件指定|

### 9.3.3. 通过 IDE 打开项目源码运行

通过 IDE 打开当前项目，由 Gradle 管理依赖库，可使用源码运行

#### 9.3.3.1. 通过配置文件指定参数运行

假如需要通过配置文件指定参数，可修改项目中的配置文件相关，再运行各种场景对应的入口类，在 test 模块的 test.jacg 包中

项目运行模块需要选择 test，以使 test 模块中的 log4j2 配置文件生效

#### 9.3.3.2. 通过代码指定参数运行

假如需要通过代码指定参数，可直接执行各场景对应的示例方法，或者参考示例方法进行修改

### 9.3.4. 在其他项目中引用当前项目的库运行

在其他的项目中，使用 Maven/Gradle 等管理依赖库，并添加对当前项目的依赖

- 使用 Maven 管理依赖

```xml
<dependency>
    <groupId>com.github.adrninistrator</groupId>
    <artifactId>java-all-call-graph</artifactId>
    <version>版本号</version>
</dependency>
```

- 使用 Gradle 管理依赖

```
implementation("com.github.adrninistrator:java-all-call-graph: 版本号")
```

最新版本号可查看 [https://mvnrepository.com/artifact/com.github.adrninistrator/java-all-call-graph](https://mvnrepository.com/artifact/com.github.adrninistrator/java-all-call-graph)

`本工具仅引入了 slf4j-api 组件，在引入本工具组件的项目中，还需要引入 log4j2、logback 等日志组件，且保证配置正确，能够在本地正常运行`

`由于 Maven 间接依赖的组件版本不会自动使用最大的版本号，因此可能需要在项目中手工指定 java-all-call-graph 依赖组件的版本号，避免因为依赖组件版本不一致导致问题，可通过 java-all-call-graph 与 java-callgraph2 的 pom 文件的 dependencies 元素查看依赖组件版本`

#### 9.3.4.1. 通过配置文件指定参数运行

执行依赖的 java-all-call-graph 库中的 com.adrninistrator.jacg.unzip.UnzipFile 类，执行模块选择当前项目引入 java-all-call-graph 库的对应模块

当前步骤只需要执行一次，`但假如 java-all-call-graph 升级后，若对配置文件有新增或修改，则需要再执行当前步骤，否则可能会因为缺少配置文件导致执行失败`

以上类会将在其他项目中释放 java-all-call-graph 项目的入口类与配置文件，及 java-callgraph2 项目的配置文件

释放的根目录按照优先级如下所示：

|释放的根目录|前提条件|
|---|---|
|src/test|src/test 目录存在|
|src/unit.test|src/test 目录不存在，src/unit.test 目录存在|
|_jacg-{时间戳}|以上目录都不存在|

java-all-call-graph 项目的入口类会写入释放的根目录下的 java/test/jacg 目录，配置文件会写入释放的根目录下的 resources/目录，子目录名为`_el_example`，或以`_jacg_`开头

java-callgraph2 项目的配置文件会写入释放的根目录下的 resources/目录，子目录名为`_el_example`，或以`_javacg2_`开头

修改配置文件相关参数后，可运行对应的入口类

#### 9.3.4.2. 通过代码指定参数运行

与“通过 IDE 打开项目源码运行”的说明相同

### 9.3.5. 使用项目源码构建后运行

#### 9.3.5.1. 构建方式

在项目根目录执行以下命令

```
gradlew gen_run_jar
```

#### 9.3.5.2. 生成的文件

构建完成后，会在项目根目录 jar_output_dir 生成相关目录及文件

|目录、文件名|作用|
|---|---|
|_el_example|表达式相关的示例与说明文件|
|_javacg2_xxx|当前项目使用的配置文件保存目录|
|config|log4j2 配置文件保存目录|
|jar|当前项目编译生成的 jar 文件保存目录|
|lib|当前项目的依赖库 jar 文件|
|log|保存日志文件目录，运行后会生成|
|xxx.bat|不同场景用于执行的脚本|
|xxx.sh|不同场景用于执行的脚本|

#### 9.3.5.3. 通过配置文件指定参数运行

对配置文件参数进行配置后，可执行对应的 xxx.bat 或 xxx.sh 脚本，调用对应的入口类

### 9.3.6. 通过代码执行的其他示例

参考 [通过代码执行的其他示例](docs/run_by_code_example.md)

# 10. 更新说明

[更新说明](docs/change_log.md)
