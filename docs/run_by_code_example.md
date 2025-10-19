# 1. 解析测试代码并将结果写入Neo4j数据库

- 示例类名

```
test.neo4j.runner.Test0WriteData2Neo4j
```

- 示例说明

结果仅写入Neo4j，不写入数据库

# 2. 解析测试代码并将结果写入Neo4j及数据库

- 示例类名

```
test.neo4j.runner.Test0WriteData2Neo4jAndDb
```

# 3. 根据Neo4j的数据生成向下的方法完整调用链

- 示例类名

```
test.neo4j.runner.TestNeo4jRunnerGenAllGraph4Caller
```

# 4. 生成向下完整方法调用链时，验证方法参数作为被调用对象涉及多态时的类型替换

- 示例类名

```
test.runbycode.callgraph.calleeargtypepolymorphism.TestGenCallerGraphCalleeArgTypePolymorphism
```

# 5. 为指定包中的全部方法生成完整调用链

- 示例类名

```
test.runbycode.callgraph.TestGenAllGraph4CallerByPackage
```

- 示例说明

首先查询指定包中的类的全部方法

再为这些方法生成向下的完整方法调用链

# 6. 仅创建数据库表

- 示例类名

```
test.runbycode.createtable.TestCreateTable
```

- 示例说明

不写入数据

# 7. 解析包含重复同名类的代码并将结果写入数据库

- 示例类名

```
test.runbycode.dupclass.TestDupClass
```

- 示例说明

需要先执行 gradle 命令，生成用于比较的示例jar包

gradlew test_gen_diff_jar -Pexample_flag=1

gradlew test_gen_diff_jar -Pexample_flag=2

# 8. 获取直接或间接调用指定方法的入口方法

- 示例类名

```
test.runbycode.extractor.callee.TestExtractCalleeGraphToEntry
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链中找到对应的入口方法（即向上没有被其他方法调用的方法）

# 9. 获取直接或间接调用指定方法的Spring Controller方法

- 示例类名

```
test.runbycode.extractor.callee.TestExtractCalleeGraphToSPC
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到Spring Controller方法

# 10. 获取直接或间接调用指定方法的Spring Controller文件下载方法

- 示例类名

```
test.runbycode.extractor.callee.TestExtractCalleeGraphToSPCFileDownload
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到（可能的）Spring Controller文件下载方法

# 11. 获取直接或间接调用指定方法的Spring Controller文件上传方法

- 示例类名

```
test.runbycode.extractor.callee.TestExtractCalleeGraphToSPCFileUpload
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到（可能的）Spring Controller文件上传方法

# 12. 获取直接或间接调用指定方法的Spring Controller文件下载方法，支持自定义筛选

- 示例类名

```
test.runbycode.extractor.callee.TestMyCalleeGraphSPCFileDownloadExtractor
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到（可能的）Spring Controller文件下载方法

对于需要查找的（可能的）Spring Controller文件下载方法，支持通过自定义处理进行筛选等操作

# 13. 查询方法被直接调用的情况

- 示例类名

```
test.runbycode.handler.methodcall.TestQueryNormalMethodCallByCalleeClassMethod
```

- 示例说明

对于指定的类名及方法名，查询这些方法在其他方法中被直接调用的情况（字节码中存在对应的方法调用指令）

查询结果包含相关的调用方法与被调用方法的详情信息

# 14. 解析代码并将结果写入数据库，对Spring AOP相关信息进行解析

- 示例类名

```
test.runbycode.spring.aop.TestSpringAopRunnerWriteDb
```

- 示例说明

需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar

通过代码指定配置参数的主要功能示例

# 15. 解析代码并将结果写入数据库，对Spring AOP相关信息进行解析

- 示例类名

```
test.runbycode.spring.aop.TestSpringAopRunnerWriteDbEl
```

- 示例说明

需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar

通过代码指定配置参数的主要功能示例

通过EL表达式指定在处理Spring AOP时需要忽略哪些Spring Bean类

# 16. 通过表达式配置需要处理的XML中的Spring Bean

- 示例类名

```
test.runbycode.spring.bean.TestSpringBeanInXmlRunnerWriteDbEl
```

- 示例说明

支持通过Bean名称、Bean类名、profile进行过滤

## 16.1. 仅处理Bean名称匹配的记录

- 示例方法名

```
onlyHandleBeanNameMatches
```

## 16.2. 全部都处理

- 示例方法名

```
fixedFalseParseAll
```

## 16.3. 仅处理profile为空的记录

- 示例方法名

```
onlyHandleProfileEmpty
```

## 16.4. 仅处理profile匹配的记录，一级

- 示例方法名

```
onlyHandleProfileMatchesLevelOne
```

## 16.5. 仅处理Bean类名匹配的记录

- 示例方法名

```
onlyHandleClassNameMatches
```

## 16.6. 仅处理profile匹配的记录，二级

- 示例方法名

```
onlyHandleProfileMatchesLevelTwo
```

# 17. 解析代码并将结果写入数据库，简单模式

- 示例类名

```
test.runbycodemain.TestRBC0RunnerSimpleWriteDb
```

- 示例说明

处理方法调用时不解析被调用对象和参数可能的类型与值

需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar

通过代码指定配置参数的主要功能示例

# 18. 解析代码并将结果写入数据库，使用表达式忽略特定内容

- 示例类名

```
test.runbycodemain.TestRBC0RunnerWriteDbEl
```

- 示例说明

需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar

通过代码指定配置参数的主要功能示例

## 18.1. 仅解析指定包下的类的方法调用

- 示例方法名

```
testElOnlyParseSomeMethodCall
```

- 示例说明

通过表达式实现，仅当类的包名以 test.callgraph.methodcall. 开头时不跳过

## 18.2. 仅解析jar文件中指定路径下的jar文件

- 示例方法名

```
testElOnlyParseNonLibJarInJar
```

- 示例说明

通过表达式实现，当jar文件的目录名称为'lib'时跳过

需要先执行以下命令生成包含jar文件的jar文件

gradlew gen_run_jar gen_jar_in_jar

## 18.3. 仅解析指定包下的类

- 示例方法名

```
testElOnlyParseSomeClass
```

- 示例说明

通过表达式实现，仅当类的包名以 test.callgraph.methodcall. 开头时不跳过

## 18.4. 仅解析目录中指定路径下指定名称的jar文件

- 示例方法名

```
testElOnlyParseSomeJarInDir
```

- 示例说明

通过表达式实现，仅处理lib中文件名以commons-开头的jar文件

需要先执行以下命令生成包含jar文件的jar文件

gradlew gen_run_jar

## 18.5. 仅解析war文件中指定路径下的jar文件

- 示例方法名

```
testElOnlyParseNonLibJarInWar
```

- 示例说明

通过表达式实现，当jar文件的目录名称为'WEB-INF/lib'时跳过

需要先执行以下命令生成包含jar文件的war文件

gradlew gen_run_jar gen_jar_in_war

## 18.6. 所有的内容都不解析

- 示例方法名

```
testElFixedTrueParseNone
```

- 示例说明

通过表达式实现

# 19. 解析代码并将结果写入数据库

- 示例类名

```
test.runbycodemain.TestRBC1RunnerWriteDb
```

- 示例说明

需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar

通过代码指定配置参数的主要功能示例

# 20. 获得方法向上到包含关键字的调用堆栈

- 示例类名

```
test.runbycodemain.TestRBCFindCallStackTrace4ee
```

- 示例说明

首先会生成指定方法向上的完整方法调用链

通过代码指定配置参数的主要功能示例

# 21. 获得方法向下到包含关键字的调用堆栈

- 示例类名

```
test.runbycodemain.TestRBCFindCallStackTrace4er
```

- 示例说明

首先会生成指定方法向下的完整方法调用链

通过代码指定配置参数的主要功能示例

# 22. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4Callee
```

- 示例说明

通过代码指定配置参数的主要功能示例

## 22.1. 方法调用链数据仅写入文件

- 示例方法名

```
testWriteToFile
```

- 示例说明

方法调用链数据不在内存中返回

## 22.2. 方法调用链数据写入文件，也在内存中返回

- 示例方法名

```
testBoth
```

## 22.3. 方法调用链数据仅在内存中返回，返回多个方法

- 示例方法名

```
testReturnInMemoryMulti
```

- 示例说明

方法调用链数据不写入文件

## 22.4. 方法调用链数据仅写入文件，生成文件名使用更短的模式

- 示例方法名

```
testWriteToFileShortName
```

- 示例说明

方法调用链数据不在内存中返回

## 22.5. 方法调用链数据仅在内存中返回

- 示例方法名

```
testReturnInMemory
```

- 示例说明

方法调用链数据不写入文件

# 23. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CalleeDetail0
```

- 示例说明

输出方法调用链格式使用最详细，包含返回类型

通过代码指定配置参数的主要功能示例

# 24. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CalleeEmpty
```

- 示例说明

指定的方法未被其他方法调用

通过代码指定配置参数的主要功能示例

# 25. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CalleeLimitDepth
```

- 示例说明

限制允许生成的方法调用链深度限制

通过代码指定配置参数的主要功能示例

## 25.1. 方法调用链数据仅写入文件

- 示例方法名

```
testWriteToFile
```

- 示例说明

方法调用链数据不在内存中返回

## 25.2. 方法调用链数据仅在内存中返回

- 示例方法名

```
testReturnInMemory
```

- 示例说明

方法调用链数据不写入文件

# 26. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CalleeLimitNum
```

- 示例说明

限制每个方法允许生成的方法调用数量限制

通过代码指定配置参数的主要功能示例

## 26.1. 方法调用链数据仅写入文件

- 示例方法名

```
testWriteToFile
```

- 示例说明

方法调用链数据不在内存中返回

## 26.2. 方法调用链数据仅在内存中返回

- 示例方法名

```
testReturnInMemory
```

- 示例说明

方法调用链数据不写入文件

# 27. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CalleeNotFound
```

- 示例说明

指定的方法或类不存在

通过代码指定配置参数的主要功能示例

# 28. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4Caller
```

- 示例说明

通过代码指定配置参数的主要功能示例

## 28.1. 方法调用链数据仅写入文件

- 示例方法名

```
testWriteToFile
```

- 示例说明

方法调用链数据不在内存中返回

## 28.2. 方法调用链数据写入文件，也在内存中返回

- 示例方法名

```
testBoth
```

## 28.3. 方法调用链数据仅在内存中返回，返回多个方法

- 示例方法名

```
testReturnInMemoryMulti
```

- 示例说明

方法调用链数据不写入文件

## 28.4. 方法调用链数据仅写入文件，生成文件名使用更短的模式

- 示例方法名

```
testWriteToFileShortName
```

- 示例说明

方法调用链数据不在内存中返回

## 28.5. 方法调用链数据仅在内存中返回

- 示例方法名

```
testReturnInMemory
```

- 示例说明

方法调用链数据不写入文件

# 29. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CallerDetail0
```

- 示例说明

输出方法调用链格式使用最详细，包含返回类型

通过代码指定配置参数的主要功能示例

# 30. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CallerEmpty
```

- 示例说明

指定的方法未调用其他方法

通过代码指定配置参数的主要功能示例

# 31. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CallerLimitDepth
```

- 示例说明

限制允许生成的方法调用链深度限制

通过代码指定配置参数的主要功能示例

## 31.1. 方法调用链数据仅写入文件

- 示例方法名

```
testWriteToFile
```

- 示例说明

方法调用链数据不在内存中返回

## 31.2. 方法调用链数据仅在内存中返回

- 示例方法名

```
testReturnInMemory
```

- 示例说明

方法调用链数据不写入文件

# 32. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CallerLimitNum
```

- 示例说明

限制每个方法允许生成的方法调用数量限制

通过代码指定配置参数的主要功能示例

## 32.1. 方法调用链数据仅写入文件

- 示例方法名

```
testWriteToFile
```

- 示例说明

方法调用链数据不在内存中返回

## 32.2. 方法调用链数据仅在内存中返回

- 示例方法名

```
testReturnInMemory
```

- 示例说明

方法调用链数据不写入文件

# 33. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRBCRunnerGenAllGraph4CallerNotFound
```

- 示例说明

指定的方法或类不存在

通过代码指定配置参数的主要功能示例

# 34. 解析代码并将结果写入文件

- 示例类名

```
test.runbycodemain.TestRBCRunnerWriteCallGraphFile
```

- 示例说明

解析类、方法、方法调用等信息

生成的数据写入文件，但不写入数据库

通过代码指定配置参数的主要功能示例

