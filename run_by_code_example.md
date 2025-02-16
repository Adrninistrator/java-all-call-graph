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

# 4. 仅创建数据库表

- 示例类名

```
test.runbycode.createtable.TestCreateTable
```

- 示例说明

不写入数据

# 5. 解析包含重复同名类的代码并将结果写入数据库

- 示例类名

```
test.runbycode.dupclass.TestDupClass
```

- 示例说明

需要先执行 unittest.gradle 中的命令，生成用于比较的示例jar包

# 6. 获取直接或间接调用指定方法的入口方法

- 示例类名

```
test.runbycode.example.TestExtractCalleeGraphToEntry
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链中找到对应的入口方法（即向上没有被其他方法调用的方法）

# 7. 获取直接或间接调用指定方法的Spring Controller方法

- 示例类名

```
test.runbycode.example.TestExtractCalleeGraphToSPC
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到Spring Controller方法

# 8. 获取直接或间接调用指定方法的Spring Controller文件下载方法

- 示例类名

```
test.runbycode.example.TestExtractCalleeGraphToSPCFileDownload
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到（可能的）Spring Controller文件下载方法

# 9. 获取直接或间接调用指定方法的Spring Controller文件上传方法

- 示例类名

```
test.runbycode.example.TestExtractCalleeGraphToSPCFileUpload
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到（可能的）Spring Controller文件上传方法

# 10. 为指定包中的全部方法生成完整调用链

- 示例类名

```
test.runbycode.example.TestGenAllGraph4CallerByPackage
```

- 示例说明

首先查询指定包中的类的全部方法

再为这些方法生成向下的完整方法调用链

# 11. 获取直接或间接调用指定方法的Spring Controller文件下载方法，支持自定义筛选

- 示例类名

```
test.runbycode.example.TestMyCalleeGraphSPCFileDownloadExtractor
```

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链向上找到（可能的）Spring Controller文件下载方法

对于需要查找的（可能的）Spring Controller文件下载方法，支持通过自定义处理进行筛选等操作

# 12. 查询方法被直接调用的情况

- 示例类名

```
test.runbycode.example.TestQueryNormalMethodCallByCalleeClassMethod
```

- 示例说明

对于指定的类名及方法名，查询这些方法在其他方法中被直接调用的情况（字节码中存在对应的方法调用指令）

查询结果包含相关的调用方法与被调用方法的详情信息

# 13. 通过代码修改 java-callgraph2 组件使用的配置参数

- 示例类名

```
test.runbycode.example.TestSetJavaCG2Config
```

- 示例说明

调用 RunnerWriteDb.run() 方法时指定 JavaCG2ConfigureWrapper 参数

# 14. 解析代码并将结果写入数据库，简单模式

- 示例类名

```
test.runbycodemain.Test0RunnerSimpleWriteDb
```

- 示例说明

处理方法调用时不解析被调用对象和参数可能的类型与值

仅解析.class文件，不解析.xml、.properties等其他类型的文件

# 15. 解析代码并将结果写入数据库

- 示例类名

```
test.runbycodemain.Test1RunnerWriteDb
```

# 16. 获得方法向上到包含关键字的调用堆栈

- 示例类名

```
test.runbycodemain.TestFindCallStackTrace4ee
```

- 示例说明

首先会生成指定方法向上的完整方法调用链

# 17. 获得方法向下到包含关键字的调用堆栈

- 示例类名

```
test.runbycodemain.TestFindCallStackTrace4er
```

- 示例说明

首先会生成指定方法向下的完整方法调用链

# 18. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRunnerGenAllGraph4Callee
```

# 19. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRunnerGenAllGraph4CalleeEmptyClass
```

- 示例说明

生成结果为空

# 20. 生成指定方法向上的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRunnerGenAllGraph4CalleeLimit
```

- 示例说明

限制每个方法允许生成的方法调用数量限制

# 21. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRunnerGenAllGraph4Caller
```

# 22. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRunnerGenAllGraph4CallerEmptyClass
```

- 示例说明

生成结果为空

# 23. 生成指定方法向下的完整方法调用链

- 示例类名

```
test.runbycodemain.TestRunnerGenAllGraph4CallerLimit
```

- 示例说明

限制每个方法允许生成的方法调用数量限制

# 24. 解析代码并将结果写入文件

- 示例类名

```
test.runbycodemain.TestRunnerWriteCallGraphFile
```

- 示例说明

生成的数据不写入数据库

