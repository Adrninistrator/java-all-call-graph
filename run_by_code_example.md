# 1. 解析测试代码并将结果写入Neo4j数据库

- 示例类名

test.neo4j.runner.Test0WriteData2Neo4j

- 示例说明

结果仅写入Neo4j，不写入数据库

# 2. 解析测试代码并将结果写入Neo4j及数据库

- 示例类名

test.neo4j.runner.Test0WriteData2Neo4jAndDb

# 3. 根据Neo4j的数据生成向下的方法完整调用链

- 示例类名

test.neo4j.runner.TestNeo4jRunnerGenAllGraph4Caller

# 4. 获取调用指定方法的入口方法

- 示例类名

test.runbycode.example.TestExtractCalleeGraphToEntry

- 示例说明

首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况

再从这些完整方法调用链中找到对应的入口方法（即向上没有被其他方法调用的方法）

# 5. 为指定包中的全部方法生成完整调用链

- 示例类名

test.runbycode.example.TestGenAllGraph4CallerByPackage

- 示例说明

首先查询指定包中的类的全部方法

再为这些方法生成向下的完整方法调用链

# 6. 查询方法被直接调用的情况

- 示例类名

test.runbycode.example.TestQueryNormalMethodCallByCalleeClassMethod

- 示例说明

对于指定的类名及方法名，查询这些方法在其他方法中被直接调用的情况

查询结果包含相关的调用方法与被调用方法的详情信息

# 7. 解析代码并将结果写入数据库

- 示例类名

test.runbycode.TestRBC0RunnerAllWriteDb

- 示例说明

所有的包名都处理

# 8. 解析代码并将结果写入数据库

- 示例类名

test.runbycode.TestRBC0RunnerWriteDb

- 示例说明

仅处理指定的包名

# 9. 获得方法向上到包含关键字的调用堆栈

- 示例类名

test.runbycode.TestRBCFindCallStackTrace4ee

- 示例说明

首先会生成指定方法向上的完整方法调用链

# 10. 获得方法向下到包含关键字的调用堆栈

- 示例类名

test.runbycode.TestRBCFindCallStackTrace4er

- 示例说明

首先会生成指定方法向下的完整方法调用链

# 11. 生成指定方法向上的完整方法调用链

- 示例类名

test.runbycode.TestRBCRunnerGenAllGraph4Callee

# 12. 生成指定方法向下的完整方法调用链

- 示例类名

test.runbycode.TestRBCRunnerGenAllGraph4Caller

# 13. 解析代码并将结果写入文件

- 示例类名

test.runbycode.TestRBCRunnerWriteCallGraphFile

- 示例说明

生成的数据不写入数据库

