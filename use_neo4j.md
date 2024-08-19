# 1. 说明

对部分功能进行了使用Neo4j的支持，包括导入方法调用相关数据库表到Neo4j，读取Neo4j生成向下的完整方法调用链等

# 2. 参数配置

使用的Neo4j的配置需要在配置文件ogm.properties中进行配置，格式如下：

```
URI=bolt://username:password@host:port
```

# 3. 解析数据并导入Neo4j

参考 test.neo4j.runner.Test0Neo4jRunnerWriteDb 类

# 4. 读取Neo4j生成向下的完整方法调用链

参考 test.neo4j.runner.TestNeo4jRunnerGenAllGraph4Caller 类
