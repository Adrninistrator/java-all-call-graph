# 1. 说明

对部分功能进行了使用Neo4j的支持，包括导入方法调用相关数据库表到Neo4j，读取Neo4j生成向下的完整方法调用链等

# 2. 组件依赖

使用Neo4j时，需要在项目中添加spring-data-neo4j的组件依赖，如下所示：

```
org.springframework.data:spring-data-neo4j:5.3.9.RELEASE
```

# 3. 参数配置

使用的Neo4j的配置需要在配置文件ogm.properties中进行配置，格式如下：

```
URI=bolt://username:password@host:port
```

# 4. 解析数据并导入Neo4j

参考 test.neo4j.runner.Test0Neo4jRunnerWriteDb 类

# 5. 读取Neo4j生成向下的完整方法调用链

参考 test.neo4j.runner.TestNeo4jRunnerGenAllGraph4Caller 类
