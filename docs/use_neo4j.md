# 1. 说明

对部分功能进行了使用Neo4j的支持，包括导入方法调用相关数据库表到Neo4j，读取Neo4j生成向下的方法完整调用链等

# 2. 组件依赖

使用Neo4j时，需要在项目中添加spring-data-neo4j的组件依赖，如下所示：

```
org.springframework.data:spring-data-neo4j:5.3.9.RELEASE
```

# 3. 参数配置

使用的Neo4j的配置需要在配置文件ogm.properties中进行配置，格式如下：

```properties
URI=bolt://username:password@host:port
```

假如使用Neo4j集群，则需要配置如下：

```properties
URIS=neo4j://host1:port1,host2:port2,host3:port3
username=xxx
password=xxx
```

# 4. 解析数据并将结果写入Neo4j

参考 test.neo4j.runner.Test0WriteData2Neo4j 类

# 2. 解析数据并将结果写入Neo4j及数据库

参考 test.neo4j.runner.Test0WriteData2Neo4jAndDb 类

# 5. 读取Neo4j生成向下的方法完整调用链

参考 test.neo4j.runner.TestNeo4jRunnerGenAllGraph4Caller 类
