# 1. 输出结果示例

## 1.1. 调用指定类方法向上的完整调用链示例

调用指定类方法向上的完整调用链如下所示：

```
[0]#DestClass.destfunc()
[1]#  ClassA3.funcA3()	(ClassA3:10)
[2]#    ClassA2.funcA2()	(ClassA2:19)
[3]#      ClassA1.funcA1()	(ClassA1:23)    !entry!
[1]#  ClassB1.funcB1()	(ClassB1:57)    !entry!
[1]#  ClassC2.funcC2()	(ClassC2:31)
[2]#    ClassC1.funcC1()	(ClassC1:9)    !entry!
```

以上对应的调用关系如下所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/pic/example-cg-4callee.png)

调用指定类方法向上的完整调用链输出结果格式类似一棵树，每行代表一个调用者Java方法，前面的数字越大代表调用层级越靠上，0代表被调用的指定类中的方法。

每行后部的“(TestClass:39)”格式的类名及数字代表当前调用者类名，及调用者方法对应的源代码行号。

对于不被其他方法调用的方法，认为是入口方法，在对应行的最后会显示“!entry!”。

以下为使用本工具生成的调用Mybatis的MyBatisExceptionTranslator类的部分方法向上完整调用链（方法参数太长，已省略）：

```
org.mybatis.spring.MyBatisExceptionTranslator:initExceptionTranslator()
[0]#org.mybatis.spring.MyBatisExceptionTranslator:initExceptionTranslator
[1]#  org.mybatis.spring.MyBatisExceptionTranslator:translateExceptionIfPossible	(MyBatisExceptionTranslator:88)	!entry!
[1]#  org.mybatis.spring.MyBatisExceptionTranslator:<init>	(MyBatisExceptionTranslator:72)
[2]#    org.mybatis.spring.MyBatisExceptionTranslator:<init>	(MyBatisExceptionTranslator:55)
[3]#      org.mybatis.spring.SqlSessionTemplate:<init>	(SqlSessionTemplate:106)
[4]#        org.mybatis.spring.SqlSessionTemplate:<init>	(SqlSessionTemplate:92)
[5]#          org.mybatis.spring.support.SqlSessionDaoSupport:createSqlSessionTemplate	(SqlSessionDaoSupport:70)
[6]#            org.mybatis.spring.support.SqlSessionDaoSupport:setSqlSessionFactory	(SqlSessionDaoSupport:52)	!entry!
[4]#        org.mybatis.spring.batch.MyBatisPagingItemReader:doReadPage	(MyBatisPagingItemReader:99)	!entry!
[4]#        org.mybatis.spring.batch.MyBatisBatchItemWriter:setSqlSessionFactory	(MyBatisBatchItemWriter:85)
[5]#          org.mybatis.spring.batch.builder.MyBatisBatchItemWriterBuilder:build	(MyBatisBatchItemWriterBuilder:113)	!entry!
```

`IDEA使用技巧：在IntelliJ IDEA中，打开“Navigate Class...”窗口，即根据类名进入对应代码文件的窗口后，若输入[类名]:[行号]格式的内容并回车，可打开对应的代码文件并跳转到对应的行号。`

## 1.2. 指定方法向下完整调用链示例

指定方法向下完整调用链如下所示：

```
[0]#SrcClass.srcfunc()
[1]#  [SrcClass:15]	ClassA1.funcA1()
[2]#    [ClassA1:27]	ClassA2a.funcA2a()
[2]#    [ClassA1:59]	ClassA2b.funcA2b()
[3]#      [ClassA2b:39]	ClassA3.funcA3()
[1]#  [SrcClass:17]	ClassB1.funcB1()
[1]#  [SrcClass:23]	ClassC1.funcC1()
[2]#    [ClassC1:75]	ClassC2.funcC2()
```

以上对应的调用关系如下所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/pic/example-cg-4caller.png)

指定方法向下完整调用链输出结果类似一棵树，每行代表一个被调用者Java方法，与实际的代码执行顺序一致，前面的数字越大代表调用层级越靠下，0代表指定方法。

每行前部的“\[TestClass:39\]”格式的类名及数字，代表当前调用者类名，及调用者方法及对应的源代码行号。

以下为使用本工具生成的Mybatis的BatchExecutor:doUpdate()方法向下的完整调用链：

```
org.apache.ibatis.executor.BatchExecutor:doUpdate(org.apache.ibatis.mapping.MappedStatement,java.lang.Object)
[0]#org.apache.ibatis.executor.BatchExecutor:doUpdate
[1]#  [BatchExecutor:57]	org.apache.ibatis.session.Configuration:newStatementHandler
[2]#    [Configuration:659]	org.apache.ibatis.plugin.InterceptorChain:pluginAll
[3]#      [InterceptorChain:31]	org.apache.ibatis.plugin.Interceptor:plugin
[4]#        [Interceptor:28]	org.apache.ibatis.plugin.Plugin:wrap
[1]#  [BatchExecutor:64]	org.apache.ibatis.executor.BatchExecutor:applyTransactionTimeout
[2]#    [BatchExecutor:0]	org.apache.ibatis.executor.BaseExecutor:applyTransactionTimeout
[3]#      [BaseExecutor:301]	org.apache.ibatis.executor.statement.StatementUtil:applyTransactionTimeout
[1]#  [BatchExecutor:65]	org.apache.ibatis.executor.statement.StatementHandler:parameterize
[2]#    [StatementHandler:0]	org.apache.ibatis.executor.statement.RoutingStatementHandler:parameterize
[3]#      [RoutingStatementHandler:64]	org.apache.ibatis.executor.statement.StatementHandler:parameterize	!cycle[1]!
[1]#  [BatchExecutor:67]	org.apache.ibatis.executor.BatchResult:addParameterObject
[1]#  [BatchExecutor:70]	org.apache.ibatis.executor.statement.StatementHandler:prepare
[2]#    [StatementHandler:0]	org.apache.ibatis.executor.statement.BaseStatementHandler:prepare
[3]#      [BaseStatementHandler:85]	org.apache.ibatis.executor.ErrorContext:instance
[3]#      [BaseStatementHandler:85]	org.apache.ibatis.executor.ErrorContext:sql
[3]#      [BaseStatementHandler:88]	org.apache.ibatis.executor.statement.BaseStatementHandler:instantiateStatement
[4]#        [BaseStatementHandler:0]	org.apache.ibatis.executor.statement.SimpleStatementHandler:instantiateStatement
[4]#        [BaseStatementHandler:0]	org.apache.ibatis.executor.statement.CallableStatementHandler:instantiateStatement
[4]#        [BaseStatementHandler:0]	org.apache.ibatis.executor.statement.PreparedStatementHandler:instantiateStatement
[3]#      [BaseStatementHandler:93]	org.apache.ibatis.executor.statement.BaseStatementHandler:closeStatement
[3]#      [BaseStatementHandler:96]	org.apache.ibatis.executor.statement.BaseStatementHandler:closeStatement
[2]#    [StatementHandler:0]	org.apache.ibatis.executor.statement.RoutingStatementHandler:prepare
[3]#      [RoutingStatementHandler:59]	org.apache.ibatis.executor.statement.StatementHandler:prepare	!cycle[1]!
[1]#  [BatchExecutor:71]	org.apache.ibatis.executor.statement.StatementHandler:parameterize
[2]#    [StatementHandler:0]	org.apache.ibatis.executor.statement.RoutingStatementHandler:parameterize
[3]#      [RoutingStatementHandler:64]	org.apache.ibatis.executor.statement.StatementHandler:parameterize	!cycle[1]!
[1]#  [BatchExecutor:77]	org.apache.ibatis.executor.statement.StatementHandler:batch
[2]#    [StatementHandler:0]	org.apache.ibatis.executor.statement.RoutingStatementHandler:batch
[3]#      [RoutingStatementHandler:69]	org.apache.ibatis.executor.statement.StatementHandler:batch	!cycle[1]!
```

除此之外，当方法指定了注解时，也可以显示在结果中，格式为“@xxx”；

当出现方法循环调用时，会显示出现循环调用的方法，格式为“!cycle\[x\]!”。
