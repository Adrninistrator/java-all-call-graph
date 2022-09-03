# 1. 其他功能

## 1.1. 生成包含关键字的所有方法到起始方法之间的调用链

通常情况下，生成的向上或向下的Java方法完整调用链内容通常会比较多，如果只关注某个方法到起始方法之间的调用链时，可以按照以下步骤进行处理。相当于将关注的树形结构的方法调用链转换为多个链表的形式。

如果需要生成包含关键字的所有方法到起始方法之间的调用链，例如获得入口方法到被调用的起始方法之间的调用链，或起始方法到Mybatis的Mapper之间的调用链等场景，可以按照以下步骤生成：

执行以下java类：

|完整类名|说明|
|---|---|
|test.jacg.TestFindKeywordCallGraph4ee|处理向上的完整调用链文件，按照层级减小的方向显示|
|test.jacg.TestFindKeywordCallGraph4er|处理向下的完整调用链文件，按照层级增大的方向显示|

以上类在执行时支持不指定程序参数（即main()方法处理的参数），或指定程序参数，建议使用不指定程序参数的方式。

- 不指定程序参数

执行以上类时不指定程序参数，则会先生成对应的向上（或向下）方法完整调用链，再对生成目录的文件根据关键字生成到起始方法的调用链。

执行TestFindKeywordCallGraph4ee类时，关键字在配置文件“_jacg_find_keyword/find_keyword_4callee.properties”中指定；执行TestFindKeywordCallGraph4er类时，关键字在配置文件“_jacg_find_keyword/find_keyword_4caller.properties”中指定。

- 指定程序参数

在程序参数中指定对应的向上或向下的Java方法完整调用链文件路径，及对应的关键字，支持批量查询，格式为“\[完整调用链文件路径/保存完整调用链文件的目录路径\] \[关键字1\] \[关键字2\] ... \[关键字n\]”。

- 生成结果示例

例如完整调用链文件“dir\\a.txt”内容如上所示。

假如希望知道包含关键字“!entry!”的所有方法到起始方法“\[0\]#DestClass.destfunc()”之间的调用关系，可在执行以上类时指定程序参数为“dir\\a.txt !entry!”，则生成调用关系如下：

```
# 行号: 4
[0]#DestClass.destfunc()
[1]#  ClassA3.funcA3()	(ClassA3:10)
[2]#    ClassA2.funcA2()	(ClassA2:19)
[3]#      ClassA1.funcA1()	(ClassA1:23)    !entry!

# 行号: 5
[0]#DestClass.destfunc()
[1]#  ClassB1.funcB1()	(ClassB1:57)    !entry!

# 行号: 7
[0]#DestClass.destfunc()
[1]#  ClassC2.funcC2()	(ClassC2:31)
[2]#    ClassC1.funcC1()	(ClassC1:9)    !entry!
```

以上功能也支持对保存完整调用链文件的目录进行处理，生成的文件保存在指定目录的“find_keyword_\[yyyyMMdd-HHmmss.SSS\]”子目录中。

## 1.2. 处理循环方法调用

在生成Java方法完整调用链时，若出现了循环方法调用，本工具会从循环调用中跳出，并在生成的方法调用链中对出现循环调用的方法增加标记“!cycle\[n\]!”，其中n代表被循环调用的方法对应层级。

生成向上的Java方法完整调用链时，出现循环方法调用的示例如下：

```
org.springframework.transaction.TransactionDefinition:getIsolationLevel()
[0]#org.springframework.transaction.TransactionDefinition:getIsolationLevel
[1]#  org.springframework.transaction.support.DelegatingTransactionDefinition:getIsolationLevel	(DelegatingTransactionDefinition:56)
[2]#    org.springframework.transaction.TransactionDefinition:getIsolationLevel	(TransactionDefinition:0)	!cycle[0]!
```

生成向下的Java方法完整调用链时，出现循环方法调用的示例如下：

```
org.springframework.transaction.support.TransactionTemplate:execute(org.springframework.transaction.support.TransactionCallback)
[0]#org.springframework.transaction.support.TransactionTemplate:execute
[1]#  [TransactionTemplate:127]	org.springframework.transaction.support.CallbackPreferringPlatformTransactionManager:execute
[2]#    [CallbackPreferringPlatformTransactionManager:0]	org.springframework.transaction.jta.WebSphereUowTransactionManager:execute
[3]#      [WebSphereUowTransactionManager:225]	org.springframework.transaction.support.DefaultTransactionDefinition:<init>
[4]#        [DefaultTransactionDefinition:74]	java.lang.Object:<init>
[3]#      [WebSphereUowTransactionManager:228]	org.springframework.transaction.TransactionDefinition:getTimeout
[4]#        [TransactionDefinition:0]	org.springframework.transaction.support.DefaultTransactionDefinition:getTimeout
[4]#        [TransactionDefinition:0]	org.springframework.transaction.support.DelegatingTransactionDefinition:getTimeout
[5]#          [DelegatingTransactionDefinition:61]	org.springframework.transaction.TransactionDefinition:getTimeout	!cycle[3]!
```
