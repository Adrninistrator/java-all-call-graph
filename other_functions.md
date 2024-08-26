# 1. 其他功能

## 1.1. 生成包含关键字的所有方法到起始方法之间的调用链

通常情况下，生成的向上或向下的Java方法完整调用链内容通常会比较多，如果只关注某个方法到起始方法之间的调用链时，可以按照以下步骤进行处理。相当于将关注的树形结构的方法调用链转换为多个链表的形式。

如果需要生成包含关键字的所有方法到起始方法之间的调用链，例如获得入口方法到被调用的起始方法之间的调用链，或起始方法到Mybatis的Mapper之间的调用链等场景，可以按照以下步骤生成：

执行以下java类：

|完整类名|说明|
|---|---|
|test.jacg.TestFindCallStackTrace4ee|处理向上的完整调用链文件，按照层级减小的方向显示|
|test.jacg.TestFindCallStackTrace4er|处理向下的完整调用链文件，按照层级增大的方向显示|

以上类在执行时支持不指定程序参数（即main()方法处理的参数），或指定程序参数，建议使用不指定程序参数的方式。

- 不指定程序参数

执行以上类时不指定程序参数，则会先生成对应的向上（或向下）方法完整调用链，再对生成目录的文件根据关键字生成到起始方法的调用链。

执行TestFindCallStackTrace4ee类时，关键字在配置文件“_jacg_find_keyword/find_keyword_4callee.properties”中指定；执行TestFindCallStackTrace4er类时，关键字在配置文件“_jacg_find_keyword/find_keyword_4caller.properties”中指定。

- 指定程序参数

在程序参数中指定对应的向上或向下的Java方法完整调用链文件路径，及对应的关键字，支持批量查询，格式为“\[完整调用链文件路径/保存完整调用链文件的目录路径\] \[关键字1\] \[关键字2\] ... \[关键字n\]”。

- 生成结果示例

例如完整调用链文件“dir\\a.txt”内容如上所示。

假如希望知道包含关键字“!entry!”的所有方法到起始方法“\[0\]#DestClass.destfunc()”之间的调用关系，执行以上类生成调用关系如下：

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

## 1.2. 获取调用指定方法的入口方法

- 获取方式

假如存在调用链“ a -> b -> c -> d -> e ”，将方法a称为入口方法（即向上没有被其他方法调用的方法），获得从哪些入口方法会直接或间接调用到方法e（或b、c、d），可以使用以下方法获取：

```java
com.adrninistrator.jacg.extractor.entry.CalleeGraphBaseExtractor#baseExtract(com.adrninistrator.jacg.conf.ConfigureWrapper)
```

通过 OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE 指定需要查找的被调用方法

可参考以下方法中的调用方式

```java
test.runbycode.example.TestExtractCalleeGraphToEntry
```

- 示例

以上为 TestMCCallee.test2() 方法的被调用情况

```java
public class TestMCCaller {
    public void testFindEntryA1() {
        testFindEntryA2();
    }

    public void testFindEntryA2() {
        TestMCCallee.testFindEntry();
    }

    public void testFindEntryB1() {
        TestMCCallee.testFindEntry();
    }
}
```

通过以上方式获取调用 TestMCCallee.testFindEntry() 方法的入口方法的结果，找到对应的入口方法： TestMCCaller:testFindEntryB1() 、TestMCCaller:testFindEntryA1() ，示例如下：


```log
{
  "stackFilePath" : "D:\\gitee-dir\\pri-code\\java-callgraph-dir\\java-all-call-graph\\_jacg_o_ee\\test_rbc_20240826-192941.564\\_stack\\TestMCCallee@testFindEntry@fLQrsLa-8-Wfbwr0udN48g==#036.md",
  "stackFileName" : "TestMCCallee@testFindEntry@fLQrsLa-8-Wfbwr0udN48g==#036.md",
  "emptyStackFile" : false,
  "simpleClassName" : "TestMCCallee",
  "className" : "test.callgraph.methodcall.TestMCCallee",
  "methodName" : "testFindEntry",
  "methodHash" : "fLQrsLa-8-Wfbwr0udN48g==#036",
  "fullMethod" : "test.callgraph.methodcall.TestMCCallee:testFindEntry()",
  "calleeExtractedLineList" : [ {
    "dataSeq" : 1,
    "lineNumber" : 13,
    "lineContent" : "[1]#  test.callgraph.methodcall.TestMCCaller:testFindEntryB1()\t(TestMCCaller:219)\t!entry!",
    "callGraphLineParsed" : {
      "methodLevel" : 1,
      "methodDetail" : {
        "className" : "test.callgraph.methodcall.TestMCCaller",
        "methodName" : "testFindEntryB1",
        "fullMethod" : "test.callgraph.methodcall.TestMCCaller:testFindEntryB1()",
        "argTypeStr" : "",
        "argTypeList" : [ ]
      },
      "callerLineNumber" : 219,
      "businessDataList" : [ ],
      "cycleCall" : false,
      "cycleCallLevel" : 0,
      "entryMethod" : true,
      "runInOtherThread" : false,
      "runInTransaction" : false,
      "callerLineNumberStr" : "219"
    },
    "runInOtherThread" : false,
    "runInTransaction" : false,
    "nextLineContent" : "[0]#test.callgraph.methodcall.TestMCCallee:testFindEntry()"
  }, {
    "dataSeq" : 2,
    "lineNumber" : 20,
    "lineContent" : "[2]#    test.callgraph.methodcall.TestMCCaller:testFindEntryA1()\t(TestMCCaller:211)\t!entry!",
    "callGraphLineParsed" : {
      "methodLevel" : 2,
      "methodDetail" : {
        "className" : "test.callgraph.methodcall.TestMCCaller",
        "methodName" : "testFindEntryA1",
        "fullMethod" : "test.callgraph.methodcall.TestMCCaller:testFindEntryA1()",
        "argTypeStr" : "",
        "argTypeList" : [ ]
      },
      "callerLineNumber" : 211,
      "businessDataList" : [ ],
      "cycleCall" : false,
      "cycleCallLevel" : 0,
      "entryMethod" : true,
      "runInOtherThread" : false,
      "runInTransaction" : false,
      "callerLineNumberStr" : "211"
    },
    "runInOtherThread" : false,
    "runInTransaction" : false,
    "nextLineContent" : "[1]#  test.callgraph.methodcall.TestMCCaller:testFindEntryA2()\t(TestMCCaller:215)"
  } ]
}
```

在以上输出结果中， calleeExtractedLineList\[n\].callGraphLineParsed 代表入口方法的信息

### 1.2.1. 执行顺序

使用以上方式，首先需要使对应jar包完成解析并写入数据库，使用以下方法：

```
com.adrninistrator.jacg.runner.RunnerWriteDb:run()
```

## 1.3. 处理循环方法调用

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
