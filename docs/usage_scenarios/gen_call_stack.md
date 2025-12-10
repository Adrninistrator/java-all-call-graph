# 1. 静态分析生成 Java 代码方法调用堆栈

## 1.1. 前言

通常情况下，生成的方法完整调用链中会包含大量节点，分析复杂度会比较高

在某些场景下，关注的是从起始方法到目标方法之间的调用堆栈（即调用路径，中间会经过哪些被调用的方法）

java-all-call-graph 项目在生成指定方法向上或向下的完整调用链后，支持根据指定关键字找到目标方法，并生成从起始方法直接或间接调用到目标方法之间的调用堆栈

java-all-call-graph 项目地址为 [https://github.com/Adrninistrator/java-all-call-graph](https://github.com/Adrninistrator/java-all-call-graph)

方法调用堆栈需要根据方法完整调用链获得，生成方法调用堆栈后，可以此为基础进行其他分析，例如向上找到某个方法被直接或间接调用的所有入口方法，以获取方法的影响范围；或向下获得某个方法会调用的特定方法，以及会执行的业务层面的操作等

## 1.2. 生成的调用堆栈文件示例

生成的调用堆栈文件内容结构如下图中红色的调用路径所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/method_call_color.png)

以下为生成的方法调用堆栈文件内容示例，文件格式的说明可参考后续内容

### 1.2.1. 从指定方法开始向上的调用堆栈文件示例

以下为从 DestClass:destfunc 方法开始向上的调用堆栈文件，再查找包含“!entry!”关键字的调用堆栈文件示例，包含 3 个调用堆栈：

```
[0]#DestClass:destfunc()
[1]#  ClassA3:funcA3()	(ClassA3:10)
[2]#    ClassA2:funcA2()	(ClassA2:19)
[3]#      ClassA1:funcA1()	(ClassA1:23)    !entry!

[0]#DestClass:destfunc()
[1]#  ClassB1:funcB1()	(ClassB1:57)    !entry!

[0]#DestClass:destfunc()
[1]#  ClassC2:funcC2()	(ClassC2:31)
[2]#    ClassC1:funcC1()	(ClassC1:9)    !entry!
```

以上对应的调用关系如下所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/example-cg-4callee.png)

### 1.2.2. 从指定方法开始向下的调用堆栈文件示例

以下为从 SrcClass:srcfunc 方法开始向下的调用堆栈文件，再查找包含“ClassA3:funcA3”关键字的调用堆栈文件示例，包含 2 个调用堆栈：

```
[0]#SrcClass:srcfunc()
[1]#  [SrcClass:15]	ClassA1:funcA1()
[2]#    [ClassA1:27]	ClassA2a:funcA2a()
[3]#      [ClassA2b:39]	ClassA3:funcA3()

[0]#SrcClass:srcfunc()
[1]#  [SrcClass:15]	ClassA1:funcA1()
[2]#    [ClassA1:59]	ClassA2b:funcA2b()
[3]#      [ClassA2b:39]	ClassA3:funcA3()
```

以上对应的调用关系如下所示：

![](https://gitee.com/adrninistrator/java-all-call-graph/raw/main/docs/pic/example-cg-4caller.png)

## 1.3. 基础操作

生成方法调用堆栈以方法完整调用链文件为基础，因此在使用当前功能前，可先参考 [静态分析生成 Java 代码方法完整调用链](gen_all_call_graph.md)

## 1.4. 配置参数说明

### 1.4.1. 当前场景使用的配置参数

当前场景使用的配置参数使用说明，可参考 

[FindCallStackTrace-4ee - 获得方法向上到包含关键字的调用堆栈](../class_use_config/FindCallStackTrace-4ee.md)

[FindCallStackTrace-4er - 获得方法向下到包含关键字的调用堆栈](../class_use_config/FindCallStackTrace-4er.md)

### 1.4.2. 重要配置参数-java-all-call-graph

#### 1.4.2.1. _jacg_config/config_db.properties

指定需要使用的数据库信息

#### 1.4.2.2. _jacg_gen_all_call_graph/method_class_4callee.properties

指定需要生成向上的方法完整调用链的类或方法

#### 1.4.2.3. _jacg_gen_all_call_graph/method_class_4caller.properties

指定需要生成向下的方法完整调用链的类或方法

#### 1.4.2.4. _jacg_find_stack_keyword/find_stack_keyword_4ee.properties

生成向上的方法完整调用链文件后，从最底层被调用方法开始向上查找包含指定关键字的方法的调用堆栈时，使用的关键字

#### 1.4.2.5. _jacg_find_stack_keyword/find_stack_keyword_4er.properties

生成向下的方法完整调用链文件后，从最顶层调用方法开始向下查找包含指定关键字的方法的调用堆栈时，使用的关键字

## 1.5. 入口类

### 1.5.1. 生成向上的方法完整调用链

- 通过配置文件指定配置参数

当通过配置文件指定配置参数时，入口类为 test.jacg.TestFindCallStackTrace4ee，调用构造函数时参数值需要为 true

该类会调用下面的类

- 通过代码指定配置参数

通过代码指定配置参数时，入口类为 com.adrninistrator.jacg.findstack.FindCallStackTrace

### 1.5.2. 生成向下的方法完整调用链

- 通过配置文件指定配置参数

当通过配置文件指定配置参数时，入口类为 test.jacg.TestFindCallStackTrace4er

该类会调用下面的类

- 通过代码指定配置参数

通过代码指定配置参数时，入口类为 com.adrninistrator.jacg.findstack.FindCallStackTrace，调用构造函数时参数值需要为 false

## 1.6. 示例代码

以下为通过代码指定配置参数执行时的示例代码

### 1.6.1. 获得方法向上到包含关键字的调用堆栈

```java
ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, System.class.getName());
configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB);

FindCallStackTrace findCallStackTrace = new FindCallStackTrace(true, configureWrapper);
CallStackFileResult callStackFileResult = findCallStackTrace.find();
Assert.assertTrue(callStackFileResult.isSuccess());    
```

对应 test.runbycodemain.TestRBCFindCallStackTrace4ee:testExample 方法

### 1.6.2. 获得方法向下到包含关键字的调用堆栈

```java
ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, TestBranch1.class.getName());
configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER, System.class.getName()
);

FindCallStackTrace findCallStackTrace = new FindCallStackTrace(false, configureWrapper);
CallStackFileResult callStackFileResult = findCallStackTrace.find();
Assert.assertTrue(callStackFileResult.isSuccess());
```

对应 test.runbycodemain.TestRBCFindCallStackTrace4er:testExample 方法

## 1.7. 生成的调用堆栈文件使用说明

### 1.7.1. 调用堆栈文件生成位置

方法调用堆栈文件会生成在对应的方法完整调用链文件生成目录的_stack 目录中，通过控制方法完整调用链文件生成位置，可指定方法调用堆栈文件生成位置

假如某个方法完整调用链中根据关键字有找到目标方法，则对应的调用堆栈文件会生成在_stack 目录中

假如没有找到目标方法，则对应的调用堆栈文件会生成在_stack 目录的_keywords_not_found 目录中

### 1.7.2. 调用堆栈文件名格式

每个方法会生成一个对应的调用堆栈文件

生成的调用堆栈文件名格式为： {唯一类名}@{方法名}@{完整方法 HASH+长度}.md

与方法完整调用链文件名类似

### 1.7.3. 生成的调用堆栈文件通用格式说明

以下实际生成的向上及向下的方法调用堆栈文件，内容使用 Markdown 格式

查找的关键字，以及调用堆栈内容，会使用三个“`”包含起来，以下的示例中未显示

### 1.7.4. 向上的方法调用堆栈文件格式

向上的方法调用堆栈文件内容示例如下，每个“#”对应的标题下，为一个方法调用堆栈，内容及格式与方法完整调用链相同

```
- 处理调用链文件：xxx\_jacg_o_ee\jacg_20251205-223134.806\System@getProperty@S5Bh33qb6mZuKx9KobEq8g==#050.txt
- 方法向上调用链对应的调用堆栈，按层级减小方向打印
- 查找的关键字：
!entry!

# 1. 调用链文件行号：5

[3]#      test.callgraph.interfaces.interfaces.InterfaceChild3:testSuper2()	(InterfaceChild3:0)	!entry!
[2]#    test.callgraph.interfaces.interfaces.InterfaceChild2:testSuper2()	(InterfaceChild2:0)
[1]#  test.callgraph.interfaces.classes.ImplChildClass2A:testSuper2()	(ImplChildClass2A:18)
[0]#java.lang.System:getProperty(java.lang.String,java.lang.String)

# 2. 调用链文件行号：6

[2]#    test.callgraph.interfaces.classes.ImplChildClass2A:test1()	(ImplChildClass2A:23)	!entry!
[1]#  test.callgraph.interfaces.classes.ImplChildClass2A:testSuper2()	(ImplChildClass2A:18)
[0]#java.lang.System:getProperty(java.lang.String,java.lang.String)
```

### 1.7.5. 向下的方法调用堆栈文件格式

向下的方法调用堆栈文件内容示例如下，每个“#”对应的标题下，为一个方法调用堆栈，内容及格式与方法完整调用链相同

```
- 处理调用链文件：xxx\_jacg_o_er\jacg_20251205-214234.855\TestBranch1@testString@HkaKsHnLtCJNPxNSuQavgw==#04f.txt
- 方法向下调用链对应的调用堆栈，按层级增大方向打印
- 查找的关键字：
java.lang.System

# 1. 调用链文件行号：5

[0]#test.callgraph.branch.TestBranch1:testString(java.lang.String)
[1]#  [TestBranch1:148]	java.lang.System:currentTimeMillis()	!no_callee!
```

### 1.7.6. 对调用堆栈文件进行解析

假如需要对生成的调用堆栈文件进行解析，可参考 [从方法调用堆栈文件提取关注的信息](extract_call_stack_with_info.md) 相关内容

## 1.8. 生成其他形式的调用堆栈文件

### 1.8.1. 配置参数

配置文件_jacg_config/config.properties 中有配置参数 call.graph.gen.stack.other.forms

该参数用于控制使用 FindCallStackTrace 类生成方法调用堆栈文件时，是否生成其他形式的调用堆栈文件。该参数为 true 时代表需要生成，默认值为 false

仅当 call.graph.output.detail 参数值为 0 时支持

### 1.8.2. 其他形式的调用堆栈文件生成位置

其他形式的调用堆栈文件生成在调用堆栈文件所在目录的_other_forms 目录中，每个方法对应一个子目录

子目录的名称格式为 {唯一类名}@{方法名}@{完整方法 HASH+长度}

### 1.8.3. 其他形式的调用堆栈文件类型

生成的其他形式的调用堆栈文件是后缀为“.md”的文本文件，若文件内容非空，则会生成 Excel 格式后缀为“.xlsx”的对应文件，文件名前缀同上

### 1.8.4. 向上的其他形式的调用堆栈文件

#### 1.8.4.1. 向上的调用堆栈文件内容示例

假如生成的向上的调用堆栈文件内容如下：

```
- 处理调用链文件：xxx\_jacg_o_ee\test_rbc_20251206-205244.116\System@exit@YLj1K4WOY28db-X3ATGdxA==#01f.txt
- 方法向上调用链对应的调用堆栈，按层级减小方向打印
- 查找的关键字：
!entry!
<init>

# 1. 调用链文件行号：5

[3]#      test.callgraph.multi.TestMulti:test2():void	(TestMulti:31)	!entry!
[2]#    test.callgraph.implement.AbstractClass1:f2():void	(AbstractClass1:0)
[1]#  test.callgraph.implement.ChildClass1:f2():void	(ChildClass1:17)
[0]#java.lang.System:exit(int):void

# 2. 调用链文件行号：6

[3]#      test.callgraph.multi.TestMulti2:test2():void	(TestMulti2:31)	!entry!
[2]#    test.callgraph.implement.AbstractClass1:f2():void	(AbstractClass1:0)
[1]#  test.callgraph.implement.ChildClass1:f2():void	(ChildClass1:17)
[0]#java.lang.System:exit(int):void

# 3. 调用链文件行号：8

[4]#        test.callgraph.multi.TestMulti:test1():void	(TestMulti:23)	!entry!
[3]#      test.callgraph.implement.Interface1:f2():void	(Interface1:0)
[2]#    test.callgraph.implement.AbstractClass1:f2():void	(AbstractClass1:0)
[1]#  test.callgraph.implement.ChildClass1:f2():void	(ChildClass1:17)
[0]#java.lang.System:exit(int):void

# 4. 调用链文件行号：9

[4]#        test.callgraph.multi.TestMulti2:test1():void	(TestMulti2:23)	!entry!
[3]#      test.callgraph.implement.Interface1:f2():void	(Interface1:0)
[2]#    test.callgraph.implement.AbstractClass1:f2():void	(AbstractClass1:0)
[1]#  test.callgraph.implement.ChildClass1:f2():void	(ChildClass1:17)
[0]#java.lang.System:exit(int):void
```

#### 1.8.4.2. 生成的表格形式的调用堆栈文件内容

表格形式的调用堆栈文件名为 callee_stack_table.md

当前文件内容与调用堆栈文件的内容类似，只是修改格式为表格形式

文件内容包含多列，每列之间的分隔符为“\t”，每列的名称如下：

```
调用堆栈在文件中的序号
调用堆栈内部的序号
完整调用方法/被调用方法
调用方法代码行号
```

生成的文件内容示例如下：

|调用堆栈在文件中的序号|调用堆栈内部的序号|完整调用方法/被调用方法|调用方法代码行号|
|---|---|---|---|
|000001|1|test.callgraph.multi.TestMulti:test2()|31|
|000001|2|test.callgraph.implement.AbstractClass1:f2()|0|
|000001|3|test.callgraph.implement.ChildClass1:f2()|17|
|000001|4|java.lang.System:exit(int)|0|
|000002|1|test.callgraph.multi.TestMulti2:test2()|31|
|000002|2|test.callgraph.implement.AbstractClass1:f2()|0|
|000002|3|test.callgraph.implement.ChildClass1:f2()|17|
|000002|4|java.lang.System:exit(int)|0|
|000003|1|test.callgraph.multi.TestMulti:test1()|23|
|000003|2|test.callgraph.implement.Interface1:f2()|0|
|000003|3|test.callgraph.implement.AbstractClass1:f2()|0|
|000003|4|test.callgraph.implement.ChildClass1:f2()|17|
|000003|5|java.lang.System:exit(int)|0|
|000004|1|test.callgraph.multi.TestMulti2:test1()|23|
|000004|2|test.callgraph.implement.Interface1:f2()|0|
|000004|3|test.callgraph.implement.AbstractClass1:f2()|0|
|000004|4|test.callgraph.implement.ChildClass1:f2()|17|
|000004|5|java.lang.System:exit(int)|0|

#### 1.8.4.3. 生成的调用堆栈汇总文件内容

调用堆栈汇总文件名为 callee_stack_summary.md

每个调用堆栈在当前文件中对应一行记录

文件内容包含多列，每列之间的分隔符为“\t”，每列的名称如下：

```
调用堆栈在文件中的序号
完整被调用方法
上层完整调用方法
向上通过关键字找到的完整方法
向上通过关键字找到的方法返回类型
```

生成的文件内容示例如下：

|调用堆栈在文件中的序号|完整被调用方法|上层完整调用方法|向上通过关键字找到的完整方法|向上通过关键字找到的方法返回类型|
|---|---|---|---|---|
|000001|java.lang.System:exit(int)|test.callgraph.implement.ChildClass1:f2()|test.callgraph.multi.TestMulti:test2()|void|
|000002|java.lang.System:exit(int)|test.callgraph.implement.ChildClass1:f2()|test.callgraph.multi.TestMulti2:test2()|void|
|000003|java.lang.System:exit(int)|test.callgraph.implement.ChildClass1:f2()|test.callgraph.multi.TestMulti:test1()|void|
|000004|java.lang.System:exit(int)|test.callgraph.implement.ChildClass1:f2()|test.callgraph.multi.TestMulti2:test1()|void|

### 1.8.5. 向下的其他形式的调用堆栈文件

#### 1.8.5.1. 向下的调用堆栈文件内容示例

假如生成的向下的调用堆栈文件内容如下：

```
- 处理调用链文件：xxx\_jacg_o_er\test_rbc_20251206-221654.460\TestUseSpringTx@test@iHvOd91ywCpxq0uI-l4LGw==#034.txt
- 方法向下调用链对应的调用堆栈，按层级增大方向打印
- 查找的关键字：
System:
java.lang.Deprecated

# 1. 调用链文件行号：6	!run_in_spring_tx!

[0]#test.callgraph.spring.tx.TestUseSpringTx:test():void
[1]#  [TestUseSpringTx:12]	test.callgraph.spring.tx.TestSpringTx:test1():void@org.springframework.transaction.annotation.Transactional	!run_in_spring_tx!
[2]#    [TestSpringTx:15]	java.lang.System:getProperty(java.lang.String):java.lang.String	!no_callee!

# 2. 调用链文件行号：8	!run_in_spring_tx!

[0]#test.callgraph.spring.tx.TestUseSpringTx:test():void
[1]#  [TestUseSpringTx:13]	test.callgraph.spring.tx.TestSpringTx:test2():void@org.springframework.transaction.annotation.Transactional(propagation=REQUIRES_NEW)	!run_in_spring_tx!
[2]#    [TestSpringTx:20]	java.lang.System:setOut(java.io.PrintStream):void	!no_callee!
```

#### 1.8.5.2. 生成的表格形式的调用堆栈文件内容

表格形式的调用堆栈文件名为 caller_stack_table.md

当前文件内容与调用堆栈文件的内容类似，只是修改格式为表格形式

文件内容包含多列，每列之间的分隔符为“\t”，每列的名称如下：

```
调用堆栈在文件中的序号
调用堆栈内部的序号
完整调用方法/被调用方法
调用方法代码行号
```

生成的文件内容示例如下：

|调用堆栈在文件中的序号|调用堆栈内部的序号|完整调用方法/被调用方法|调用方法代码行号|
|---|---|---|---|
|000001|1|test.callgraph.spring.tx.TestUseSpringTx:test()|12|
|000001|2|test.callgraph.spring.tx.TestSpringTx:test1()|15|
|000001|3|java.lang.System:getProperty(java.lang.String)|0|
|000002|1|test.callgraph.spring.tx.TestUseSpringTx:test()|13|
|000002|2|test.callgraph.spring.tx.TestSpringTx:test2()|20|
|000002|3|java.lang.System:setOut(java.io.PrintStream)|0|

#### 1.8.5.3. 生成的调用堆栈汇总文件内容

调用堆栈汇总文件名为 caller_stack_summary.md

每个调用堆栈在当前文件中对应一行记录

文件内容包含多列，每列之间的分隔符为“\t”，每列的名称如下：

```
调用堆栈在文件中的序号
完整调用方法
向下通过关键字找到的完整方法
向下通过关键字找到的方法返回类型
```

生成的文件内容示例如下：

|调用堆栈在文件中的序号|完整调用方法|向下通过关键字找到的完整方法|向下通过关键字找到的方法返回类型|
|---|---|---|---|
|000001|test.callgraph.spring.tx.TestUseSpringTx:test()|java.lang.System:getProperty(java.lang.String)|java.lang.String|
|000002|test.callgraph.spring.tx.TestUseSpringTx:test()|java.lang.System:setOut(java.io.PrintStream)|void|

## 1.9. 向上找到入口方法后生成向下的方法调用堆栈

### 1.9.1. 使用场景

在某些代码分析场景中，需要首先向上获得有直接或间接调用指定方法的入口方法，再获得这些入口方法向下会调用的方法。即首先获取指定方法向上的影响范围，再获得向下的影响范围

当前项目支持执行以上方式的分析

### 1.9.2. 配置参数说明

#### 1.9.2.1. 当前场景使用的配置参数

当前场景使用的配置参数使用说明，可参考 

[FindCallStackUpAndDown - 向上获得入口方法再向下生成到包含关键字的调用堆栈](../class_use_config/FindCallStackUpAndDown.md)

#### 1.9.2.2. 重要配置参数-java-all-call-graph

- 1.4.2.1. _jacg_config/config_db.properties

指定需要使用的数据库信息

- 1.4.2.2. _jacg_gen_all_call_graph/method_class_4callee.properties

指定需要生成向上的方法完整调用链的类或方法

- 1.4.2.3. _jacg_gen_all_call_graph/method_class_4caller.properties

不需要指定，会自动配置为找到的入口方法

- 1.4.2.4. _jacg_find_stack_keyword/find_stack_keyword_4ee.properties

不需要指定，会自动配置为入口方法关键字

- 1.4.2.5. _jacg_find_stack_keyword/find_stack_keyword_4er.properties

生成向下的方法完整调用链文件后，从最顶层调用方法开始向下查找包含指定关键字的方法的调用堆栈时，使用的关键字

### 1.9.3. 入口类

com.adrninistrator.jacg.findstack.FindCallStackUpAndDown

对应的入口方法为 com.adrninistrator.jacg.findstack.FindCallStackUpAndDown:find，当前方法的返回值代表生成的向下的方法调用堆栈文件信息

### 1.9.4. 示例代码

```java
ConfigureWrapper configureWrapper = new ConfigureWrapper();
configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
        System.class.getName() + ":exit("
);
configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
        System.class.getName() + ":getProperty("
);
FindCallStackUpAndDown findCallStackUpAndDown = new FindCallStackUpAndDown(configureWrapper);
CallStackFileResult callStackFileResult = findCallStackUpAndDown.find();
Assert.assertTrue(callStackFileResult.isSuccess());
```

对应 test.runbycodemain.TestRBCFindCallStackUpAndDown#testExample 方法

### 1.9.5. 生成的调用堆栈文件使用说明

#### 1.9.5.1. 调用堆栈文件生成位置

当前功能会首先生成向上的方法调用堆栈文件，再生成向下的方法调用堆栈文件

在生成向上的方法调用堆栈文件时，生成的子目录名中会包含关键字“_upward”，例如“jacg_upward_20251207-231001.029”

在生成向下的方法调用堆栈文件时，生成的子目录名中会包含关键字“_downward”，例如“jacg_downward_20251207-231004.803”
