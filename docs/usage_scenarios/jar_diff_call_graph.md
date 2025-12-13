# 1. JarDiff 比较 jar 文件版本差异及代码影响范围（内容还需要修改）

## 1.1. 比较 jar 文件修改的方法的调用链及影响范围-作用

以下提供的功能，支持对比 jar 文件新旧版本，获得修改的方法，并生成这些方法向上或向下的方法完整调用链，分析

以下功能属于 java-all-call-graph 组件的一部分，项目地址参考

https://github.com/Adrninistrator/java-all-call-graph

https://gitee.com/adrninistrator/java-all-call-graph

## 1.2. 特性说明

### 1.2.1. jar 文件名比较

在对比 jar 文件时，会忽略文件名中的版本号，仅比较文件名前缀，忽略“-数字及后面的内容”

例如“jar-diff-2.0.0.jar”与“jar-diff-1.0.0.jar”的文件名前缀都是“jar-diff”，认为是同一个 jar 文件的不同版本

### 1.2.2. 比较的类与方法

若某个类的完整类名仅在新版 jar 文件中出现，未在旧版 jar 文件中出现时，则认为该类为新出现的类，该类的全部方法都会被用于生成方法完整调用链

若某个类的完整类名在新旧版 jar 文件中都有出现，则逐个比较方法，包括方法名称、方法参数类型、方法返回类型；若新版 jar 文件的某个方法在旧版 jar 文件中找不到以上信息完全相同的方法，则认为该方法为新出现的方法，该方法会被用于生成方法完整调用链

若某个方法在新旧版 jar 文件中的以上信息均相同，则比较方法的代码，具体方式见下一条；若不相同，则该方法会被用于生成方法完整调用链

### 1.2.3. 比较方法编译后的代码

在比较同一个方法在新旧版 jar 文件中是否修改时，会使用编译后的字节码进行比较，仅当方法的代码出现变化时才认为方法修改，代码格式化、空行等通常不会导致方法被认定为有修改

### 1.2.4. 常量值修改是否影响认定方法修改

假如方法中使用的常量值对应的类名与常量字段名称均未改变，但常量值内容改变时，也会使对应方法被认定为被修改

例如以下方法 a 中使用了常量 Constants1.FLAG，当常量值改变时，即使对应方法代码没有改动，也会认为方法被修改

```java
public void a() {
    System.out.println(Constants1.FLAG);
}
```

## 1.3. 生成结果

以下的“当前目录”都是指当前方法完整调用链文件所在目录

### 1.3.1. 比较 jar 文件修改的方法并生成向上的方法完整调用链与调用堆栈

#### 1.3.1.1. 文件-修改的方法基本信息-汇总

生成的文件路径为`{当前目录}/_callee_jar_diff_summary/modified_methods_base.md`，每次执行只生成一个文件

文件内容代表修改的方法，及所在的 jar 文件信息。每一列使用、t 分隔，内容示例如下

```
jar 文件名称前缀	新 jar 文件名称	旧 jar 文件名称	修改的完整方法	方法在旧 jar 文件中是否存在
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.controller.TestController1:get1()	true
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.controller.TestController1:get2()	true
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.controller.TestController1:post(java.lang.String)	true
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.controller.TestController1:test()	false
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.service.TestService1:testA()	true
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.service.TestService1:testB()	true
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.task.TestTask1:test1()	true
jar-diff	jar-diff-2.0.0.jar	jar-diff-1.0.0.jar	test.diffjar.task.TestTask1:test2()	true
```

#### 1.3.1.2. 文件-修改的方法向上的调用堆栈信息-汇总

生成的文件路径为`{当前目录}/_callee_jar_diff_summary/modified_methods_stack.md`，每次执行只生成一个文件

文件内容代表每个修改的方法向上找到的入口方法信息。文件每一列使用、t 分隔，内容示例如下

```
新 jar 文件名称	被调用完整方法	调用堆栈在文件中的序号	上层调用完整方法	入口方法	入口方法信息
jar-diff-2.0.0.jar	test.diffjar.controller.TestController1:test()	000001	test.diffjar.controller.TestController1:get1()	test.diffjar.controller.TestController1:get1()	{"type":"spc","controllerUri":"/test1/get1"}
jar-diff-2.0.0.jar	test.diffjar.service.TestService1:testA()	000001	test.diffjar.controller.TestController1:test()	test.diffjar.controller.TestController1:get1()	{"type":"spc","controllerUri":"/test1/get1"}
jar-diff-2.0.0.jar	test.diffjar.service.TestService1:testA()	000002	test.diffjar.task.TestTask1:test1()	test.diffjar.task.TestTask1:test1()	{"type":"spt"}
jar-diff-2.0.0.jar	test.diffjar.service.TestService1:testB()	000001	test.diffjar.controller.TestController1:post(java.lang.String)	test.diffjar.controller.TestController1:post(java.lang.String)	{"type":"spc","controllerUri":"/test1/post"}
```

入口方法信息可通过后续内容说明的继承 AbstractEntryMethodInfoFiller 的插件类进行自定义处理

#### 1.3.1.3. 文件-向上的方法完整调用链文件

文件生成目录为`{当前目录}`，每个方法对应一个文件，文件名中包含了类名、方法名、完整方法 HASH

文件内容代表从修改的方法开始向上查找到的所有的方法完整调用链，示例如下

```
test.diffjar.service.TestService1:testA()
[0]#test.diffjar.service.TestService1:testA():java.lang.String
[1]##  test.diffjar.controller.TestController1:test():java.lang.String	(TestController1:44)
[2]##    test.diffjar.controller.TestController1:get1():java.lang.String@org.springframework.web.bind.annotation.GetMapping(/test1/get1)	(TestController1:29)	!entry!
[1]##  test.diffjar.task.TestTask1:test1():void@org.springframework.scheduling.annotation.Scheduled	(TestTask1:23)	!entry!
```

文件格式说明见 [output_example.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/docs/output_example.md)

#### 1.3.1.4. 文件-修改的方法向上找入口方法的调用堆栈-Markdown 形式

文件生成的目录为`{当前目录}/_stack`，每个方法对应一个文件，文件名中包含了类名、方法名、完整方法 HASH

文件内容代表从修改的方法开始向上查找到的所有的入口方法的调用堆栈，为 Markdown 形式，示例如下

```
- 处理调用链文件：java-all-call-graph\_jacg_o_ee\jar_diff_jar_diff_20250518-110607.742\TestService1@testA@zhnHFyZfYjpRAd7mzve6gg==#03a.txt
- 方法向上调用链对应的调用堆栈，按层级减小方向打印
- 查找的关键字：
	!entry!

## 1. 调用链文件行号：4

[2]##    test.diffjar.controller.TestController1:get1():java.lang.String@org.springframework.web.bind.annotation.GetMapping(/test1/get1)	(TestController1:29)	!entry!
[1]##  test.diffjar.controller.TestController1:test():java.lang.String	(TestController1:44)
[0]#test.diffjar.service.TestService1:testA():java.lang.String

## 2. 调用链文件行号：5

[1]##  test.diffjar.task.TestTask1:test1():void@org.springframework.scheduling.annotation.Scheduled	(TestTask1:23)	!entry!
[0]#test.diffjar.service.TestService1:testA():java.lang.String
```

文件格式说明见 [other_functions.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/docs/other_functions.md)

#### 1.3.1.5. 文件-修改的方法向上到入口方法的调用堆栈-表格形式

文件生成的目录为`{当前目录}/_stack/_other_forms/{每个方法的目录}/callee_stack_table.md`，每个方法对应一个目录，目录名中包含了类名、方法名、完整方法 HASH

文件内容代表从修改的方法开始向上查找到的所有的入口方法的调用堆栈，为表格形式，文件每一列使用、t 分隔，示例如下

```
调用堆栈在文件中的序号	调用堆栈内部的序号	完整调用方法/被调用方法	调用方法代码行号
000001	1	test.diffjar.controller.TestController1:get1()	29
000001	2	test.diffjar.controller.TestController1:test()	44
000001	3	test.diffjar.service.TestService1:testA()	0
000002	1	test.diffjar.task.TestTask1:test1()	23
000002	2	test.diffjar.service.TestService1:testA()	0
```

#### 1.3.1.6. 文件-修改的方法向上到入口方法的调用堆栈汇总信息

文件生成的目录为`{当前目录}/_stack/_other_forms/{每个方法的目录}/callee_stack_summary.md`，每个方法对应一个目录，目录名中包含了类名、方法名、完整方法 HASH

文件内容代表从修改的方法开始向上查找到的所有的入口方法的调用堆栈的汇总信息，每行代表一个调用堆栈，为表格形式，文件每一列使用、t 分隔，示例如下

```
调用堆栈在文件中的序号	完整被调用方法	上层完整调用方法	向上通过关键字找到的完整方法	向上通过关键字找到的方法返回类型
000001	test.diffjar.service.TestService1:testA()	test.diffjar.controller.TestController1:test()	test.diffjar.controller.TestController1:get1()	java.lang.String
000002	test.diffjar.service.TestService1:testA()	test.diffjar.task.TestTask1:test1()	test.diffjar.task.TestTask1:test1()	void
```

#### 1.3.1.7. 内存数据-修改的 jar 文件中修改的方法信息

提供的方法返回类型为 com.adrninistrator.jacg.diff.dto.result.JarDiffResult

Map 格式的字段 jarModifiedMethodInfoMap 代表修改的 jar 文件中修改的方法信息，内容示例如下

Map 中存在 key 为“jar-diff-2.0.0.jar”，value 如下的元素，fullMethod 为修改的完整方法，methodReturnType 为方法类型，oldMethodExists 代表方法在旧 jar 文件中是否存在

```json
[ {
  "fullMethod" : "test.diffjar.controller.TestController1:get1()",
  "methodReturnType" : "java.lang.String",
  "oldMethodExists" : true
}, {
  "fullMethod" : "test.diffjar.controller.TestController1:get2()",
  "methodReturnType" : "java.lang.String",
  "oldMethodExists" : true
}, {
  "fullMethod" : "test.diffjar.controller.TestController1:post(java.lang.String)",
  "methodReturnType" : "java.lang.String",
  "oldMethodExists" : true
}, {
  "fullMethod" : "test.diffjar.controller.TestController1:test()",
  "methodReturnType" : "java.lang.String",
  "oldMethodExists" : false
}, {
  "fullMethod" : "test.diffjar.service.TestService1:testA()",
  "methodReturnType" : "java.lang.String",
  "oldMethodExists" : true
}, {
  "fullMethod" : "test.diffjar.service.TestService1:testB()",
  "methodReturnType" : "int",
  "oldMethodExists" : true
}, {
  "fullMethod" : "test.diffjar.task.TestTask1:test1()",
  "methodReturnType" : "void",
  "oldMethodExists" : true
}, {
  "fullMethod" : "test.diffjar.task.TestTask1:test2()",
  "methodReturnType" : "void",
  "oldMethodExists" : true
} ]
```

### 1.3.2. 比较 jar 文件修改的方法并生成向下的方法完整调用链

#### 1.3.2.1. 文件-修改的方法基本信息

生成的文件路径为`{当前目录}/_caller_jar_diff_summary/modified_methods_base.md`，每次执行只生成一个文件

文件内容代表修改的方法，及所在的 jar 文件信息。每一列使用、t 分隔

内容示例同上，略

#### 1.3.2.2. 文件-向下的方法完整调用链文件

文件生成目录为`{当前目录}`，每个方法对应一个文件，文件名中包含了类名、方法名、完整方法 HASH

文件内容代表从修改的方法开始向下查找到的所有的方法完整调用链，示例如下

```
test.diffjar.controller.TestController1:post(java.lang.String)
[0]#test.diffjar.controller.TestController1:post(java.lang.String):java.lang.String@org.springframework.web.bind.annotation.PostMapping(/test1/post)
[1]##  [TestController1:39]	org.slf4j.Logger:info(java.lang.String,java.lang.Object):void	!no_callee!
[1]##  [TestController1:40]	test.diffjar.service.TestService1:testB():int	!no_callee!
[1]##  [TestController1:40]	java.lang.String:valueOf(int):java.lang.String	!no_callee!
```

文件格式说明链接同上

#### 1.3.2.3. 内存数据-修改的 jar 文件中修改的方法信息

同上，略

## 1.4. 使用方式

### 1.4.1. 组件依赖等基础准备工作

参考 java-all-call-graph 组件说明

以下生成的 _jacg_o_ee、_jacg_o_er 目录默认位置在当前目录

### 1.4.2. 使用配置文件

#### 1.4.2.1. 项目构建

在控制台执行`gradlew gen_run_jar`命令

会在项目根目录中生成 jar_output_dir 目录，其中保存了可以直接运行的程序、配置文件及脚本

#### 1.4.2.2. 比较 jar 文件修改的方法并生成向上的方法完整调用链与调用堆栈

进入以上目录，修改 _jacg_jar_diff/jar_diff_callee_graph_dir.properties 配置文件

按照配置文件说明指定保存了需要比较的新旧版本 jar 文件的目录

执行脚本 jar_diff_callee_graph.bat/jar_diff_callee_graph.sh ，其中会执行 test.jacg.TestRunnerGenJarDiffCalleeGraph 类

生成的文件在 _jacg_o_ee 目录中创建的新目录中

#### 1.4.2.3. 比较 jar 文件修改的方法并生成向下的方法完整调用链

进入以上目录，修改 _jacg_jar_diff/jar_diff_caller_graph_dir.properties 配置文件

按照配置文件说明指定保存了需要比较的新旧版本 jar 文件的目录

执行脚本 jar_diff_caller_graph.bat/jar_diff_caller_graph.sh ，其中会执行 test.jacg.TestRunnerGenJarDiffCallerGraph 类

生成的文件在 _jacg_o_er 目录中创建的新目录中

### 1.4.3. 使用代码中的配置

测试类 TestAbstractRunnerGenJarDiffCallGraph 的子类用于验证 JarDiff 功能，以下为子类类名及用于验证的场景

|测试类名|验证的场景|
|---|---|
|TestRunnerGenJarDiffCallGraphDiffSame|比较的新旧版本 jar 文件有相同的，也有不同的|
|TestRunnerGenJarDiffCallGraphNewJar|比较的新版本 jar 文件在旧版本中不存在|
|TestRunnerGenJarDiffCallGraphOneJarDiff|比较一个 jar 文件，新版本与旧版本不同|
|TestRunnerGenJarDiffCallGraphOneJarSame|比较一个 jar 文件，新版本与旧版本相同|

#### 1.4.3.1. 比较 jar 文件修改的方法并生成向上的方法完整调用链与调用堆栈

参考 test.runbycode.jardiffcallgraph.TestAbstractRunnerGenJarDiffCallGraph:testJarDiffCalleeGraph 方法

调用 com.adrninistrator.javacg2.conf.BaseConfigureWrapper:setOtherConfigList 方法，通过 OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLEE_GRAPH_DIR 枚举设置需要进行 JarDiff 功能的保存新旧版本 jar 文件的目录

创建用于比较 jar 文件修改的方法并生成向上的方法完整调用链与调用堆栈的 com.adrninistrator.jacg.diff.runner.RunnerGenJarDiffCalleeGraph 类的实例，构造函数参数如下：

|参数序号|参数类型|参数作用|
|---|---|---|
|1|JavaCG2ConfigureWrapper|进行 Java 静态分析的 java-callgraph2 组件的配置参数包装类|
|2|ConfigureWrapper|java-all-call-graph 组件的配置参数包装类|
|3|AbstractEntryMethodInfoFiller...|对入口方法信息进行填充的插件类数组，可为空|

调用 RunnerGenJarDiffCalleeGraph 实例的 generate 方法，执行 JarDiff 处理

#### 1.4.3.2. 比较 jar 文件修改的方法并生成向下的方法完整调用链

参考 test.runbycode.jardiffcallgraph.TestAbstractRunnerGenJarDiffCallGraph:testJarDiffCallerGraph 方法

调用 com.adrninistrator.javacg2.conf.BaseConfigureWrapper:setOtherConfigList 方法，通过 OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLER_GRAPH_DIR 枚举设置需要进行 JarDiff 功能的保存新旧版本 jar 文件的目录

创建用于比较 jar 文件修改的方法并生成向上的方法完整调用链与调用堆栈的 com.adrninistrator.jacg.diff.runner.RunnerGenJarDiffCallerGraph 类的实例，构造函数参数如下：

|参数序号|参数类型|参数作用|
|---|---|---|
|1|JavaCG2ConfigureWrapper|同上|
|2|ConfigureWrapper|同上|

调用 RunnerGenJarDiffCallerGraph 实例的 generate 方法，执行 JarDiff 处理