# 1. 原理说明

## 1.1. Java方法调用关系获取

在获取Java方法调用关系时，参考了 [https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph) 项目，使用Apache Commons BCEL（Byte Code Engineering Library）解析Java方法调用关系。

原始java-callgraph在多数场景下能够获取到Java方法调用关系，但存在一些场景调用关系会缺失。

针对调用关系缺失的问题，在java-callgraph2项目中进行了优化和其他功能的增强，地址为[https://github.com/Adrninistrator/java-callgraph2](https://github.com/Adrninistrator/java-callgraph2)。

java-callgraph2能够生成缺失的调用关系。对于更复杂的情况，例如存在接口Interface1，及其抽象实现类Abstract1，及其子类ChildImpl1，若在某个类中引入了抽象实现类Abstract1并调用其方法的情况，生成的方法调用关系中也不会出现缺失。

原始java-callgraph缺失的调用关系如下所示：

- 接口与实现类方法

假如存在接口Interface1，及其实现类Impl1，若在某个类Class1中引入了接口Interface1，实际为实现类Impl1的实例（使用Spring时的常见场景），在其方法Class1.func1()中调用了Interface1.fi()方法；

原始java-callgraph生成的方法调用关系中，只包含Class1.func1()调用Interface1.fi()的关系，Class1.func1()调用Impl1.fi()，及Impl1.fi()向下调用的关系会缺失。

- Runnable实现类线程调用

假如f1()方法中使用内部匿名类形式的Runnable实现类在线程中执行操作，在线程中执行了f2()方法，如下所示：

```java
private void f1() {
    new Thread(new Runnable() {
        @Override
        public void run() {
            f2();
        }
    }).start();
}
```

原始java-callgraph生成的方法调用关系中，f1()调用f2()，及f2()向下调用的关系会缺失；

对于使用命名类形式的Runnable实现类在线程中执行操作的情况，存在相同的问题，原方法调用线程中执行的方法，及继续向下的调用关系会缺失。

- Callable实现类线程调用

与Runnable实现类线程调用情况类似，略。

- Thread子类线程调用

与Runnable实现类线程调用情况类似，略。

- lambda表达式（含线程调用等）

假如f1()方法中使用lambda表达式的形式在线程中执行操作，在线程中执行了f2()方法，如下所示：

```java
private void f1() {
    new Thread(() -> f2()).start();
}
```

原始java-callgraph生成的方法调用关系中，f1()调用f2()，及f2()向下调用的关系会缺失；

对于其他使用lambda表达式的情况，存在相同的问题，原方法调用lambda表达式中执行的方法，及继续向下的调用关系会缺失。

- Stream调用

在使用Stream时，通过xxx::func方式调用方法，原始java-callgraph生成的方法调用关系中会缺失。如以下示例中，当前方法调用当前类的map2()、filter2()，及TestDto1类的getStr()方法的调用关系会缺失。

```java
list.stream().map(this::map2).filter(this::filter2).collect(Collectors.toList());
list.stream().map(TestDto1::getStr).collect(Collectors.toList());
```

- 父类调用子类的实现方法

假如存在抽象父类Abstract1，及其非抽象子类ChildImpl1，若在某个类Class1中引入了抽象父类Abstract1，实际为子类ChildImpl1的实例（使用Spring时的常见场景），在其方法Class1.func1()中调用了Abstract1.fa()方法；

原始java-callgraph生成的方法调用关系中，只包含Class1.func1()调用Abstract1.fa()的关系，Class1.func1()调用ChildImpl1.fa()的关系会缺失。

- 子类调用父类的实现方法

假如存在抽象父类Abstract1，及其非抽象子类ChildImpl1，若在ChildImpl1.fc1()方法中调用了父类Abstract1实现的方法fi()；

原始java-callgraph生成的方法调用关系中，ChildImpl1.fc1()调用Abstract1.fi()的关系会缺失。

## 1.2. Java方法完整调用链生成

- 数据库表

在获取了Java方法调用关系之后，将其保存在数据库中，可查看java-all-callgraph.jar释放的_jacg_sql目录中的.sql文件，相关数据库表如下所示：

|表名前缀|注释|作用|
|---|---|---|
|class_annotation_|类上的注解信息表||
|class_name_|类名信息表|保存相关类的完整类名及简单类名|
|extended_data_|自定义数据表||
|jar_info_|jar包信息表|保存用于解析方法调用关系的jar包信息|
|manual_add_extended_data_|人工添加的自定义数据表||
|method_annotation_|方法上的注解信息表||
|method_call_|方法调用关系表|保存各方法之间调用信息|
|method_line_number_|方法代码行号信息表|保存各方法的起始代码行号|

上述数据库表在创建时使用表名前缀加上配置文件`_jacg_config/config.properties`中的`app.name`参数值。

本工具会主要从方法调用关系表中逐级查询数据，生成完整的方法调用链。
