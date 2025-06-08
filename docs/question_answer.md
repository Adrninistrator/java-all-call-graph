# 执行时提示找不到类或方法

假如执行时提示找不到类或方法，包括 ClassNotFoundException、NoClassDefFoundError、NoSuchMethodException 等异常，可尝试以下方式：

- 查看 [how_to_use.md](how_to_use.md) 中给出的 java-all-call-graph、java-callgraph2 项目 Maven 中央仓库 pom 文件链接，查看提示找不到的类或方法所对应的组件版本
- 本地安装Gradle，以Gradle项目方式将 java-all-call-graph 项目代码导入 IDE ，查看提示找不到的类或方法所对应的组件版本

对比 java-all-call-graph、java-callgraph2 项目使用的组件，与出现问题的项目中对应的组件

通常是项目中使用的其他组件版本不同导致，升级版本后一般能解决

# 忽略特定的方法调用关系

Class1:f1()调用Class2:f2()

修改method_call表中对应记录，将enabled字段值设为0，可以忽略对应的方法调用关系，在生成方法完整调用链文件时不会出现对应的调用关系