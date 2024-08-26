[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-all-call-graph.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-all-call-graph/)

[![Apache License 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](https://github.com/Adrninistrator/java-all-call-graph/blob/master/LICENSE)

# 1. 加群讨论

[加群讨论](group_discussions.md)

# 2. 前言

在很多场景下，如果能够生成Java代码中方法之间的调用链，是很有帮助的，例如分析代码执行流程、确认被修改代码的影响范围、代码审计/漏洞分析等。

IDEA提供了显示调用指定Java方法向上的完整调用链的功能，可以通过“Navigate -> Call Hierarchy”菜单(快捷键：Ctrl+Alt+H)使用；Eclipse也提供了相同的功能。但以上都需要针对每个方法进行手工处理，不支持对方法进行过滤或者其他扩展功能。

以下实现了一个工具，能够通过静态分析的方式批量生成指定Java方法向下的完整调用链，对于关注的Java方法，能够生成其向下调用的方法信息，及被调用方法再向下调用的方法，直到最下层被调用的方法。

也可以生成调用指定Java类方法向上的完整调用链，对于关注的Java类的方法，能够生成调用对应方法的方法信息，及调用上述方法的信息，直到最上层未被其他方法调用的方法（通常是对外提供的服务，或定时任务等）。

本工具生成的Java方法完整调用链中，支持显示相关的包名、类名、方法名、方法参数、调用者源代码行号、方法注解、循环调用，入口方法。

本工具支持生成某个方法到起始方法之间的调用链，也支持根据关键字查找关注的方法，生成其到起即方法之间的调用链。

`本项目提供了扩展功能，可用于为Java代码自动生成UML时序图`，可参考[https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)，根据关键字查找关注的方法时，可使用自定义Java代码判断是否满足关键字，在该文档中会有说明。

本项目用于获取Java方法调用关系的功能在[https://github.com/Adrninistrator/java-callgraph2](https://github.com/Adrninistrator/java-callgraph2)中实现

在github查看文档时，图片可能无法显示，影响使用说明文档的查看，可查看gitee中的当前项目[https://gitee.com/Adrninistrator/java-all-call-graph](https://gitee.com/Adrninistrator/java-all-call-graph)。

# 3. 输出结果示例

[输出结果示例](output_example.md)

# 4. 快速开始

[快速开始](quick_start.md)

# 5. 更新说明

[更新说明](change_log.md)

# 6. 配置参数示例

以下配置参数的详细说明见对应配置文件

[配置参数示例](config_example.md)

# 7. 使用说明

[使用说明](how_to_use.md)

# 8. 通过源码执行

[通过源码执行](run_by_code.md)

# 9. 数据库表说明

[数据库表说明](db_tables.md)

# 10. 使用Neo4j图数据库

[使用Neo4j图数据库](use_neo4j.md)

# 11. JVM参数及Java代码开关

[JVM参数及Java代码开关](jvm_options_java_switch.md)

# 12. 通过代码执行的其他示例

[通过代码执行的其他示例](run_by_code_example.md)

# 13. 其他功能

[其他功能](other_functions.md)

# 14. 扩展功能

[扩展功能](extensions.md)

# 15. 原理说明

[原理说明](how_to_implementation.md)

# 16. 其他说明

[其他说明](other_instructions.md)

# 17. 常见问题

[常见问题](question_answer.md)
