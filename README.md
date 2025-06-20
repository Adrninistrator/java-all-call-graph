[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-all-call-graph.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-all-call-graph/)

[![Apache License 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](https://github.com/Adrninistrator/java-all-call-graph/blob/master/LICENSE)

# 1. 使用案例

[使用案例](docs/use_cases.md)

# 2. 加群讨论

[加群讨论](docs/group_discussions.md)

# 3. DeepWiki 链接

[https://deepwiki.com/Adrninistrator/java-all-call-graph](https://deepwiki.com/Adrninistrator/java-all-call-graph)

通过大模型分析项目代码，可向大模型提出关于项目的问题，包括使用方法等

# 4. 前言

在很多场景下，如果能够生成 Java 代码中方法之间的调用链，是很有帮助的，例如分析代码执行流程、确认被修改代码的影响范围、代码审计/漏洞分析等。

IDEA 提供了显示调用指定 Java 方法向上的完整调用链的功能，可以通过“Navigate -> Call Hierarchy”菜单（快捷键：Ctrl+Alt+H) 使用；Eclipse 也提供了相同的功能。但以上都需要针对每个方法进行手工处理，不支持对方法进行过滤或者其他扩展功能。

以下实现了一个工具，能够通过静态分析的方式批量生成指定 Java 方法向下的完整调用链，对于关注的 Java 方法，能够生成其向下调用的方法信息，及被调用方法再向下调用的方法，直到最下层被调用的方法。

也可以生成调用指定 Java 类方法向上的完整调用链，对于关注的 Java 类的方法，能够生成调用对应方法的方法信息，及调用上述方法的信息，直到最上层未被其他方法调用的方法（通常是对外提供的服务，或定时任务等）。

本工具生成的 Java 方法完整调用链中，支持显示相关的包名、类名、方法名、方法参数、调用者源代码行号、方法注解、循环调用，入口方法。

本工具支持生成某个方法到起始方法之间的调用链，也支持根据关键字查找关注的方法，生成其到起即方法之间的调用链。

`本项目提供了扩展功能，可用于为 Java 代码自动生成 UML 时序图`，可参考 [https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)，根据关键字查找关注的方法时，可使用自定义 Java 代码判断是否满足关键字，在该文档中会有说明。

本项目用于获取 Java 方法调用关系的功能在 [https://github.com/Adrninistrator/java-callgraph2](https://github.com/Adrninistrator/java-callgraph2) 中实现

在 github 查看文档时，图片可能无法显示，影响使用说明文档的查看，可查看 gitee 中的当前项目 [https://gitee.com/Adrninistrator/java-all-call-graph](https://gitee.com/Adrninistrator/java-all-call-graph)。

# 5. 输出结果示例

[输出结果示例](docs/output_example.md)

# 6. 快速开始

[快速开始](docs/quick_start.md)

# 7. 更新说明

[更新说明](docs/change_log.md)

# 8. 配置参数示例

[配置参数示例](docs/config_example.md)

# 9. 使用说明

[使用说明](docs/how_to_use.md)

# 10. 通过源码执行

[通过源码执行](docs/run_by_code.md)

# 11. 数据库表说明

[数据库表说明](docs/db_tables.md)

# 12. 使用 Neo4j 图数据库

[使用 Neo4j 图数据库](docs/use_neo4j.md)

# 13. 通过代码执行的其他示例

[通过代码执行的其他示例](docs/run_by_code_example.md)

# 14. 其他功能

[其他功能](docs/other_functions.md)

# 15. 扩展功能

[扩展功能](docs/extensions.md)

# 16. 原理说明

[原理说明](docs/how_to_implementation.md)

# 17. 其他说明

[其他说明](docs/other_instructions.md)

# 18. 常见问题

[常见问题](docs/question_answer.md)
