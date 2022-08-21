# 1. 说明

在某些情况下需要进行一些定制化的处理，可以使用以下JVM参数及Java代码开关。

## 1.1. JVM参数

在JVM参数中通过“-Dxxx=yyy”的形式，可以指定以下参数及对应的值。

### 1.1.1. 指定配置文件根目录-input.root.path

- 参数名

input.root.path

- 作用

以上参数用于指定"~jacg_config"、"~jacg_extensions"、"~jacg_find_keyword"、"~jacg_sql"等配置文件目录所在的路径：

优先从以上指定的目录中获取以上配置文件；

再从当前目录中获取配置文件；

再从classpath中获取配置文件；

最后从jar包中获取配置文件。

- 参数值格式

需要指定绝对路径形式，结尾可不指定目录分隔符"/"或"\"

### 1.1.2. 指定是否在结果文件中写入配置信息-write.config

- 参数名

write.config

- 作用

指定在生成的结果文件中，是否写入当前使用的配置信息

默认值为false

- 参数值格式

true: 写入

false: 不写入

### 1.1.3. 指定生成结果文件根目录-output.root.path

- 参数名

output.root.path

- 作用

指定生成结果文件时使用的根目录，默认使用当前目录

指定以上参数后，在生成结果文件时，会以参数值对应的目录作为根目录，每次生成的目录名会以`app.name`参数开头

- 参数值格式

需要指定绝对路径形式，结尾可不指定目录分隔符"/"或"\"

### 1.1.4. 指定跳过检查Jar包文件是否有更新-skip.check.jar.file.updated

- 参数名

skip.check.jar.file.updated

- 作用

以上参数用于指定，在生成向上或向下的方法完整调用链之前，是否需要检查对应的Jar包文件内容有没有更新

默认值为false

- 参数值格式

true: 跳过检查

false: 执行检查

### 1.1.5. 指定批量写入数据库时每次插入的数量-db.insert.batch.size

- 参数名

db.insert.batch.size

- 作用

指定批量向数据库写入数据时，每次执行插入操作时的记录数量

默认值为1000

假如向数据库写入注解信息时，有出现重复键异常，可将以上参数值设置为1，再次执行时，会在日志中显示出现重复的注解信息

- 参数值格式

指定为正整数

## 1.2. Java代码开关

### 1.2.1. 操作结束时不关闭数据源

默认情况下，执行完向数据库写入数据、生成向上或向下的方法完整调用链操作时，会关闭当前使用的数据源

假如需要以上操作执行完毕后不关闭数据源，可以在执行以上操作之前，调用com.adrninistrator.jacg.runner.base.AbstractRunner类的setCloseDsBeforeExit()方法，传入参数为false，调用一次即可

进行以上处理后，在应用退出前需要关闭当前使用的数据源，调用com.adrninistrator.jacg.dboper.DbOperator类实例的closeDs()方法
