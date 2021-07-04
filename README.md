# 来源
https://mp.weixin.qq.com/s/bAR-yfFStD8Hj5rLiMPhug
测试效率提升之精准测试

在精准测试中，如何获取方法与方法之间的调用链路呢？我们引入开源框架java-callgraph，该工具可以通过静态或动态方式获取java项目的方法调用关系图。java-callgraph的开源地址：https://github.com/gousiosg/java-callgraph

下载以上代码，解压，使用Maven编译
mvn install

之后按照说明使用java -jar命令执行，生成结果会输出到stdout，可以重定向到文件，一次执行生成到一个文件

生成结果示例：

M:cn.wb.cd.service.AccountPlusiiOpenAccountService:doRecordDefaultCardFlow(java.lang.String,cn.wb.cd.entity.Ap2CollectUserInfo,cn.wb.cd.apcommon.dto.OpenAccountResp,boolean) (I)cn.wb.cd.dao.Ap2DefaultCardChangeFlowDao:insert(cn.wb.cd.entity.Ap2DefaultCardChangeFlow)

M:cn.wb.cd.service.AccountPlusiiOpenAccountService:lambda$afterOpenAccount$1(java.lang.String,cn.wb.cd.entity.Ap2CollectUserInfo,cn.wb.cd.apcommon.dto.OpenAccountResp,org.apache.commons.lang3.tuple.Pair,cn.wb.cd.apcommon.dto.OpenAccountReq,cn.wb.cd.entity.Ap2CncCustInfo) (O)cn.wb.cd.service.AccountPlusiiOpenAccountService:doRecordDefaultCardFlow(java.lang.String,cn.wb.cd.entity.Ap2CollectUserInfo,cn.wb.cd.apcommon.dto.OpenAccountResp,boolean)

以上生成的调用关系只有一层直接调用关系，如果需要多层间接调用关系，可以使用程序解析输出结果，写入数据库中，每个调用关系占一行，再使用程序获得多层的间接调用关系

# 生成Java方法调用的开源项目

https://github.com/gousiosg/java-callgraph/blob/master/README.md

生成文件格式
类引用：

```
C:class1 class2
```

示例：

```
C:cn.wb.cd.wlx_radomreduce.entity.RadomReducePriceCountEntity cn.wb.cd.wlx_radomreduce.entity.RadomReducePriceCountEntity
```

方法调用：

```
M:class1:<method1>(arg_types) (typeofcall)class2:<method2>(arg_types)

typeofcall：
The line means that method1 of class1 called method2 of class2. The type of call can have one of the following values (refer to the JVM specification for the meaning of the calls):

M for invokevirtual calls
I for invokeinterface calls
O for invokespecial calls
S for invokestatic calls
D for invokedynamic calls
```

示例：

```
M:cn.wb.cd.wlx_radomreduce.dto.QueryFreeCountRsp:<init>() (O)cn.wb.cd.apcommon.dto.BaseResp:<init>()
M:cn.wb.cd.wlx_radomreduce.service.impl.QueryIfCanBeFreeServiceImpl:queryFreeCount(cn.wb.cd.wlx_radomreduce.dto.QueryFreeCountReq,cn.wb.cd.wlx_radomreduce.dto.QueryFreeCountRsp,java.lang.String) (M)cn.wb.cd.wlx_radomreduce.dto.QueryFreeCountRsp:setRetCode(java.lang.String)
```

以上生成的调用关系，根据代码顺序依次往下排列，若A.a()方法调用B.b()方法调用了多次，也会分别出现多次

# 数据存储

使用数据库表保存调用关系时

完整方法名可能很长，MySQL默认行格式一个VARCHAR字段最多767字节，索引长度也有限制，因此不对原始的完整方法名添加索引，而是计算HASH（MD5）并加上长度后，对该字段添加索引。在进行关联查询时，也通过以上HASH+长度查询

# 查找所有的被调用方法

需要将每一条调用关系都保留（对于需要的数据），这样在查看某方法所调用的所有方法的调用关系时，才能还原实际的调用情况

在查找时，每次查询一条被调用方法，使用深度优先的方式，使用一个List保存当前正在处理的节点信息，当查询到下层被调用方法时，加到List末尾，继续

若查询下层被调用方法不存在时，若非第0个节点，则返回List上一个记录，在上次查询的基础上，查询当前方法的下一个被调用方法，若查询到，则将当前记录信息更新；若未查询到，则再返回上一个记录

若查询下层被调用方法不存在时，若为第0个节点，说明已经查询完毕，结束循环

第一次查询，是找到指定的方法，及其hash

之后的每次查询，都是从方法调用关系表中，查找caller_method_hash等于当前方法hash的记录，并将callee相关信息记录到文件中

查找所有的被调用方法，与查找调用指定类方法的所有方法的过程是类似的，只是从数据库查询时的callee和caller字段使用反过来了

每个方法生成一个文件，文件名为配置文件中指定的类名（简单类名或全名）+方法名+方法名hash

方法名HASH使用BASE64编码，需要使用org.apache.commons.codec.binary.Base64.encodeBase64URLSafeString方法，使输出结果范围为字母+“-”+“_”，不是原始的字母+“+”+“/”，否则当出现“/”时，创建文件会失败

在文件第1行写入当前方法的完整信息，以#开头

第2行写入当前方法的信息，写入格式由配置参数call.graph.output.detail决定

之后的行写入被调用的方法信息，写入格式同样由以上参数决定

# 查找调用指定类方法的所有方法

查找调用某个类方法的所有方法时，不需要将调用的顺序展示出来，也无法展示出来

因为调用关系存在重复，因此查找调用指定类方法的所有方法时，根据索引INDEX idx_cg_callee_method_hash(callee_method_hash, caller_method_hash)进行查找，每次查询callee_method_hash等于某值的一条数据，查询时判断caller_method_hash应大于上一次的值，可以忽略重复的数据（第一次查询不判断caller_method_hash）
