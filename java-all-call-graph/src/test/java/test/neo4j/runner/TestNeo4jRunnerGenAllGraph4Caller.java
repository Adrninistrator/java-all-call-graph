package test.neo4j.runner;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.neo4j.runner.Neo4jRunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.argument.TestArgumentGenerics1;
import test.callgraph.extendcomplex2.TestUseService;
import test.callgraph.extendsimple.ChildClassSA1;
import test.callgraph.methodcall.TestMCCaller;
import test.callgraph.spring.bean.beanannotation.variables.TestSPBVariablesDefine;
import test.callgraph.spring.bean.use.complex.TestUseComplexService;
import test.callgraph.superjdk.objectinputstream.TestReadObject1;
import test.callgraph.thread.callable.TestCallable;
import test.callgraph.thread.runnable.TestRunnable;
import test.callgraph.thread.thread.TestThread;
import test.callgraph.thread.threadpool.TestThreadPool;
import test.callgraph.thread.timertask.TestTimerTask;
import test.callgraph.type.TestReturnTypeGenerics1;
import test.callgraph.type.TestType1;
import test.neo4j.base.TestNeo4jBase;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public class TestNeo4jRunnerGenAllGraph4Caller extends TestNeo4jBase {

    @Test
    public void test() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestTimerTask.class.getName(),
                TestThreadPool.class.getName(),
                TestThread.class.getName(),
                TestRunnable.class.getName(),
                TestCallable.class.getName(),
                TestArgumentGenerics1.class.getName(),
                TestReturnTypeGenerics1.class.getName(),
                TestSPBVariablesDefine.class.getName(),
                ChildClassSA1.class.getName(),
                TestUseComplexService.class.getName(),
                TestUseService.class.getName(),
                TestType1.class.getName(),
                TestReadObject1.class.getName(),
                TestMCCaller.class.getName() + ":59 60-61"
        );

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_WRITE_TABLE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType()
        );
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_JSON_CALLER, Boolean.TRUE.toString());
        Assert.assertTrue(new Neo4jRunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
