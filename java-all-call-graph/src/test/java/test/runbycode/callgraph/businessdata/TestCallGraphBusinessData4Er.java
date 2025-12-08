package test.runbycode.callgraph.businessdata;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallLineData4Er;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.annotation.MethodWithAnnotation;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/3/2
 * @description:
 */
public class TestCallGraphBusinessData4Er extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testGenCallGraph() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName);
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_WRITE_TABLE.getType()
        );
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void testGenCallGraphInMemory() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName);
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_WRITE_TABLE.getType()
        );
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                MethodWithAnnotation.class.getName() + ":test3()");

        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        Map<String, List<MethodCallLineData4Er>> allMethodCallLineData4ErMap = runnerGenAllGraph4Caller.getAllMethodCallLineData4ErMap();
        for (Map.Entry<String, List<MethodCallLineData4Er>> entry : allMethodCallLineData4ErMap.entrySet()) {
            List<MethodCallLineData4Er> methodCallLineData4ErList = entry.getValue();
            Assert.assertTrue(checkListDataAllFieldFilled(methodCallLineData4ErList));
        }
    }
}
