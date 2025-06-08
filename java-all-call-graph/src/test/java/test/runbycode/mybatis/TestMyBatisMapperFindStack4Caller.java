package test.runbycode.mybatis;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.mybatis.service.listener.ListenerService1Impl;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/5/24
 * @description:
 */
public class TestMyBatisMapperFindStack4Caller extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void testCallerStack() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                ListenerService1Impl.class.getName()
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType()
        );
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                JACGConstants.CALL_FLAG_BUSINESS_DATA + DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType()
        );
        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(false, configureWrapper);
        CallStackFileResult callStackFileResult = findCallStackTrace.find();
        Assert.assertTrue(callStackFileResult.isSuccess());
    }
}
