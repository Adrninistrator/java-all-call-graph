package test.runbycode.ignoredupcallee;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/6/6
 * @description:
 */
public class TestIgnoreDupCallee extends TestRunByCodeBase {

    private final String simpleClassName = this.getClass().getSimpleName();

    @Test
    public void test() {
        commonWriteDb();

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(), DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, simpleClassName + "-data_no-ignore_no");
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, simpleClassName + "-data_no-ignore_yes");
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, simpleClassName + "-data_yes-ignore_no");
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, simpleClassName + "-data_yes-ignore_yes");
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
