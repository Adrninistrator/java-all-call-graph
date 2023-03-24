package test.method_call_add;

import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Test;
import test.extensions.method_call_add.MCAExt4ActionListener;
import test.extensions.method_call_add.MCAExt4FixedService1;
import test.extensions.method_call_add.MCAExt4UnfixedService1;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
public class TestMethodCallAdd extends TestMCABase {
    @Test
    public void test0WithOutExtensions() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();

        configureWrapperCopy.setConfig(ConfigKeyEnum.CKE_APP_NAME, "test_mca_off");
        configureWrapperCopy.setConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_mca_off");
        // 指定扩展类为空
        configureWrapperCopy.clearOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_CALL_ADD);

        new RunnerWriteDb().run(configureWrapperCopy);
        new RunnerGenAllGraph4Callee().run(configureWrapperCopy);
        new RunnerGenAllGraph4Caller().run(configureWrapperCopy);
    }

    @Test
    public void test1WithExtensions() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setConfig(ConfigKeyEnum.CKE_APP_NAME, "test_mca_on");
        configureWrapperCopy.setConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_mca_on");
        // 指定扩展类
        configureWrapperCopy.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_CALL_ADD,
                MCAExt4ActionListener.class.getName(),
                MCAExt4FixedService1.class.getName(),
                MCAExt4UnfixedService1.class.getName()
        );

        new RunnerWriteDb().run(configureWrapperCopy);
        new RunnerGenAllGraph4Callee().run(configureWrapperCopy);
        new RunnerGenAllGraph4Caller().run(configureWrapperCopy);
    }
}
