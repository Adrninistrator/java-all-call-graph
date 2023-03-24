package test.method_call_add;

import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import org.junit.Before;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
public abstract class TestMCABase {
    protected ConfigureWrapper configureWrapper = new ConfigureWrapper();

    @Before
    public void initTestMCABase() {
        // 清空全部参数，在设置参数之前执行，避免jar包或项目中的配置文件有值时对生成结果产生干扰
        configureWrapper.clearAllConfig();

        configureWrapper.setConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.setConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.setConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "1000");
        configureWrapper.setConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, Boolean.TRUE.toString());

        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "build/libs/test.jar"
        );

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                "System"
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                "TestActionListener:test1",
                "TestActionListener:test2",
                "TestActionListener:test3",
                "TestFixedManualAddCallGraph:test1",
                "TestFixedManualAddCallGraph:test2",
                "TestUnfixedManualAddCallGraph:test1a",
                "TestUnfixedManualAddCallGraph:test1b",
                "TestUnfixedManualAddCallGraph:test2"
        );

        // 添加所有预置的扩展类
        configureWrapper.addAllPreBuildExtensions();
    }
}
