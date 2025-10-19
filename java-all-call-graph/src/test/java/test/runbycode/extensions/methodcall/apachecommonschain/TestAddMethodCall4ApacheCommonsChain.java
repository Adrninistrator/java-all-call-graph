package test.runbycode.extensions.methodcall.apachecommonschain;

import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extensions.methodcall.JACGApacheCommonsChainMethodCallExtension;
import com.adrninistrator.jacg.extensions.methodcall.JavaCG2ApacheCommonsChainMethodCallExtension;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.chain.Command;
import org.apache.commons.chain.impl.ChainBase;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.chain.use.TestUseChain1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description: 验证补充 Apache Commons Chain 的方法调用
 * 为 org.apache.commons.chain.impl.ChainBase#addCommand(org.apache.commons.chain.Command) 方法补充被调用方法
 */
public class TestAddMethodCall4ApacheCommonsChain extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestAddMethodCall4ApacheCommonsChain.class);

    @Test
    public void $test0RunnerWriteDb() {
        testWriteDb();
    }

    @Test
    public void $test0RunnerWriteDbAddLib() {
        String apacheCommonsChainJarPath = JavaCG2FileUtil.getJarFilePathOfClass(ChainBase.class);
        logger.info("apacheCommonsChain jar文件路径 {}", apacheCommonsChainJarPath);

        javaCG2ConfigureWrapper.addOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, apacheCommonsChainJarPath);
        // 由于解析了commons-chain-xx.jar，因此需要忽略 org.apache.commons.chain.Command:execute() 方法调用实现类方法的调用
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL,
                CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE.getVariableName() + "=='" + JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS.getType() + "'" +
                        " && " + CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + "=='" + Command.class.getName() + "'" +
                        " && " + CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName() + "=='execute'");
        testWriteDb();
    }

    private void testWriteDb() {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JAVACG2_METHOD_CALL,
                JavaCG2ApacheCommonsChainMethodCallExtension.class.getName());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL,
                JACGApacheCommonsChainMethodCallExtension.class.getName());
        commonWriteDbForce();
    }

    @Test
    public void testRunnerGenAllGraph4Caller() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestUseChain1.class.getName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void testRunnerGenAllGraph4Callee() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName() + ":currentTimeMillis()");
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}