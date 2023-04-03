package test.composite;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class TestRunnerGenAllGraphComposite extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRunnerGenAllGraphComposite.class);

    public static final Boolean[] BOOLEAN_ARRAY = new Boolean[]{Boolean.FALSE, Boolean.TRUE};

    @Before
    public void initTestRunnerGenAllGraphComposite() {
        new RunnerWriteDb().run(configureWrapper, javaCGConfigureWrapper);
    }

    @Test
    public void test() {
        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }

            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());

            if (!new RunnerGenAllGraph4Callee().run(configureWrapper)) {
                logger.error("执行失败");
                return;
            }
        }

        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }

            for (boolean ignore : BOOLEAN_ARRAY) {
                for (boolean ignoreDup : BOOLEAN_ARRAY) {
                    configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
                    configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, String.valueOf(ignoreDup));

                    RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller();
                    if (!runnerGenAllGraph4Caller.run(configureWrapper)) {
                        logger.error("执行失败");
                        return;
                    }
                }
            }
        }
    }
}
