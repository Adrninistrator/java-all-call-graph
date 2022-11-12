package test.composite;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.TestRunByCodeBase;

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
        new RunnerWriteDb().run(configureWrapper);
    }

    @Test
    public void test() {
        System.setProperty(JACGConstants.PROPERTY_WRITE_CONFIG_IN_RESULT, "1");

        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }

            for (boolean annotation : BOOLEAN_ARRAY) {
                for (boolean combined : BOOLEAN_ARRAY) {
                    for (boolean line : BOOLEAN_ARRAY) {
                        configureWrapper.addConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
                        configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_METHOD_ANNOTATION, String.valueOf(annotation));
                        configureWrapper.addConfig(ConfigKeyEnum.CKE_GEN_COMBINED_OUTPUT, String.valueOf(combined));
                        configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_CALLER_LINE_NUM, String.valueOf(line));

                        if (!new RunnerGenAllGraph4Callee().run(configureWrapper)) {
                            logger.error("执行失败");
                            return;
                        }
                    }
                }
            }
        }

        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }

            for (boolean annotation : BOOLEAN_ARRAY) {
                for (boolean combined : BOOLEAN_ARRAY) {
                    for (boolean line : BOOLEAN_ARRAY) {
                        for (boolean ignore : BOOLEAN_ARRAY) {
                            for (boolean ignoreDup : BOOLEAN_ARRAY) {
                                for (boolean inCurrentFile : BOOLEAN_ARRAY) {
                                    configureWrapper.addConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
                                    configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_METHOD_ANNOTATION, String.valueOf(annotation));
                                    configureWrapper.addConfig(ConfigKeyEnum.CKE_GEN_COMBINED_OUTPUT, String.valueOf(combined));
                                    configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_CALLER_LINE_NUM, String.valueOf(line));
                                    configureWrapper.addConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, String.valueOf(ignoreDup));
                                    configureWrapper.addConfig(ConfigKeyEnum.CKE_MULTI_IMPL_GEN_IN_CURRENT_FILE, String.valueOf(inCurrentFile));

                                    RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller();
                                    runnerGenAllGraph4Caller.setSupportIgnore(ignore);
                                    if (!runnerGenAllGraph4Caller.run(configureWrapper)) {
                                        logger.error("执行失败");
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
