package test.composite;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class TestRunnerGenAllGraphComposite {

    public static final Boolean[] BOOLEAN_ARRAY = new Boolean[]{Boolean.FALSE, Boolean.TRUE};

    public static void main(String[] args) {
        System.setProperty(JACGConstants.PROPERTY_WRITE_CONFIG_IN_RESULT, "1");

        ConfInfo confInfo = ConfManager.getConfInfo();
        if (confInfo == null) {
            return;
        }

        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            for (boolean annotation : BOOLEAN_ARRAY) {
                for (boolean combined : BOOLEAN_ARRAY) {
                    for (boolean line : BOOLEAN_ARRAY) {
                        confInfo.setCallGraphOutputDetail(outputDetailEnum.getDetail());
                        confInfo.setShowMethodAnnotation(annotation);
                        confInfo.setGenCombinedOutput(combined);
                        confInfo.setShowCallerLineNum(line);

                        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee();
                        runnerGenAllGraph4Callee.run();
                    }
                }
            }
        }

        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            for (boolean annotation : BOOLEAN_ARRAY) {
                for (boolean combined : BOOLEAN_ARRAY) {
                    for (boolean line : BOOLEAN_ARRAY) {
                        for (boolean ignore : BOOLEAN_ARRAY) {
                            for (boolean ignoreDup : BOOLEAN_ARRAY) {
                                confInfo.setCallGraphOutputDetail(outputDetailEnum.getDetail());
                                confInfo.setShowMethodAnnotation(annotation);
                                confInfo.setGenCombinedOutput(combined);
                                confInfo.setShowCallerLineNum(line);
                                confInfo.setIgnoreDupCalleeInOneCaller(ignoreDup);

                                RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller();
                                runnerGenAllGraph4Caller.setSupportIgnore(ignore);
                                runnerGenAllGraph4Caller.run();
                            }
                        }
                    }
                }
            }
        }
    }
}
