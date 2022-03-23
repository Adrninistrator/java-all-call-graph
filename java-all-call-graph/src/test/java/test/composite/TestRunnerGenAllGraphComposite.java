package test.composite;

import com.adrninistrator.jacg.common.JACGConstants;
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

    public static final String[] OUTPUT_DETAIL_ARRAY = new String[]{
            JACGConstants.CONFIG_OUTPUT_DETAIL_1,
            JACGConstants.CONFIG_OUTPUT_DETAIL_2,
            JACGConstants.CONFIG_OUTPUT_DETAIL_3};

    public static void main(String[] args) {
        System.setProperty(JACGConstants.PROPERTY_WRITE_CONFIG_IN_RESULT, "1");

        ConfInfo confInfo = ConfManager.getConfInfo();

        for (String outputDetail : OUTPUT_DETAIL_ARRAY) {
            for (boolean annotation : BOOLEAN_ARRAY) {
                for (boolean combined : BOOLEAN_ARRAY) {
                    for (boolean line : BOOLEAN_ARRAY) {
                        for (boolean methods : BOOLEAN_ARRAY) {
                            confInfo.setCallGraphOutputDetail(outputDetail);
                            confInfo.setShowMethodAnnotation(annotation);
                            confInfo.setGenCombinedOutput(combined);
                            confInfo.setShowCallerLineNum(line);
                            confInfo.setGenUpwardsMethodsFile(methods);

                            RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee();
                            runnerGenAllGraph4Callee.run();
                        }
                    }
                }
            }
        }

        for (String outputDetail : OUTPUT_DETAIL_ARRAY) {
            for (boolean annotation : BOOLEAN_ARRAY) {
                for (boolean combined : BOOLEAN_ARRAY) {
                    for (boolean line : BOOLEAN_ARRAY) {
                        for (boolean ignore : BOOLEAN_ARRAY) {
                            for (boolean ignoreDup : BOOLEAN_ARRAY) {
                                confInfo.setCallGraphOutputDetail(outputDetail);
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
