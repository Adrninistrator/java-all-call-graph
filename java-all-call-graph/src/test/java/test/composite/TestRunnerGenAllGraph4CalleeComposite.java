package test.composite;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class TestRunnerGenAllGraph4CalleeComposite {

    public static void main(String[] args) {
        System.setProperty(Constants.PROPERTY_WRITE_CONFIG_IN_RESULT, "1");

        ConfInfo confInfo = ConfManager.getConfInfo();

        for (String outputDetail : CompositeConstants.OUTPUT_DETAIL_ARRAY) {
            for (boolean annotation : CompositeConstants.BOOLEAN_ARRAY) {
                for (boolean combined : CompositeConstants.BOOLEAN_ARRAY) {
                    for (boolean line : CompositeConstants.BOOLEAN_ARRAY) {
                        for (boolean methods : CompositeConstants.BOOLEAN_ARRAY) {
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
    }
}
