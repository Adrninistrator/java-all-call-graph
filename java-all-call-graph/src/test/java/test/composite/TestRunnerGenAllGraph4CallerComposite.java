package test.composite;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class TestRunnerGenAllGraph4CallerComposite {

    public static void main(String[] args) {
        System.setProperty(Constants.PROPERTY_WRITE_CONFIG_IN_RESULT, "1");
        ConfInfo confInfo = ConfManager.getConfInfo();

        for (String outputDetail : CompositeConstants.OUTPUT_DETAIL_ARRAY) {
            for (boolean annotation : CompositeConstants.BOOLEAN_ARRAY) {
                for (boolean combined : CompositeConstants.BOOLEAN_ARRAY) {
                    for (boolean line : CompositeConstants.BOOLEAN_ARRAY) {
                        for (boolean ignore : CompositeConstants.BOOLEAN_ARRAY) {
                            confInfo.setCallGraphOutputDetail(outputDetail);
                            confInfo.setShowMethodAnnotation(annotation);
                            confInfo.setGenCombinedOutput(combined);
                            confInfo.setShowCallerLineNum(line);

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
