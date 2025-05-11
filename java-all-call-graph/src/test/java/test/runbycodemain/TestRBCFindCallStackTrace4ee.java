package test.runbycodemain;

import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "获得方法向上到包含关键字的调用堆栈",
        desc = {"首先会生成指定方法向上的完整方法调用链",
                "通过代码指定配置参数的主要功能示例"})
public class TestRBCFindCallStackTrace4ee extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_0.getDetail());
        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapper));
    }
}
