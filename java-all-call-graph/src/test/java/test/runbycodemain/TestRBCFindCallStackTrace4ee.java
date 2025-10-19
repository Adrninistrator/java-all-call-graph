package test.runbycodemain;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.util.JACGTestUtil;

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
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        // 设置基本的配置参数
        TestConfigGenerator.setBaseConfig(configureWrapper);
        // 使用H2数据库
        TestConfigGenerator.useH2Db(configureWrapper);
        // 设置需要生成向上的方法完整调用链的类或方法
        TestConfigGenerator.setGenCalleeGraphMethodClass(configureWrapper);
        // 设置生成向上的方法完整调用链文件后，从最底层被调用方法开始向上查找包含指定关键字的方法的调用堆栈时，使用的关键字
        TestConfigGenerator.setFindStackKeyword4ee(configureWrapper);

        // 尝试使用本地的配置参数
        JACGTestUtil.useLocalConfig(configureWrapper);

        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapper));
    }
}
