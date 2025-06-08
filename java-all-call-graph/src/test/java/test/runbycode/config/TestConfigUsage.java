package test.runbycode.config;

import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.interfaces.ConfigInterface;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2025/6/2
 * @description:
 */
public class TestConfigUsage {

    private static final Logger logger = LoggerFactory.getLogger(TestConfigUsage.class);

    private final JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();

    @Test
    public void test() {
        doTest(ConfigDbKeyEnum.values());
        doTest(ConfigKeyEnum.values());
        doTest(OtherConfigFileUseListEnum.values());
        doTest(OtherConfigFileUseSetEnum.values());
    }

    private void doTest(ConfigInterface[] configInterfaces) {
        for (ConfigInterface configInterface : configInterfaces) {
            String usage = javaCG2ConfigureWrapper.genConfigUsage(configInterface);
            logger.info("### {}", usage);
        }
    }
}
