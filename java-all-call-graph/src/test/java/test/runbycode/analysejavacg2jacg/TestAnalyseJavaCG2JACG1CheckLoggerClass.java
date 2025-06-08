package test.runbycode.analysejavacg2jacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.handler.methodcall.LoggerFactoryGetLoggerClassChecker;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/6/2
 * @description: 检查java-callgraph2、java-all-call-graph组件LoggerFactory.getLogger方法参数是否都是当前类
 */
public class TestAnalyseJavaCG2JACG1CheckLoggerClass {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJavaCG2JACG1CheckLoggerClass.class);

    @Test
    public void test() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        check(configureWrapper);
    }

    public void check(ConfigureWrapper configureWrapper) {
        try (LoggerFactoryGetLoggerClassChecker loggerFactoryGetLoggerClassChecker = new LoggerFactoryGetLoggerClassChecker(configureWrapper)) {
            Set<String> invalidClassSet = loggerFactoryGetLoggerClassChecker.queryInvalidClassList();
            if (invalidClassSet.isEmpty()) {
                logger.info("检查通过");
                return;
            }
            logger.error("以下类调用LoggerFactory.getLogger方法参数不是当前类 {}", StringUtils.join(invalidClassSet, " "));
            Assert.fail("检查不通过");
        }
    }
}
