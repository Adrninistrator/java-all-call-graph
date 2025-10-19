package test.runbycode.util;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;

/**
 * @author adrninistrator
 * @date 2024/3/12
 * @description:
 */
public class JACGTestUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGTestUtil.class);

    // 尝试使用本地的配置参数
    public static void useLocalConfig(ConfigureWrapper configureWrapper) {
        // 以下为本地调试时使用
        try {
            Class<?> testRunLocalConfigClass = Class.forName("test.runlocal.config.TestRunLocalConfig");
            Method testRunLocalConfigSetMethod = testRunLocalConfigClass.getMethod("setConfig", ConfigureWrapper.class);
            testRunLocalConfigSetMethod.invoke(testRunLocalConfigClass, configureWrapper);
            logger.warn("!!!调用本地的修改配置参数方法!!!");
        } catch (ClassNotFoundException e) {
            logger.info("未找到指定的类，不执行调用本地的修改配置参数方法 {}", e.getMessage());
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    private JACGTestUtil() {
        throw new IllegalStateException("illegal");
    }
}
