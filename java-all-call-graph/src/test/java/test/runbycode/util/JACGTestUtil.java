package test.runbycode.util;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
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

    // 使用本地的配置参数
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

    // 使用H2数据库
    public static void useH2Db(ConfigureWrapper configureWrapper) {
        logger.info("使用H2数据库");
        // H2
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");
    }

    private JACGTestUtil() {
        throw new IllegalStateException("illegal");
    }
}
