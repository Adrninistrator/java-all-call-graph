package test.runbycodemain.db;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.config.TestConfigGenerator;

/**
 * @author adrninistrator
 * @date 2025/11/19
 * @description:
 */
@JACGExample(title = "解析测试代码并将结果写入MySQL数据库",
        desc = {})
public class TestUseMySQL {

    @Test
    public void test() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, com.mysql.cj.jdbc.Driver.class.getName());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:mysql://127.0.0.1:3307/testdb?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
