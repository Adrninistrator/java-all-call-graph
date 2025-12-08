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
@JACGExample(title = "解析测试代码并将结果写入H2数据库",
        desc = {})
public class TestUseH2Db {

    @Test
    public void test() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
