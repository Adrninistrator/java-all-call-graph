package test.runbycode.analysejavacg2jacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2025/6/2
 * @description: 解析java-callgraph2、java-all-call-graph组件jar文件并写入数据库
 */
public class TestAnalyseJavaCG2JACG0WriteDb {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJavaCG2JACG0WriteDb.class);

    @Test
    public void test() {
        String javaCG2JarFilePath = JavaCG2FileUtil.getJarFilePathOfClass(JavaCG2Entry.class);

//        TestAnalyseJACG0WriteDb testAnalyseJACG0WriteDb = new TestAnalyseJACG0WriteDb();
//        String jacgJarFilePath = testAnalyseJACG0WriteDb.getJACGJarFilePath();
//        logger.info("找到jar文件 {} {}", javaCG2JarFilePath, jacgJarFilePath);
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, javaCG2JarFilePath, "out/");

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED, Boolean.FALSE.toString());
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        Assert.assertTrue(runnerWriteDb.run());
    }
}
