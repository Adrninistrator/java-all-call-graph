package test.runbycode.analysejacg;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/5/28
 * @description: 解析java-all-call-graph组件jar文件并写入数据库
 */
public class TestAnalyseJACG0WriteDb {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJACG0WriteDb.class);

    public static final String DIR_BUILD_LIBS = "build/libs";

    public String getJACGJarFilePath() {
        List<File> jarFileList = JACGFileUtil.findFileInCurrentDir(DIR_BUILD_LIBS, JavaCG2Constants.EXT_JAR);
        if (JavaCG2Util.isCollectionEmpty(jarFileList)) {
            Assert.fail("未找到生成的jar文件，需要先执行 gradlew jar 命令");
        }

        long maxJarFileModifiedTime = 0;
        String maxJarFilePath = null;
        for (File jarFile : jarFileList) {
            String jarFileName = jarFile.getName();
            if (!jarFileName.startsWith("java-all-call-graph-")) {
                continue;
            }

            if (maxJarFileModifiedTime == 0 || jarFile.lastModified() > maxJarFileModifiedTime) {
                maxJarFileModifiedTime = jarFile.lastModified();
                maxJarFilePath = jarFile.getAbsolutePath();
            }
        }
        if (maxJarFilePath == null) {
            Assert.fail("未找到jacg生成的jar文件，需要先执行 gradlew jar 命令");
        }
        logger.info("找到jacg生成的jar文件 {}", maxJarFilePath);
        return maxJarFilePath;
    }

    @Test
    public void test() {
        String jacgJarFilePath = getJACGJarFilePath();
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, jacgJarFilePath);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED, Boolean.FALSE.toString());
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        Assert.assertTrue(runnerWriteDb.run());
    }
}
