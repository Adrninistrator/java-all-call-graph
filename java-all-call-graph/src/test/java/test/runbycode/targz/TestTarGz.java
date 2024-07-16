package test.runbycode.targz;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.unpacker.targz.TarGzUnpacker;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description:
 */
public class TestTarGz extends TestRunByCodeBase {

    public static final String TAR_GZ_DIR = "build/tar_gz";
    public static final String TAR_GZ_FILE_NAME = "java-all-call-graph-2.0.1.tar.gz";
    public static final String TAR_GZ_UNPACK_DIR = TAR_GZ_DIR + File.separator + "tar_gz@" + TAR_GZ_FILE_NAME;

    @Test
    public void test1Unpack() {
        TarGzUnpacker tarGzUnpacker = new TarGzUnpacker(TAR_GZ_DIR + File.separator + TAR_GZ_FILE_NAME,
                TAR_GZ_UNPACK_DIR,
                Arrays.asList("jar", "lib"),
                Arrays.asList("run_jacg", "java-callgraph2", "mybatis-mysql-table-parser", "log4j-"),
                Arrays.asList(".xml", "properties"),
                Arrays.asList("adrninistrator", "org.apache.logging.log4j.core"),
                Collections.singletonList("com.adrninistrator.mybatismysqltableparser"));
        Assert.assertTrue(tarGzUnpacker.unpack());
    }

    @Test
    public void test2WriteDb() {
        configureWrapper.setAllowAllClasses();
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR, TAR_GZ_UNPACK_DIR);
        Assert.assertTrue(new RunnerWriteDb(configureWrapper).run());
    }
}
