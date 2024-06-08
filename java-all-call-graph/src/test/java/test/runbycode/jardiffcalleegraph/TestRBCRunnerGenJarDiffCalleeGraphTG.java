package test.runbycode.jardiffcalleegraph;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.unpacker.targz.TarGzUnpacker;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.util.JACGTestUtil;

import java.util.Arrays;
import java.util.Collections;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description:
 */
public class TestRBCRunnerGenJarDiffCalleeGraphTG extends TestAbstractRunnerGenJarDiffCalleeGraph {

    public static final String APP_NAME = "jar_diff_tg";
    public static final String TAR_GZ_DIR = "build/tar_gz/";

    public static final String TAR_GZ_FILE_NAME_OLD = "java-all-call-graph-1.0.15.tar.gz";
    public static final String TAR_GZ_FILE_NAME_NEW = "java-all-call-graph-1.0.99.tar.gz";

    @Test
    public void test1UseLocalConfig() {
        testFull(true);
    }

    @Test
    public void test2SkipWriteDbUseLocalConfig() {
        testSkipWriteDb(true);
    }

    @Test
    public void test3NotLocalConfig() {
        testFull(false);
    }

    @Test
    public void test4SkipWriteDbNotLocalConfig() {
        testSkipWriteDb(false);
    }

    private TarGzUnpacker genTarGzUnpacker(String tarGzFilePath, String tarGzUnpackDirPath) {
        return new TarGzUnpacker(tarGzFilePath,
                tarGzUnpackDirPath,
                Collections.singletonList("jar"),
                Arrays.asList(".xml", "properties"),
                Collections.singletonList("adrninistrator"),
                Collections.singletonList("com/adrninistrator/mybatismysqltableparser"));
    }

    private void testFull(boolean useLocalConfig) {
        String currentTime = JavaCGUtil.currentTime();
        String tarGzUnpackDirPathOld = TAR_GZ_DIR + TAR_GZ_FILE_NAME_OLD + "_" + currentTime;
        TarGzUnpacker tarGzUnpackerOld = genTarGzUnpacker(TAR_GZ_DIR + TAR_GZ_FILE_NAME_OLD, tarGzUnpackDirPathOld);
        Assert.assertTrue(tarGzUnpackerOld.unpack());

        String tarGzUnpackDirPathNew = TAR_GZ_DIR + TAR_GZ_FILE_NAME_NEW + "_" + currentTime;
        TarGzUnpacker tarGzUnpackerNew = genTarGzUnpacker(TAR_GZ_DIR + TAR_GZ_FILE_NAME_NEW, tarGzUnpackDirPathNew);
        Assert.assertTrue(tarGzUnpackerNew.unpack());

        ConfigureWrapper configureWrapper = genConfigureWrapper(APP_NAME);
        if (useLocalConfig) {
            JACGTestUtil.useLocalConfig(configureWrapper);
        }

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR, tarGzUnpackDirPathOld, tarGzUnpackDirPathNew);

        testGenJarDiffCalleeGraph(configureWrapper, false);
    }

    private void testSkipWriteDb(boolean useLocalConfig) {
        ConfigureWrapper configureWrapper = genConfigureWrapper(APP_NAME);
        if (useLocalConfig) {
            JACGTestUtil.useLocalConfig(configureWrapper);
        }
        testGenJarDiffCalleeGraph(configureWrapper, true);
    }
}
