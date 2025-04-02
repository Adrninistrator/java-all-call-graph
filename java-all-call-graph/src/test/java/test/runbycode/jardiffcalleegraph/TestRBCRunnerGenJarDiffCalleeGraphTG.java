//package test.runbycode.jardiffcalleegraph;
//
//import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
//import com.adrninistrator.jacg.conf.ConfigureWrapper;
//import com.adrninistrator.jacg.unpacker.targz.TarGzUnpacker;
//import com.adrninistrator.javacg2.util.JavaCG2Util;
//import org.junit.Assert;
//import org.junit.Test;
//import test.runbycode.util.JACGTestUtil;
//
//import java.util.Arrays;
//import java.util.Collections;
//
///**
// * @author adrninistrator
// * @date 2024/3/9
// * @description:
// */
//public class TestRBCRunnerGenJarDiffCalleeGraphTG extends TestAbstractRunnerGenJarDiffCalleeGraph {
//
//    public static final String APP_NAME = "jar_diff_tg";
//    public static final String TAR_GZ_DIR = "build/test_diff_tar_gz/";
//
//    public static final String TAR_GZ_FILE_NAME_OLD = "jar-diff-version-1.tar.gz";
//    public static final String TAR_GZ_FILE_NAME_NEW = "jar-diff-version-2.tar.gz";
//
//    @Test
//    public void test1UseLocalConfig() {
//        testFull(true);
//    }
//
//    @Test
//    public void test2SkipWriteDbUseLocalConfig() {
//        testSkipWriteDb(true);
//    }
//
//    @Test
//    public void test3NotLocalConfig() {
//        testFull(false);
//    }
//
//    @Test
//    public void test4SkipWriteDbNotLocalConfig() {
//        testSkipWriteDb(false);
//    }
//  todo
//    private TarGzUnpacker genTarGzUnpacker(String tarGzFilePath, String tarGzUnpackDirPath) {
//        TarGzUnpacker tarGzUnpacker = new TarGzUnpacker(tarGzFilePath, tarGzUnpackDirPath);
//        tarGzUnpacker.setUnpackDirList(Collections.singletonList("jar"));
//        tarGzUnpacker.setUnpackOtherFileTypeList(Arrays.asList(".xml", "properties"));
//        tarGzUnpacker.setUnpackPackageList(Collections.singletonList("diffjar"));
//        tarGzUnpacker.setNoUnpackPackageList(Collections.singletonList("test/diffjar/task"));
//        return tarGzUnpacker;
//    }
//
//    private void testFull(boolean useLocalConfig) {
//        String currentTime = JavaCG2Util.currentTime();
//        String tarGzUnpackDirPathOld = TAR_GZ_DIR + TAR_GZ_FILE_NAME_OLD + "_" + currentTime;
//        TarGzUnpacker tarGzUnpackerOld = genTarGzUnpacker(TAR_GZ_DIR + TAR_GZ_FILE_NAME_OLD, tarGzUnpackDirPathOld);
//        Assert.assertTrue(tarGzUnpackerOld.unpack());
//
//        String tarGzUnpackDirPathNew = TAR_GZ_DIR + TAR_GZ_FILE_NAME_NEW + "_" + currentTime;
//        TarGzUnpacker tarGzUnpackerNew = genTarGzUnpacker(TAR_GZ_DIR + TAR_GZ_FILE_NAME_NEW, tarGzUnpackDirPathNew);
//        Assert.assertTrue(tarGzUnpackerNew.unpack());
//
//        ConfigureWrapper configureWrapper = genConfigureWrapper(APP_NAME);
//        if (useLocalConfig) {
//            JACGTestUtil.useLocalConfig(configureWrapper);
//        }
//
//        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR, tarGzUnpackDirPathOld, tarGzUnpackDirPathNew);
//
//        testGenJarDiffCalleeGraph(configureWrapper, false);
//    }
//
//    private void testSkipWriteDb(boolean useLocalConfig) {
//        ConfigureWrapper configureWrapper = genConfigureWrapper(APP_NAME);
//        if (useLocalConfig) {
//            JACGTestUtil.useLocalConfig(configureWrapper);
//        }
//        testGenJarDiffCalleeGraph(configureWrapper, true);
//    }
//}
