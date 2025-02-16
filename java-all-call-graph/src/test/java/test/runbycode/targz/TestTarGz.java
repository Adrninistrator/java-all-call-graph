package test.runbycode.targz;

import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.targz.TarGzUnpacker;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

import java.io.File;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description:
 */
public class TestTarGz extends TestRunByCodeBase {

    public static final String TAR_GZ_DIR = "build/tar_gz";
    public static final String TAR_GZ_FILE_NAME = "java-all-call-graph-2.1.0.tar.gz";
    public static final String TAR_GZ_UNPACK_DIR = TAR_GZ_DIR + File.separator + "tar_gz@" + TAR_GZ_FILE_NAME;

    @Test
    public void test1Unpack() {
        TarGzUnpacker tarGzUnpacker = new TarGzUnpacker(TAR_GZ_DIR + File.separator + TAR_GZ_FILE_NAME, TAR_GZ_UNPACK_DIR);
        Assert.assertTrue(tarGzUnpacker.unpack());
    }

    @Test
    public void test2WriteDb() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, TAR_GZ_UNPACK_DIR);
        commonWriteDb();
    }
}
