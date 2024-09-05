package test.runbycode.handler.jarinfo;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.handler.jarinfo.JarInfoHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/3/11
 * @description:
 */
public class TestJarInfoHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test1() {
        try (JarInfoHandler jarInfoHandler = new JarInfoHandler(configureWrapper)) {
            List<WriteDbData4JarInfo> jarInfoList = jarInfoHandler.queryAllJarInfo();
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(jarInfoList));
            printListContent(jarInfoList);
        }
    }

    @Test
    public void test2() {
        try (JarInfoHandler jarInfoHandler = new JarInfoHandler(configureWrapper)) {
            String outputDirPath = jarInfoHandler.queryOutputDirPath();
            printObjectContent(outputDirPath);
        }
    }
}
