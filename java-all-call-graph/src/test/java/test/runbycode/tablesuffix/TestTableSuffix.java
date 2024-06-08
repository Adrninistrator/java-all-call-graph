package test.runbycode.tablesuffix;

import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/3/9
 * @description:
 */
public class TestTableSuffix extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testInsertMulti1() {
        doTest();
    }

    @Test
    public void testInsertMulti2() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_suffix");
        doTest();
    }


    @Test
    public void testTableSuffix1() {
        ConfigureWrapper configureWrapperTableSuffixEmpty = configureWrapper.copy();
        configureWrapperTableSuffixEmpty.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, "");
        try (ClassInfoHandler classInfoHandler1 = new ClassInfoHandler(configureWrapperTableSuffixEmpty)) {
            classInfoHandler1.queryClassNameByPackagePrefix("");
        }

        ConfigureWrapper configureWrapperTableSuffix1 = configureWrapper.copy();
        configureWrapperTableSuffix1.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, "");
        try (ClassInfoHandler classInfoHandler2 = new ClassInfoHandler(configureWrapperTableSuffix1)) {
            classInfoHandler2.queryClassNameByPackagePrefix("");
        }
    }

    private void doTest() {
        for (int i = 0; i < 3; i++) {
            ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
            configureWrapperCopy.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, String.valueOf(i));
            Assert.assertTrue(new RunnerWriteDb(configureWrapperCopy).run());
            Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapperCopy).run());
            Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapperCopy).run());
        }
    }
}
