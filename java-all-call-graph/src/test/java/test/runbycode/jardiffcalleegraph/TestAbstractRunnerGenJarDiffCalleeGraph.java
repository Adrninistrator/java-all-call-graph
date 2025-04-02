package test.runbycode.jardiffcalleegraph;

import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.diff.runner.RunnerGenJarDiffCalleeGraph;
import com.adrninistrator.jacg.handler.entrymethodinfo.EntryMethodInfoFiller4Spring;
import org.junit.Assert;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.util.JACGTestUtil;

/**
 * @author adrninistrator
 * @date 2024/4/20
 * @description:
 */
public abstract class TestAbstractRunnerGenJarDiffCalleeGraph extends TestRunByCodeBase {

    protected ConfigureWrapper genConfigureWrapper(String appName) {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, appName);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "1000");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, "");

        // H2
        JACGTestUtil.useH2Db(configureWrapper);
        return configureWrapper;
    }

    protected void testGenJarDiffCalleeGraph(ConfigureWrapper configureWrapper, boolean skipWriteDb) {
        try (EntryMethodInfoFiller4Spring entryMethodInfoFiller4Spring = new EntryMethodInfoFiller4Spring(configureWrapper)) {
            RunnerGenJarDiffCalleeGraph genJarDiffCalleeGraph = new RunnerGenJarDiffCalleeGraph(javaCG2ConfigureWrapper, configureWrapper);
            if (skipWriteDb) {
                genJarDiffCalleeGraph.setSkipWriteDb(true);
            }
            Assert.assertTrue(genJarDiffCalleeGraph.generate(entryMethodInfoFiller4Spring));
        }
    }

    protected void testDiff(boolean useLocalConfig) {
        ConfigureWrapper configureWrapper = genConfigureWrapper(chooseAppName());
        if (useLocalConfig) {
            JACGTestUtil.useLocalConfig(configureWrapper);
        }

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR, chooseRootJarDir() + chooseDirNameOld(), chooseRootJarDir() + chooseDirNameNew());

        testGenJarDiffCalleeGraph(configureWrapper, false);
    }

    protected abstract String chooseAppName();

    protected abstract String chooseRootJarDir();

    protected abstract String chooseDirNameOld();

    protected abstract String chooseDirNameNew();
}
