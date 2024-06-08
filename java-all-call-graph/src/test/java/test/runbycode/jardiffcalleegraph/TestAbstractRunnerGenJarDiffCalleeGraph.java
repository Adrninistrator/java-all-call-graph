package test.runbycode.jardiffcalleegraph;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
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
            RunnerGenJarDiffCalleeGraph genJarDiffCalleeGraph = new RunnerGenJarDiffCalleeGraph(configureWrapper);
            if (skipWriteDb) {
                genJarDiffCalleeGraph.setSkipWriteDb(true);
            }
            Assert.assertTrue(genJarDiffCalleeGraph.generate(entryMethodInfoFiller4Spring));
        }
    }
}
