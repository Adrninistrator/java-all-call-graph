package test.runbycode.jardiffcallgraph;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.handler.calleemethodinfo.AbstractCalleeMethodInfoFiller;
import com.adrninistrator.jacg.handler.calleemethodinfo.CalleeMethodInfoFiller4MyBatisMSXml;
import com.adrninistrator.jacg.handler.entrymethodinfo.AbstractEntryMethodInfoFiller;
import com.adrninistrator.jacg.handler.entrymethodinfo.EntryMethodInfoFiller4Spring;
import com.adrninistrator.jacg.jardiff.dto.result.JarDiffResult;
import com.adrninistrator.jacg.jardiff.filter.ModifiedMethodFilterInterface;
import com.adrninistrator.jacg.jardiff.runner.RunnerGenJarDiffCalleeGraph;
import com.adrninistrator.jacg.jardiff.runner.RunnerGenJarDiffCallerGraph;
import org.junit.Assert;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.jardiffcallgraph.filler.EntryMethodInfoFiller4XmlCommand;
import test.runbycode.parsedcustomdata.parser.TestCodeParserWithCustomData1;
import test.runbycode.util.JACGTestUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/4/20
 * @description:
 */
public abstract class TestAbstractRunnerGenJarDiffCallGraph extends TestRunByCodeBase {

    protected void testJarDiffCalleeGraph() {
        doTestJarDiffCalleeGraph(false, false, false);
        doTestJarDiffCalleeGraph(true, true, true);
    }

    /**
     * 执行jar diff功能，生成发生变化的方法向上的调用链
     */
    protected void doTestJarDiffCalleeGraph(boolean useCalleeFiler4MyBatisMSXml, boolean useEntryFiler4Spring, boolean useEntryFiler4CommandXml,
                                            ModifiedMethodFilterInterface... modifiedMethodFilters) {
        ConfigureWrapper configureWrapper = TestConfigGenerator.genConfigureWrapper();
        // 使用本地的配置参数
        JACGTestUtil.useLocalConfig(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, chooseAppName());

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLEE_GRAPH_DIR, chooseRootJarDir() + chooseDirNameOld(),
                chooseRootJarDir() + chooseDirNameNew());
        if (useEntryFiler4CommandXml) {
            configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER, TestCodeParserWithCustomData1.class.getName());
        }

        try (CalleeMethodInfoFiller4MyBatisMSXml calleeMethodInfoFiller4MyBatisMSXml = new CalleeMethodInfoFiller4MyBatisMSXml(configureWrapper);
             EntryMethodInfoFiller4Spring entryMethodInfoFiller4Spring = new EntryMethodInfoFiller4Spring(configureWrapper);
             EntryMethodInfoFiller4XmlCommand entryMethodInfoFiller4XmlCommand = new EntryMethodInfoFiller4XmlCommand(configureWrapper)) {
            List<AbstractCalleeMethodInfoFiller> calleeMethodInfoFillerList = new ArrayList<>();
            if (useCalleeFiler4MyBatisMSXml) {
                calleeMethodInfoFillerList.add(calleeMethodInfoFiller4MyBatisMSXml);
            }
            AbstractCalleeMethodInfoFiller[] calleeMethodInfoFillers = calleeMethodInfoFillerList.toArray(new AbstractCalleeMethodInfoFiller[]{});

            List<AbstractEntryMethodInfoFiller> entryMethodInfoFillerList = new ArrayList<>();
            if (useEntryFiler4Spring) {
                entryMethodInfoFillerList.add(entryMethodInfoFiller4Spring);
            }
            if (useEntryFiler4CommandXml) {
                entryMethodInfoFillerList.add(entryMethodInfoFiller4XmlCommand);
            }
            AbstractEntryMethodInfoFiller[] entryMethodInfoFillers = entryMethodInfoFillerList.toArray(new AbstractEntryMethodInfoFiller[]{});

            RunnerGenJarDiffCalleeGraph genJarDiffCalleeGraph = new RunnerGenJarDiffCalleeGraph(javaCG2ConfigureWrapper, configureWrapper, calleeMethodInfoFillers,
                    entryMethodInfoFillers, modifiedMethodFilters);
            JarDiffResult jarDiffResult = genJarDiffCalleeGraph.generate();
            Assert.assertTrue(jarDiffResult.isSuccess());
            printMapContent(jarDiffResult.getJarModifiedMethodInfoMap());
            Assert.assertEquals(isResultEmpty(), jarDiffResult.getJarModifiedMethodInfoMap().isEmpty());
        }
    }

    /**
     * 执行jar diff功能，生成发生变化的方法向下的调用链
     */
    protected void testJarDiffCallerGraph(ModifiedMethodFilterInterface... modifiedMethodFilters) {
        ConfigureWrapper configureWrapper = TestConfigGenerator.genConfigureWrapper();
        // 使用本地的配置参数
        JACGTestUtil.useLocalConfig(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, chooseAppName());

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_CALLER_GRAPH_DIR, chooseRootJarDir() + chooseDirNameOld(),
                chooseRootJarDir() + chooseDirNameNew());

        RunnerGenJarDiffCallerGraph genJarDiffCallerGraph = new RunnerGenJarDiffCallerGraph(javaCG2ConfigureWrapper, configureWrapper, modifiedMethodFilters);
        JarDiffResult jarDiffResult = genJarDiffCallerGraph.generate();
        Assert.assertTrue(jarDiffResult.isSuccess());
        printMapContent(jarDiffResult.getJarModifiedMethodInfoMap());
        Assert.assertEquals(isResultEmpty(), jarDiffResult.getJarModifiedMethodInfoMap().isEmpty());
    }

    protected abstract String chooseAppName();

    protected abstract String chooseRootJarDir();

    protected abstract String chooseDirNameOld();

    protected abstract String chooseDirNameNew();

    protected abstract boolean isResultEmpty();
}
