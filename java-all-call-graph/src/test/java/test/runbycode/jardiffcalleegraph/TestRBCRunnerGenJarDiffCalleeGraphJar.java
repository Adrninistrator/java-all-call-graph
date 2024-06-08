package test.runbycode.jardiffcalleegraph;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import org.junit.Test;
import test.runbycode.util.JACGTestUtil;

/**
 * @author adrninistrator
 * @date 2024/4/20
 * @description:
 */
public class TestRBCRunnerGenJarDiffCalleeGraphJar extends TestAbstractRunnerGenJarDiffCalleeGraph {

    public static final String APP_NAME = "jar_diff_jar";
    public static final String JAR_GZ_DIR = "build/libs/";

    public static final String JAR_DIR_NAME_OLD = "jar-diff-version-1";
    public static final String JAR_DIR_NAME_NEW = "jar-diff-version-2";

    @Test
    public void testUseLocalConfig() {
        testFull(true);
    }

    @Test
    public void testNotLocalConfig() {
        testFull(false);
    }

    private void testFull(boolean useLocalConfig) {
        ConfigureWrapper configureWrapper = genConfigureWrapper(APP_NAME);
        if (useLocalConfig) {
            JACGTestUtil.useLocalConfig(configureWrapper);
        }

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIFF_DIR, JAR_GZ_DIR + JAR_DIR_NAME_OLD, JAR_GZ_DIR + JAR_DIR_NAME_NEW);

        testGenJarDiffCalleeGraph(configureWrapper, false);
    }
}
