package test.manual_add_call_graph;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import org.junit.Test;
import test.jacg.TestRunnerWriteDb;

import java.util.HashSet;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
public class TestMACG0RunnerWriteDbWithOutExtensions extends TestMACGBase {
    @Test
    public void test() {
        // 指定插件为空
        ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_EXTENSIONS_CODE_PARSER, new HashSet());

        TestRunnerWriteDb.main(null);
    }
}
