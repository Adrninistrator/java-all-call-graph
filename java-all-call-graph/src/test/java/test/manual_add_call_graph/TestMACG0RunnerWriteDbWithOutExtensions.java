package test.manual_add_call_graph;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Test;

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
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_EXTENSIONS_CODE_PARSER, new HashSet<>());

        new RunnerWriteDb().run(configureWrapper);
    }
}
