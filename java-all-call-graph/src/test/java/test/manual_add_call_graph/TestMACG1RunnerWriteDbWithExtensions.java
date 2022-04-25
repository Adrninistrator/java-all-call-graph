package test.manual_add_call_graph;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import org.junit.Test;
import test.extensions.code_parser.MACGActionListenerParser;
import test.extensions.code_parser.MACGFixedService1Parser;
import test.extensions.code_parser.MACGUnfixedService1Parser;
import test.jacg.TestRunnerWriteDb;

import java.util.Arrays;
import java.util.HashSet;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
public class TestMACG1RunnerWriteDbWithExtensions extends TestMACGBase {
    @Test
    public void test() {
        // 指定插件
        ConfigureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_EXTENSIONS_CODE_PARSER, new HashSet(Arrays.asList(
                MACGActionListenerParser.class.getName(),
                MACGFixedService1Parser.class.getName(),
                MACGUnfixedService1Parser.class.getName()
        )));

        TestRunnerWriteDb.main(null);
    }
}
