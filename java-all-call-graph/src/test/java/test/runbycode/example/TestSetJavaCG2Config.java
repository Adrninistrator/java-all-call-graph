package test.runbycode.example;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/11/18
 * @description:
 */
@JACGExample(title = "通过代码修改 java-callgraph2 组件使用的配置参数",
        desc = {"调用 RunnerWriteDb.run() 方法时指定 JavaCG2ConfigureWrapper 参数"})
public class TestSetJavaCG2Config extends TestRunByCodeBase {

    @Test
    public void test() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = configureWrapper.genJavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.FALSE.toString());

        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(configureWrapper);
        Assert.assertTrue(runnerWriteDb.run(javaCG2ConfigureWrapper));
    }
}
