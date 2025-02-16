package test.runbycode.example;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
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
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.FALSE.toString());

        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        Assert.assertTrue(runnerWriteDb.run());
    }
}
