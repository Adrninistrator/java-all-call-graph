package test.runbycode;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入数据库",
        desc = {"所有的包名都处理"})
public class TestRBC0RunnerAllWriteDb extends TestRunByCodeBase {

    @Test
    public void test() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setAllowAllClasses();
        Assert.assertTrue(new RunnerWriteDb(configureWrapperCopy).run());
    }
}
