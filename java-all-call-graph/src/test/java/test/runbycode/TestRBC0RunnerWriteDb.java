package test.runbycode;

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
        desc = {"仅处理指定的包名"})
public class TestRBC0RunnerWriteDb extends TestRunByCodeBase {

    @Test
    public void test() {
        Assert.assertTrue(new RunnerWriteDb(configureWrapper).run());
    }
}
