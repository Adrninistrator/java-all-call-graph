package test.jacg;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;

/**
 * @author adrninistrator
 * @date 2021/6/23
 * @description: 读取jar包内容，生成方法调用关系，并写入数据库
 */

public class TestRunnerWriteDb {

    public static void main(String[] args) {
        Assert.assertTrue(new RunnerWriteDb().run());
    }
}
