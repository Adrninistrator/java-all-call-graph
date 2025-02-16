package test.runbycode.createtable;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/12/18
 * @description:
 */
@JACGExample(title = "仅创建数据库表",
        desc = {"不写入数据"})
public class TestCreateTable extends TestRunByCodeBase {

    @Test
    public void test() {
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        runnerWriteDb.createTables(false);
    }
}
