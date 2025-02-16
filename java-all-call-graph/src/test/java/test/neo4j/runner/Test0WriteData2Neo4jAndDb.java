package test.neo4j.runner;

import com.adrninistrator.jacg.neo4j.runner.Neo4jRunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.neo4j.base.TestNeo4jBase;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
@JACGExample(title = "解析测试代码并将结果写入Neo4j及数据库",
        desc = {})
public class Test0WriteData2Neo4jAndDb extends TestNeo4jBase {

    @Test
    public void test() {
        Neo4jRunnerWriteDb neo4jRunnerWriteDb = new Neo4jRunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        neo4jRunnerWriteDb.setWriteDbFlag(true);
        Assert.assertTrue(neo4jRunnerWriteDb.run());
    }
}
