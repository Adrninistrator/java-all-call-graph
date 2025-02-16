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
@JACGExample(title = "解析测试代码并将结果写入Neo4j数据库",
        desc = {"结果仅写入Neo4j，不写入数据库"})
public class Test0WriteData2Neo4j extends TestNeo4jBase {

    @Test
    public void test() {
        Assert.assertTrue(new Neo4jRunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
