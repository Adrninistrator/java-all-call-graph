package test.neo4j.runner;

import com.adrninistrator.jacg.neo4j.runner.Neo4jRunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.neo4j.base.TestNeo4jBase;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
public class Test0Neo4jRunnerWriteDb extends TestNeo4jBase {

    @Test
    public void test() {
        Assert.assertTrue(new Neo4jRunnerWriteDb(configureWrapper).run());
    }
}
