package test.neo4j.base;

import com.adrninistrator.jacg.spring.context.SpringContextManager;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:applicationContext_jacg.xml"})
public abstract class TestNeo4jBase extends TestRunByCodeBase {

    @Autowired
    private SpringContextManager springContextManager;
}
