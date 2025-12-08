package test.callgraph.spring.tx;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author adrninistrator
 * @date 2025/11/23
 * @description:
 */
public class TestSpringTx {

    @Transactional
    public void test1() {
        System.getProperty("");
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void test2() {
        System.setOut(null);
    }
}
