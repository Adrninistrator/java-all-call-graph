package test.call_graph.spring.bean.define.impl;

import org.springframework.stereotype.Repository;
import test.call_graph.spring.bean.define.AbstractSpringServiceC;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Repository("ThisIsSpringServiceImplC2")
public class SpringServiceImplC2 extends AbstractSpringServiceC {
    @Override
    public void test1() {
        System.getProperty("");
    }

    @Override
    public String test2() {
        return null;
    }
}
