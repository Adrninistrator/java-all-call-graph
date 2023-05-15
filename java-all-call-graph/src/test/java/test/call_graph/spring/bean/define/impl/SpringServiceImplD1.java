package test.call_graph.spring.bean.define.impl;

import org.springframework.stereotype.Repository;
import test.call_graph.spring.bean.define.AbstractSpringServiceD;

/**
 * @author adrninistrator
 * @date 2023/3/12
 * @description:
 */
@Repository
public class SpringServiceImplD1 extends AbstractSpringServiceD {
    @Override
    public void test1() {
        System.getProperty("");
    }

    @Override
    public String test2() {
        return null;
    }
}
