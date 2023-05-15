package test.call_graph.spring.bean.define.impl;

import org.springframework.stereotype.Component;
import test.call_graph.spring.bean.define.AbstractSpringServiceC;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Component("test.call_graph.spring.bean.define.impl.SpringServiceImplC1")
public class SpringServiceImplC1 extends AbstractSpringServiceC {
    @Override
    public void test1() {
        System.getProperty("");
    }

    @Override
    public String test2() {
        return null;
    }
}
