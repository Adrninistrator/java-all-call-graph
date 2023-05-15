package test.call_graph.spring.bean.define.impl;

import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.SpringInterfaceA;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service("test.call_graph.spring.bean.define.impl.SpringServiceImplA1")
public class SpringServiceImplA1 implements SpringInterfaceA {
    @Override
    public void test1() {
        System.getProperty(this.getClass().getName());
    }

    @Override
    public String test2() {
        return null;
    }
}
