package test.callgraph.spring.bean.define.impl;

import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.SpringInterfaceB;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class SpringServiceImplB2 implements SpringInterfaceB {

    public static final String BEAN_NAME = "springServiceImplB2";

    @Override
    public void test1() {
        System.setProperty(this.getClass().getName(), "");
    }

    @Override
    public String test2() {
        return null;
    }
}
