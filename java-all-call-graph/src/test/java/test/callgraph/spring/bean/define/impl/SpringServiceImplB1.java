package test.callgraph.spring.bean.define.impl;

import org.springframework.stereotype.Controller;
import test.callgraph.spring.bean.define.SpringInterfaceB;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Controller(SpringServiceImplB1.BEAN_NAME)
public class SpringServiceImplB1 implements SpringInterfaceB {

    public static final String BEAN_NAME = "test.callgraph.spring.bean.define.impl.SpringServiceImplB1";

    @Override
    public void test1() {
        System.getProperty("1");
    }

    @Override
    public String test2() {
        return null;
    }
}
