package test.callgraph.spring.bean.define.impl;

import org.springframework.stereotype.Controller;
import test.callgraph.spring.bean.define.SpringInterfaceB;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Controller("test.callgraph.spring.bean.define.impl.SpringServiceImplB1")
public class SpringServiceImplB1 implements SpringInterfaceB {
    @Override
    public void test1() {
        System.getProperty("");
    }

    @Override
    public String test2() {
        return null;
    }
}
