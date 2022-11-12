package test.call_graph.spring.bean.define;

import org.springframework.stereotype.Controller;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Controller("test.call_graph.spring.bean.define.SpringServiceImplB1")
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
