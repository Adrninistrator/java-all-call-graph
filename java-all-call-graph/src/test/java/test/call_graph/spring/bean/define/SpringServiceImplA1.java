package test.call_graph.spring.bean.define;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service("test.call_graph.spring.bean.define.SpringServiceImplA1")
public class SpringServiceImplA1 implements SpringInterfaceA {
    @Override
    public void test1() {
        System.getProperty("");
    }

    @Override
    public String test2() {
        return null;
    }
}
