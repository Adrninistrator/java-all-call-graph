package test.call_graph.spring.bean.define;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class SpringServiceImplB2 implements SpringInterfaceB {
    @Override
    public void test1() {
        System.setProperty("", "");
    }

    @Override
    public String test2() {
        return null;
    }
}
