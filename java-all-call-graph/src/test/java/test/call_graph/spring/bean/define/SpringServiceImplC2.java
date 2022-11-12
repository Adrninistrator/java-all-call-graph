package test.call_graph.spring.bean.define;

import org.springframework.stereotype.Repository;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Repository("test.call_graph.spring.bean.define.SpringServiceImplC2")
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
