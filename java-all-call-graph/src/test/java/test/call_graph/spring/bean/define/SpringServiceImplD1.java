package test.call_graph.spring.bean.define;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2023/3/12
 * @description:
 */
@Service
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
