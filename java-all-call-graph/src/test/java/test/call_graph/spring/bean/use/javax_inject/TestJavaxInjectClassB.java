package test.call_graph.spring.bean.use.javax_inject;

import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.javax_inject.JavaxInjectInterfaceB;

import javax.inject.Inject;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
@Service
public class TestJavaxInjectClassB {
    @Inject
    private JavaxInjectInterfaceB thisIsATestJavaxInjectClassB1;

    @Inject
    private JavaxInjectInterfaceB javaxInjectClassB2;

    public void test() {
        thisIsATestJavaxInjectClassB1.test1();
        javaxInjectClassB2.test1();
    }
}
