package test.call_graph.spring.bean.use.javax_inject;

import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.javax_inject.JavaxInjectInterfaceA;

import javax.inject.Inject;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
@Service
public class TestJavaxInjectClassA {
    @Inject
    private JavaxInjectInterfaceA JavaxInjectClassA1;

    @Inject
    private JavaxInjectInterfaceA javaxInjectClassA2;

    public void test() {
        JavaxInjectClassA1.test1();
        javaxInjectClassA2.test1();
    }
}
