package test.callgraph.spring.bean.use.javaxinject;

import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.javaxinject.JavaxInjectInterfaceC;

import javax.inject.Inject;
import javax.inject.Named;

/**
 * @author adrninistrator
 * @date 2023/4/27
 * @description:
 */
@Service
public class TestJavaxInjectClassC {
    @Named("javaxInjectClassC2")
    @Inject
    private JavaxInjectInterfaceC javaxInjectClassC1;

    @Named("javaxInjectClassC1")
    @Inject
    private JavaxInjectInterfaceC javaxInjectClassC2;

    public void test() {
        javaxInjectClassC1.test1();
        javaxInjectClassC2.test1();
    }
}
