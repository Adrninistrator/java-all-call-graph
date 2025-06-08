package test.callgraph.spring.bean.define.complex;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2023/5/31
 * @description:
 */
@Service(TestComplexServiceImpl1.BEAN_NAME)
public class TestComplexServiceImpl1 extends TestComplexAbstractService1 implements TestComplexInterface {

    public static final String BEAN_NAME = "test.callgraph.spring.bean.define.complex.TestComplexServiceImpl1";

    @Override
    public void test2() {
        System.out.println("test2");
    }
}
