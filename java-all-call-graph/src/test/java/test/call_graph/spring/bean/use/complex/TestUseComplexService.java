package test.call_graph.spring.bean.use.complex;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.complex.TestComplexAbstractService1;
import test.call_graph.spring.bean.define.complex.TestComplexInterface;
import test.call_graph.spring.bean.define.complex.TestComplexServiceImpl1;

/**
 * @author adrninistrator
 * @date 2023/5/31
 * @description:
 */
@Service
public class TestUseComplexService {

    @Autowired
    private TestComplexInterface testComplexInterfaceA;

    @Autowired
    private TestComplexAbstractService1 testComplexAbstractService1B;

    @Autowired
    private TestComplexServiceImpl1 testComplexServiceImpl1C;

    public void test() {
        System.out.println(testComplexInterfaceA.getClass().getName());
        testComplexInterfaceA.test1();
        testComplexInterfaceA.test2();

        System.out.println(testComplexAbstractService1B.getClass().getName());
        testComplexAbstractService1B.test1();

        System.out.println(testComplexServiceImpl1C.getClass().getName());
        testComplexServiceImpl1C.test1();
        testComplexServiceImpl1C.test2();
    }
}
