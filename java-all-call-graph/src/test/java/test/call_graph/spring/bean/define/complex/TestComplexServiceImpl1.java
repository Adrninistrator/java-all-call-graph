package test.call_graph.spring.bean.define.complex;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2023/5/31
 * @description:
 */
@Service
public class TestComplexServiceImpl1 extends TestComplexAbstractService1 implements TestComplexInterface{

    @Override
    public void test2() {
        System.out.println("test2");
    }
}
