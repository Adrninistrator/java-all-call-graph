package test.junit.spring;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.callgraph.interfacesgeneric.TestInterfacesGeneric1;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2023/7/9
 * @description:
 */
@Service
public class TestSPRunInterfacesGeneric1 extends TestSpringBase {

    @Autowired
    private TestInterfacesGeneric1 testInterfacesGeneric1;

    @Test
    public void test1() {
        testInterfacesGeneric1.test1();
    }

    @Test
    public void test2() {
        testInterfacesGeneric1.test2();
    }

    @Test
    public void test3() {
        testInterfacesGeneric1.test3();
    }
}
