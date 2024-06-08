package test.callgraph.interfacesgeneric;

import org.springframework.stereotype.Service;
import test.callgraph.interfacesgeneric.classes.GenericClassImplChild1;
import test.callgraph.interfacesgeneric.classes.GenericClassImplSuper1;
import test.callgraph.interfacesgeneric.interfaces.GenericInterfaceChild1;
import test.callgraph.interfacesgeneric.interfaces.GenericInterfaceSuper1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2023/7/9
 * @description:
 */
@Service
public class TestInterfacesGeneric1 {

    @Resource(name = "test.callgraph.interfaces_generic.classes.GenericClassImplChild1")
    private GenericInterfaceSuper1 genericInterfaceSuper1Bean;

    @Resource(name = "test.callgraph.interfaces_generic.classes.GenericClassImplChild1")
    private GenericInterfaceChild1 genericInterfaceChild1Bean;

    private GenericInterfaceSuper1 genericInterfaceSuper1;

    private GenericInterfaceChild1 genericInterfaceChild1;

    public void test1() {
        GenericClassImplSuper1 genericClassImplSuper1 = new GenericClassImplSuper1();
        genericClassImplSuper1.test(null, null, null, 1);

        GenericClassImplChild1 genericClassImplChild1 = new GenericClassImplChild1();
        genericClassImplChild1.test(null, null, null, 1);
    }

    public void test2() {
        try {
            genericInterfaceSuper1Bean.test(null, null, null, 1);
        } catch (Exception e) {
            e.printStackTrace();
        }
        genericInterfaceChild1Bean.test(null, null, null, 1);
    }

    public void test3() {
        try {
            genericInterfaceSuper1.test(null, null, null, 1);
            genericInterfaceChild1.test(null, null, null, 1);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
