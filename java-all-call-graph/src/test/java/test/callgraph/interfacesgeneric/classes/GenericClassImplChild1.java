package test.callgraph.interfacesgeneric.classes;

import org.springframework.stereotype.Service;
import test.callgraph.extendcomplex.ChildClassA1;
import test.callgraph.extendcomplex.ChildClassB2;
import test.callgraph.interfacesgeneric.interfaces.GenericInterfaceChild1;

/**
 * @author adrninistrator
 * @date 2023/7/9
 * @description:
 */
@Service("test.callgraph.interfaces_generic.classes.GenericClassImplChild1")
public class GenericClassImplChild1 implements GenericInterfaceChild1 {
    @Override
    public void test(ChildClassA1 t1, ChildClassB2 t2, String str, int i) {
        System.out.println(this.getClass().getName());
        System.getProperty("a");
    }
}
