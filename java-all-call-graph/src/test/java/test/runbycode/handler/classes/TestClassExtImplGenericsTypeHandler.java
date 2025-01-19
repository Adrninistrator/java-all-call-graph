package test.runbycode.handler.classes;

import com.adrninistrator.jacg.handler.classes.ClassExtImplGenericsTypeHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.signature.TestAbstractClassWithSignatureA;
import test.callgraph.signature.TestClassWithSignature4Empty;
import test.callgraph.signature.TestClassWithSignatureA1;
import test.callgraph.signature.TestInterfaceWithSignature1;
import test.callgraph.signature.child.TestChildClassWithSignature1;
import test.callgraph.signature.child.TestChildClassWithSignature2;
import test.callgraph.signature.child.TestChildClassWithSignature3Empty;
import test.callgraph.signature.child.TestChildInterfaceWithSignature1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/27
 * @description:
 */
public class TestClassExtImplGenericsTypeHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void testGetExtImplGenericsType4ClassTo() {
        try (ClassExtImplGenericsTypeHandler classExtImplGenericsTypeHandler = new ClassExtImplGenericsTypeHandler(configureWrapper)) {
            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestClassWithSignatureA1.class.getName(), TestAbstractClassWithSignatureA.class.getName(), true);
            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestClassWithSignatureA1.class.getName(), TestInterfaceWithSignature1.class.getName(), true);

            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestChildClassWithSignature1.class.getName(), String.class.getName(), false);
            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestChildClassWithSignature1.class.getName(), TestChildInterfaceWithSignature1.class.getName(),
                    true);
            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestChildClassWithSignature1.class.getName(), TestInterfaceWithSignature1.class.getName(), true);

            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestChildClassWithSignature3Empty.class.getName(), TestInterfaceWithSignature1.class.getName(),
                    false);

            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestClassWithSignature4Empty.class.getName(), TestInterfaceWithSignature1.class.getName(), false);

            doTestGetExtImplGenericsType4ClassTo(classExtImplGenericsTypeHandler, TestChildClassWithSignature2.class.getName(), TestInterfaceWithSignature1.class.getName(), true);
        }
    }

    private void doTestGetExtImplGenericsType4ClassTo(ClassExtImplGenericsTypeHandler classSignatureEi1Handler4Query, String fromClassName, String toClassName, boolean exists) {
        List<String> list = classSignatureEi1Handler4Query.getExtImplGenericsType4ClassTo(fromClassName, toClassName);
        printListContent(list, fromClassName, toClassName);
        Assert.assertEquals(!exists, JavaCG2Util.isCollectionEmpty(list));
    }
}
