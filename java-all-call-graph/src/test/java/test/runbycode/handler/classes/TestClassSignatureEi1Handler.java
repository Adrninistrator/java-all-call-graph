package test.runbycode.handler.classes;

import com.adrninistrator.jacg.handler.classes.ClassSignatureEi1Handler;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.manualaddmethodcall.unfixed.AbstractUnFixedService1;
import test.callgraph.manualaddmethodcall.unfixed.UnfixedService1a;
import test.callgraph.signature.TestClassWithSignature1;
import test.callgraph.signature.TestClassWithSignature2;
import test.callgraph.signature.TestInterfaceWithSignature;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/27
 * @description:
 */
public class TestClassSignatureEi1Handler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testClassSignatureEi1() {
        try (ClassSignatureEi1Handler classSignatureEi1Handler4Query = new ClassSignatureEi1Handler(configureWrapper)) {
            doTestClassSignatureEi1(classSignatureEi1Handler4Query, UnfixedService1a.class.getName(), AbstractUnFixedService1.class.getName());
            doTestClassSignatureEi1(classSignatureEi1Handler4Query, TestClassWithSignature1.class.getName(), TestInterfaceWithSignature.class.getName());
            doTestClassSignatureEi1(classSignatureEi1Handler4Query, TestClassWithSignature2.class.getName(), TestInterfaceWithSignature.class.getName());
        }
    }

    private void doTestClassSignatureEi1(ClassSignatureEi1Handler classSignatureEi1Handler4Query, String className, String upperClassName) {
        List<String> list = classSignatureEi1Handler4Query.queryClassSignatureEi1InfoFull(className, upperClassName);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list));
        printListContent(list, className);
    }
}
