package test.runbycode.handler.classes;

import com.adrninistrator.jacg.handler.classes.ClassExtImplGenericsTypeHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.interfaces.AbstractMapper;
import test.callgraph.interfaces.interfaces.extend.BaseMapper;
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
public class TestClassExtImplGenericsTypeHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void testClassSignatureEi1() {
        try (ClassExtImplGenericsTypeHandler classExtImplGenericsTypeHandler = new ClassExtImplGenericsTypeHandler(configureWrapper)) {
            doTestClassSignatureEi1(classExtImplGenericsTypeHandler, UnfixedService1a.class.getName(), AbstractUnFixedService1.class.getName(), true);
            doTestClassSignatureEi1(classExtImplGenericsTypeHandler, TestClassWithSignature1.class.getName(), TestInterfaceWithSignature.class.getName(), true);
            doTestClassSignatureEi1(classExtImplGenericsTypeHandler, TestClassWithSignature2.class.getName(), TestInterfaceWithSignature.class.getName(), true);
            doTestClassSignatureEi1(classExtImplGenericsTypeHandler, AbstractMapper.class.getName(), BaseMapper.class.getName(), false);
        }
    }

    private void doTestClassSignatureEi1(ClassExtImplGenericsTypeHandler classSignatureEi1Handler4Query, String className, String upperClassName, boolean exists) {
        List<String> list = classSignatureEi1Handler4Query.queryClassExtImplGenericsTypeListByFull(className, upperClassName);
        printListContent(list, className, upperClassName);
        Assert.assertEquals(!exists, JavaCG2Util.isCollectionEmpty(list));
    }
}
