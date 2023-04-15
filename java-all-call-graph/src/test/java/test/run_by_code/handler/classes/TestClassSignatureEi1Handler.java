package test.run_by_code.handler.classes;

import com.adrninistrator.jacg.handler.classes.ClassSignatureEi1Handler;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.manual_add_method_call.unfixed.AbstractUnFixedService1;
import test.call_graph.manual_add_method_call.unfixed.UnfixedService1a;
import test.call_graph.signature.TestClassWithSignature1;
import test.call_graph.signature.TestClassWithSignature2;
import test.call_graph.signature.TestInterfaceWithSignature;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/27
 * @description:
 */
public class TestClassSignatureEi1Handler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestClassSignatureEi1Handler.class);

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
        logger.info("{}\n{}", className, StringUtils.join(list, "\n"));
    }
}
