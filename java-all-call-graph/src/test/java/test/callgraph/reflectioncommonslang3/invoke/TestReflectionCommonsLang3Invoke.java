package test.callgraph.reflectioncommonslang3.invoke;

import org.apache.commons.lang3.reflect.MethodUtils;
import org.junit.Test;
import test.callgraph.reflectioncommonslang3.example.TestReflectionCommonsLang3ExampleSuper1;

import java.lang.reflect.InvocationTargetException;

/**
 * @author adrninistrator
 * @date 2025/11/9
 * @description:
 */
public class TestReflectionCommonsLang3Invoke {

    @Test
    public void testSuper1() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleSuper1 = new TestReflectionCommonsLang3ExampleSuper1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test1");
    }
}
