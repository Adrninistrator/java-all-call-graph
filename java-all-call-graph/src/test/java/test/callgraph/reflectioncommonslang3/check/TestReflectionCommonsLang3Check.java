package test.callgraph.reflectioncommonslang3.check;

import org.apache.commons.lang3.reflect.MethodUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.reflectioncommonslang3.example.TestReflectionCommonsLang3ExampleChild1;
import test.callgraph.reflectioncommonslang3.example.TestReflectionCommonsLang3ExampleSuper1;

import java.lang.reflect.InvocationTargetException;

/**
 * @author adrninistrator
 * @date 2025/11/9
 * @description:
 */
public class TestReflectionCommonsLang3Check {

    @Test
    public void testSuper1() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleSuper1 = new TestReflectionCommonsLang3ExampleSuper1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test1");
    }

    @Test
    public void testSuper1WithChild() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleChild1 = new TestReflectionCommonsLang3ExampleChild1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleChild1, "test1");
    }

    @Test
    public void testSuper2() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleSuper1 = new TestReflectionCommonsLang3ExampleSuper1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test2", "ok");
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test2", Integer.valueOf(1));
    }

    @Test
    public void testSuper2Null() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleSuper1 = new TestReflectionCommonsLang3ExampleSuper1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test2", new Object[]{null});

        NoSuchMethodException e1 = Assert.assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test2",
                new Object[]{}));
        e1.printStackTrace();
        NoSuchMethodException e2 = Assert.assertThrows(NoSuchMethodException.class, () -> MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test2", null));
        e2.printStackTrace();
    }

    @Test
    public void testSuper3Null() throws InvocationTargetException, NoSuchMethodException, IllegalAccessException {
        TestReflectionCommonsLang3ExampleSuper1 testReflectionCommonsLang3ExampleSuper1 = new TestReflectionCommonsLang3ExampleSuper1();
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test3", new Object[]{"ok", null});
        MethodUtils.invokeMethod(testReflectionCommonsLang3ExampleSuper1, "test3", "ok", null);
    }
}
