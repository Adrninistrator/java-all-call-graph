package test.other;

import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.junit.Test;

import java.io.IOException;

/**
 * @author adrninistrator
 * @date 2022/3/21
 * @description:
 */
public class TestBcel {

    @Test
    public void test() throws IOException {
        ClassParser cp = new ClassParser("out/test/classes/TestA1$TestB.class");
        JavaClass javaClass = cp.parse();
        for (Method method : javaClass.getMethods()) {
            System.out.println(method.getName());
        }
    }
}
