package test.runbycode.handler.multi;

import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.jarinfo.JarInfoHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/3/16
 * @description:
 */
public class TestMultiHandler extends TestRunByCodeBase {

    @Test
    public void test() {
        long startTime = System.currentTimeMillis();
        try (JarInfoHandler jarInfoHandler = new JarInfoHandler(configureWrapper);
             ClassInfoHandler classInfoHandler = new ClassInfoHandler(configureWrapper);
             MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper)) {
            long spendTime = System.currentTimeMillis() - startTime;
            printObjectContent(spendTime);
        }
    }
}
