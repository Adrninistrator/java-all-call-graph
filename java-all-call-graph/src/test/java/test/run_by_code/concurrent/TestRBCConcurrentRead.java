package test.run_by_code.concurrent;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.find_stack.FindCallStackTrace;
import org.junit.Test;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/11/8
 * @description:
 */
public class TestRBCConcurrentRead extends TestRunByCodeBase {

    @Test
    public void test() {
        ConfigureWrapper configureWrapper1 = configureWrapper.copy();

        Thread thread1 = new Thread(() -> {
            new FindCallStackTrace().find(true, configureWrapper);
        });
        thread1.start();

        Thread thread2 = new Thread(() -> {
            new FindCallStackTrace().find(false, configureWrapper1);
        });
        thread2.start();

        try {
            thread1.join();
            thread2.join();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
