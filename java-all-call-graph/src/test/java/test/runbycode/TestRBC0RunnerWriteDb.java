package test.runbycode;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBC0RunnerWriteDb extends TestRunByCodeBase {
    @Test
    public void test() {
        Assert.assertTrue(new RunnerWriteDb(configureWrapper).run());
    }

    @Test
    public void testAll() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setAllowAllClasses();
        Assert.assertTrue(new RunnerWriteDb(configureWrapperCopy).run());
    }
}
