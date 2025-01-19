package test.runbycodemain;

import org.junit.Test;
import test.annotation.JACGExample;

/**
 * @author adrninistrator
 * @date 2025/1/19
 * @description:
 */
@JACGExample(title = "通过代码指定配置参数的主要功能示例",
        desc = {"处理所有包下的类"})
public class TestRBCAllPackages extends TestRBCBase {

    @Test
    public void $test0RunnerWriteDb() {
        super.$test0RunnerWriteDb();
    }

    @Test
    public void testRunnerSimpleWriteDb() {
        super.testRunnerSimpleWriteDb();
    }

    @Test
    public void testFindCallStackTrace4ee() {
        super.testFindCallStackTrace4ee();
    }

    @Test
    public void testFindCallStackTrace4er() {
        super.testFindCallStackTrace4er();
    }

    @Test
    public void testRunnerGenAllGraph4Callee() {
        super.testRunnerGenAllGraph4Callee();
    }

    @Test
    public void testRunnerGenAllGraph4CalleeLimit() {
        super.testRunnerGenAllGraph4CalleeLimit();
    }

    @Test
    public void testRunnerGenAllGraph4CalleeEmptyClass() {
        super.testRunnerGenAllGraph4CalleeEmptyClass();
    }

    @Test
    public void testRunnerGenAllGraph4Caller() {
        super.testRunnerGenAllGraph4Caller();
    }

    @Test
    public void testRunnerGenAllGraph4CallerLimit() {
        super.testRunnerGenAllGraph4CallerLimit();
    }

    @Test
    public void testRunnerGenAllGraph4CallerEmptyClass() {
        super.testRunnerGenAllGraph4CallerEmptyClass();
    }

    @Test
    public void testRunnerWriteCallGraphFile() {
        super.testRunnerWriteCallGraphFile();
    }
}
