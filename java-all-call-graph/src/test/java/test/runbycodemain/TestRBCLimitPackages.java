package test.runbycodemain;

import org.junit.Test;
import test.annotation.JACGExample;

/**
 * @author adrninistrator
 * @date 2025/1/19
 * @description:
 */
@JACGExample(title = "通过代码指定配置参数的主要功能示例",
        desc = {"仅处理指定包下的类"})
public class TestRBCLimitPackages extends TestRBCBase {

    @JACGExample(title = "解析代码并将结果写入数据库",
            desc = {})
    @Test
    public void $test0RunnerWriteDb() {
        super.$test0RunnerWriteDb();
    }

    @JACGExample(title = "解析代码并将结果写入数据库，简单模式",
            desc = {"处理方法调用时不解析被调用对象和参数可能的类型与值",
                    "仅解析.class文件，不解析.xml、.properties等其他类型的文件"})
    @Test
    public void testRunnerSimpleWriteDb() {
        super.testRunnerSimpleWriteDb();
    }

    @JACGExample(title = "解析代码并将结果写入文件，不写入数据库",
            desc = {})
    @Test
    public void testRunnerWriteFile() {
        super.testRunnerWriteFile();
    }

    @JACGExample(title = "获得方法向上到包含关键字的调用堆栈",
            desc = {"首先会生成指定方法向上的完整方法调用链"})
    @Test
    public void testFindCallStackTrace4ee() {
        super.testFindCallStackTrace4ee();
    }

    @JACGExample(title = "获得方法向下到包含关键字的调用堆栈",
            desc = {"首先会生成指定方法向下的完整方法调用链"})
    @Test
    public void testFindCallStackTrace4er() {
        super.testFindCallStackTrace4er();
    }

    @JACGExample(title = "生成指定方法向上的完整方法调用链",
            desc = {})
    @Test
    public void testRunnerGenAllGraph4Callee() {
        super.testRunnerGenAllGraph4Callee();
    }

    @JACGExample(title = "生成指定方法向上的完整方法调用链",
            desc = {"限制每个方法允许生成的方法调用数量限制"})
    @Test
    public void testRunnerGenAllGraph4CalleeLimit() {
        super.testRunnerGenAllGraph4CalleeLimit();
    }

    @JACGExample(title = "生成指定方法向上的完整方法调用链",
            desc = {"生成结果为空"})
    @Test
    public void testRunnerGenAllGraph4CalleeEmptyClass() {
        super.testRunnerGenAllGraph4CalleeEmptyClass();
    }

    @JACGExample(title = "生成指定方法向下的完整方法调用链",
            desc = {})
    @Test
    public void testRunnerGenAllGraph4Caller() {
        super.testRunnerGenAllGraph4Caller();
    }

    @JACGExample(title = "生成指定方法向下的完整方法调用链",
            desc = {"限制每个方法允许生成的方法调用数量限制"})
    @Test
    public void testRunnerGenAllGraph4CallerLimit() {
        super.testRunnerGenAllGraph4CallerLimit();
    }

    @JACGExample(title = "生成指定方法向下的完整方法调用链",
            desc = {"生成结果为空"})
    @Test
    public void testRunnerGenAllGraph4CallerEmptyClass() {
        super.testRunnerGenAllGraph4CallerEmptyClass();
    }

    @JACGExample(title = "解析代码并将结果写入文件",
            desc = {"生成的数据不写入数据库"})
    @Test
    public void testRunnerWriteCallGraphFile() {
        super.testRunnerWriteCallGraphFile();
    }
}
