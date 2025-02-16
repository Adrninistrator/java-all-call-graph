package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.handler.methodcall.reporter.MethodCallStaticFieldReporter;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.enums.DbStatementEnum;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description:
 */
public class TestMethodCallStaticFieldReporter extends TestRunByCodeBase {

    @Test
    public void test1() {
        MethodCallStaticFieldReporter methodCallStaticFieldReporter = new MethodCallStaticFieldReporter(javaCG2ConfigureWrapper, configureWrapper, "build/all", false, false);
        Assert.assertTrue(methodCallStaticFieldReporter.generate(DbStatementEnum.class.getName()));
    }

    @Test
    public void test2() {
        MethodCallStaticFieldReporter methodCallStaticFieldReporter = new MethodCallStaticFieldReporter(javaCG2ConfigureWrapper, configureWrapper, "build/some1", true, false);
        Assert.assertTrue(methodCallStaticFieldReporter.generate(DbStatementEnum.class.getName(), DbStatementEnum.DSE_INSERT.name(), DbStatementEnum.DSE_UPDATE.name()));
    }

    @Test
    public void test3() {
        MethodCallStaticFieldReporter methodCallStaticFieldReporter1 = new MethodCallStaticFieldReporter(javaCG2ConfigureWrapper, configureWrapper, "build/some2", false, false);
        Assert.assertTrue(methodCallStaticFieldReporter1.generate(DbStatementEnum.class.getName(), DbStatementEnum.DSE_INSERT.name()));

        MethodCallStaticFieldReporter methodCallStaticFieldReporter2 = new MethodCallStaticFieldReporter(javaCG2ConfigureWrapper, configureWrapper, "build/some2", true, true);
        Assert.assertTrue(methodCallStaticFieldReporter2.generate(DbStatementEnum.class.getName(), DbStatementEnum.DSE_UPDATE.name()));
    }
}
