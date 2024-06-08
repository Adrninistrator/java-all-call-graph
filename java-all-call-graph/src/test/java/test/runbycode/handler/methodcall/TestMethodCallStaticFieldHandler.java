package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithStaticField;
import com.adrninistrator.jacg.handler.methodcall.MethodCallStaticFieldHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.enums.DbStatementEnum;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/2/3
 * @description:
 */
public class TestMethodCallStaticFieldHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        try (MethodCallStaticFieldHandler methodCallStaticFieldHandler = new MethodCallStaticFieldHandler(configureWrapper)) {
            List<MethodCallWithStaticField> methodCallWithStaticFieldList1 = methodCallStaticFieldHandler.queryMethodCallWithStaticFieldList(DbStatementEnum.class.getName());
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(methodCallWithStaticFieldList1));
            printListContent(methodCallWithStaticFieldList1, "all");

            List<MethodCallWithStaticField> methodCallWithStaticFieldList2 = methodCallStaticFieldHandler.queryMethodCallWithStaticFieldList(DbStatementEnum.class.getName(),
                    DbStatementEnum.DSE_INSERT.name(), DbStatementEnum.DSE_UPDATE.name());
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(methodCallWithStaticFieldList2));
            printListContent(methodCallWithStaticFieldList2, "some");
        }
    }
}
