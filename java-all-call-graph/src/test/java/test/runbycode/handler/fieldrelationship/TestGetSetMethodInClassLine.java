package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.classes.ClassLineNumberHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.fieldrelationships.frb.FRBClass1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/12/8
 * @description:
 */
public class TestGetSetMethodInClassLine extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        writeDbSupportField();
    }

    @Test
    public void testQueryGetSetMethodInClassLine() {
        try (ClassLineNumberHandler classLineNumberHandler = new ClassLineNumberHandler(configureWrapper)) {
            doTestQueryGetSetMethodInClassLine(classLineNumberHandler, FRBClass1.class.getName(), 1, false);
            doTestQueryGetSetMethodInClassLine(classLineNumberHandler, FRBClass1.class.getName(), 14, true);
        }
    }

    private void doTestQueryGetSetMethodInClassLine(ClassLineNumberHandler classLineNumberHandler, String className, int lineNumber, boolean exists) {
        ListWithResult<BaseWriteDbData4GetSetMethod> getSetMethodList = classLineNumberHandler.queryGetSetMethodInClassLine(className, lineNumber);
        Assert.assertTrue(getSetMethodList.isSuccess());
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(getSetMethodList.getList()));
        printListContent(getSetMethodList.getList(), className, String.valueOf(lineNumber));
    }
}
