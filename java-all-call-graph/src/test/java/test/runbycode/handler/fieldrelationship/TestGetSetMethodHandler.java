package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.handler.fieldrelationship.GetSetMethodHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.fieldrelationships.fra.FRADtoA;
import test.callgraph.fieldrelationships.frc.FRCDtoC;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/21
 * @description:
 */
public class TestGetSetMethodHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        writeDbSupportField();
    }

    @Test
    public void test2() {
        try (GetSetMethodHandler getSetMethodHandler = new GetSetMethodHandler(configureWrapper)) {
            List<BaseWriteDbData4GetSetMethod> list = getSetMethodHandler.queryGetSetMethodByClassName(true, FRADtoA.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, "queryGetSetMethodByClassName-get");
        }
    }

    @Test
    public void test3() {
        try (GetSetMethodHandler getSetMethodHandler = new GetSetMethodHandler(configureWrapper)) {
            List<BaseWriteDbData4GetSetMethod> list = getSetMethodHandler.queryGetSetMethodByClassName(false, FRADtoA.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, "queryGetSetMethodByClassName-set");
        }
    }

    @Test
    public void test4() {
        try (GetSetMethodHandler getSetMethodHandler = new GetSetMethodHandler(configureWrapper)) {
            BaseWriteDbData4GetSetMethod getSetMethod1 = getSetMethodHandler.queryGetSetMethodByClassMethod(true, FRADtoA.class.getName(), "getLongA1");
            Assert.assertNotNull(getSetMethod1);
            printObjectContent(getSetMethod1, "queryGetSetMethodByMethodName-get");

            BaseWriteDbData4GetSetMethod getSetMethod2 = getSetMethodHandler.queryGetSetMethodByFieldName(true, FRADtoA.class.getName(), "strA1");
            Assert.assertNotNull(getSetMethod2);
            printObjectContent(getSetMethod2, "queryGetSetMethodByFieldName-get");
        }
    }

    @Test
    public void test5() {
        try (GetSetMethodHandler getSetMethodHandler = new GetSetMethodHandler(configureWrapper)) {
            BaseWriteDbData4GetSetMethod getSetMethod1 = getSetMethodHandler.queryGetSetMethodByClassMethod(false, FRADtoA.class.getName(), "setStrA1");
            Assert.assertNotNull(getSetMethod1);
            printObjectContent(getSetMethod1, "queryGetSetMethodByMethodName-set");

            BaseWriteDbData4GetSetMethod getSetMethod2 = getSetMethodHandler.queryGetSetMethodByFieldName(false, FRADtoA.class.getName(), "intA1");
            Assert.assertNotNull(getSetMethod2);
            printObjectContent(getSetMethod2, "queryGetSetMethodByFieldName-set");
        }
    }

    @Test
    public void test6() {
        try (GetSetMethodHandler getSetMethodHandler = new GetSetMethodHandler(configureWrapper)) {
            BaseWriteDbData4GetSetMethod getSetMethod1 = getSetMethodHandler.queryGetSetMethodByFieldNameSuper(true, FRCDtoC.class.getName(), "iField1");
            Assert.assertNotNull(getSetMethod1);
            printObjectContent(getSetMethod1, "querySuperGetSetMethodByFieldName-get");

            BaseWriteDbData4GetSetMethod getSetMethod2 = getSetMethodHandler.queryGetSetMethodByFieldNameSuper(false, FRCDtoC.class.getName(), "iField1");
            Assert.assertNotNull(getSetMethod2);
            printObjectContent(getSetMethod2, "querySuperGetSetMethodByFieldName-set");
        }
    }
}
