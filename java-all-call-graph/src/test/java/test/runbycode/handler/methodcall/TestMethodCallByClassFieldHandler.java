package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallStaticFieldMCR;
import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4MethodCallClassField;
import com.adrninistrator.jacg.handler.dto.methodcall.MethodCallWithInfo;
import com.adrninistrator.jacg.handler.methodcall.FindMethodCallByCallInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallClassFieldHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.enums.DbStatementEnum;
import test.callgraph.field.TestField1;
import test.runbycode.base.TestRunByCodeBase;

import java.io.PrintStream;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/5/31
 * @description:
 */
public class TestMethodCallByClassFieldHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testStaticField() {
        try (FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestClassField(findMethodCallByCallInfoHandler, true, true, DbStatementEnum.class.getName(), null);
            doTestClassField(findMethodCallByCallInfoHandler, true, true, DbStatementEnum.class.getName(), DbStatementEnum.DSE_INSERT.name());
            doTestClassField(findMethodCallByCallInfoHandler, true, true, DbStatementEnum.class.getName(), DbStatementEnum.DSE_UPDATE.name());
            doTestClassField(findMethodCallByCallInfoHandler, true, true, System.class.getName(), "err");
        }
    }

    @Test
    public void testNonStaticField1() {
        try (FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestClassField(findMethodCallByCallInfoHandler, true, false, TestField1.class.getName(), null);
        }
    }

    @Test
    public void testStaticField4Method1() {
        try (FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestClassField4Method1(findMethodCallByCallInfoHandler, true, true, DbStatementEnum.class.getName(), null, PrintStream.class.getName(), "println");
        }
    }

    @Test
    public void testStaticField4Method2() {
        try (FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestClassField4Method2(findMethodCallByCallInfoHandler, true, true, DbStatementEnum.class.getName(), null, "java.io.PrintStream:println(java.lang.Object)");
        }
    }

    private void doTestClassField(FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler, boolean exists, boolean queryStaticField, String className, String fieldName) {
        List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> methodCallWithInfoList = findMethodCallByCallInfoHandler.queryMethodCallByClassField(queryStaticField,
                className, fieldName);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        printListContent(methodCallWithInfoList, className + "@" + fieldName);
    }

    private void doTestClassField4Method1(FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler, boolean exists, boolean queryStaticField, String className,
                                          String fieldName, String calleeClassName, String calleeMethodName) {
        List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> methodCallWithInfoList = findMethodCallByCallInfoHandler.queryMethodCallByClassField4Method(queryStaticField,
                className, fieldName, calleeClassName, calleeMethodName);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        printListContent(methodCallWithInfoList, className + "@" + fieldName);
    }

    private void doTestClassField4Method2(FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler, boolean exists, boolean queryStaticField, String className,
                                          String fieldName, String calleeFullMethod) {
        List<MethodCallWithInfo<BaseWriteDbData4MethodCallClassField>> methodCallWithInfoList = findMethodCallByCallInfoHandler.queryMethodCallByClassField4Method(queryStaticField,
                className, fieldName, calleeFullMethod);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        printListContent(methodCallWithInfoList, className + "@" + fieldName);
    }

    @Test
    public void testQueryClassFieldNameList() {
        try (MethodCallClassFieldHandler methodCallStaticFieldHandler = new MethodCallClassFieldHandler(configureWrapper)) {
            List<String> classFieldNameList = methodCallStaticFieldHandler.queryClassFieldNameInMCList(true, DbStatementEnum.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(classFieldNameList));
            printListContent(classFieldNameList);
        }
    }

    @Test
    public void testStaticFieldMCR() {
        try (FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestClassFieldMCR(findMethodCallByCallInfoHandler, true, DbStatementEnum.class.getName(), null);
            doTestClassFieldMCR(findMethodCallByCallInfoHandler, true, DbStatementEnum.class.getName(), DbStatementEnum.DSE_INSERT.name());
            doTestClassFieldMCR(findMethodCallByCallInfoHandler, false, DbStatementEnum.class.getName(), DbStatementEnum.DSE_ILLEGAL.name());
        }
    }

    @Test
    public void testStaticFieldMCR4Field1() {
        try (FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestClassFieldMCR4Field1(findMethodCallByCallInfoHandler, true, DbStatementEnum.class.getName(), null, PrintStream.class.getName(), "println");
        }
    }

    @Test
    public void testStaticFieldMCR4Field2() {
        try (FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler = new FindMethodCallByCallInfoHandler(configureWrapper)) {
            doTestClassFieldMCR4Field2(findMethodCallByCallInfoHandler, true, DbStatementEnum.class.getName(), null, "java.io.PrintStream:println(java.lang.String)");
        }
    }

    private void doTestClassFieldMCR(FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler, boolean exists, String className, String fieldName) {
        List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> methodCallWithInfoList = findMethodCallByCallInfoHandler.queryMethodCallByStaticFieldMCR(className,
                fieldName);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        printListContent(methodCallWithInfoList, className + "@" + fieldName);
    }

    private void doTestClassFieldMCR4Field1(FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler, boolean exists, String className,
                                            String fieldName, String calleeClassName, String calleeMethodName) {
        List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> methodCallWithInfoList = findMethodCallByCallInfoHandler.queryMethodCallByStaticFieldMCR4Method(className,
                fieldName, calleeClassName, calleeMethodName);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        printListContent(methodCallWithInfoList, className + "@" + fieldName);
    }

    private void doTestClassFieldMCR4Field2(FindMethodCallByCallInfoHandler findMethodCallByCallInfoHandler, boolean exists, String className,
                                            String fieldName, String calleeFullMethod) {
        List<MethodCallWithInfo<WriteDbData4MethodCallStaticFieldMCR>> methodCallWithInfoList = findMethodCallByCallInfoHandler.queryMethodCallByStaticFieldMCR4Method(className,
                fieldName, calleeFullMethod);
        Assert.assertEquals(exists, !JavaCG2Util.isCollectionEmpty(methodCallWithInfoList));
        printListContent(methodCallWithInfoList, className + "@" + fieldName);
    }
}
