package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.handler.dto.field.JACGFieldInfo;
import com.adrninistrator.jacg.handler.fieldrelationship.QueryGSFieldsHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.fieldrelationships.frd.FRDDtoA;
import test.callgraph.fieldrelationships.frf.FRFDtoC;
import test.callgraph.innerclass.TestInClass;
import test.callgraph.innerclass.TestOutClass;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description:
 */
public class TestQueryFieldsHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        writeDbSupportField();
    }

    @Test
    public void test1() {
        try (QueryGSFieldsHandler queryFieldsHandler = new QueryGSFieldsHandler(configureWrapper)) {
            List<JACGFieldInfo> list = queryFieldsHandler.queryAllFieldInfoList(true, TestOutClass.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, TestOutClass.class.getName());

            list = queryFieldsHandler.queryAllFieldInfoList(true, TestInClass.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, TestInClass.class.getName());

            list = queryFieldsHandler.queryAllFieldInfoList(true, FRFDtoC.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, FRFDtoC.class.getName());
        }
    }

    @Test
    public void test2() {
        try (QueryGSFieldsHandler queryFieldsHandler = new QueryGSFieldsHandler(configureWrapper)) {
            List<String> list = queryFieldsHandler.queryCustomFieldTypeList(true, TestOutClass.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, TestOutClass.class.getName());

            list = queryFieldsHandler.queryCustomFieldTypeList(true, TestInClass.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, TestInClass.class.getName());

            list = queryFieldsHandler.queryCustomFieldTypeList(true, FRFDtoC.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, FRFDtoC.class.getName());
        }
    }

    @Test
    public void test3() {
        try (QueryGSFieldsHandler queryFieldsHandler = new QueryGSFieldsHandler(configureWrapper)) {
            List<String> list = queryFieldsHandler.queryClassesByFieldType(true, TestOutClass.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, TestOutClass.class.getName());

            list = queryFieldsHandler.queryClassesByFieldType(true, TestInClass.TestInInnerData.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, TestInClass.TestInInnerData.class.getName());

            list = queryFieldsHandler.queryClassesByFieldType(true, TestInClass.TestInInnerData.TestInInnerData2.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, TestInClass.TestInInnerData.TestInInnerData2.class.getName());

            list = queryFieldsHandler.queryClassesByFieldType(true, FRDDtoA.class.getName());
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
            printListContent(list, FRDDtoA.class.getName());
        }
    }
}
