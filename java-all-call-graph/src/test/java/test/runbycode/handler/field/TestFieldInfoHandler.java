package test.runbycode.handler.field;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.innerclass.TestOutClass;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/1/12
 * @description:
 */
public class TestFieldInfoHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test1() {
        try (FieldInfoHandler fieldInfoHandler = new FieldInfoHandler(configureWrapper)) {
            List<WriteDbData4FieldInfo> fieldInfoList = fieldInfoHandler.queryClassFieldsByPackageExcludePSF(TestOutClass.class.getName(), "java.util.");
            printListContent(fieldInfoList, "not_exclude");
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(fieldInfoList));

            List<WriteDbData4FieldInfo> fieldInfoList2 = fieldInfoHandler.queryClassFieldsByPackageExcludePSF(TestOutClass.class.getName(), "java.util.", List.class.getName());
            printListContent(fieldInfoList2, "exclude");
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(fieldInfoList2));
        }
    }

    @Test
    public void test2() {
        try (FieldInfoHandler fieldInfoHandler = new FieldInfoHandler(configureWrapper)) {
            List<WriteDbData4FieldInfo> fieldInfoList = fieldInfoHandler.queryClassCustomTypeFields(TestOutClass.class.getName());
            printListContent(fieldInfoList, "custom_type");
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(fieldInfoList));
        }
    }
}
