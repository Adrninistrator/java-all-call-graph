package test.runbycode.handler.field;

import com.adrninistrator.jacg.handler.dto.field.CommonFieldInfoInClass;
import com.adrninistrator.jacg.handler.field.FieldInfoHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.field.TestField1;
import test.callgraph.field.cycle.TestUseFieldCycle1;
import test.callgraph.field.cycle.TestUseFieldGenericsCycle1;
import test.callgraph.field.extend.TestExtendsField1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/10/25
 * @description:
 */
public class TestQueryAllCommonField extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testFieldCycle() {
        try (FieldInfoHandler fieldInfoHandler = new FieldInfoHandler(configureWrapper)) {
            queryAllCommonFieldInfoInClass(fieldInfoHandler, TestUseFieldCycle1.class.getName(), true, true, true);
        }
    }

    @Test
    public void testFieldGenericsCycle() {
        try (FieldInfoHandler fieldInfoHandler = new FieldInfoHandler(configureWrapper)) {
            queryAllCommonFieldInfoInClass(fieldInfoHandler, TestUseFieldGenericsCycle1.class.getName(), true, true, true);
        }
    }

    @Test
    public void testMix() {
        try (FieldInfoHandler fieldInfoHandler = new FieldInfoHandler(configureWrapper)) {
            queryAllCommonFieldInfoInClass(fieldInfoHandler, TestField1.class.getName(), true, true, true);
        }
    }

    @Test
    public void testExtends() {
        try (FieldInfoHandler fieldInfoHandler = new FieldInfoHandler(configureWrapper)) {
            queryAllCommonFieldInfoInClass(fieldInfoHandler, TestExtendsField1.class.getName(), true, true, true);
        }
    }

    private void queryAllCommonFieldInfoInClass(FieldInfoHandler fieldInfoHandler, String className, boolean includeSuperClass, boolean includeCustomField,
                                                boolean includeCollectionFieldGenericsType) {
        Set<String> foundCustomClasNameSet = new HashSet<>();
        List<CommonFieldInfoInClass> commonFieldInfoInClassList = fieldInfoHandler.queryAllCommonFieldInfoInClass(className, includeSuperClass, includeCustomField,
                includeCollectionFieldGenericsType, foundCustomClasNameSet);
        Assert.assertFalse(JavaCG2Util.isCollectionEmpty(commonFieldInfoInClassList));
        printListContent(commonFieldInfoInClassList, "commonFieldInfoInClassList_" + className + "_" + includeSuperClass + "_"
                + includeCustomField + "_" + includeCollectionFieldGenericsType);
        printSetContent(foundCustomClasNameSet, "foundCustomClasNameSet");
    }
}
