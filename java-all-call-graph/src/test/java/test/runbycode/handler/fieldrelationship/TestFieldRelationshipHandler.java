package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldRelationship;
import com.adrninistrator.jacg.handler.fieldrelationship.FieldRelationshipHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.fieldrelationships.fra.FRADtoA;
import test.callgraph.fieldrelationships.fra.FRADtoB;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/7/22
 * @description:
 */
public class TestFieldRelationshipHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        writeDbSupportField(configureWrapper);
    }

    @Test
    public void test2() {
        try (FieldRelationshipHandler fieldRelationshipHandler = new FieldRelationshipHandler(configureWrapper)) {
            List<WriteDbData4FieldRelationship> list = fieldRelationshipHandler.queryCallerMethodByRelationship(FRADtoB.class.getName(), "setStrField1", FRADtoA.class.getName(),
                    "getStr1");
            Assert.assertTrue(JavaCGUtil.isCollectionEmpty(list));
            printListContent(list);
        }
    }

    @Test
    public void test3() {
        try (FieldRelationshipHandler fieldRelationshipHandler = new FieldRelationshipHandler(configureWrapper)) {
            List<WriteDbData4FieldRelationship> list1 = fieldRelationshipHandler.queryRelatedSetMethod4Get(FRADtoA.class.getName(), "getStrA1");
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list1));
            printListContent(list1, "queryRelatedSetMethod4Get");

            List<WriteDbData4FieldRelationship> list2 = fieldRelationshipHandler.queryRelatedGetMethod4Set(FRADtoB.class.getName(), "setStrFieldB1");
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(list2));
            printListContent(list2, "queryRelatedGetMethod4Set");
        }
    }
}
