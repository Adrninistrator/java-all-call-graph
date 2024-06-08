package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.handler.dto.field.JACGFieldMethodInfo;
import com.adrninistrator.jacg.handler.fieldrelationship.FieldRelationshipHandler;
import com.adrninistrator.jacg.util.JACGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.fieldrelationships.fra.FRADtoA;
import test.callgraph.fieldrelationships.frc.FRCDtoC;
import test.runbycode.base.TestRunByCodeBase;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2024/3/20
 * @description:
 */
public class TestQueryFieldMethodInfo extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        configureWrapper.setAllowAllClasses();
        writeDbSupportField(configureWrapper);
    }

    @Test
    public void testQuery1() {
        try (FieldRelationshipHandler fieldRelationshipHandler = new FieldRelationshipHandler(configureWrapper)) {
            Map<String, JACGFieldMethodInfo> map1 = fieldRelationshipHandler.queryAllFieldMethodInfo(FRADtoA.class.getName(), true, false);
            Assert.assertFalse(JACGUtil.isMapEmpty(map1));
            printMapContent(map1, "get");

            Map<String, JACGFieldMethodInfo> map2 = fieldRelationshipHandler.queryAllFieldMethodInfo(FRADtoA.class.getName(), false, false);
            Assert.assertFalse(JACGUtil.isMapEmpty(map2));
            printMapContent(map2, "set");
        }
    }

    @Test
    public void testQuery2() {
        try (FieldRelationshipHandler fieldRelationshipHandler = new FieldRelationshipHandler(configureWrapper)) {
            Map<String, JACGFieldMethodInfo> map1 = fieldRelationshipHandler.queryAllFieldMethodInfo(FRCDtoC.class.getName(), true, false);
            Assert.assertFalse(JACGUtil.isMapEmpty(map1));
            printMapContent(map1, "get");

            Map<String, JACGFieldMethodInfo> map2 = fieldRelationshipHandler.queryAllFieldMethodInfo(FRCDtoC.class.getName(), false, false);
            Assert.assertFalse(JACGUtil.isMapEmpty(map2));
            printMapContent(map2, "set");
        }
    }

    @Test
    public void testQuery3() {
        try (FieldRelationshipHandler fieldRelationshipHandler = new FieldRelationshipHandler(configureWrapper)) {
            Map<String, JACGFieldMethodInfo> map1 = fieldRelationshipHandler.queryAllFieldMethodInfo(FRCDtoC.class.getName(), true, true);
            Assert.assertFalse(JACGUtil.isMapEmpty(map1));
            printMapContent(map1, "get");

            Map<String, JACGFieldMethodInfo> map2 = fieldRelationshipHandler.queryAllFieldMethodInfo(FRCDtoC.class.getName(), false, true);
            Assert.assertFalse(JACGUtil.isMapEmpty(map2));
            printMapContent(map2, "set");
        }
    }
}
