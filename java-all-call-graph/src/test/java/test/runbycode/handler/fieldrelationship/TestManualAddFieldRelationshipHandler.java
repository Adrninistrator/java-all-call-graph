package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.handler.fieldrelationship.ManualAddFieldRelationshipHandler;
import com.adrninistrator.javacg.common.enums.JavaCGFieldRelationshipTypeEnum;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

import java.util.Collections;

/**
 * @author adrninistrator
 * @date 2023/7/25
 * @description:
 */
public class TestManualAddFieldRelationshipHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        configureWrapper.setAllowAllClasses();
        writeDbSupportField(configureWrapper);
    }

    @Test
    public void testManualAdd1() {
        try (ManualAddFieldRelationshipHandler manualAddFieldRelationshipHandler = new ManualAddFieldRelationshipHandler(configureWrapper)) {
            Assert.assertTrue(manualAddFieldRelationshipHandler.beforeAdd());
            manualAddFieldRelationshipHandler.manualAddFieldRelationship("a.b.C:f()", 123, 0, 0, "a.b.GetClass", "getB2", "a.b.SetClass", "setA1",
                    JavaCGFieldRelationshipTypeEnum.FRTE_BEAN_UTIL, 0);
            manualAddFieldRelationshipHandler.afterAdd();
        }
    }

    @Test
    public void testInsertDbAndAdd1() {
        configureWrapper.setAllowAllClasses();
        writeDbSupportField(configureWrapper);

        try (ManualAddFieldRelationshipHandler manualAddFieldRelationshipHandler = new ManualAddFieldRelationshipHandler(configureWrapper)) {
            Assert.assertTrue(manualAddFieldRelationshipHandler.beforeAdd());
            // org.springframework.beans.BeanUtils，参数1对应get方法，参数2对应set方法
            Assert.assertTrue(manualAddFieldRelationshipHandler.handleMethodCallByEEEntry(org.springframework.beans.BeanUtils.class.getName(), Collections.singletonList(
                    "copyProperties"), 2, 1));

            // org.apache.commons.beanutils.BeanUtils，参数1对应set方法，参数2对应get方法
            Assert.assertTrue(manualAddFieldRelationshipHandler.handleMethodCallByEEEntry(org.apache.commons.beanutils.BeanUtils.class.getName(), Collections.singletonList(
                    "copyProperties"), 1, 2));
            manualAddFieldRelationshipHandler.afterAdd();
        }
    }
}
