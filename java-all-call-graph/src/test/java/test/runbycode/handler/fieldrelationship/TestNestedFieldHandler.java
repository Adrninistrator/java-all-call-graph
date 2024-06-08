package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.handler.dto.field.CustomFieldType;
import com.adrninistrator.jacg.handler.dto.field.NestedFieldTopClassInfo;
import com.adrninistrator.jacg.handler.fieldrelationship.NestedGSFieldHandler;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.fieldrelationships.frd.FRDDtoA;
import test.callgraph.fieldrelationships.frd.FRDDtoB;
import test.callgraph.fieldrelationships.fre.FREDtoA;
import test.callgraph.fieldrelationships.frf.FRFDtoA;
import test.callgraph.fieldrelationships.frf.FRFDtoB;
import test.callgraph.fieldrelationships.frf.FRFDtoC;
import test.callgraph.fieldrelationships.frf.FRFDtoD;
import test.callgraph.fieldrelationships.frf.FRFDtoE;
import test.callgraph.innerclass.TestInClass;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/10/4
 * @description:
 */
public class TestNestedFieldHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        writeDbSupportField(configureWrapper);
    }

    @Test
    public void test() {
        CustomFieldType customFieldType = new CustomFieldType();
        try (NestedGSFieldHandler nestedFieldHandler = new NestedGSFieldHandler(configureWrapper)) {
            doTest(nestedFieldHandler, FRDDtoA.class.getName(), customFieldType, false);
            doTest(nestedFieldHandler, FRDDtoB.class.getName(), customFieldType, true);
            doTest(nestedFieldHandler, TestInClass.TestInInnerData.class.getName(), customFieldType, false);
            doTest(nestedFieldHandler, TestInClass.TestInInnerData.TestInInnerData2.class.getName(), customFieldType, false);
            doTest(nestedFieldHandler, FREDtoA.class.getName(), customFieldType, true);
        }
    }

    @Test
    public void testCycle() {
        CustomFieldType customFieldType = new CustomFieldType();
        try (NestedGSFieldHandler nestedFieldHandler = new NestedGSFieldHandler(configureWrapper)) {
            doTest(nestedFieldHandler, FRFDtoA.class.getName(), customFieldType, false);
            doTest(nestedFieldHandler, FRFDtoB.class.getName(), customFieldType, false);
            doTest(nestedFieldHandler, FRFDtoC.class.getName(), customFieldType, false);
            doTest(nestedFieldHandler, FRFDtoD.class.getName(), customFieldType, false);
            doTest(nestedFieldHandler, FRFDtoE.class.getName(), customFieldType, false);
        }
    }

    private void doTest(NestedGSFieldHandler nestedFieldHandler, String className, CustomFieldType customFieldType, boolean exists) {
        NestedFieldTopClassInfo uniqueNestedFieldTopClassInfo = nestedFieldHandler.queryUniqueNestedFieldTopClassInfo(className, customFieldType);
        Assert.assertEquals(exists, uniqueNestedFieldTopClassInfo != null);
        printObjectContent(uniqueNestedFieldTopClassInfo, className);
    }
}
