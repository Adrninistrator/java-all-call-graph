package test.runbycode.handler.fieldrelationship;

import com.adrninistrator.jacg.handler.fieldrelationship.FieldRelationshipRawReportHandler;
import com.adrninistrator.jacg.handler.fieldrelationship.filler.MyBatisEntityFieldBehaviorFiller;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/9/15
 * @description:
 */
public class TestFieldRelationshipRawReportHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        writeDbSupportField(configureWrapper);
    }

    @Test
    public void testGen1() {
        try (FieldRelationshipRawReportHandler fieldRawRelationshipReportHandler = new FieldRelationshipRawReportHandler(configureWrapper);
             MyBatisEntityFieldBehaviorFiller myBatisEntityFieldBehaviorFiller = new MyBatisEntityFieldBehaviorFiller(configureWrapper)) {
            Assert.assertTrue(fieldRawRelationshipReportHandler.genRawFieldsReport("build/raw_field_relationship1.md", myBatisEntityFieldBehaviorFiller));
        }
    }

    @Test
    public void testWriteDbAndGen1() {
        writeDbSupportField(configureWrapper);

        try (FieldRelationshipRawReportHandler fieldRawRelationshipReportHandler = new FieldRelationshipRawReportHandler(configureWrapper);
             MyBatisEntityFieldBehaviorFiller myBatisEntityFieldBehaviorFiller = new MyBatisEntityFieldBehaviorFiller(configureWrapper)) {
            Assert.assertTrue(fieldRawRelationshipReportHandler.genRawFieldsReport("build/raw_field_relationship1.md", myBatisEntityFieldBehaviorFiller));
        }
    }

    @Test
    public void testWriteDbAndGen2() {
        try (FieldRelationshipRawReportHandler fieldRawRelationshipReportHandler = new FieldRelationshipRawReportHandler(configureWrapper);
             MyBatisEntityFieldBehaviorFiller myBatisEntityFieldBehaviorFiller = new MyBatisEntityFieldBehaviorFiller(configureWrapper)) {
            writeDbSupportField(configureWrapper);
            Assert.assertTrue(fieldRawRelationshipReportHandler.genRawFieldsReport("build/raw_field_relationship1.md", myBatisEntityFieldBehaviorFiller));
        }
    }
}
