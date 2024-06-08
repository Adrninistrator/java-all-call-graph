package test.runbycode.handler.conf;

import com.adrninistrator.jacg.handler.conf.PropertiesConfHandler;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/9/18
 * @description:
 */
public class TestPropertiesConfHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        try (PropertiesConfHandler propertiesConfHandler = new PropertiesConfHandler(configureWrapper)) {
            int maxRecordId = propertiesConfHandler.queryMaxRecordId();
            printObjectContent(maxRecordId, "maxRecordId");
        }
    }
}
