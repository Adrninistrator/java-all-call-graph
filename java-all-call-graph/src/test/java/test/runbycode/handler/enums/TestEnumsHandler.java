package test.runbycode.handler.enums;

import com.adrninistrator.jacg.handler.enums.EnumsHandler;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.enums.DbStatementEnum;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/1/18
 * @description:
 */
public class TestEnumsHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        try (EnumsHandler enumsHandler = new EnumsHandler(configureWrapper)) {
            for (DbStatementEnum dbStatementEnum : DbStatementEnum.values()) {
                doTest(enumsHandler, DbStatementEnum.class.getName(), dbStatementEnum.name(), "statement", dbStatementEnum.getStatement());
                doTest(enumsHandler, DbStatementEnum.class.getName(), dbStatementEnum.name(), "desc", dbStatementEnum.getDesc());
            }
            doTest(enumsHandler, "a", "a", "a", null);
        }
    }

    private void doTest(EnumsHandler enumsHandler, String enumClassName, String enumConstName, String fieldName, String expectedValue) {
        String enumFieldValue = enumsHandler.queryEnumFieldValue(enumClassName, enumConstName, fieldName);
        printObjectContent(enumFieldValue, enumClassName, enumConstName, fieldName);
        if (StringUtils.isNotEmpty(expectedValue)) {
            Assert.assertEquals(expectedValue, enumFieldValue);
        } else {
            Assert.assertTrue(StringUtils.isEmpty(enumFieldValue));
        }

    }
}
