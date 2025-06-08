package test.runbycode.parsedcustomdata;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ParsedCustomData;
import com.adrninistrator.jacg.handler.businessdata.ParsedCustomDataHandler;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.parsedcustomdata.parser.TestCodeParserWithCustomData1;
import test.runbycode.parsedcustomdata.parser.TestCodeParserWithCustomData1Child;
import test.runbycode.parsedcustomdata.parser.TestParser4XmlCommand;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/6/6
 * @description:
 */
public class TestUseCodeParserWithCustomData1 extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED, Boolean.FALSE.toString());
        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER, TestCodeParserWithCustomData1.class.getName());
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        try (ParsedCustomDataHandler parsedCustomDataHandler = new ParsedCustomDataHandler(configureWrapper)) {
            List<WriteDbData4ParsedCustomData> list = parsedCustomDataHandler.queryParsedCustomDataByType(TestParser4XmlCommand.DATA_TYPE);
            printListContent(list);
            Assert.assertFalse(JavaCG2Util.isCollectionEmpty(list));
        }
    }

    @Test
    public void testDupError() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED, Boolean.FALSE.toString());
        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER, TestCodeParserWithCustomData1.class.getName());
        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER, TestCodeParserWithCustomData1Child.class.getName());
        Assert.assertFalse(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
