package test.runbycode.extractor.businessdata;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extractor.callback.CallerExtractedFileCallback;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.entry.CallerGraphBusinessDataExtractor;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.empty.TestEmptyClass1;
import test.callgraph.methodargument.TestArgumentGenerics1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/9/10
 * @description:
 */
public class TestCallerBusinessDataExtractor extends TestRunByCodeBase implements CallerExtractedFileCallback {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testBusinessDataExtractorMethodCallInfo() {
        doTestBusinessDataExtractorMethodCallInfo();
    }

    @Test
    public void testBusinessDataExtractorMethodCallInfoShortName() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_FILE_SHORT_MODE, Boolean.TRUE.toString());
        doTestBusinessDataExtractorMethodCallInfo();
    }

    private void doTestBusinessDataExtractorMethodCallInfo() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER, DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                "\tjava.lang.System:"
        );
        CallerGraphBusinessDataExtractor callerGraphBusinessDataExtractor = new CallerGraphBusinessDataExtractor(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName);
        Assert.assertTrue(callerGraphBusinessDataExtractor.extract(this, DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType()));
    }

    @Test
    public void testBusinessDataExtractorMethodCallInfoNotFound() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestEmptyClass1.class.getName(),
                "not_exists_class_name");
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER, DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                "nafdno32540324n0erinf0349j5324inogfwdngr329041h324n3214in3214oi21n4k321n4"
        );
        CallerGraphBusinessDataExtractor callerGraphBusinessDataExtractor = new CallerGraphBusinessDataExtractor(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName);
        Assert.assertTrue(callerGraphBusinessDataExtractor.extract(this, DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType()));
    }

    @Test
    public void testBusinessDataExtractorMethodArgGenericsType() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, TestArgumentGenerics1.class.getName());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(), DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType());
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                "\tjava.io.PrintStream:"
        );
        CallerGraphBusinessDataExtractor callerGraphBusinessDataExtractor = new CallerGraphBusinessDataExtractor(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName);
        Assert.assertTrue(callerGraphBusinessDataExtractor.extract(this, DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType()));
    }

    @Override
    public boolean handle(CallerExtractedFile callerExtractedFile) {
        printObjectContent(callerExtractedFile);
        return true;
    }
}
