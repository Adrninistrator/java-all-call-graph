package test.runbycode.example;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import com.adrninistrator.jacg.extractor.entry.CalleeGraphSPCFileUploadExtractor;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/11/19
 * @description:
 */
@JACGExample(title = "获取直接或间接调用指定方法的Spring Controller文件上传方法",
        desc = {"首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况", "再从这些完整方法调用链向上找到（可能的）Spring Controller文件上传方法"})
public class TestExtractCalleeGraphToSPCFileUpload extends TestRunByCodeBase {

    @Before
    public void init() {
        configureWrapper.setAllowAllClasses();
    }

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void extract() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName());
        CalleeGraphSPCFileUploadExtractor calleeGraphSPCFileUploadExtractor = new CalleeGraphSPCFileUploadExtractor();
        // 设置需要解析任务指定的被调用方法的直接调用方法所在行的内容
        calleeGraphSPCFileUploadExtractor.setParseDirectlyCallerLine(true);
        ListWithResult<CalleeExtractedFile> list = calleeGraphSPCFileUploadExtractor.extract(configureWrapper);
        Assert.assertTrue(list.isSuccess());
        printListContent(list.getList());
    }
}