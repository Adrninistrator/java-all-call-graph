package test.runbycode.extractor.callee;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.extractor.spcfiledownload.MyCalleeGraphSPCFileDownloadExtractor;

/**
 * @author adrninistrator
 * @date 2024/11/22
 * @description:
 */
@JACGExample(title = "获取直接或间接调用指定方法的Spring Controller文件下载方法，支持自定义筛选",
        desc = {"首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况",
                "再从这些完整方法调用链向上找到（可能的）Spring Controller文件下载方法",
                "对于需要查找的（可能的）Spring Controller文件下载方法，支持通过自定义处理进行筛选等操作"})
public class TestMyCalleeGraphSPCFileDownloadExtractor extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void extract() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName());
        MyCalleeGraphSPCFileDownloadExtractor myCalleeGraphSPCFileDownloadExtractor = new MyCalleeGraphSPCFileDownloadExtractor(configureWrapper);
        // 设置需要解析任务指定的被调用方法的直接调用方法所在行的内容
        myCalleeGraphSPCFileDownloadExtractor.setParseDirectlyCallerLine(true);
        ListWithResult<CalleeExtractedFile> list = myCalleeGraphSPCFileDownloadExtractor.extract();
        Assert.assertTrue(list.isSuccess());
        printListContent(list.getList());
    }
}
