package test.runbycode.example;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import com.adrninistrator.jacg.extractor.entry.CalleeGraphEntryExtractor;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.methodcall.TestMCCallee;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/8/26
 * @description:
 */
@JACGExample(title = "获取直接或间接调用指定方法的入口方法",
        desc = {"首先生成指定方法向上的完整方法链，即获取指定方法的被调用情况", "再从这些完整方法调用链中找到对应的入口方法（即向上没有被其他方法调用的方法）"})
public class TestExtractCalleeGraphToEntry extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void extract() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                TestMCCallee.class.getName() + ":testFindEntry(",
                TestMCCallee.class.getName() + ":notCalled(");
        CalleeGraphEntryExtractor calleeGraphEntryExtractor = new CalleeGraphEntryExtractor();
        // 设置需要解析任务指定的被调用方法的直接调用方法所在行的内容
        calleeGraphEntryExtractor.setParseDirectlyCallerLine(true);
        ListWithResult<CalleeExtractedFile> list = calleeGraphEntryExtractor.extract(configureWrapper);
        Assert.assertTrue(list.isSuccess());
        printListContent(list.getList());
    }
}
