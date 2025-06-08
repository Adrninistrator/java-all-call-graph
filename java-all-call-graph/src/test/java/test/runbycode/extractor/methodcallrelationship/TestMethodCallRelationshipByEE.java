package test.runbycode.extractor.methodcallrelationship;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import com.adrninistrator.jacg.extractor.entry.CalleeGraphBaseExtractor;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.annotation.CallMethodWithAnnotation;
import test.callgraph.annotation.MethodWithAnnotation;
import test.callgraph.enums.DbStatementEnum;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/22
 * @description: 判断指定的两个方法之间是否存在调用关系（直接或间接的），从被调用方法开始向上查找
 */
public class TestMethodCallRelationshipByEE extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    // 存在调用关系
    @Test
    public void testExists() {
        // 指定需要生成向上方法完整调用链的文件
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                MethodWithAnnotation.class.getName() + ":test1()");

        // 指定生成方法调用堆栈时的方法关键字
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                CallMethodWithAnnotation.class.getName() + ":test1()");

        CalleeGraphBaseExtractor calleeGraphBaseExtractor = new CalleeGraphBaseExtractor(configureWrapper);
        ListWithResult<CalleeExtractedFile> list = calleeGraphBaseExtractor.baseExtract();
        Assert.assertTrue(list.isSuccess());
        printListContent(list.getList());
    }

    // 不存在调用关系
    @Test
    public void testNotFound() {
        // 指定需要生成向上方法完整调用链的文件
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                MethodWithAnnotation.class.getName() + ":test1()");

        // 指定生成方法调用堆栈时的方法关键字
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                DbStatementEnum.class.getName() + ":getFromStatement()");

        CalleeGraphBaseExtractor calleeGraphBaseExtractor = new CalleeGraphBaseExtractor(configureWrapper);
        ListWithResult<CalleeExtractedFile> list = calleeGraphBaseExtractor.baseExtract();
        Assert.assertTrue(list.isSuccess());
        printListContent(list.getList());
    }
}
