package test.run_by_code.extractor;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CalleeExtractedFile;
import com.adrninistrator.jacg.extractor.entry.CalleeGraphBaseExtractor;
import org.junit.Test;
import test.call_graph.annotation.CallMethodWithAnnotation;
import test.call_graph.annotation.MethodWithAnnotation;
import test.call_graph.enums.DbStatementEnum;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/22
 * @description: 判断指定的两个方法之间是否存在调用关系（直接或间接的），从被调用方法开始向上查找
 */
public class TestMethodCallRelationshipByEE extends TestRunByCodeBase {

    // 存在调用关系
    @Test
    public void testExists() {
        // 指定需要生成向上方法完整调用链的文件
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                MethodWithAnnotation.class.getName() + ":test1()");

        // 指定生成方法调用堆栈时的方法关键字
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                CallMethodWithAnnotation.class.getName() + ":test1()");

        CalleeGraphBaseExtractor calleeGraphEntryExtractor = new CalleeGraphBaseExtractor();
        List<CalleeExtractedFile> list = calleeGraphEntryExtractor.baseExtract(configureWrapper);
        printListContent(list);
    }

    // 不存在调用关系
    @Test
    public void testNotExists() {
        // 指定需要生成向上方法完整调用链的文件
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                MethodWithAnnotation.class.getName() + ":test1()");

        // 指定生成方法调用堆栈时的方法关键字
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                DbStatementEnum.class.getName() + ":getFromStatement()");

        CalleeGraphBaseExtractor calleeGraphEntryExtractor = new CalleeGraphBaseExtractor();
        List<CalleeExtractedFile> list = calleeGraphEntryExtractor.baseExtract(configureWrapper);
        printListContent(list);
    }
}
