package test.run_by_code.extractor;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.entry.CallerGraphBaseExtractor;
import org.junit.Test;
import test.call_graph.annotation.CallMethodWithAnnotation;
import test.call_graph.annotation.MethodWithAnnotation;
import test.call_graph.enums.DbStatementEnum;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/22
 * @description: 判断指定的两个方法之间是否存在调用关系（直接或间接的），从调用方法开始向下查找
 */
public class TestMethodCallRelationshipByER extends TestRunByCodeBase {

    // 存在调用关系
    @Test
    public void testExists() {
        // 指定需要生成向下方法完整调用链的文件
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                CallMethodWithAnnotation.class.getName() + ":test1()");

        // 指定生成方法调用堆栈时的方法关键字
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                MethodWithAnnotation.class.getName() + ":test1()");

        CallerGraphBaseExtractor callerGraphBaseExtractor = new CallerGraphBaseExtractor();
        List<CallerExtractedFile> list = callerGraphBaseExtractor.baseExtract(configureWrapper);
        printListContent(list);
    }

    // 不存在调用关系
    @Test
    public void testNotExists() {
        // 指定需要生成向下方法完整调用链的文件
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                DbStatementEnum.class.getName() + ":getFromStatement()");

        // 指定生成方法调用堆栈时的方法关键字
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                MethodWithAnnotation.class.getName() + ":test1()");

        CallerGraphBaseExtractor callerGraphBaseExtractor = new CallerGraphBaseExtractor();
        List<CallerExtractedFile> list = callerGraphBaseExtractor.baseExtract(configureWrapper);
        printListContent(list);
    }
}
