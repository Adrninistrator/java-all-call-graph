package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;
import com.adrninistrator.jacg.extractor.callback.CallerExtractedFileCallback;
import com.adrninistrator.jacg.extractor.callback.StackFileParsedCallback;
import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CallerExtractedFile;
import com.adrninistrator.jacg.handler.dto.businessdata.BaseBusinessData;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGCallStackUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/16
 * @description: 对向下的方法完整调用链文件进行数据提取，获取方法调用业务功能数据
 */
public class CallerGraphBusinessDataExtractor extends CallerGraphBaseExtractor implements StackFileParsedCallback {

    private static final Logger logger = LoggerFactory.getLogger(CallerGraphBusinessDataExtractor.class);

    public CallerGraphBusinessDataExtractor(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取方法调用业务功能数据并返回，使用代码指定的参数
     *
     * @param callerExtractedFileCallback 对处理后的文件进行自定义处理的回调类
     * @param businessDataTypes           需要处理的方法调用业务功能数据类型
     * @return true: 处理成功 false: 处理失败
     */
    public boolean extract(CallerExtractedFileCallback callerExtractedFileCallback, String... businessDataTypes) {
        if (callerExtractedFileCallback == null) {
            logger.error("未指定 {} 实现类", CallerExtractedFileCallback.class);
            return false;
        }

        if (ArrayUtils.isEmpty(businessDataTypes)) {
            logger.error("未指定需要处理的方法调用业务功能数据类型");
            return false;
        }

        try {
            // 生成向下的方法完整调用链文件，并根据关键字生成调用堆栈文件
            ListWithResult<String> stackFilePathList = genStackFiles();
            if (!stackFilePathList.isSuccess()) {
                return false;
            }

            for (String stackFilePath : stackFilePathList.getList()) {
                // 处理调用堆栈文件中的方法信息
                CallerExtractedFile callerExtractedFile = handleStackFile(stackFilePath, businessDataTypes);
                if (callerExtractedFile == null) {
                    logger.info("处理调用堆栈文件失败 {}", stackFilePath);
                    return false;
                }
                if (JavaCG2Util.isCollectionEmpty(callerExtractedFile.getCallerExtractedLineList())) {
                    logger.info("从调用堆栈文件获取信息为空 {}", stackFilePath);
                    continue;
                }
                // 调用自定义类处理文件，文件中存在指定类型的方法调用业务功能数据时才处理
                if (!callerExtractedFileCallback.handle(callerExtractedFile)) {
                    logger.info("对调用堆栈文件进行自定义处理失败 {}", stackFilePath);
                    return false;
                }
            }
            logger.info("处理完毕");
            return true;
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    // 处理调用堆栈文件中的方法信息
    // 生成向下的调用堆栈文件处理后对应行的信息
    private CallerExtractedFile handleStackFile(String stackFilePath, String[] businessDataTypes) {
        // 保存当前处理的调用堆栈文件行
        List<CallerExtractedLine> callerExtractedLineList = new ArrayList<>();

        // 解析调用堆栈文件
        if (!JACGCallStackUtil.parseStackFile(this, stackFilePath, callerExtractedLineList, businessDataTypes)) {
            return null;
        }

        CallerExtractedFile callerExtractedFile = new CallerExtractedFile(callerExtractedLineList);
        // 处理调用堆栈文件信息
        fillExtractedFileInfo4Caller(stackFilePath, callerExtractedFile);
        return callerExtractedFile;
    }

    @Override
    public void handleCallStackData(int dataSeq, List<String> lineList, List<Integer> lineNumberList, boolean runInOtherThread, boolean runInTransaction, Object... args) {
        List<CallerExtractedLine> callerExtractedLineList = JACGUtil.getArgAt(0, args);
        String[] businessDataTypes = JACGUtil.getArgAt(1, args);
        for (int i = 0; i < lineList.size(); i++) {
            String line = lineList.get(i);
            // 当前行是调用链对应的行，解析当前行包含的内容
            CallGraphLineParsed callGraphLineParsed = JACGCallGraphFileUtil.parseCallGraphLine4er(line);
            List<BaseBusinessData> businessDataList = callGraphLineParsed.getBusinessDataList();
            if (businessDataList == null) {
                // 当前行不包含业务功能数据
                continue;
            }

            boolean existsBusinessData = false;
            for (BaseBusinessData businessData : businessDataList) {
                if (StringUtils.equalsAny(businessData.getDataType(), businessDataTypes)) {
                    // 当前行包含的业务功能数据需要处理
                    existsBusinessData = true;
                    break;
                }
            }

            if (!existsBusinessData) {
                // 当前行包含的业务功能数据不需要处理
                continue;
            }

            // 当前行包含方法调用业务功能数据，且类型需要处理，进行处理
            String lastLine = i > 0 ? lineList.get(i - 1) : null;
            // 生成向下的调用堆栈文件处理后对应行的信息
            CallerExtractedLine callerExtractedLine = genCallerExtractedLine(line, lastLine, dataSeq, lineNumberList.get(i), callGraphLineParsed, false, false);
            callerExtractedLineList.add(callerExtractedLine);
            // 当前的调用堆栈处理了需要类型的业务功能数据后，不再处理后续行的数据
            break;
        }
    }
}