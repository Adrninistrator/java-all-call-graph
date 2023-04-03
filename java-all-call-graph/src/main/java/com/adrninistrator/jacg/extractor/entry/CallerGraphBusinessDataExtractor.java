package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_line.CallGraphLineParsed;
import com.adrninistrator.jacg.extensions.dto.business_data.BaseBusinessData;
import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CallerExtractedFile;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/16
 * @description: 对调用链结果文件进行数据提取，向下的方法调用链，获取方法调用业务功能数据
 */
public class CallerGraphBusinessDataExtractor extends CallerGraphBaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(CallerGraphBusinessDataExtractor.class);

    // 需要处理的业务功能数据类型
    private String[] handleBusinessDataTypes;

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取方法调用业务功能数据并返回，使用配置文件中的参数
     *
     * @param businessDataTypes 需要处理的方法调用业务功能数据类型
     * @return
     */
    public List<CallerExtractedFile> extract(String... businessDataTypes) {
        return extract(new ConfigureWrapper(false), businessDataTypes);
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取方法调用业务功能数据并返回，使用代码指定的参数
     *
     * @param configureWrapper
     * @param businessDataTypes 需要处理的方法调用业务功能数据类型
     * @return
     */
    public List<CallerExtractedFile> extract(ConfigureWrapper configureWrapper, String... businessDataTypes) {
        if (ArrayUtils.isEmpty(businessDataTypes)) {
            throw new JavaCGRuntimeException("未指定需要处理的方法调用业务功能数据类型");
        }

        handleBusinessDataTypes = businessDataTypes;
        try {
            // 生成向下的方法完整调用链文件，并根据关键字生成调用堆栈文件
            List<String> stackFilePathList = genStackFiles(configureWrapper);
            if (stackFilePathList == null) {
                return null;
            }

            List<CallerExtractedFile> callerExtractedFileList = new ArrayList<>(stackFilePathList.size());
            for (String stackFilePath : stackFilePathList) {
                // 处理文件中的方法调用业务功能数据
                CallerExtractedFile callerExtractedFile = handleStackFile(stackFilePath);
                if (callerExtractedFile == null) {
                    return null;
                }
                if (!JavaCGUtil.isCollectionEmpty(callerExtractedFile.getCallerExtractedLineList())) {
                    // 文件中存在指定类型的方法调用业务功能数据时才添加
                    callerExtractedFileList.add(callerExtractedFile);
                }
            }
            logger.info("处理完毕");
            return callerExtractedFileList;
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    @Override
    protected void handleCallStackData(int dataSeq, List<String> lineList, List<Integer> lineNumberList, boolean runInOtherThread, boolean runInTransaction) {
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
                if (StringUtils.equalsAny(businessData.getDataType(), handleBusinessDataTypes)) {
                    // 当前行包含的业务功能数据需要处理
                    existsBusinessData = true;
                    break;
                }
            }

            if (!existsBusinessData) {
                // 当前行包含的业务功能数据不需要处理
                continue;
            }

            String lastLine = i > 0 ? lineList.get(i - 1) : null;
            // 当前行包含方法调用业务功能数据，且类型需要处理，进行处理
            // 生成向下的调用堆栈文件处理后对应行的信息
            CallerExtractedLine callerExtractedLine = genCallerExtractedLine(line, lastLine, dataSeq, lineNumberList.get(i), callGraphLineParsed, false, false);
            callerExtractedLineList.add(callerExtractedLine);
        }
    }
}