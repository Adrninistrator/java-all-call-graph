package com.adrninistrator.jacg.handler.businessdata;

import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CallerExtractedFile;
import com.adrninistrator.jacg.handler.dto.businessdata.BaseBusinessData;
import com.adrninistrator.jacg.handler.dto.businessdata.MethodBehaviorByBusinessData;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/9/10
 * @description: 根据业务功能数据获得调用方法与被调用方法的行为后，分别将JSON格式数据写入到文件的基类，生成文件的格式为：[调用方法的行为JSON字符串]\t[被调用方法的行为JSON字符串]
 */
public abstract class EREEMethodBehaviorWriter {
    private static final Logger logger = LoggerFactory.getLogger(EREEMethodBehaviorWriter.class);

    /**
     * 根据业务功能数据获得调用方法与被调用方法的行为后，写入到文件
     *
     * @param outputFilePath       输出文件路径，在该文件后面追加
     * @param callerMethodBehavior 调用方法通过业务功能数据获取到的行为
     * @param callerExtractedFile  调用方法向下的调用堆栈文件处理后的文件信息
     * @return
     */
    public boolean write(String outputFilePath, MethodBehaviorByBusinessData callerMethodBehavior, CallerExtractedFile callerExtractedFile) {
        if (StringUtils.isBlank(outputFilePath) || callerMethodBehavior == null || callerExtractedFile == null) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        logger.info("当前处理的方法调用堆栈文件为 {}", callerExtractedFile.getStackFilePath());

        List<CallerExtractedLine> callerExtractedLineList = callerExtractedFile.getCallerExtractedLineList();
        if (JavaCG2Util.isCollectionEmpty(callerExtractedLineList)) {
            logger.info("当前处理的方法调用堆栈文件未获取到业务功能数据 {}", callerExtractedFile.getStackFilePath());
            return true;
        }

        String callerMethodBehaviorJson = JACGJsonUtil.getJsonStr(callerMethodBehavior);
        try (BufferedWriter writer = JavaCG2FileUtil.genBufferedWriter(outputFilePath, true)) {
            for (CallerExtractedLine callerExtractedLine : callerExtractedLineList) {
                List<BaseBusinessData> businessDataList = callerExtractedLine.getCallGraphLineParsed().getBusinessDataList();
                for (BaseBusinessData businessData : businessDataList) {
                    // 根据被调用方法的业务功能数据获取对应的方法行为
                    List<MethodBehaviorByBusinessData> methodBehaviorByBusinessDataList = handleBusinessData(businessData);
                    if (JavaCG2Util.isCollectionEmpty(methodBehaviorByBusinessDataList)) {
                        continue;
                    }
                    for (MethodBehaviorByBusinessData calleeMethodBehavior : methodBehaviorByBusinessDataList) {
                        String calleeMethodBehaviorJson = JACGJsonUtil.getJsonStr(calleeMethodBehavior);
                        JavaCG2FileUtil.write2FileWithTab(writer, callerMethodBehaviorJson, calleeMethodBehaviorJson);
                    }
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 根据被调用方法的业务功能数据获取对应的方法行为
     *
     * @param businessData
     * @return
     */
    public abstract List<MethodBehaviorByBusinessData> handleBusinessData(BaseBusinessData businessData);
}
