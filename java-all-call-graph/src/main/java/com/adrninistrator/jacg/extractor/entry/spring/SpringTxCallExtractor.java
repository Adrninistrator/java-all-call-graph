package com.adrninistrator.jacg.extractor.entry.spring;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.comparator.Comparator4AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dto.infowithhash.AbstractInfoWithMethodHash;
import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.springtx.extractcombined.SpTxCallCombined;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxCallByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxCallByTplFile;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 对调用链结果文件进行数据提取，查找Spring事务发起的指定操作（例如查找事务中发起RPC调用等耗时操作的情况）
 */
public class SpringTxCallExtractor extends AbstractSpringTxExtractor {

    public SpringTxCallExtractor(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 查找Spring事务嵌套的调用情况，使用代码指定的参数
     *
     * @return
     */
    public SpTxCallCombined extract() {
        // 指定公共配置参数
        setCommonConfig();

        // 创建数据库相关对象
        genDbObject(configureWrapper);

        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            String outputDirName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME);
            if (!outputDirName.isEmpty()) {
                // 有指定生成调用链文件的子目录名，以下会生成两次方法调用链文件，需要分别使用不同的输出子目录名，否则会输出到同一个目录中
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, outputDirName + JACGConstants.FLAG_AT + JACGConstants.SPRING_TX_TYPE_ANNOTATION);
            }
            // 处理事务注解
            ListWithResult<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList = handleTxAnnotation(annotationHandler);

            if (!outputDirName.isEmpty()) {
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, outputDirName + JACGConstants.FLAG_AT + JACGConstants.SPRING_TX_TYPE_TEMPLATE);
            }
            // 处理事务模板
            ListWithResult<SpTxCallByTplFile> spTxCallByTplFileList = handleTxTpl();
            return new SpTxCallCombined(spTxCallByAnnotationFileList, spTxCallByTplFileList);
        } finally {
            closeDs();
        }
    }

    /**
     * 处理事务注解
     *
     * @param annotationHandler
     * @return
     */
    private ListWithResult<SpTxCallByAnnotationFile> handleTxAnnotation(AnnotationHandler annotationHandler) {
        // 提取使用@Transactional注解的方法相关信息
        ListWithResult<CallerExtractedFile> callerExtractedFileList = extractTxAnnotation(annotationHandler);
        if (!callerExtractedFileList.isSuccess()) {
            return ListWithResult.genFail();
        }

        List<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList = new ArrayList<>(callerExtractedFileList.getList().size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList.getList()) {
            String txEntryFullMethod = callerExtractedFile.getFullMethod();
            // 查询事务注解对应的事务传播行为
            String txPropagation = queryTxAnnotationPropagation(annotationHandler, txEntryFullMethod, callerExtractedFile.getReturnType());
            SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation = new SpTxEntryMethodTxAnnotation(txEntryFullMethod, txPropagation);

            // 根据调用堆栈文件，生成Spring事务被调用信息列表
            List<BaseCalleeExtractedMethod> calleeExtractedMethodList = genBaseCalleeExtractedMethodList(callerExtractedFile.getCallerExtractedLineList());

            SpTxCallByAnnotationFile spTxCallByAnnotationFile = new SpTxCallByAnnotationFile(spTxEntryMethodTxAnnotation,
                    calleeExtractedMethodList);
            spTxCallByAnnotationFile.copy(callerExtractedFile);
            spTxCallByAnnotationFileList.add(spTxCallByAnnotationFile);
        }
        // 文件信息列表排序
        spTxCallByAnnotationFileList.sort(Comparator4AbstractCallGraphExtractedFile.getInstance());
        return new ListWithResult<>(spTxCallByAnnotationFileList);
    }

    /**
     * 处理事务模板
     *
     * @return
     */
    private ListWithResult<SpTxCallByTplFile> handleTxTpl() {
        List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList = new ArrayList<>();
        ListWithResult<CallerExtractedFile> callerExtractedFileList = extractTxTpl(spTxEntryMethodTxTplList);
        if (!callerExtractedFileList.isSuccess()) {
            return ListWithResult.genFail();
        }

        /*
            将事务模板对应的入口方法列表转换为Map形式
            key
                方法HASH+长度
            value
                SpTxEntryMethodTxTpl
         */
        Map<String, SpTxEntryMethodTxTpl> spTxEntryMethodTxTplMap = AbstractInfoWithMethodHash.buildMap(spTxEntryMethodTxTplList);

        // 处理根据事务模板找到的事务嵌套，找到对应的方法入口
        List<SpTxCallByTplFile> spTxCallByTplFileList = new ArrayList<>(callerExtractedFileList.getList().size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList.getList()) {
            // 根据调用堆栈文件，生成Spring事务被调用信息列表
            List<BaseCalleeExtractedMethod> calleeExtractedMethodList = genBaseCalleeExtractedMethodList(callerExtractedFile.getCallerExtractedLineList());
            SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = spTxEntryMethodTxTplMap.get(callerExtractedFile.getMethodHash());
            SpTxCallByTplFile spTxCallByTplFile = new SpTxCallByTplFile(spTxEntryMethodTxTpl,
                    calleeExtractedMethodList);
            spTxCallByTplFile.copy(callerExtractedFile);
            spTxCallByTplFileList.add(spTxCallByTplFile);
        }
        // 文件信息列表排序
        spTxCallByTplFileList.sort(Comparator4AbstractCallGraphExtractedFile.getInstance());
        return new ListWithResult<>(spTxCallByTplFileList);
    }
}
