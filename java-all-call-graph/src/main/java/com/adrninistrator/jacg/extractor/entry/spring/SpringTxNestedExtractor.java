package com.adrninistrator.jacg.extractor.entry.spring;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.infowithhash.AbstractInfoWithMethodHash;
import com.adrninistrator.jacg.extensions.findstackfilter.SpringTxMethodCallFilter;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.springtx.extract.SpTxCalleeInfo;
import com.adrninistrator.jacg.extractor.dto.springtx.extractcombined.SpTxNestedCombined;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxNestedByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxNestedByTplFile;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description: 对调用链结果文件进行数据提取，查找Spring事务嵌套的调用情况，包括@Transactional、TransactionTemplate
 */
public class SpringTxNestedExtractor extends AbstractSpringTxExtractor {
    /**
     * 查找Spring事务嵌套的调用情况，使用配置文件中的参数
     *
     * @return
     */
    public SpTxNestedCombined extract() {
        return extract(new ConfigureWrapper(false));
    }

    /**
     * 查找Spring事务嵌套的调用情况，使用代码指定的参数
     *
     * @param configureWrapper
     * @return
     */
    public SpTxNestedCombined extract(ConfigureWrapper configureWrapper) {
        // 指定配置参数
        setConfig(configureWrapper);

        // 创建数据库相关对象
        genDbObject(configureWrapper);

        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            String outputDirName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME);
            if (!outputDirName.isEmpty()) {
                // 有指定生成调用链文件的子目录名，以下会生成两次方法调用链文件，需要分别使用不同的输出子目录名，否则会输出到同一个目录中
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, outputDirName + JACGConstants.FLAG_AT + JACGConstants.SPRING_TX_TYPE_ANNOTATION);
            }
            // 处理事务注解
            ListWithResult<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList = handleTxAnnotation(configureWrapper, annotationHandler);

            if (!outputDirName.isEmpty()) {
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, outputDirName + JACGConstants.FLAG_AT + JACGConstants.SPRING_TX_TYPE_TEMPLATE);
            }
            // 处理事务模板
            ListWithResult<SpTxNestedByTplFile> spTxNestedByTplFileList = handleTxTpl(configureWrapper, annotationHandler);

            return new SpTxNestedCombined(spTxNestedByAnnotationFileList, spTxNestedByTplFileList);
        } finally {
            closeDs();
        }
    }

    /**
     * 处理事务注解
     *
     * @param configureWrapper
     * @param annotationHandler
     * @return
     */
    private ListWithResult<SpTxNestedByAnnotationFile> handleTxAnnotation(ConfigureWrapper configureWrapper, AnnotationHandler annotationHandler) {
        // 提取使用@Transactional注解的方法相关信息
        ListWithResult<CallerExtractedFile> callerExtractedFileList = extractTxAnnotation(configureWrapper, annotationHandler);
        if (!callerExtractedFileList.isSuccess()) {
            return ListWithResult.genFail();
        }

        List<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList = new ArrayList<>(callerExtractedFileList.getList().size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList.getList()) {
            String txEntryFullMethod = callerExtractedFile.getFullMethod();
            // 查询事务注解对应的事务传播行为
            String txPropagation = queryTxAnnotationPropagation(annotationHandler, txEntryFullMethod);
            SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation = new SpTxEntryMethodTxAnnotation(txEntryFullMethod, txPropagation);

            // 根据调用堆栈文件，生成Spring事务被调用信息列表
            List<SpTxCalleeInfo> spTxCalleeInfoList = genSpTxCalleeInfoList(annotationHandler, callerExtractedFile.getCallerExtractedLineList());

            SpTxNestedByAnnotationFile spTxNestedByAnnotationFile = new SpTxNestedByAnnotationFile(spTxEntryMethodTxAnnotation,
                    spTxCalleeInfoList);
            AbstractCallGraphExtractedFile.copy(callerExtractedFile, spTxNestedByAnnotationFile);
            spTxNestedByAnnotationFileList.add(spTxNestedByAnnotationFile);
        }
        return new ListWithResult<>(spTxNestedByAnnotationFileList);
    }

    /**
     * 处理事务模板
     *
     * @param configureWrapper
     * @return
     */
    private ListWithResult<SpTxNestedByTplFile> handleTxTpl(ConfigureWrapper configureWrapper, AnnotationHandler annotationHandler) {
        List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList = new ArrayList<>();
        ListWithResult<CallerExtractedFile> callerExtractedFileList = extractTxTpl(configureWrapper, spTxEntryMethodTxTplList);
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
        List<SpTxNestedByTplFile> spTxNestedByTplFileList = new ArrayList<>(callerExtractedFileList.getList().size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList.getList()) {
            // 根据调用堆栈文件，生成Spring事务被调用信息列表
            List<SpTxCalleeInfo> spTxCalleeInfoList = genSpTxCalleeInfoList(annotationHandler, callerExtractedFile.getCallerExtractedLineList());
            SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = spTxEntryMethodTxTplMap.get(callerExtractedFile.getMethodHash());
            SpTxNestedByTplFile spTxNestedByTplFile = new SpTxNestedByTplFile(spTxEntryMethodTxTpl,
                    spTxCalleeInfoList);
            AbstractCallGraphExtractedFile.copy(callerExtractedFile, spTxNestedByTplFile);
            spTxNestedByTplFileList.add(spTxNestedByTplFile);
        }

        return new ListWithResult<>(spTxNestedByTplFileList);
    }

    // 指定配置参数
    private void setConfig(ConfigureWrapper configureWrapper) {
        // 指定公共配置参数
        setCommonConfig(configureWrapper);

        // 指定对完整调用链文件生成调用堆栈时使用的过滤器扩展类
        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER,
                SpringTxMethodCallFilter.class.getName()
        );
    }
}
