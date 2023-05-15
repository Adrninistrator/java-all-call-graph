package com.adrninistrator.jacg.extractor.entry.spring;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.info_with_hash.AbstractInfoWithMethodHash;
import com.adrninistrator.jacg.extensions.find_stack_filter.SpringTxMethodCallFilter;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract.SpTxCalleeInfo;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_combined.SpTxNestedCombined;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxNestedByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxNestedByTplFile;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.ArrayList;
import java.util.Collections;
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
            ConfigureWrapper usedConfigureWrapper;
            String outputSubDirName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME);
            if (!outputSubDirName.isEmpty()) {
                // 有指定生成调用链文件的子目录名，需要生成新的配置对象，避免修改传入的配置对象
                usedConfigureWrapper = configureWrapper.copy();
            } else {
                usedConfigureWrapper = configureWrapper;
            }

            if (!outputSubDirName.isEmpty()) {
                // 有指定生成调用链文件的子目录名，以下会生成两次方法调用链文件，需要分别使用不同的输出子目录名，否则会输出到同一个目录中
                usedConfigureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, outputSubDirName + JACGConstants.FLAG_AT + JACGConstants.SPRING_TX_TYPE_ANNOTATION);
            }
            // 处理事务注解
            List<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList = handleTxAnnotation(usedConfigureWrapper, annotationHandler);

            if (!outputSubDirName.isEmpty()) {
                usedConfigureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, outputSubDirName + JACGConstants.FLAG_AT + JACGConstants.SPRING_TX_TYPE_TEMPLATE);
            }
            // 处理事务模板
            List<SpTxNestedByTplFile> spTxNestedByTplFileList = handleTxTpl(usedConfigureWrapper, annotationHandler);

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
    private List<SpTxNestedByAnnotationFile> handleTxAnnotation(ConfigureWrapper configureWrapper, AnnotationHandler annotationHandler) {
        // 提取使用@Transactional注解的方法相关信息
        List<CallerExtractedFile> callerExtractedFileList = extractTxAnnotation(configureWrapper, annotationHandler);
        if (JavaCGUtil.isCollectionEmpty(callerExtractedFileList)) {
            return Collections.emptyList();
        }

        List<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList = new ArrayList<>(callerExtractedFileList.size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList) {
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
        return spTxNestedByAnnotationFileList;
    }

    /**
     * 处理事务模板
     *
     * @param configureWrapper
     * @return
     */
    private List<SpTxNestedByTplFile> handleTxTpl(ConfigureWrapper configureWrapper, AnnotationHandler annotationHandler) {
        List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList = new ArrayList<>();
        List<CallerExtractedFile> callerExtractedFileList = extractTxTpl(configureWrapper, spTxEntryMethodTxTplList);
        if (JavaCGUtil.isCollectionEmpty(callerExtractedFileList)) {
            return Collections.emptyList();
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
        List<SpTxNestedByTplFile> spTxNestedByTplFileList = new ArrayList<>(callerExtractedFileList.size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList) {

            // 根据调用堆栈文件，生成Spring事务被调用信息列表
            List<SpTxCalleeInfo> spTxCalleeInfoList = genSpTxCalleeInfoList(annotationHandler, callerExtractedFile.getCallerExtractedLineList());
            SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = spTxEntryMethodTxTplMap.get(callerExtractedFile.getMethodHash());
            SpTxNestedByTplFile spTxNestedByTplFile = new SpTxNestedByTplFile(spTxEntryMethodTxTpl,
                    spTxCalleeInfoList);
            AbstractCallGraphExtractedFile.copy(callerExtractedFile, spTxNestedByTplFile);
            spTxNestedByTplFileList.add(spTxNestedByTplFile);
        }

        return spTxNestedByTplFileList;
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
