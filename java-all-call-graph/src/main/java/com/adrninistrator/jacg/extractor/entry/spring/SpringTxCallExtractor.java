package com.adrninistrator.jacg.extractor.entry.spring;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.comparator.Comparator4AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.info_with_hash.AbstractInfoWithMethodHash;
import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_combined.SpTxCallCombined;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxCallByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxCallByTplFile;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 对调用链结果文件进行数据提取，查找Spring事务发起的指定操作（例如查找事务中发起RPC调用等耗时操作的情况）
 */
public class SpringTxCallExtractor extends AbstractSpringTxExtractor {
    private static final Logger logger = LoggerFactory.getLogger(SpringTxCallExtractor.class);

    /**
     * 查找Spring事务嵌套的调用情况，使用配置文件中的参数
     *
     * @return
     */
    public SpTxCallCombined extract() {
        return extract(new ConfigureWrapper(false));
    }

    /**
     * 查找Spring事务嵌套的调用情况，使用代码指定的参数
     *
     * @param configureWrapper
     * @return
     */
    public SpTxCallCombined extract(ConfigureWrapper configureWrapper) {
        List<String> keywordList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER, true);
        if (keywordList.isEmpty()) {
            logger.error("未在配置文件中指定搜索关键字 {}", OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER);
            return null;
        }

        // 指定公共配置参数
        setCommonConfig(configureWrapper);

        // 创建数据库相关对象
        if (!genDbObject(configureWrapper)) {
            return null;
        }

        try (AnnotationHandler annotationHandler = new AnnotationHandler(configureWrapper)) {
            // 处理事务注解
            List<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList = handleTxAnnotation(configureWrapper, annotationHandler);

            // 处理事务模板
            List<SpTxCallByTplFile> spTxCallByTplFileList = handleTxTpl(configureWrapper);

            return new SpTxCallCombined(spTxCallByAnnotationFileList, spTxCallByTplFileList);
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
    private List<SpTxCallByAnnotationFile> handleTxAnnotation(ConfigureWrapper configureWrapper, AnnotationHandler annotationHandler) {
        // 提取使用@Transactional注解的方法相关信息
        List<CallerExtractedFile> callerExtractedFileList = extractTxAnnotation(configureWrapper, annotationHandler);
        if (JavaCGUtil.isCollectionEmpty(callerExtractedFileList)) {
            return Collections.emptyList();
        }

        List<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList = new ArrayList<>(callerExtractedFileList.size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList) {
            String txEntryFullMethod = callerExtractedFile.getFullMethod();
            // 查询事务注解对应的事务传播行为
            String txPropagation = queryTxAnnotationPropagation(annotationHandler, txEntryFullMethod);
            SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation = new SpTxEntryMethodTxAnnotation(txEntryFullMethod, txPropagation);

            // 根据调用堆栈文件，生成Spring事务被调用信息列表
            List<BaseCalleeExtractedMethod> calleeExtractedMethodList = genBaseCalleeExtractedMethodList(callerExtractedFile.getCallerExtractedLineList());

            SpTxCallByAnnotationFile spTxCallByAnnotationFile = new SpTxCallByAnnotationFile(spTxEntryMethodTxAnnotation,
                    calleeExtractedMethodList);
            AbstractCallGraphExtractedFile.copy(callerExtractedFile, spTxCallByAnnotationFile);
            spTxCallByAnnotationFileList.add(spTxCallByAnnotationFile);
        }
        // 文件信息列表排序
        spTxCallByAnnotationFileList.sort(Comparator4AbstractCallGraphExtractedFile.getInstance());
        return spTxCallByAnnotationFileList;
    }

    /**
     * 处理事务模板
     *
     * @param configureWrapper
     * @return
     */
    private List<SpTxCallByTplFile> handleTxTpl(ConfigureWrapper configureWrapper) {
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
        List<SpTxCallByTplFile> spTxCallByTplFileList = new ArrayList<>(callerExtractedFileList.size());
        for (CallerExtractedFile callerExtractedFile : callerExtractedFileList) {

            // 根据调用堆栈文件，生成Spring事务被调用信息列表
            List<BaseCalleeExtractedMethod> calleeExtractedMethodList = genBaseCalleeExtractedMethodList(callerExtractedFile.getCallerExtractedLineList());
            SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = spTxEntryMethodTxTplMap.get(callerExtractedFile.getMethodHash());
            SpTxCallByTplFile spTxCallByTplFile = new SpTxCallByTplFile(spTxEntryMethodTxTpl,
                    calleeExtractedMethodList);
            AbstractCallGraphExtractedFile.copy(callerExtractedFile, spTxCallByTplFile);
            spTxCallByTplFileList.add(spTxCallByTplFile);
        }
        // 文件信息列表排序
        spTxCallByTplFileList.sort(Comparator4AbstractCallGraphExtractedFile.getInstance());
        return spTxCallByTplFileList;
    }
}
