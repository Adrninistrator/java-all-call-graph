package com.adrninistrator.jacg.extractor.entry.spring;

import com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.SpecialCallTypeEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_line.CallGraphLineParsed;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCallDetail;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.method_call.MethodCallPair;
import com.adrninistrator.jacg.extractor.common.enums.SpringTxTypeEnum;
import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract.SpTxCalleeInfo;
import com.adrninistrator.jacg.extractor.entry.CallerGraphBaseExtractor;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.extends_impl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByClassMethodName;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 对调用链结果文件进行数据提取，查找Spring事务相关调用情况的基类
 */
public abstract class AbstractSpringTxExtractor extends CallerGraphBaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(AbstractSpringTxExtractor.class);

    /**
     * 提取使用@Transactional注解的方法相关信息
     *
     * @param configureWrapper
     * @param annotationHandler 外层需要使用try-with-resource的方式，确保使用完后close
     * @return
     */
    protected List<CallerExtractedFile> extractTxAnnotation(ConfigureWrapper configureWrapper, AnnotationHandler annotationHandler) {
        List<String> springTransactionalMethodList = annotationHandler.queryMethodsWithAnnotations(true, JACGCommonNameConstants.SPRING_TX_ANNOTATION);
        if (JavaCGUtil.isCollectionEmpty(springTransactionalMethodList)) {
            logger.info("未找到@Transactional注解对应的方法");
            return Collections.emptyList();
        }

        logger.info("找到@Transactional注解对应的方法\n{}", StringUtils.join(springTransactionalMethodList, "\n"));
        // 设置入口方法
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, new HashSet<>(springTransactionalMethodList));

        // 调用父类的方法生成调用堆栈文件，参数2设为false，使父类不关闭数据源
        List<CallerExtractedFile> callerExtractedFileList = baseExtract(configureWrapper, false);
        if (JavaCGUtil.isCollectionEmpty(callerExtractedFileList)) {
            logger.info("未找到@Transactional注解对应的方法对应的调用堆栈文件");
            return Collections.emptyList();
        }
        logger.info("找到@Transactional注解对应的方法对应的调用堆栈文件 {}", callerExtractedFileList.size());
        return callerExtractedFileList;
    }

    // 查询事务模板对应的入口方法

    /**
     * 提取TransactionTemplate对应的方法相关信息
     *
     * @param configureWrapper
     * @param spTxEntryMethodTxTplList
     * @return
     */
    protected List<CallerExtractedFile> extractTxTpl(ConfigureWrapper configureWrapper, List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList) {
        List<String> txTplEntryMethodList = new ArrayList<>();
        // 查询事务模板对应的入口方法
        queryTxTplEntryMethodInfo(configureWrapper, spTxEntryMethodTxTplList, txTplEntryMethodList);
        if (txTplEntryMethodList.isEmpty()) {
            logger.info("未找到TransactionTemplate对应的方法");
            return Collections.emptyList();
        }

        logger.info("找到TransactionTemplate对应的方法\n{}", StringUtils.join(txTplEntryMethodList, "\n"));
        // 设置入口方法
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, new HashSet<>(txTplEntryMethodList));

        // 调用父类的方法生成调用堆栈文件，参数2设为false，使父类不关闭数据源
        List<CallerExtractedFile> callerExtractedFileList = baseExtract(configureWrapper, false);
        if (JavaCGUtil.isCollectionEmpty(callerExtractedFileList)) {
            logger.info("未找到TransactionTemplate对应的方法对应的调用堆栈文件");
            return Collections.emptyList();
        }
        logger.info("找到TransactionTemplate对应的方法对应的调用堆栈文件 {}", callerExtractedFileList.size());
        return callerExtractedFileList;
    }

    protected void queryTxTplEntryMethodInfo(ConfigureWrapper configureWrapper, List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList, List<String> txTplEntryMethodList) {
        // 查询TransactionTemplate使用匿名内部类的方法
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            // 查询事务模板使用匿名内部类的信息
            /*
                查询使用TransactionCallback的情况
                查找调用构造函数的代码，能够找到正确的上层调用方法
                不查找调用doInTransaction方法的代码，否则找到上层调用方法是构造函数，下同
             */
            queryTxTplAnonymousInnerClassInfo(jacgExtendsImplHandler, JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK,
                    JavaCGCommonNameConstants.METHOD_NAME_INIT, spTxEntryMethodTxTplList, txTplEntryMethodList);

            // 查询使用TransactionCallbackWithoutResult的情况
            queryTxTplAnonymousInnerClassInfo(jacgExtendsImplHandler, JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK_WITHOUT_RESULT,
                    JavaCGCommonNameConstants.METHOD_NAME_INIT, spTxEntryMethodTxTplList, txTplEntryMethodList);

            if (!JavaCGUtil.isCollectionEmpty(txTplEntryMethodList)) {
                logger.info("找到TransactionTemplate使用匿名内部类的方法\n{}", StringUtils.join(txTplEntryMethodList, "\n"));
            }
        }

        // 查询TransactionTemplate使用Lambda表达式的方法
        try (LambdaMethodHandlerByClassMethodName lambdaMethodHandlerByClassMethodName = new LambdaMethodHandlerByClassMethodName(configureWrapper)) {
            // 查询通过Lambda表达式使用TransactionTemplate的情况
            List<LambdaMethodCallDetail> lambdaMethodCallDetailList = lambdaMethodHandlerByClassMethodName.queryDetailByLambdaCallee(
                    JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK, JavaCGCommonNameConstants.METHOD_DO_IN_TRANSACTION);
            if (!JavaCGUtil.isCollectionEmpty(lambdaMethodCallDetailList)) {
                List<String> lambdaEntryMethodList = new ArrayList<>(lambdaMethodCallDetailList.size());

                for (LambdaMethodCallDetail lambdaMethodCallDetail : lambdaMethodCallDetailList) {
                    SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = new SpTxEntryMethodTxTpl(SpecialCallTypeEnum.SCTE_LAMBDA, lambdaMethodCallDetail.getCalleeFullMethod(),
                            lambdaMethodCallDetail.getCallerFullMethod(), lambdaMethodCallDetail.getCallerLineNumber());
                    spTxEntryMethodTxTplList.add(spTxEntryMethodTxTpl);
                    lambdaEntryMethodList.add(lambdaMethodCallDetail.getCalleeFullMethod());
                }

                logger.info("找到TransactionTemplate使用Lambda表达式的方法\n{}", StringUtils.join(lambdaMethodCallDetailList, "\n"));
                txTplEntryMethodList.addAll(lambdaEntryMethodList);
            }
        }
    }

    // 查询事务模板使用匿名内部类的信息
    protected void queryTxTplAnonymousInnerClassInfo(JACGExtendsImplHandler jacgExtendsImplHandler,
                                                     String superClassName,
                                                     String method,
                                                     List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList,
                                                     List<String> txTplEntryMethodList) {
        // 查询指定的事务子类
        List<String> childClassNameList = jacgExtendsImplHandler.queryChildClassListByFull(superClassName, false, true, false, true);
        if (JavaCGUtil.isCollectionEmpty(childClassNameList)) {
            return;
        }

        for (String childClassName : childClassNameList) {
            // 查询对指定类指定事务方法的调用
            List<MethodCallPair> methodCallPairList = dbOperWrapper.getMethodCallByCalleeFullClassMethod(childClassName, method);
            if (JavaCGUtil.isCollectionEmpty(methodCallPairList)) {
                continue;
            }

            for (MethodCallPair methodCallPair : methodCallPairList) {
                // 记录Spring事务入口方法
                SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = new SpTxEntryMethodTxTpl(SpecialCallTypeEnum.SCTE_ANONYMOUS_INNER_CLASS, methodCallPair.getCalleeFullMethod(),
                        methodCallPair.getCallerFullMethod(), methodCallPair.getCallerLineNumber());
                spTxEntryMethodTxTplList.add(spTxEntryMethodTxTpl);
                txTplEntryMethodList.add(methodCallPair.getCalleeFullMethod());
            }
        }
    }

    /**
     * 根据调用堆栈文件，生成Spring事务被调用信息列表
     *
     * @param annotationHandler
     * @param callerExtractedLineList
     * @return
     */
    protected List<SpTxCalleeInfo> genSpTxCalleeInfoList(AnnotationHandler annotationHandler, List<CallerExtractedLine> callerExtractedLineList) {
        List<SpTxCalleeInfo> spTxCalleeInfoList = new ArrayList<>(callerExtractedLineList.size());
        for (CallerExtractedLine callerExtractedLine : callerExtractedLineList) {
            CallGraphLineParsed callGraphLineParsed = callerExtractedLine.getCallGraphLineParsed();
            MethodDetail callGraphLineMethodDetail = callGraphLineParsed.getMethodDetail();

            SpringTxTypeEnum springTxTypeEnum;
            String calleeUpperFullMethod = "";
            String txPropagation = "";
            if (JACGCommonNameConstants.SPRING_TRANSACTION_TEMPLATE_CLASS.equals(callGraphLineMethodDetail.getClassName())) {
                // 被调用类为TransactionTemplate，类型为事务模板
                springTxTypeEnum = SpringTxTypeEnum.STTE_TEMPLATE;
                // 处理被调用上一层方法
                CallGraphLineParsed lastLineParsed = callerExtractedLine.getLastLineParsed();
                if (lastLineParsed != null) {
                    calleeUpperFullMethod = lastLineParsed.getMethodDetail().getFullMethod();
                }
            } else {
                // 类型为事务注解
                springTxTypeEnum = SpringTxTypeEnum.STTE_ANNOTATION;
                // 查询事务注解对应的事务传播行为
                txPropagation = queryTxAnnotationPropagation(annotationHandler, callGraphLineMethodDetail.getFullMethod());
            }
            SpTxCalleeInfo spTxCalleeInfo = new SpTxCalleeInfo(callerExtractedLine.getDataSeq(),
                    callerExtractedLine.getLineNumber(),
                    callGraphLineMethodDetail.getFullMethod(),
                    calleeUpperFullMethod,
                    callerExtractedLine.isRunInOtherThread(),
                    springTxTypeEnum,
                    txPropagation);
            spTxCalleeInfoList.add(spTxCalleeInfo);
        }
        return spTxCalleeInfoList;
    }

    /**
     * 根据调用堆栈文件，生成被调用信息列表
     *
     * @param callerExtractedLineList
     * @return
     */
    protected List<BaseCalleeExtractedMethod> genBaseCalleeExtractedMethodList(List<CallerExtractedLine> callerExtractedLineList) {
        List<BaseCalleeExtractedMethod> calleeExtractedMethodList = new ArrayList<>(callerExtractedLineList.size());
        for (CallerExtractedLine callerExtractedLine : callerExtractedLineList) {
            String calleeUpperFullMethod = null;
            CallGraphLineParsed lastLineParsed = callerExtractedLine.getLastLineParsed();
            if (lastLineParsed != null) {
                calleeUpperFullMethod = lastLineParsed.getMethodDetail().getFullMethod();
            }

            BaseCalleeExtractedMethod calleeExtractedMethod = new BaseCalleeExtractedMethod(callerExtractedLine.getDataSeq(),
                    callerExtractedLine.getLineNumber(),
                    callerExtractedLine.getCallGraphLineParsed().getMethodDetail().getFullMethod(),
                    calleeUpperFullMethod,
                    callerExtractedLine.isRunInOtherThread(),
                    callerExtractedLine.isRunInTransaction());
            calleeExtractedMethodList.add(calleeExtractedMethod);
        }
        return calleeExtractedMethodList;
    }

    // 查询事务注解对应的事务传播行为
    protected String queryTxAnnotationPropagation(AnnotationHandler annotationHandler, String fullMethod) {
        return annotationHandler.querySpringTxAnnotationPropagation(fullMethod);
    }

    // 指定公共配置参数
    protected void setCommonConfig(ConfigureWrapper configureWrapper) {
        // 指定方法注解处理类
        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER,
                SpringTransactionalFormatter.class.getName(),
                DefaultAnnotationFormatter.class.getName()
        );

        // 在需要处理的类名前缀中增加Spring事务模板类
        setAllowedClassNamePrefix(configureWrapper);
    }

    // 在需要处理的类名前缀中增加Spring事务模板类
    public void setAllowedClassNamePrefix(ConfigureWrapper configureWrapper) {
        configureWrapper.addAllowedClassNamePrefixes(JACGCommonNameConstants.SPRING_TRANSACTION_TEMPLATE_CLASS,
                JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK_WITHOUT_RESULT,
                JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK
        );
    }
}
