package com.adrninistrator.jacg.extractor.entry.spring;

import com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.enums.SpecialCallTypeEnum;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;
import com.adrninistrator.jacg.dto.lambda.LambdaMethodCall;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.extractor.common.enums.SpringTxTypeEnum;
import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CallerExtractedFile;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.springtx.extract.SpTxCalleeInfo;
import com.adrninistrator.jacg.extractor.entry.CallerGraphBaseExtractor;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.lambda.LambdaMethodHandlerByClassMethodName;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 对调用链结果文件进行数据提取，查找Spring事务相关调用情况的基类
 */
public abstract class AbstractSpringTxExtractor extends CallerGraphBaseExtractor {

    private static final Logger logger = LoggerFactory.getLogger(AbstractSpringTxExtractor.class);

    protected AbstractSpringTxExtractor(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 提取使用@Transactional注解的方法相关信息
     *
     * @param annotationHandler 外层需要使用try-with-resource的方式，确保使用完后close
     * @return
     */
    protected ListWithResult<CallerExtractedFile> extractTxAnnotation(AnnotationHandler annotationHandler) {
        List<FullMethodWithReturnType> springTransactionalMethodList = annotationHandler.queryMethodsWithAnnotation(JACGCommonNameConstants.SPRING_TX_ANNOTATION);
        if (springTransactionalMethodList == null) {
            return ListWithResult.genFail();
        }
        if (JavaCG2Util.isCollectionEmpty(springTransactionalMethodList)) {
            logger.info("未找到@Transactional注解对应的方法");
            return ListWithResult.genEmpty();
        }

        // 设置入口方法
        Set<String> entryMethodSet = new HashSet<>(JACGClassMethodUtil.genFullMethodWithReturnTypeStrList(springTransactionalMethodList));
        logger.info("找到@Transactional注解对应的方法\n{}", StringUtils.join(entryMethodSet, "\n"));
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, entryMethodSet);

        // 调用父类的方法生成调用堆栈文件，参数2设为false，使父类不关闭数据源
        ListWithResult<CallerExtractedFile> callerExtractedFileList = baseExtract(false);
        if (!callerExtractedFileList.isSuccess()) {
            logger.info("未找到@Transactional注解对应的方法对应的调用堆栈文件");
        } else {
            logger.info("找到@Transactional注解对应的方法对应的调用堆栈文件 {}", callerExtractedFileList.getList().size());
        }
        return callerExtractedFileList;
    }

    /**
     * 提取TransactionTemplate对应的方法相关信息
     *
     * @param spTxEntryMethodTxTplList
     * @return
     */
    protected ListWithResult<CallerExtractedFile> extractTxTpl(List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList) {
        List<String> txTplEntryMethodList = new ArrayList<>();
        // 查询事务模板对应的入口方法
        queryTxTplEntryMethod(spTxEntryMethodTxTplList, txTplEntryMethodList);
        if (txTplEntryMethodList.isEmpty()) {
            logger.info("未找到TransactionTemplate对应的方法");
            return ListWithResult.genEmpty();
        }

        logger.info("找到TransactionTemplate对应的方法\n{}", StringUtils.join(txTplEntryMethodList, "\n"));
        // 设置入口方法
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, new HashSet<>(txTplEntryMethodList));

        // 调用父类的方法生成调用堆栈文件，参数2设为false，使父类不关闭数据源
        ListWithResult<CallerExtractedFile> callerExtractedFileList = baseExtract(false);
        if (!callerExtractedFileList.isSuccess()) {
            logger.info("未找到TransactionTemplate对应的方法对应的调用堆栈文件");
        } else {
            logger.info("找到TransactionTemplate对应的方法对应的调用堆栈文件 {}", callerExtractedFileList.getList().size());
        }
        return callerExtractedFileList;
    }

    protected void queryTxTplEntryMethod(List<SpTxEntryMethodTxTpl> spTxEntryMethodTxTplList, List<String> txTplEntryMethodList) {
        // 查询TransactionTemplate使用匿名内部类的方法
        try (JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper)) {
            // 查询事务模板使用匿名内部类的信息
            /*
                查询使用TransactionCallback的情况
                查找调用构造函数的代码，能够找到正确的上层调用方法
                不查找调用doInTransaction方法的代码，否则找到上层调用方法是构造函数，下同
             */
            queryTxTplAnonymousInnerClassInfo(jacgExtendsImplHandler, JavaCG2CommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK,
                    JavaCG2CommonNameConstants.METHOD_NAME_INIT, spTxEntryMethodTxTplList, txTplEntryMethodList);

            // 查询使用TransactionCallbackWithoutResult的情况
            queryTxTplAnonymousInnerClassInfo(jacgExtendsImplHandler, JavaCG2CommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK_WITHOUT_RESULT,
                    JavaCG2CommonNameConstants.METHOD_NAME_INIT, spTxEntryMethodTxTplList, txTplEntryMethodList);

            if (!JavaCG2Util.isCollectionEmpty(txTplEntryMethodList)) {
                logger.info("找到TransactionTemplate使用匿名内部类的方法\n{}", StringUtils.join(txTplEntryMethodList, "\n"));
            }
        }

        // 查询TransactionTemplate使用Lambda表达式的方法
        try (LambdaMethodHandlerByClassMethodName lambdaMethodHandlerByClassMethodName = new LambdaMethodHandlerByClassMethodName(configureWrapper)) {
            // 查询通过Lambda表达式使用TransactionTemplate的情况
            List<LambdaMethodCall> lambdaMethodCallList = lambdaMethodHandlerByClassMethodName.queryDetailByLambdaCallee(
                    JavaCG2CommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK, JavaCG2CommonNameConstants.METHOD_NAME_DO_IN_TRANSACTION);
            if (!JavaCG2Util.isCollectionEmpty(lambdaMethodCallList)) {
                List<String> lambdaEntryMethodList = new ArrayList<>(lambdaMethodCallList.size());

                for (LambdaMethodCall lambdaMethodCall : lambdaMethodCallList) {
                    SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = new SpTxEntryMethodTxTpl(SpecialCallTypeEnum.SCTE_LAMBDA, lambdaMethodCall.getCalleeFullMethod(),
                            lambdaMethodCall.getRawReturnType(), lambdaMethodCall.getCallerFullMethod(), lambdaMethodCall.getCallerLineNumber());
                    spTxEntryMethodTxTplList.add(spTxEntryMethodTxTpl);
                    lambdaEntryMethodList.add(lambdaMethodCall.getCalleeFullMethod());
                }

                logger.info("找到TransactionTemplate使用Lambda表达式的方法\n{}", StringUtils.join(lambdaMethodCallList, "\n"));
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
        if (JavaCG2Util.isCollectionEmpty(childClassNameList)) {
            return;
        }

        for (String childClassName : childClassNameList) {
            // 查询对指定类指定事务方法的调用
            List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryNormalMethodCallByCalleeClassMethod(childClassName, method, false);
            if (JavaCG2Util.isCollectionEmpty(methodCallList)) {
                continue;
            }

            for (WriteDbData4MethodCall methodCall : methodCallList) {
                // 记录Spring事务入口方法
                SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = new SpTxEntryMethodTxTpl(SpecialCallTypeEnum.SCTE_ANONYMOUS_INNER_CLASS, methodCall.getCalleeFullMethod(),
                        methodCall.getRawReturnType(), methodCall.getCallerFullMethod(), methodCall.getCallerLineNumber());
                spTxEntryMethodTxTplList.add(spTxEntryMethodTxTpl);
                txTplEntryMethodList.add(methodCall.getCalleeFullMethod());
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
                CallGraphLineParsed lastLineParsed = callerExtractedLine.getDirectlyCalleeLineParsed();
                if (lastLineParsed != null) {
                    calleeUpperFullMethod = lastLineParsed.getMethodDetail().getFullMethod();
                }
            } else {
                // 类型为事务注解
                springTxTypeEnum = SpringTxTypeEnum.STTE_ANNOTATION;
                // 查询事务注解对应的事务传播行为
                txPropagation = queryTxAnnotationPropagation(annotationHandler, callGraphLineMethodDetail.getFullMethod(), callGraphLineMethodDetail.getReturnType());
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
            CallGraphLineParsed lastLineParsed = callerExtractedLine.getDirectlyCalleeLineParsed();
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
    protected String queryTxAnnotationPropagation(AnnotationHandler annotationHandler, String fullMethod, String returnType) {
        return annotationHandler.querySpringTxAnnotationPropagation(fullMethod, returnType);
    }

    // 指定公共配置参数
    protected void setCommonConfig() {
        // 添加方法注解处理类
        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER,
                SpringTransactionalFormatter.class.getName(),
                DefaultAnnotationFormatter.class.getName()
        );
    }
}
