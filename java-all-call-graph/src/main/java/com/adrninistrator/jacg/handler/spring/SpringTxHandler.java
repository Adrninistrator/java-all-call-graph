package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.comparator.Comparator4FullMethodWithReturnType;
import com.adrninistrator.jacg.comparator.Comparator4MethodCallByCaller1;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethod;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethodCall;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeObjTypeEnum;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description: Spring事务相关的查询处理类
 */
public class SpringTxHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(SpringTxHandler.class);

    private final AnnotationHandler annotationHandler;
    private final MethodCallHandler methodCallHandler;
    private final MethodInfoHandler methodInfoHandler;

    public SpringTxHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    /**
     * 查询Spring事务注解方法非法调用（调用当前实例的@Transactional注解方法）
     *
     * @return
     */
    public List<SpringInvalidTxAnnotationMethodCall> querySpringInvalidTxAnnotationMethodCall() {
        List<FullMethodWithReturnType> springTransactionalMethodList = annotationHandler.queryMethodsWithAnnotation(JACGCommonNameConstants.SPRING_TX_ANNOTATION);
        if (JavaCG2Util.isCollectionEmpty(springTransactionalMethodList)) {
            logger.info("未查询到@Transactional注解所在方法1");
            return Collections.emptyList();
        }

        // @Transactional注解所在方法列表排序
        springTransactionalMethodList.sort(Comparator4FullMethodWithReturnType.getInstance());
        List<SpringInvalidTxAnnotationMethodCall> springInvalidTxAnnotationMethodCallList = new ArrayList<>();
        // 查找调用当前实例的@Transactional注解方法
        for (FullMethodWithReturnType springTransactionalMethod : springTransactionalMethodList) {
            String methodHash = JACGClassMethodUtil.genMethodHashWithLen(springTransactionalMethod.getFullMethod(), springTransactionalMethod.getReturnType());
            List<WriteDbData4MethodCall> methodCallList = methodCallHandler.queryMethodCallByCalleeHashObjType(methodHash, JavaCG2CalleeObjTypeEnum.COTE_THIS.getType());
            if (JavaCG2Util.isCollectionEmpty(methodCallList)) {
                // 当前@Transactional注解方法不存在当前实例调用的情况
                continue;
            }
            // 当前@Transactional注解方法存在当前实例调用的情况
            // 查询被调用方法Spring事务注解@Transactional对应的事务传播行为
            String calleeTxPropagation = annotationHandler.querySpringTxAnnotationPropagation(springTransactionalMethod.getFullMethod(), springTransactionalMethod.getReturnType());

            // 对调用信息列表排序
            methodCallList.sort(Comparator4MethodCallByCaller1.getInstance());
            for (WriteDbData4MethodCall methodCall : methodCallList) {
                String callerFullMethod = methodCall.getCallerFullMethod();
                String callerReturnType = methodCall.getCallerReturnType();
                boolean callerWithSpringTx = false;
                String callerTxPropagation = "";
                // 查询调用方法的Spring事务注解信息
                Map<String, BaseAnnotationAttribute> transactionalAnnotationAttributeMap = annotationHandler.queryMethodAnnotationAttributes(callerFullMethod, callerReturnType,
                        JACGCommonNameConstants.SPRING_TX_ANNOTATION);
                if (!JavaCG2Util.isMapEmpty(transactionalAnnotationAttributeMap)) {
                    callerWithSpringTx = true;
                    BaseAnnotationAttribute txPropagationAttribute = transactionalAnnotationAttributeMap.get(JACGCommonNameConstants.SPRING_TX_ATTRIBUTE_PROPAGATION);
                    callerTxPropagation = AnnotationAttributesParseUtil.getSpringTxAnnotationPropagation(txPropagationAttribute);
                }

                SpringInvalidTxAnnotationMethodCall springInvalidTxAnnotationMethodCall = new SpringInvalidTxAnnotationMethodCall(callerFullMethod,
                        methodCall.getCallerLineNumber(), methodCall.getCalleeFullMethod(), callerWithSpringTx, callerTxPropagation, calleeTxPropagation);
                springInvalidTxAnnotationMethodCallList.add(springInvalidTxAnnotationMethodCall);
            }
        }
        return springInvalidTxAnnotationMethodCallList;
    }

    /**
     * 查询Spring非法事务注解方法（@Transactional注解方法为private/protected、static、final方法）
     *
     * @return
     */
    public List<SpringInvalidTxAnnotationMethod> querySpringInvalidTxAnnotationMethod() {
        List<FullMethodWithReturnType> springTransactionalMethodList = annotationHandler.queryMethodsWithAnnotation(JACGCommonNameConstants.SPRING_TX_ANNOTATION);
        if (JavaCG2Util.isCollectionEmpty(springTransactionalMethodList)) {
            logger.info("未查询到@Transactional注解所在方法2");
            return Collections.emptyList();
        }

        List<SpringInvalidTxAnnotationMethod> springInvalidTxAnnotationMethodList = new ArrayList<>();
        // @Transactional注解所在方法列表排序
        springTransactionalMethodList.sort(Comparator4FullMethodWithReturnType.getInstance());
        for (FullMethodWithReturnType springTransactionalMethod : springTransactionalMethodList) {
            String methodHash = JACGClassMethodUtil.genMethodHashWithLen(springTransactionalMethod.getFullMethod(), springTransactionalMethod.getReturnType());
            // 获取方法对应的标志
            Integer methodFlags = methodInfoHandler.queryMethodFlags(methodHash);
            if (methodFlags == null) {
                continue;
            }

            List<String> methodFlagList = new ArrayList<>();
            JavaCG2AccessFlags javaCG2AccessFlags = new JavaCG2AccessFlags(methodFlags);
            if (javaCG2AccessFlags.isPrivate()) {
                methodFlagList.add("private");
            } else if (javaCG2AccessFlags.isProtected()) {
                methodFlagList.add("protected");
            }
            if (javaCG2AccessFlags.isStatic()) {
                methodFlagList.add("static");
            }
            if (javaCG2AccessFlags.isFinal()) {
                methodFlagList.add("final");
            }
            if (!methodFlagList.isEmpty()) {
                // @Transactional注解所在方法非法
                String methodFlagsDesc = StringUtils.join(methodFlagList, " ");
                SpringInvalidTxAnnotationMethod springInvalidTxAnnotationMethod = new SpringInvalidTxAnnotationMethod(springTransactionalMethod.getFullMethod(),
                        springTransactionalMethod.getReturnType(), methodFlagsDesc);
                springInvalidTxAnnotationMethodList.add(springInvalidTxAnnotationMethod);
            }
        }
        return springInvalidTxAnnotationMethodList;
    }
}