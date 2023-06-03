package com.adrninistrator.jacg.handler.spring;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.comparator.Comparator4MethodCallByCaller1;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethod;
import com.adrninistrator.jacg.handler.dto.spring.SpringInvalidTxAnnotationMethodCall;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGCalleeObjTypeEnum;
import com.adrninistrator.javacg.dto.access_flag.JavaCGAccessFlags;
import com.adrninistrator.javacg.util.JavaCGUtil;
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

    public SpringTxHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 查询Spring事务注解方法非法调用（调用当前实例的@Transactional注解方法）
     *
     * @return
     */
    public List<SpringInvalidTxAnnotationMethodCall> querySpringInvalidTxAnnotationMethodCall() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(dbOperWrapper)) {
            List<String> springTransactionalMethodList = annotationHandler.queryMethodsWithAnnotations(true, JACGCommonNameConstants.SPRING_TX_ANNOTATION);
            if (JavaCGUtil.isCollectionEmpty(springTransactionalMethodList)) {
                logger.info("未查询到@Transactional注解所在方法");
                return Collections.emptyList();
            }

            // @Transactional注解所在方法列表排序
            Collections.sort(springTransactionalMethodList);
            List<SpringInvalidTxAnnotationMethodCall> springInvalidTxAnnotationMethodCallList = new ArrayList<>();
            // 查找调用当前实例的@Transactional注解方法
            for (String springTransactionalMethod : springTransactionalMethodList) {
                String methodHash = JACGUtil.genHashWithLen(springTransactionalMethod);
                List<WriteDbData4MethodCall> methodCallList = dbOperWrapper.getMethodCallByCalleeHashObjType(methodHash, JavaCGCalleeObjTypeEnum.COTE_THIS.getType());
                if (JavaCGUtil.isCollectionEmpty(methodCallList)) {
                    // 当前@Transactional注解方法不存在当前实例调用的情况
                    continue;
                }
                // 当前@Transactional注解方法存在当前实例调用的情况
                // 查询被调用方法Spring事务注解@Transactional对应的事务传播行为
                String calleeTxPropagation = annotationHandler.querySpringTxAnnotationPropagation(springTransactionalMethod);

                // 对调用信息列表排序
                methodCallList.sort(Comparator4MethodCallByCaller1.getInstance());
                for (WriteDbData4MethodCall methodCall : methodCallList) {
                    String callerFullMethod = methodCall.getCallerFullMethod();
                    boolean callerWithSpringTx = false;
                    String callerTxPropagation = "";
                    // 查询调用方法的Spring事务注解信息
                    Map<String, BaseAnnotationAttribute> transactionalAnnotationAttributeMap = annotationHandler.queryMethodAnnotationAttributes(callerFullMethod,
                            JACGCommonNameConstants.SPRING_TX_ANNOTATION);
                    if (!JACGUtil.isMapEmpty(transactionalAnnotationAttributeMap)) {
                        callerWithSpringTx = true;
                        BaseAnnotationAttribute txPropagationAttribute = transactionalAnnotationAttributeMap.get(JACGCommonNameConstants.SPRING_TX_ANNOTATION);
                        callerTxPropagation = annotationHandler.getSpringTxAnnotationPropagation(txPropagationAttribute);
                    }

                    SpringInvalidTxAnnotationMethodCall springInvalidTxAnnotationMethodCall = new SpringInvalidTxAnnotationMethodCall(callerFullMethod,
                            methodCall.getCallerLineNumber(), methodCall.getCalleeFullMethod(), callerWithSpringTx, callerTxPropagation, calleeTxPropagation);
                    springInvalidTxAnnotationMethodCallList.add(springInvalidTxAnnotationMethodCall);
                }
            }
            return springInvalidTxAnnotationMethodCallList;
        }
    }

    /**
     * 查询Spring非法事务注解方法（@Transactional注解方法为private/protected、static、final方法）
     *
     * @return
     */
    public List<SpringInvalidTxAnnotationMethod> querySpringInvalidTxAnnotationMethod() {
        try (AnnotationHandler annotationHandler = new AnnotationHandler(dbOperWrapper)) {
            List<String> springTransactionalMethodList = annotationHandler.queryMethodsWithAnnotations(true, JACGCommonNameConstants.SPRING_TX_ANNOTATION);
            if (JavaCGUtil.isCollectionEmpty(springTransactionalMethodList)) {
                logger.info("未查询到@Transactional注解所在方法");
                return Collections.emptyList();
            }

            List<SpringInvalidTxAnnotationMethod> springInvalidTxAnnotationMethodList = new ArrayList<>();
            // @Transactional注解所在方法列表排序
            Collections.sort(springTransactionalMethodList);
            for (String springTransactionalMethod : springTransactionalMethodList) {
                String methodHash = JACGUtil.genHashWithLen(springTransactionalMethod);
                // 获取方法对应的标志
                Integer methodFlags = dbOperWrapper.getMethodFlags(methodHash);
                if (methodFlags == null) {
                    continue;
                }

                List<String> methodFlagList = new ArrayList<>();
                JavaCGAccessFlags javaCGAccessFlags = new JavaCGAccessFlags(methodFlags);
                if (javaCGAccessFlags.isPrivate()) {
                    methodFlagList.add("private");
                } else if (javaCGAccessFlags.isProtected()) {
                    methodFlagList.add("protected");
                }
                if (javaCGAccessFlags.isStatic()) {
                    methodFlagList.add("static");
                }
                if (javaCGAccessFlags.isFinal()) {
                    methodFlagList.add("final");
                }
                if (!methodFlagList.isEmpty()) {
                    // @Transactional注解所在方法非法
                    String methodFlagsDesc = StringUtils.join(methodFlagList, " ");
                    SpringInvalidTxAnnotationMethod springInvalidTxAnnotationMethod = new SpringInvalidTxAnnotationMethod(springTransactionalMethod, methodFlagsDesc);
                    springInvalidTxAnnotationMethodList.add(springInvalidTxAnnotationMethod);
                }
            }
            return springInvalidTxAnnotationMethodList;
        }
    }
}