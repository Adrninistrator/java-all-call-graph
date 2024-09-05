package com.adrninistrator.jacg.extensions.codeparser.methodannotation;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.dto.call.MethodCall;
import com.adrninistrator.javacg2.dto.call.MethodCallList;
import com.adrninistrator.javacg2.extensions.codeparser.MethodAnnotationParser;
import com.adrninistrator.javacg2.util.JavaCG2AnnotationUtil;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2023/2/13
 * @description: 处理MyBatis方法上的注解，包括@SelectProvider、@InsertProvider、@DeleteProvider、@UpdateProvider
 */
public class MyBatisAnnotationCodeParser implements MethodAnnotationParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisAnnotationCodeParser.class);

    private static final String[] MYBATIS_ANNOTATION_ARRAY = new String[]{
            "org.apache.ibatis.annotations.DeleteProvider",
            "org.apache.ibatis.annotations.InsertProvider",
            "org.apache.ibatis.annotations.SelectProvider",
            "org.apache.ibatis.annotations.UpdateProvider"
    };

    private static final String ATTRIBUTE_NAME_TYPE = "type";
    private static final String ATTRIBUTE_NAME_METHOD = "method";

    // 指定需要处理的MyBatis注解
    @Override
    public String[] chooseMethodAnnotationClassName() {
        return MYBATIS_ANNOTATION_ARRAY;
    }

    // 处理MyBatis注解，添加方法调用关系
    @Override
    public void parseMethodAnnotation(String callerClassName,
                                      String callerMethodName,
                                      String callerMethodArgs,
                                      String callerReturnType,
                                      String annotationClassName,
                                      AnnotationEntry annotationEntry,
                                      MethodCallList methodCallList) {
        String type = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, ATTRIBUTE_NAME_TYPE);
        String method = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, ATTRIBUTE_NAME_METHOD);
        if (StringUtils.isAnyBlank(type, method)) {
            logger.error("获取MyBatis注解属性为空 {} {} {} {} {}", callerClassName, callerMethodName, annotationClassName, type, method);
            return;
        }
        MethodCall methodCall = new MethodCall();
        methodCall.setCallerClassName(callerClassName);
        methodCall.setCallerMethodName(callerMethodName);
        methodCall.setCallerMethodArgTypes(callerMethodArgs);
        methodCall.setCallerSourceLine(JavaCG2Constants.DEFAULT_LINE_NUMBER);
        methodCall.setCallerReturnType(callerReturnType);
        methodCall.setMethodCallType(JavaCG2CallTypeEnum.CTE_METHOD_ANNOTATION_ADDED);
        methodCall.setCalleeClassName(type);
        methodCall.setCalleeMethodName(method);
        methodCall.setCalleeMethodArgTypes(JavaCG2Constants.EMPTY_METHOD_ARGS);
        methodCall.setRawReturnType("");
        methodCall.setActualReturnType("");
        methodCallList.addMethodCallAutoCallId(methodCall);
    }
}
