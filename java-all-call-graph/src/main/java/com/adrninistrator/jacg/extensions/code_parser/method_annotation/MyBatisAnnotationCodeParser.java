package com.adrninistrator.jacg.extensions.code_parser.method_annotation;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.dto.call.MethodCall;
import com.adrninistrator.javacg.dto.call.MethodCallList;
import com.adrninistrator.javacg.extensions.code_parser.MethodAnnotationParser;
import com.adrninistrator.javacg.util.JavaCGAnnotationUtil;
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
                                      String annotationClassName,
                                      AnnotationEntry annotationEntry,
                                      MethodCallList methodCallList) {
        String type = JavaCGAnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, ATTRIBUTE_NAME_TYPE);
        String method = JavaCGAnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, ATTRIBUTE_NAME_METHOD);
        if (StringUtils.isAnyBlank(type, method)) {
            logger.error("获取MyBatis注解属性为空 {} {} {} {} {}", callerClassName, callerMethodName, annotationClassName, type, method);
            return;
        }
        MethodCall methodCall = new MethodCall(
                callerClassName,
                callerMethodName,
                callerMethodArgs,
                JavaCGCallTypeEnum.CTE_METHOD_ANNOTATION_ADDED,
                type,
                method,
                JavaCGConstants.EMPTY_METHOD_ARGS,
                JavaCGConstants.DEFAULT_LINE_NUMBER,
                null,
                "",
                ""
        );
        methodCallList.addMethodCall(methodCall);
    }
}
