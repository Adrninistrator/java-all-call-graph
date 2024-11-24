package test.runbycode.handler.protocol;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgument;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.handler.annotation.AnnotationHandler;
import com.adrninistrator.jacg.handler.dto.field.CommonFieldInfoInClass;
import com.adrninistrator.jacg.handler.protocol.BaseProtocolFieldHandler;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.annotation.TestAnnotation1;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/10/27
 * @description:
 */
public class TestProtocolFieldHandler extends BaseProtocolFieldHandler {
    private static final Logger logger = LoggerFactory.getLogger(TestProtocolFieldHandler.class);

    private final AnnotationHandler annotationHandler;

    public TestProtocolFieldHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
    }

    public TestProtocolFieldHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        annotationHandler = new AnnotationHandler(dbOperWrapper);
    }

    // 处理Spring Controller某个方法的某个参数
    @Override
    protected void doHandleSPCMethodArg(WriteDbData4SpringController springController, WriteDbData4MethodArgument methodArgument) {
        if (checkFieldName(methodArgument.getArgName())) {
            logger.info("处理Spring Controller某个方法的某个参数 {} {}", JACGJsonUtil.getJsonStr(springController), JACGJsonUtil.getJsonStr(methodArgument));
        }
    }

    // 处理Spring Controller某个方法的某个自定义类型参数中的常用数据类型的字段信息
    @Override
    protected void doHandleCommonFieldInfoInSPCCustomArg(WriteDbData4SpringController springController, WriteDbData4MethodArgument methodArgument,
                                                         CommonFieldInfoInClass commonFieldInfoInClass) {
        if (checkCommonFieldInfoInClass(commonFieldInfoInClass)) {
            logger.info("处理Spring Controller某个方法的某个自定义类型参数中的常用数据类型的字段信息 {} {} {}", JACGJsonUtil.getJsonStr(springController), JACGJsonUtil.getJsonStr(methodArgument),
                    JACGJsonUtil.getJsonStr(commonFieldInfoInClass));
        }
    }

    // 处理Spring Controller某个方法的某个参数的泛型类型中的常用数据类型的字段信息
    @Override
    protected void doHandleCommonFieldInfoInSPCArgGT(WriteDbData4SpringController springController, WriteDbData4MethodArgument methodArgument,
                                                     String methodArgGenericsType, CommonFieldInfoInClass commonFieldInfoInClass) {
        if (checkCommonFieldInfoInClass(commonFieldInfoInClass)) {
            logger.info("处理Spring Controller某个方法的某个参数的泛型类型中的常用数据类型的字段信息 {} {} {} {}", JACGJsonUtil.getJsonStr(springController), JACGJsonUtil.getJsonStr(methodArgument),
                    methodArgGenericsType, JACGJsonUtil.getJsonStr(commonFieldInfoInClass));
        }
    }

    // 处理Spring Controller某个方法的返回类型中的常用数据类型的字段信息
    @Override
    protected void doHandleCommonFieldInfoInSPCReturnType(WriteDbData4SpringController springController, WriteDbData4MethodInfo methodInfo,
                                                          CommonFieldInfoInClass commonFieldInfoInClass) {
        if (checkCommonFieldInfoInClass(commonFieldInfoInClass)) {
            logger.info("处理Spring Controller某个方法的返回类型中的常用数据类型的字段信息 {} {} {}", JACGJsonUtil.getJsonStr(springController), JACGJsonUtil.getJsonStr(methodInfo),
                    JACGJsonUtil.getJsonStr(commonFieldInfoInClass));
        }
    }

    // 处理Spring Controller某个方法的返回类型的泛型类型中的常用数据类型的字段信息
    @Override
    protected void doHandleCommonFieldInfoInSPCReturnTypeGT(WriteDbData4SpringController springController, WriteDbData4MethodInfo methodInfo, String methodReturnGenericsType,
                                                            CommonFieldInfoInClass commonFieldInfoInClass) {
        if (checkCommonFieldInfoInClass(commonFieldInfoInClass)) {
            logger.info("处理Spring Controller某个方法的返回类型的泛型类型中的常用数据类型的字段信息 {} {} {} {}", JACGJsonUtil.getJsonStr(springController), JACGJsonUtil.getJsonStr(methodInfo),
                    methodReturnGenericsType, JACGJsonUtil.getJsonStr(commonFieldInfoInClass));
        }
    }

    // 处理其他类中的常用数据类型的字段信息
    @Override
    protected void doHandleCommonFieldInfoInOtherClass(String className, CommonFieldInfoInClass commonFieldInfoInClass) {
        if (checkCommonFieldInfoInClass(commonFieldInfoInClass)) {
            logger.info("处理其他类中的常用数据类型的字段信息 {} {}", className, JACGJsonUtil.getJsonStr(commonFieldInfoInClass));
        }
    }

    // 处理其他传输协议字段
    @Override
    protected void handleOtherProtocolField(Set<String> customTypeSet) {
        // 查询有指定注解的类
        handleClassWithAnnotation(TestAnnotation1.class.getName(), customTypeSet);
    }

    // 查询有指定注解的类
    private void handleClassWithAnnotation(String annotationName, Set<String> customTypeSet) {
        List<String> classWithAnnotationList = annotationHandler.queryClassesWithAnnotation(false, annotationName);
        for (String classWithAnnotation : classWithAnnotationList) {
            Map<String, BaseAnnotationAttribute> annotationAttributeMap = null;
            List<CommonFieldInfoInClass> commonFieldInfoInClassList = fieldInfoHandler.queryAllCommonFieldInfoInClass(classWithAnnotation, true, true, true, customTypeSet);
            for (CommonFieldInfoInClass commonFieldInfoInClass : commonFieldInfoInClassList) {
                if (checkCommonFieldInfoInClass(commonFieldInfoInClass)) {
                    if (annotationAttributeMap == null) {
                        annotationAttributeMap = annotationHandler.queryAnnotationAttributes4Class(classWithAnnotation, annotationName);
                    }
                    String nameInAnnotation = AnnotationAttributesParseUtil.getAttributeStringValue(annotationAttributeMap, "name");
                    String descInAnnotation = AnnotationAttributesParseUtil.getAttributeStringValue(annotationAttributeMap, "desc");
                    logger.info("处理有指定注解的类的常用数据类型的字段信息 {} {} {} {}", classWithAnnotation, JACGJsonUtil.getJsonStr(commonFieldInfoInClass), nameInAnnotation, descInAnnotation);
                }
            }
        }
    }

    private boolean checkCommonFieldInfoInClass(CommonFieldInfoInClass commonFieldInfoInClass) {
        WriteDbData4FieldInfo fieldInfo = commonFieldInfoInClass.getFieldInfo();
        return checkFieldName(fieldInfo.getFieldName(), commonFieldInfoInClass.getJsonAlias());
    }

    private boolean checkFieldName(String... names) {
        for (String name : names) {
            if (StringUtils.containsIgnoreCase(name, "data")) {
                return true;
            }
        }
        return false;
    }
}
