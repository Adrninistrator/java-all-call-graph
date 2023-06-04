package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodAnnotation;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringController;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.jacg.util.spring.SpringMvcRequestMappingUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，方法的注解
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ANNOTATION,
        minColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE,
        maxColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ANNOTATION
)
public class WriteDbHandler4MethodAnnotation extends AbstractWriteDbHandler<WriteDbData4MethodAnnotation> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4MethodAnnotation.class);

    private Map<String, List<String>> classRequestMappingMap;

    // 将Spring Controller信息写入数据库的类
    private WriteDbHandler4SpringController writeDbHandler4SpringController;

    // Spring Controller对应的方法HASH+长度
    private Set<String> springControllerMethodHashSet = new HashSet<>();

    // 有注解的方法HASH+长度
    private Set<String> withAnnotationMethodHashSet = new HashSet<>();

    // Spring Controller相关信息
    private final List<WriteDbData4SpringController> writeDbData4SpringControllerList = new ArrayList<>(batchSize);

    public WriteDbHandler4MethodAnnotation(JavaCGOutputInfo javaCGOutputInfo) {
        super(javaCGOutputInfo);
    }

    @Override
    protected WriteDbData4MethodAnnotation genData(String[] array) {
        // 拆分时限制列数，最后一列注解属性中可能出现空格
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String annotationName = array[1];
        // 若当前行的注解信息无属性，注解属性名称设为空字符串
        String attributeName = "";
        String attributeType = null;
        String attributeValue = null;
        if (array.length > JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE) {
            // 当前行的注解信息有属性
            attributeName = array[2];
            attributeType = array[3];
            // 从文件记录解析注解属性
            attributeValue = AnnotationAttributesParseUtil.parseFromFile(attributeType, array[4]);
        }

        // 记录有注解的方法HASH+长度
        withAnnotationMethodHashSet.add(methodHash);

        // 处理Spring Controller相关注解
        boolean isSpringMappingAnnotation = handleSpringControllerAnnotation(methodHash, fullMethod, simpleClassName, annotationName, attributeName, attributeValue);

        WriteDbData4MethodAnnotation writeDbData4MethodAnnotation = new WriteDbData4MethodAnnotation();
        writeDbData4MethodAnnotation.setMethodHash(methodHash);
        writeDbData4MethodAnnotation.setAnnotationName(annotationName);
        writeDbData4MethodAnnotation.setAttributeName(attributeName);
        writeDbData4MethodAnnotation.setAnnotationType(attributeType);
        writeDbData4MethodAnnotation.setAttributeValue(attributeValue);
        writeDbData4MethodAnnotation.setFullMethod(fullMethod);
        writeDbData4MethodAnnotation.setSimpleClassName(simpleClassName);
        writeDbData4MethodAnnotation.setSpringMappingAnnotation(JavaCGYesNoEnum.parseIntValue(isSpringMappingAnnotation));
        return writeDbData4MethodAnnotation;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodAnnotation data) {
        return new Object[]{
                genNextRecordId(),
                data.getMethodHash(),
                data.getAnnotationName(),
                data.getAttributeName(),
                data.getAnnotationType(),
                data.getAttributeValue(),
                data.getFullMethod(),
                data.getSimpleClassName(),
                data.getSpringMappingAnnotation()
        };
    }

    /**
     * 处理Spring Controller相关注解
     *
     * @param methodHash
     * @param fullMethod
     * @param simpleClassName
     * @param annotationName
     * @param attributeName
     * @param attributeValue
     * @return false: 不是Spring Controller相关注解 true: 是Spring Controller相关注解
     */
    private boolean handleSpringControllerAnnotation(String methodHash, String fullMethod, String simpleClassName, String annotationName, String attributeName,
                                                     String attributeValue) {
        // 判断对应的类是否与Spring Controller相关
        List<String> classRequestMappingPathList = classRequestMappingMap.get(simpleClassName);
        if (classRequestMappingPathList == null || !SpringMvcRequestMappingUtil.isRequestMappingAnnotation(annotationName)) {
            // 当前类与Spring Controller相关，或当前方法的注解不是@RequestMapping
            return false;
        }

        if (!attributeName.isEmpty() && !SpringMvcRequestMappingUtil.isRequestMappingPathAttribute(attributeName)) {
            // 注解属性名称非空，且不是@RequestMapping注解的path属性
            return false;
        }

        // 记录Spring Controller对应的方法HASH+长度
        springControllerMethodHashSet.add(methodHash);

        if (classRequestMappingPathList.isEmpty()) {
            // 假如类的path列表为空，则创建为只有一个空字符串的列表
            classRequestMappingPathList = Collections.singletonList("");
        }

        List<String> methodPathList = null;
        if (attributeValue != null) {
            methodPathList = AnnotationAttributesParseUtil.parseListStringAttribute(attributeValue);
        }
        if (methodPathList == null || methodPathList.isEmpty()) {
            // 假如方法的path列表为空，则创建为只有一个空字符串的列表
            methodPathList = Collections.singletonList("");
        }

        int seq = 0;
        for (String classRequestMappingPath : classRequestMappingPathList) {
            for (String methodPath : methodPathList) {
                String showUri = SpringMvcRequestMappingUtil.genShowUri(classRequestMappingPath, methodPath);

                WriteDbData4SpringController writeDbData4SpringController = new WriteDbData4SpringController();
                writeDbData4SpringController.setMethodHash(methodHash);
                writeDbData4SpringController.setSeq(seq);
                writeDbData4SpringController.setShowUri(showUri);
                writeDbData4SpringController.setClassPath(classRequestMappingPath);
                writeDbData4SpringController.setMethodPath(methodPath);
                writeDbData4SpringController.setAnnotationName(annotationName);
                writeDbData4SpringController.setSimpleClassName(simpleClassName);
                writeDbData4SpringController.setFullMethod(fullMethod);

                logger.debug("找到Spring Controller信息 {}", writeDbData4SpringController);
                writeDbData4SpringControllerList.add(writeDbData4SpringController);
                // 尝试写入Spring Controller信息
                writeDbHandler4SpringController.tryInsertDb(writeDbData4SpringControllerList);
                seq++;
            }
        }
        return true;
    }

    @Override
    protected void beforeDone() {
        // 写入Spring Controller剩余信息
        writeDbHandler4SpringController.insertDb(writeDbData4SpringControllerList);
    }

    //
    public void setClassRequestMappingMap(Map<String, List<String>> classRequestMappingMap) {
        this.classRequestMappingMap = classRequestMappingMap;
    }

    public void setWriteDbHandler4SpringController(WriteDbHandler4SpringController writeDbHandler4SpringController) {
        this.writeDbHandler4SpringController = writeDbHandler4SpringController;
    }

    public void setSpringControllerMethodHashSet(Set<String> springControllerMethodHashSet) {
        this.springControllerMethodHashSet = springControllerMethodHashSet;
    }

    public void setWithAnnotationMethodHashSet(Set<String> withAnnotationMethodHashSet) {
        this.withAnnotationMethodHashSet = withAnnotationMethodHashSet;
    }
}
