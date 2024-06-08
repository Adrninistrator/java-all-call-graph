package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.annotation.util.AnnotationAttributesParseUtil;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodAnnotation;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSpringUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileNotFoundException;
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
        minColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_2,
        maxColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE_5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ANNOTATION
)
public class WriteDbHandler4MethodAnnotation extends AbstractWriteDbHandler<WriteDbData4MethodAnnotation> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4MethodAnnotation.class);

    private Map<String, List<String>> classRequestMappingMap;

    // 将Spring Controller信息写入数据库的类
    private WriteDbHandler4SpringController writeDbHandler4SpringController;

    // 将通过注解定义的Spring Task信息写入数据库的类
    private WriteDbHandler4SpringTaskAnnotation writeDbHandler4SpringTaskAnnotation;

    // 有注解的方法HASH+长度
    private Set<String> withAnnotationMethodHashSet = new HashSet<>();

    public WriteDbHandler4MethodAnnotation(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void beforeHandle(String javaCgOutputPath) throws FileNotFoundException {
        super.beforeHandle(javaCgOutputPath);

        writeDbHandler4SpringController.beforeHandle(javaCgOutputPath);
        writeDbHandler4SpringTaskAnnotation.beforeHandle(javaCgOutputPath);
    }

    @Override
    public void afterHandle() {
        super.afterHandle();

        writeDbHandler4SpringController.afterHandle();
        writeDbHandler4SpringTaskAnnotation.afterHandle();
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
        if (array.length > JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_2) {
            // 当前行的注解信息有属性
            attributeName = array[2];
            attributeType = array[3];
            // 从文件记录解析注解属性
            attributeValue = AnnotationAttributesParseUtil.parseFromFile(attributeType, array[4]);
        }

        // 记录有注解的方法HASH+长度
        withAnnotationMethodHashSet.add(methodHash);

        // 处理Spring Controller相关注解
        handleSpringControllerAnnotation(methodHash, fullMethod, simpleClassName, annotationName, attributeName, attributeValue);

        // 处理Spring Task相关注解
        handleSpringTaskAnnotation(fullMethod, annotationName);

        WriteDbData4MethodAnnotation writeDbData4MethodAnnotation = new WriteDbData4MethodAnnotation();
        writeDbData4MethodAnnotation.setMethodHash(methodHash);
        writeDbData4MethodAnnotation.setAnnotationName(annotationName);
        writeDbData4MethodAnnotation.setAttributeName(attributeName);
        writeDbData4MethodAnnotation.setAnnotationType(attributeType);
        writeDbData4MethodAnnotation.setAttributeValue(attributeValue);
        writeDbData4MethodAnnotation.setFullMethod(fullMethod);
        writeDbData4MethodAnnotation.setSimpleClassName(simpleClassName);
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
                data.getSimpleClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "注解类名",
                "注解属性名称，空字符串代表无注解属性",
                "注解属性类型，参考AnnotationAttributesTypeEnum类",
                "注解属性值"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法上指定的注解信息",
                "若注解没有属性值，则相关字段为空",
                "若注解有属性值，则每个属性值占一行"
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
     */
    private void handleSpringControllerAnnotation(String methodHash, String fullMethod, String simpleClassName, String annotationName, String attributeName,
                                                  String attributeValue) {
        // 判断对应的类是否与Spring Controller相关
        List<String> classRequestMappingPathList = classRequestMappingMap.get(simpleClassName);
        if (classRequestMappingPathList == null || !JACGSpringUtil.isRequestMappingAnnotation(annotationName)) {
            // 当前类与Spring Controller无关，或当前方法的注解不是@RequestMapping
            return;
        }

        if (!attributeName.isEmpty() && !JACGSpringUtil.isRequestMappingPathAttribute(attributeName)) {
            // 注解属性名称非空，且不是@RequestMapping注解的path属性
            return;
        }

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
                String showUri = JACGSpringUtil.genSpringControllerShowUri(classRequestMappingPath, methodPath);

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
                writeDbHandler4SpringController.addData(writeDbData4SpringController);
                // 尝试写入Spring Controller信息
                writeDbHandler4SpringController.tryInsertDb();
                seq++;
            }
        }
    }

    // 处理Spring Task相关注解
    private void handleSpringTaskAnnotation(String fullMethod, String annotationName) {
        if (!JACGSpringUtil.isTaskAnnotation(annotationName)) {
            // 当前注解不是Spring Task的注解
            return;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        WriteDbData4SpringTask writeDbData4SpringTask = new WriteDbData4SpringTask();
        writeDbData4SpringTask.setMethodHash(JACGUtil.genHashWithLen(fullMethod));
        writeDbData4SpringTask.setSpringBeanName("");
        writeDbData4SpringTask.setClassName(className);
        writeDbData4SpringTask.setMethodName(methodName);
        writeDbData4SpringTask.setType(JACGConstants.SPRING_TASK_TYPE_ANNOTATION);
        writeDbData4SpringTask.setFullMethod(fullMethod);
        writeDbHandler4SpringTaskAnnotation.addData(writeDbData4SpringTask);
        writeDbHandler4SpringTaskAnnotation.tryInsertDb();
    }

    //
    public void setClassRequestMappingMap(Map<String, List<String>> classRequestMappingMap) {
        this.classRequestMappingMap = classRequestMappingMap;
    }

    public void setWriteDbHandler4SpringController(WriteDbHandler4SpringController writeDbHandler4SpringController) {
        this.writeDbHandler4SpringController = writeDbHandler4SpringController;
    }

    public void setWriteDbHandler4SpringTaskAnnotation(WriteDbHandler4SpringTaskAnnotation writeDbHandler4SpringTaskAnnotation) {
        this.writeDbHandler4SpringTaskAnnotation = writeDbHandler4SpringTaskAnnotation;
    }

    public void setWithAnnotationMethodHashSet(Set<String> withAnnotationMethodHashSet) {
        this.withAnnotationMethodHashSet = withAnnotationMethodHashSet;
    }
}
