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
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
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
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_ANNOTATION,
        minColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_3,
        maxColumnNum = JACGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE_6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ANNOTATION,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_CLASS_ANNOTATION}
)
public class WriteDbHandler4MethodAnnotation extends AbstractWriteDbHandler<WriteDbData4MethodAnnotation> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4MethodAnnotation.class);

    private Map<String, List<String>> classRequestMappingMap;

    // 将Spring Controller信息写入数据库的类
    private WriteDbHandler4SpringController writeDbHandler4SpringController;

    // 将通过注解定义的Spring Task信息写入数据库的类
    private WriteDbHandler4SpringTaskJava writeDbHandler4SpringTaskJava;

    // 有注解的方法HASH+长度
    private Set<String> withAnnotationMethodHashSet = new HashSet<>();

    public WriteDbHandler4MethodAnnotation(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void beforeHandle(String javaCG2OutputPath) throws FileNotFoundException {
        super.beforeHandle(javaCG2OutputPath);

        writeDbHandler4SpringController.beforeHandle(javaCG2OutputPath);
        writeDbHandler4SpringTaskJava.beforeHandle(javaCG2OutputPath);
    }

    @Override
    public void afterHandle() {
        super.afterHandle();

        writeDbHandler4SpringController.afterHandle();
        writeDbHandler4SpringTaskJava.afterHandle();
    }

    @Override
    protected WriteDbData4MethodAnnotation genData(String[] array) {
        // 拆分时限制列数，最后一列注解属性中可能出现空格
        String fullMethod = readLineData();
        String returnType = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
        String annotationName = readLineData();
        // 若当前行的注解信息无属性，注解属性名称设为空字符串
        String attributeName = "";
        String attributeType = null;
        String attributeValue = null;
        if (array.length > JACGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE_3) {
            // 当前行的注解信息有属性
            attributeName = readLineData();
            attributeType = readLineData();
            // 从文件记录解析注解属性
            attributeValue = AnnotationAttributesParseUtil.parseFromFile(attributeType, readLineData());
        }

        // 记录有注解的方法HASH+长度
        withAnnotationMethodHashSet.add(methodHash);

        // 处理Spring Controller相关注解
        handleSpringControllerAnnotation(methodHash, fullMethod, returnType, simpleClassName, annotationName, attributeName, attributeValue);

        // 处理Spring Task相关注解
        handleSpringTaskAnnotation(fullMethod, returnType, annotationName);

        WriteDbData4MethodAnnotation writeDbData4MethodAnnotation = new WriteDbData4MethodAnnotation();
        writeDbData4MethodAnnotation.setRecordId(genNextRecordId());
        writeDbData4MethodAnnotation.setMethodHash(methodHash);
        writeDbData4MethodAnnotation.setAnnotationName(annotationName);
        writeDbData4MethodAnnotation.setAttributeName(attributeName);
        writeDbData4MethodAnnotation.setAnnotationType(attributeType);
        writeDbData4MethodAnnotation.setAttributeValue(attributeValue);
        writeDbData4MethodAnnotation.setFullMethod(fullMethod);
        writeDbData4MethodAnnotation.setReturnType(returnType);
        writeDbData4MethodAnnotation.setSimpleClassName(simpleClassName);
        return writeDbData4MethodAnnotation;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodAnnotation data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getAnnotationName(),
                data.getAttributeName(),
                data.getAnnotationType(),
                data.getAttributeValue(),
                data.getFullMethod(),
                data.getReturnType(),
                data.getSimpleClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志",
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
     * @param returnType
     * @param simpleClassName
     * @param annotationName
     * @param attributeName
     * @param attributeValue
     */
    private void handleSpringControllerAnnotation(String methodHash, String fullMethod, String returnType, String simpleClassName, String annotationName, String attributeName,
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
                writeDbData4SpringController.setRecordId(writeDbHandler4SpringController.genNextRecordId());
                writeDbData4SpringController.setMethodHash(methodHash);
                writeDbData4SpringController.setSeq(seq);
                writeDbData4SpringController.setShowUri(showUri);
                writeDbData4SpringController.setClassPath(classRequestMappingPath);
                writeDbData4SpringController.setMethodPath(methodPath);
                writeDbData4SpringController.setAnnotationName(annotationName);
                writeDbData4SpringController.setSimpleClassName(simpleClassName);
                writeDbData4SpringController.setFullMethod(fullMethod);
                writeDbData4SpringController.setReturnType(returnType);

                logger.debug("找到Spring Controller信息 {}", writeDbData4SpringController);
                writeDbHandler4SpringController.addData(writeDbData4SpringController);
                // 尝试写入Spring Controller信息
                writeDbHandler4SpringController.tryInsertDb();
                seq++;
            }
        }
    }

    // 处理Spring Task相关注解
    private void handleSpringTaskAnnotation(String fullMethod, String returnType, String annotationName) {
        if (!JACGSpringUtil.isTaskAnnotation(annotationName)) {
            // 当前注解不是Spring Task的注解
            return;
        }

        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethod);
        WriteDbData4SpringTask writeDbData4SpringTask = new WriteDbData4SpringTask();
        writeDbData4SpringTask.setRecordId(writeDbHandler4SpringTaskJava.genNextRecordId());
        writeDbData4SpringTask.setMethodHash(JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType));
        writeDbData4SpringTask.setSpringBeanName("");
        writeDbData4SpringTask.setClassName(className);
        writeDbData4SpringTask.setMethodName(methodName);
        writeDbData4SpringTask.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_JAVA);
        writeDbData4SpringTask.setFullMethod(fullMethod);
        writeDbData4SpringTask.setReturnType(returnType);
        writeDbData4SpringTask.setDefineClassNameXmlPath(className);
        writeDbHandler4SpringTaskJava.addData(writeDbData4SpringTask);
        writeDbHandler4SpringTaskJava.tryInsertDb();
    }

    //
    public void setClassRequestMappingMap(Map<String, List<String>> classRequestMappingMap) {
        this.classRequestMappingMap = classRequestMappingMap;
    }

    public void setWriteDbHandler4SpringController(WriteDbHandler4SpringController writeDbHandler4SpringController) {
        this.writeDbHandler4SpringController = writeDbHandler4SpringController;
    }

    public void setWriteDbHandler4SpringTaskJava(WriteDbHandler4SpringTaskJava writeDbHandler4SpringTaskJava) {
        this.writeDbHandler4SpringTaskJava = writeDbHandler4SpringTaskJava;
    }

    public void setWithAnnotationMethodHashSet(Set<String> withAnnotationMethodHashSet) {
        this.withAnnotationMethodHashSet = withAnnotationMethodHashSet;
    }
}
