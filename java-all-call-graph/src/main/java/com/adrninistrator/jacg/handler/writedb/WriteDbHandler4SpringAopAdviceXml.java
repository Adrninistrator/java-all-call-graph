package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAdvice;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlCodeParser;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSpringUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.weaver.AdviceKind;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 写入数据库，Spring AOP advice信息，在XML中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = SpringXmlCodeParser.FILE_NAME_SPRING_AOP_ADVICE_XML,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_ADVICE,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_SPRING_AOP_ASPECT,
                DbTableInfoEnum.DTIE_SPRING_BEAN,
                DbTableInfoEnum.DTIE_METHOD_INFO}
)
public class WriteDbHandler4SpringAopAdviceXml extends AbstractWriteDbHandler<WriteDbData4SpringAopAdvice> {

    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4SpringAopAdviceXml.class);
    private SpringHandler springHandler;
    private MethodInfoHandler methodInfoHandler;

    public WriteDbHandler4SpringAopAdviceXml(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void init(WriteDbResult writeDbResult) {
        super.init(writeDbResult);
        springHandler = new SpringHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    @Override
    protected WriteDbData4SpringAopAdvice genData(String[] array) {
        String xmlAspectId = readLineData();
        String xmlAspectMethodName = readLineData();
        String adviceType = readLineData();
        String xmlPointcutRef = readLineData();
        boolean base64 = JavaCG2YesNoEnum.isYes(readLineData());
        String expression = readLineData();
        if (base64) {
            expression = JavaCG2Util.base64Decode(expression);
        }
        int aspectOrder = Integer.parseInt(readLineData());
        String xmlPath = readLineData();
        if (StringUtils.isNotBlank(xmlPointcutRef)) {
            // 根据XML中定义的pointcut ID查询对应的表达式
            expression = springHandler.queryExpressionByPointcutId(xmlPointcutRef);
            if (StringUtils.isBlank(expression)) {
                logger.error("根据XML中定义的pointcut ID未查询到对应的表达式，请检查对应的XML文件所在jar文件或目录是否有添加到指定解析范围的配置文件中 {} {}", xmlPointcutRef, xmlPath);
                throw new JavaCG2RuntimeException("根据XML中定义的pointcut ID未查询到对应的表达式");
            }
        }
        WriteDbData4SpringAopAdvice writeDbData4SpringAopAdvice = new WriteDbData4SpringAopAdvice();
        writeDbData4SpringAopAdvice.setRecordId(genNextRecordId());
        writeDbData4SpringAopAdvice.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_XML);
        writeDbData4SpringAopAdvice.setXmlAspectId(xmlAspectId);
        writeDbData4SpringAopAdvice.setXmlAspectMethodName(xmlAspectMethodName);
        AdviceKind adviceKind = JACGSpringUtil.getAdviceKindFromXmlElement(adviceType);
        writeDbData4SpringAopAdvice.setAdviceType(adviceKind.getName());
        writeDbData4SpringAopAdvice.setXmlPointcutRef(xmlPointcutRef);
        writeDbData4SpringAopAdvice.setExpression(expression);
        writeDbData4SpringAopAdvice.setAspectOrder(aspectOrder);

        // 根据XML中定义的aspect ID查询对应的Bean名称
        String aspectBeanName = springHandler.queryBeanNameByAspectId(xmlAspectId);
        if (StringUtils.isBlank(aspectBeanName)) {
            logger.error("根据XML中定义的aspect ID未查询到aspect对应的Bean名称，请检查对应的XML文件所在jar文件或目录、class所在jar文件是否有添加到指定解析范围的配置文件中 {} {}", xmlAspectId, xmlPath);
            throw new JavaCG2RuntimeException("根据XML中定义的aspect ID未查询到aspect对应的Bean名称");
        }
        String aspectClassName = springHandler.queryClassNameBySpringBeanName(aspectBeanName);
        if (StringUtils.isBlank(aspectClassName)) {
            logger.error("根据XML中定义的aspect Bean名称未查询到aspect对应的类名，请检查对应的XML文件所在jar文件或目录、class所在jar文件是否有添加到指定解析范围的配置文件中 {} {}", aspectBeanName, xmlPath);
            throw new JavaCG2RuntimeException("根据XML中定义的aspect Bean名称未查询到aspect对应的类名");
        }

        // 根据aspect类名与方法名查询对应的完整方法，查询定义在前面的
        List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodInfoByClassMethod(aspectClassName, xmlAspectMethodName);
        if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
            logger.error("根据aspect类名与方法名未查询到对应的完整方法，请检查对应的class所在jar文件是否有添加到指定解析范围的配置文件中 {} {}", aspectBeanName, xmlPath);
            throw new JavaCG2RuntimeException("根据aspect类名与方法名未查询到对应的完整方法");
        }
        WriteDbData4MethodInfo methodInfo = methodInfoList.get(0);
        writeDbData4SpringAopAdvice.setAdviceFullMethod(methodInfo.getFullMethod());
        writeDbData4SpringAopAdvice.setAdviceMethodReturnType(methodInfo.getReturnType());

        String adviceMethodHash = JACGClassMethodUtil.genMethodHashWithLen(methodInfo.getFullMethod(), methodInfo.getReturnType());

        writeDbData4SpringAopAdvice.setAdviceMethodHash(adviceMethodHash);
        writeDbData4SpringAopAdvice.setAspectClassName(methodInfo.getClassName());
        writeDbData4SpringAopAdvice.setDefineXmlPath(xmlPath);
        return writeDbData4SpringAopAdvice;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopAdvice data) {
        return JACGSqlUtil.genSpringAopAdviceArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "XML中定义的aspect的ID",
                "XML中定义的aspect的方法名",
                "XML中定义的advice XML元素的名称",
                "XML中的pointcut-ref名称",
                "pointcut表达式是否为base64格式",
                "pointcut表达式",
                "aspect排序数值",
                "XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring AOP advice信息，在XML中定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括XML中定义的aspect的ID、aspect的方法名、advice XML元素的名称表达式、pointcut-ref名称、pointcut表达式、XML文件路径等"
        };
    }
}
