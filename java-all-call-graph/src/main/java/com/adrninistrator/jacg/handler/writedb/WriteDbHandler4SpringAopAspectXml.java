package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringAopAspect;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlCodeParser;
import com.adrninistrator.jacg.util.JACGSpringUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description: 写入数据库，Spring AOP aspect信息，在XML中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = SpringXmlCodeParser.FILE_NAME_SPRING_AOP_ASPECT_XML,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_AOP_ASPECT
)
public class WriteDbHandler4SpringAopAspectXml extends AbstractWriteDbHandler<WriteDbData4SpringAopAspect> {

    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4SpringAopAspectXml.class);

    /*
        记录Spring Bean相关信息的Map
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    public WriteDbHandler4SpringAopAspectXml(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SpringAopAspect genData(String[] array) {
        String xmlAspectId = readLineData();
        String xmlAspectRef = readLineData();
        int order = Integer.parseInt(readLineData());
        String xmlPath = readLineData();

        String springBeanClassName = JACGSpringUtil.getSpringBeanClassName(springBeanMap, xmlAspectRef);
        if (springBeanClassName == null) {
            logger.error("根据XML中的Spring AOP aspect Bean的名称未获取到对应类名 {}", xmlAspectRef);
            throw new JavaCG2RuntimeException("根据XML中的Spring AOP aspect Bean的名称未获取到对应类名");
        }

        WriteDbData4SpringAopAspect writeDbData4SpringAopAspect = new WriteDbData4SpringAopAspect();
        writeDbData4SpringAopAspect.setRecordId(genNextRecordId());
        writeDbData4SpringAopAspect.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_XML);
        writeDbData4SpringAopAspect.setXmlAspectId(xmlAspectId);
        writeDbData4SpringAopAspect.setXmlAspectRef(xmlAspectRef);
        writeDbData4SpringAopAspect.setAspectOrder(order);
        writeDbData4SpringAopAspect.setClassName(springBeanClassName);
        writeDbData4SpringAopAspect.setDefineXmlPath(xmlPath);
        return writeDbData4SpringAopAspect;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringAopAspect data) {
        return JACGSqlUtil.genSpringAopAspectArray(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "XML中定义的aspect的ID",
                "XML中定义的aspect对应的Bean名称",
                "aspect排序数值",
                "XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring AOP aspect信息，在XML中定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括XML中定义的aspect的ID、对应的Bean名称、aspect排序数值、XML文件路径等"
        };
    }

    public Map<String, String> getSpringBeanMap() {
        return springBeanMap;
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}
