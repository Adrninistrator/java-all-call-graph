package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringBean;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，Spring Bean信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_BEAN,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_BEAN
)
public class WriteDbHandler4SpringBean extends AbstractWriteDbHandler<WriteDbData4SpringBean> {
    /*
        记录Spring Bean相关信息的Map
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    public WriteDbHandler4SpringBean(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SpringBean genData(String[] array) {
        String springBeanName = readLineData();
        String seq = readLineData();
        String className = readLineData();
        String beanType = readLineData();
        String annotationClassName = readLineData();
        String defineClassNameXmlPath = readLineData();
        springBeanMap.put(springBeanName, className);
        WriteDbData4SpringBean writeDbData4SpringBean = new WriteDbData4SpringBean();
        writeDbData4SpringBean.setRecordId(genNextRecordId());
        writeDbData4SpringBean.setSpringBeanName(springBeanName);
        writeDbData4SpringBean.setSeq(Integer.parseInt(seq));
        writeDbData4SpringBean.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4SpringBean.setClassName(className);
        writeDbData4SpringBean.setBeanType(beanType);
        writeDbData4SpringBean.setAnnotationClassName(annotationClassName);
        writeDbData4SpringBean.setDefineClassNameXmlPath(defineClassNameXmlPath);
        return writeDbData4SpringBean;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringBean data) {
        return new Object[]{
                data.getRecordId(),
                data.getSpringBeanName(),
                data.getSeq(),
                data.getSimpleClassName(),
                data.getClassName(),
                data.getBeanType(),
                data.getAnnotationClassName(),
                data.getDefineClassNameXmlPath()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "Spring Bean的名称",
                "序号，从0开始，大于0代表有多种可能",
                "完整类名",
                "Spring Bean的定义方式，j: 在Java代码中定义，x: 在XML文件中定义",
                "在Java代码中定义时对应的注解类名",
                "在Java代码中定义时所在的类名，或在XML中定义时对应的文件路径"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "Spring Bean信息，包括Bean的名称及完整类名"
        };
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}
