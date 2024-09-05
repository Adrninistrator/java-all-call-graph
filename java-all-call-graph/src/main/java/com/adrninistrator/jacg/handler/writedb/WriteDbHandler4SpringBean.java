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
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_BEAN
)
public class WriteDbHandler4SpringBean extends AbstractWriteDbHandler<WriteDbData4SpringBean> {
    /*
        记录Spring Bean相关信息
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
        String springBeanName = array[0];
        String seq = array[1];
        String className = array[2];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }
        String beanType = array[3];
        springBeanMap.put(springBeanName, className);
        WriteDbData4SpringBean writeDbData4SpringBean = new WriteDbData4SpringBean();
        writeDbData4SpringBean.setRecordId(genNextRecordId());
        writeDbData4SpringBean.setSpringBeanName(springBeanName);
        writeDbData4SpringBean.setSeq(Integer.parseInt(seq));
        writeDbData4SpringBean.setClassName(className);
        writeDbData4SpringBean.setBeanType(beanType);
        return writeDbData4SpringBean;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringBean data) {
        return new Object[]{
                data.getRecordId(),
                data.getSpringBeanName(),
                data.getSeq(),
                data.getClassName(),
                data.getBeanType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "Spring Bean的名称",
                "序号，从0开始，大于0代表有多种可能",
                "完整类名",
                "Spring Bean的定义方式，j: 在Java代码中定义，x: 在XML文件中定义"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "Spring Bean信息，包括Bean的名称及完整类名"
        };
    }

    public Map<String, String> getSpringBeanMap() {
        return springBeanMap;
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}
