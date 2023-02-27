package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringBean;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，Spring Bean信息
 */
public class WriteDbHandler4SpringBean extends AbstractWriteDbHandler<WriteDbData4SpringBean> {
    /*
        记录Spring Bean相关信息
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    @Override
    protected WriteDbData4SpringBean genData(String line) {
        String[] array = splitEquals(line, 3);

        String springBeanName = array[0];
        String seq = array[1];
        String className = array[2];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }
        springBeanMap.put(springBeanName, className);
        return new WriteDbData4SpringBean(genNextRecordId(), springBeanName, Integer.parseInt(seq), className);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_SPRING_BEAN;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringBean data) {
        return new Object[]{
                data.getRecordId(),
                data.getSpringBeanName(),
                data.getSeq(),
                data.getClassName()
        };
    }

    public Map<String, String> getSpringBeanMap() {
        return springBeanMap;
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}
