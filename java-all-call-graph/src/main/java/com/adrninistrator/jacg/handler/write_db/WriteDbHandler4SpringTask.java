package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringTask;
import com.adrninistrator.jacg.util.JACGUtil;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 写入数据库，Spring定时任务信息
 */
public class WriteDbHandler4SpringTask extends AbstractWriteDbHandler<WriteDbData4SpringTask> {
    /*
        记录Spring Bean相关信息
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    @Override
    protected WriteDbData4SpringTask genData(String line) {
        String[] array = splitEquals(line, 2);

        String springBeanName = array[0];
        String springBeanClassName = springBeanMap.get(springBeanName);
        if (springBeanClassName == null) {
            // 假如根据Spring Bean名称未获取到对应的类名，则将首字段修改为小写后继续尝试获取
            String springBeanNameLower = JACGUtil.getFirstLetterLowerClassName(springBeanName);
            springBeanClassName = springBeanMap.get(springBeanNameLower);
        }

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(springBeanClassName)) {
            return null;
        }

        String methodName = array[1];
        return new WriteDbData4SpringTask(springBeanName, springBeanClassName, methodName);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_SPRING_TASK;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringTask data) {
        return new Object[]{
                genNextRecordId(),
                data.getSpringBeanName(),
                data.getClassName(),
                data.getMethodName()
        };
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}