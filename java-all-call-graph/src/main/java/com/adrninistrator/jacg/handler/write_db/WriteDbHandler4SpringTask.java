package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringTask;
import com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file.SpringTaskCodeParser;
import com.adrninistrator.jacg.util.JACGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 写入数据库，Spring定时任务信息
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = SpringTaskCodeParser.FILE_NAME,
        minColumnNum = 2,
        maxColumnNum = 2,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_TASK
)
public class WriteDbHandler4SpringTask extends AbstractWriteDbHandler<WriteDbData4SpringTask> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4SpringTask.class);

    /*
        记录Spring Bean相关信息
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    @Override
    protected WriteDbData4SpringTask genData(String[] array) {
        String springBeanName = array[0];
        String springBeanClassName = springBeanMap.get(springBeanName);
        if (springBeanClassName == null) {
            // 假如根据Spring Bean名称未获取到对应的类名，则将首字段修改为小写后继续尝试获取
            String springBeanNameLower = JACGUtil.getFirstLetterLowerClassName(springBeanName);
            springBeanClassName = springBeanMap.get(springBeanNameLower);
        }

        if (springBeanClassName == null) {
            logger.error("未获取到Spring Bean的名称 {}", springBeanName);
            return null;
        }

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(springBeanClassName)) {
            return null;
        }

        String methodName = array[1];
        return new WriteDbData4SpringTask(springBeanName, springBeanClassName, methodName);
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

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "Spring Bean的名称",
                "方法名"
        };
    }

    @Override
    public String chooseOtherFileDesc() {
        return "Spring定时任务信息表";
    }

    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "Spring定时任务信息，包括Bean的名称及方法名称"
        };
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}