package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringTaskXmlCodeParser;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 写入数据库，Spring定时任务信息，通过XML定义
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = SpringTaskXmlCodeParser.FILE_NAME,
        minColumnNum = 2,
        maxColumnNum = 2,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_TASK,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_SPRING_BEAN}
)
public class WriteDbHandler4SpringTaskXml extends AbstractWriteDbHandler<WriteDbData4SpringTask> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4SpringTaskXml.class);

    /*
        记录Spring Bean相关信息
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    public WriteDbHandler4SpringTaskXml(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

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
        String fullMethod = JavaCG2ClassMethodUtil.formatFullMethodNoArgs(springBeanClassName, methodName);

        WriteDbData4SpringTask writeDbData4SpringTask = new WriteDbData4SpringTask();
        writeDbData4SpringTask.setRecordId(genNextRecordId());
        writeDbData4SpringTask.setMethodHash(JACGUtil.genHashWithLen(fullMethod));
        writeDbData4SpringTask.setSpringBeanName(springBeanName);
        writeDbData4SpringTask.setClassName(springBeanClassName);
        writeDbData4SpringTask.setMethodName(methodName);
        writeDbData4SpringTask.setType(JACGConstants.SPRING_TASK_TYPE_XML);
        writeDbData4SpringTask.setFullMethod(fullMethod);
        return writeDbData4SpringTask;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringTask data) {
        return JACGSqlUtil.genWriteDbData4SpringTask(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "Spring Bean的名称",
                "方法名"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring定时任务信息，通过XML定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括Spring定时任务Bean的名称及方法名称"
        };
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}