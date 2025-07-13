package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlCodeParser;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.util.JACGSpringUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 写入数据库，Spring定时任务信息，通过XML定义
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = SpringXmlCodeParser.FILE_NAME_SPRING_TASK_XML,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_TASK,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_SPRING_BEAN,
                DbTableInfoEnum.DTIE_METHOD_INFO}
)
public class WriteDbHandler4SpringTaskXml extends AbstractWriteDbHandler<WriteDbData4SpringTask> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4SpringTaskXml.class);

    /*
        记录Spring Bean相关信息的Map
        key
            Spring Bean名称
        value
            Spring Bean对应类名
     */
    private Map<String, String> springBeanMap;

    private MethodInfoHandler methodInfoHandler;

    public WriteDbHandler4SpringTaskXml(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void init(WriteDbResult writeDbResult) {
        super.init(writeDbResult);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    @Override
    protected WriteDbData4SpringTask genData(String[] array) {
        String springBeanName = readLineData();
        String springBeanClassName = JACGSpringUtil.getSpringBeanClassName(springBeanMap, springBeanName);
        if (springBeanClassName == null) {
            logger.warn("根据Spring Bean名称未获取到类名 {}", springBeanName);
            springBeanClassName = "";
        }

        String methodName = readLineData();
        String defineClassNameXmlPath = readLineData();
        String fullMethod = JavaCG2ClassMethodUtil.formatFullMethodNoArgs(springBeanClassName, methodName);

        List<WriteDbData4MethodInfo> methodInfoList = methodInfoHandler.queryMethodInfoByClassMethod(springBeanClassName, methodName);
        if (JavaCG2Util.isCollectionEmpty(methodInfoList)) {
            logger.error("Spring Task对应方法未查询到 {} {}", springBeanClassName, methodName);
            return null;
        }
        if (methodInfoList.size() > 1) {
            logger.error("Spring Task对应方法查询到多个 {} {} {}", springBeanClassName, methodName, JACGJsonUtil.getJsonStr(methodInfoList));
            return null;
        }
        WriteDbData4MethodInfo methodInfo = methodInfoList.get(0);
        WriteDbData4SpringTask writeDbData4SpringTask = new WriteDbData4SpringTask();
        writeDbData4SpringTask.setRecordId(genNextRecordId());
        writeDbData4SpringTask.setMethodHash(methodInfo.getMethodHash());
        writeDbData4SpringTask.setSpringBeanName(springBeanName);
        writeDbData4SpringTask.setClassName(springBeanClassName);
        writeDbData4SpringTask.setMethodName(methodName);
        writeDbData4SpringTask.setType(JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_XML);
        writeDbData4SpringTask.setFullMethod(fullMethod);
        writeDbData4SpringTask.setReturnType(methodInfo.getReturnType());
        writeDbData4SpringTask.setDefineClassNameXmlPath(defineClassNameXmlPath);
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
                "方法名",
                "在Java代码中定义时所在的类名，或在XML中定义时对应的文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring定时任务信息，通过XML定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包括Spring定时任务Bean的名称及方法名称、XML文件路径"
        };
    }

    public void setSpringBeanMap(Map<String, String> springBeanMap) {
        this.springBeanMap = springBeanMap;
    }
}