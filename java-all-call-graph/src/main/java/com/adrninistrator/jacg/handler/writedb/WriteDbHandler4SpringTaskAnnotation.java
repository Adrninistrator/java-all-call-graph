package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringTask;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGSqlUtil;

/**
 * @author adrninistrator
 * @date 2024/3/24
 * @description: 写入数据库，Spring定时任务信息，通过注解定义
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_TASK,
        writeFileEnum = WriteDbHandlerWriteFileEnum.WDHWFE_SPRING_TASK_ANNOTATION
)
public class WriteDbHandler4SpringTaskAnnotation extends AbstractWriteDbHandler<WriteDbData4SpringTask> {

    public WriteDbHandler4SpringTaskAnnotation(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "记录id，从1开始",
                "方法hash+字节数",
                "Spring Bean的名称",
                "完整类名",
                "方法名",
                "类型，XML: 在XML文件中定义，annotation: 通过注解定义",
                "完整方法（类名+方法名+参数）"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring定时任务信息，通过注解定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"包括Spring Bean的名称、对应的方法详情信息"};
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringTask data) {
        return JACGSqlUtil.genWriteDbData4SpringTask(data, genNextRecordId());
    }
}
