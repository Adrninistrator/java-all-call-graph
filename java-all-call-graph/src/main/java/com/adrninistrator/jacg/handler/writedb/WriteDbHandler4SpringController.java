package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringController;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: 写入数据库，Spring Controller信息
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_CONTROLLER,
        writeFileEnum = WriteDbHandlerWriteFileEnum.WDHWFE_SPRING_CONTROLLER
)
public class WriteDbHandler4SpringController extends AbstractWriteDbHandler<WriteDbData4SpringController> {

    public WriteDbHandler4SpringController(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法hash+字节数",
                "序号，从0开始，大于0代表有多种可能",
                "用于显示的URI",
                "类上的注解path属性原始值",
                "方法上的注解path属性原始值",
                "注解类名",
                "唯一类名",
                "完整方法（类名+方法名+参数）"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring Controller信息";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"通过类及方法注解定义的Spring Controller信息，包括对应的方法、Controller的URI"};
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringController data) {
        return new Object[]{
                data.getMethodHash(),
                data.getSeq(),
                data.getShowUri(),
                data.getClassPath(),
                data.getMethodPath(),
                data.getAnnotationName(),
                data.getSimpleClassName(),
                data.getFullMethod()
        };
    }
}
