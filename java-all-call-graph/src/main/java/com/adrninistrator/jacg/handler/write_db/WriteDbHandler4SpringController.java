package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringController;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: 写入数据库，Spring Controller信息
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_CONTROLLER
)
public class WriteDbHandler4SpringController extends AbstractWriteDbHandler<WriteDbData4SpringController> {

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
