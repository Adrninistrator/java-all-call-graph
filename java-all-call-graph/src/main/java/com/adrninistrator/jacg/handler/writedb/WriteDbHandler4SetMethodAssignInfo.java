package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.WriteDbHandlerWriteFileEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SetMethodAssignInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;

/**
 * @author adrninistrator
 * @date 2023/12/5
 * @description: 写入数据库，dto的set方法被调用时的赋值信息
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SET_METHOD_ASSIGN_INFO,
        writeFileEnum = WriteDbHandlerWriteFileEnum.WDHWFE_SET_METHOD_ASSIGN_INFO,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_FIELD_RELATIONSHIP}
)
public class WriteDbHandler4SetMethodAssignInfo extends AbstractWriteDbHandler<WriteDbData4SetMethodAssignInfo> {

    public WriteDbHandler4SetMethodAssignInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "set方法记录id，从1开始",
                "set方法被调用时的方法调用序号，从1开始",
                "set方法当前被调用时被赋值情况的序号，从0开始",
                "set方法当前被调用时被赋值时通过方法调用传递的步骤，从0开始",
                "字段关联关系id，从1开始",
                "当前的方法调用序号，从1开始",
                "调用方，方法hash+字节数",
                "调用方，完整方法（类名+方法名+参数）",
                "调用方法源代码行号",
                "被调用方，完整方法（类名+方法名+参数）",
                "set方法hash+字节数",
                "set方法完整方法（类名+方法名+参数）",
                "set方法是否在超类中，1:是，0:否",
                "set方法被调用时的赋值情况标志，见 SetMethodAssignFlagEnum 类",
                "set方法被调用时的赋值情况标志描述",
                "set方法被调用时的赋值信息",
                "是否属于等值转换前的数据，1:是，0:否"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "dto的set方法被调用时的赋值信息";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"包括通过方法调用的方法参数，及方法返回值传递数据的情况"};
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SetMethodAssignInfo data) {
        return new Object[]{
                data.getSetRecordId(),
                data.getSetMethodCallId(),
                data.getSeq(),
                data.getStep(),
                data.getFldRelationshipId(),
                data.getCurrCallId(),
                data.getCallerMethodHash(),
                data.getCallerFullMethod(),
                data.getCallerLineNumber(),
                data.getCalleeFullMethod(),
                data.getSetMethodHash(),
                data.getSetFullMethod(),
                data.getSetMethodInSuper(),
                data.getFlag(),
                data.getFlagDesc(),
                data.getAssignInfo(),
                data.getEquivalentConversion()
        };
    }
}
