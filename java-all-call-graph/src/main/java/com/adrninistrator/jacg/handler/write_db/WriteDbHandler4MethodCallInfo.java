package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCallInfo;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，方法调用信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL_INFO,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_INFO
)
public class WriteDbHandler4MethodCallInfo extends AbstractWriteDbHandler<WriteDbData4MethodCallInfo> {
    // 被调用对象及参数存在信息的call_id
    private Set<Integer> withInfoCallIdSet;

    @Override
    protected WriteDbData4MethodCallInfo genData(String[] array) {
        int callId = Integer.parseInt(array[0]);
        String objArgsSeq = array[1];
        String seq = array[2];
        String type = array[3];
        int arrayFlag = Integer.parseInt(array[4]);
        String value;
        if (JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_BASE64_VALUE.equals(type)) {
            // bv类型数据需要进行base64解码
            value = JavaCGUtil.base64Decode(array[5]);
        } else {
            value = array[5];
        }

        // 记录被调用对象及参数存在信息的call_id
        withInfoCallIdSet.add(callId);
        return new WriteDbData4MethodCallInfo(callId,
                Integer.parseInt(objArgsSeq),
                Integer.parseInt(seq),
                type,
                arrayFlag,
                value);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallInfo data) {
        return new Object[]{
                data.getCallId(),
                data.getObjArgsSeq(),
                data.getSeq(),
                data.getType(),
                data.getArrayFlag(),
                data.getTheValue()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "方法调用序号",
                "被调用对象或参数序号，0代表被调用对象，1开始为参数",
                "序号，从0开始，大于0代表有多种可能",
                "类型，t:类型，v:值，bv:BASE64编码后的值，sf:静态字段，sfm:静态字段的方法",
                "是否为数组格式，1:是，0:否",
                "对应的值"
        };
    }

    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "方法调用信息，包括方法调用中被调用对象与参数可能的类型以及值"
        };
    }

    public void setWithInfoCallIdSet(Set<Integer> withInfoCallIdSet) {
        this.withInfoCallIdSet = withInfoCallIdSet;
    }
}
