package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCallInfo;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，方法调用信息
 */
public class WriteDbHandler4MethodCallInfo extends AbstractWriteDbHandler<WriteDbData4MethodCallInfo> {
    // 被调用对象及参数存在信息的call_id
    private final Set<Integer> withInfoCallIdSet = new HashSet<>();

    @Override
    protected WriteDbData4MethodCallInfo genData(String line) {
        String[] array = splitEquals(line, 5);

        int callId = Integer.parseInt(array[0]);
        String objArgsSeq = array[1];
        String type = array[2];
        String seq = array[3];
        String value;
        if (JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_BASE64_VALUE.equals(type)) {
            // bv类型数据需要进行base64解码
            value = JavaCGUtil.base64Decode(array[4]);
        } else {
            value = array[4];
        }

        // 记录被调用对象及参数存在信息的call_id
        withInfoCallIdSet.add(callId);
        return new WriteDbData4MethodCallInfo(callId,
                Integer.parseInt(objArgsSeq),
                type,
                Integer.parseInt(seq),
                value);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_METHOD_CALL_INFO;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallInfo data) {
        return new Object[]{
                data.getCallId(),
                data.getObjArgsSeq(),
                data.getType(),
                data.getSeq(),
                data.getTheValue()
        };
    }

    public Set<Integer> getWithInfoCallIdSet() {
        return withInfoCallIdSet;
    }
}
