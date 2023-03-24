package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，方法的信息
 */
public class WriteDbHandler4MethodInfo extends AbstractWriteDbHandler<WriteDbData4MethodInfo> {
    @Override
    protected WriteDbData4MethodInfo genData(String line) {
        String[] array = splitEquals(line, 2);

        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String accessFlags = array[1];
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodName = JACGClassMethodUtil.getMethodNameFromFull(fullMethod);
        return new WriteDbData4MethodInfo(JACGUtil.genHashWithLen(fullMethod),
                dbOperWrapper.getSimpleClassName(className),
                Integer.parseInt(accessFlags),
                methodName,
                fullMethod);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_METHOD_INFO;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodInfo data) {
        return new Object[]{
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getAccessFlags(),
                data.getMethodName(),
                data.getFullMethod()
        };
    }
}
