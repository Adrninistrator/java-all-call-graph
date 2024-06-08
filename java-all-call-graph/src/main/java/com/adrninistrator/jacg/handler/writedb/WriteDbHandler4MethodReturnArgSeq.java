package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnArgSeq;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/11/4
 * @description: 写入数据库，方法返回值对应的方法参数序号
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_RETURN_ARG_SEQ,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_RETURN_ARG_SEQ
)
public class WriteDbHandler4MethodReturnArgSeq extends AbstractWriteDbHandler<WriteDbData4MethodReturnArgSeq> {

    public WriteDbHandler4MethodReturnArgSeq(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodReturnArgSeq genData(String[] array) {
        String callerFullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(callerFullMethod)) {
            return null;
        }

        int returnArgSeq = Integer.parseInt(array[1]);
        String callerMethodHash = JACGUtil.genHashWithLen(callerFullMethod);
        int equivalentConversion = Integer.parseInt(array[2]);
        return new WriteDbData4MethodReturnArgSeq(callerMethodHash, returnArgSeq, callerFullMethod, equivalentConversion);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodReturnArgSeq data) {
        return new Object[]{
                data.getCallerMethodHash(),
                data.getReturnArgSeq(),
                data.getCallerFullMethod(),
                data.getEquivalentConversion()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回值对应的方法参数序号，从0开始",
                "是否返回等值转换前的方法参数，1:是，0:否"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法返回值对应的方法参数序号信息，即调用方法以当前方法的参数作为自己的返回值时，记录对应关系"
        };
    }
}
