package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgument;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 写入数据库，方法的参数类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ARGUMENT,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ARGUMENT
)
public class WriteDbHandler4MethodArgument extends AbstractWriteDbHandler<WriteDbData4MethodArgument> {

    public WriteDbHandler4MethodArgument(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodArgument genData(String[] array) {
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        int argSeq = Integer.parseInt(array[1]);
        String argType = array[2];
        String argName = array[3];

        return new WriteDbData4MethodArgument(methodHash,
                argSeq,
                dbOperWrapper.querySimpleClassName(argType),
                argName,
                argType,
                dbOperWrapper.querySimpleClassName(className),
                fullMethod);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodArgument data) {
        return new Object[]{
                data.getMethodHash(),
                data.getArgSeq(),
                data.getSimpleArgType(),
                data.getArgName(),
                data.getArgType(),
                data.getSimpleClassName(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "参数序号，从0开始",
                "参数类型",
                "参数名称，可能为空"
        };
    }


    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法参数的类型及名称，参数名称可能为空"
        };
    }
}
