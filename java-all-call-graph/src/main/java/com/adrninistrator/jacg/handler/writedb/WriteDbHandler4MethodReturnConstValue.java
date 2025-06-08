package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnConstValue;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;

/**
 * @author adrninistrator
 * @date 2025/1/8
 * @description: 写入数据库，方法返回的常量值（含null）
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_CONST_VALUE,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_RETURN_CONST_VALUE
)
public class WriteDbHandler4MethodReturnConstValue extends AbstractWriteDbHandler<WriteDbData4MethodReturnConstValue> {

    public WriteDbHandler4MethodReturnConstValue(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodReturnConstValue genData(String[] array) {
        String fullMethod = readLineData();
        String returnType = readLineData();
        WriteDbData4MethodReturnConstValue methodReturnConstValue = new WriteDbData4MethodReturnConstValue();
        methodReturnConstValue.setRecordId(genNextRecordId());
        methodReturnConstValue.setMethodHash(JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType));
        methodReturnConstValue.setSeq(Integer.parseInt(readLineData()));
        methodReturnConstValue.setConstType(readLineData());
        boolean isBase64Value = JavaCG2YesNoEnum.isYes(readLineData());
        String constValue = readLineData();
        if (isBase64Value) {
            constValue = JavaCG2Util.base64Decode(constValue);
        }
        methodReturnConstValue.setConstValue(constValue);
        methodReturnConstValue.setFullMethod(fullMethod);
        methodReturnConstValue.setReturnType(returnType);
        return methodReturnConstValue;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodReturnConstValue data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSeq(),
                data.getConstType(),
                data.getConstValue(),
                data.getFullMethod(),
                data.getReturnType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志",
                "某个方法返回的常量值序号，从0开始",
                "常量类型，含义参考 JavaCG2ConstantTypeEnum 类",
                "常量值是否有进行BASE64编码，1:是，0:否",
                "常量的值"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法返回的常量值，包括返回null的情况，属性有常量类型、常量值"
        };
    }
}


