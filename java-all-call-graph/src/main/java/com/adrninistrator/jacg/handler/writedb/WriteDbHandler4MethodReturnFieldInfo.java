package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnFieldInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;

/**
 * @author adrninistrator
 * @date 2025/1/8
 * @description: 写入数据库，方法返回的字段（含枚举）
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_FIELD_INFO,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_RETURN_FIELD_INFO
)
public class WriteDbHandler4MethodReturnFieldInfo extends AbstractWriteDbHandler<WriteDbData4MethodReturnFieldInfo> {

    public WriteDbHandler4MethodReturnFieldInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodReturnFieldInfo genData(String[] array) {
        String fullMethod = readLineData();
        WriteDbData4MethodReturnFieldInfo methodReturnFieldInfo = new WriteDbData4MethodReturnFieldInfo();
        methodReturnFieldInfo.setRecordId(genNextRecordId());
        methodReturnFieldInfo.setMethodHash(JACGUtil.genHashWithLen(fullMethod));
        methodReturnFieldInfo.setSeq(Integer.parseInt(readLineData()));
        methodReturnFieldInfo.setStaticField(JavaCG2YesNoEnum.isYes(readLineData()));
        methodReturnFieldInfo.setFieldOfThis(JavaCG2YesNoEnum.isYes(readLineData()));
        String fieldInClassName = readLineData();
        String fieldType = readLineData();
        methodReturnFieldInfo.setFieldInSimpleClassName(dbOperWrapper.querySimpleClassName(fieldInClassName));
        methodReturnFieldInfo.setFieldSimpleType(dbOperWrapper.querySimpleClassName(fieldType));
        methodReturnFieldInfo.setFieldArrayDimensions(Integer.parseInt(readLineData()));
        methodReturnFieldInfo.setFieldName(readLineData());
        methodReturnFieldInfo.setFieldInClassName(fieldInClassName);
        methodReturnFieldInfo.setFieldType(fieldType);
        methodReturnFieldInfo.setFullMethod(fullMethod);
        return methodReturnFieldInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodReturnFieldInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSeq(),
                data.isStaticField(),
                data.isFieldOfThis(),
                data.getFieldInSimpleClassName(),
                data.getFieldSimpleType(),
                data.getFieldArrayDimensions(),
                data.getFieldName(),
                data.getFieldInClassName(),
                data.getFieldType(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "某个方法返回的字段信息序号，从0开始",
                "方法返回的字段是否为静态，1:是，0:否",
                "方法返回的字段是否属于this对象，1:是，0:否",
                "方法返回的字段所在的类完整类名",
                "方法返回的字段类型完整类名",
                "方法返回的字段数组类型的维度，为0代表不是数组类型",
                "方法返回的字段名称"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法返回的字段信息，包括返回枚举的情况，属性有字段是否为静态、字段是否属于this对象、字段所在的类、字段的类型、字段的名称"
        };
    }
}
