package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassSignatureGenerics;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/6/20
 * @description: 写入数据库，类的签名中的泛型信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_CLASS_SIGNATURE_GENERICS,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_SIGNATURE_GENERICS
)
public class WriteDbHandler4ClassSignatureGenerics extends AbstractWriteDbHandler<WriteDbData4ClassSignatureGenerics> {

    public WriteDbHandler4ClassSignatureGenerics(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ClassSignatureGenerics genData(String[] array) {
        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }
        int seq = Integer.parseInt(array[1]);
        String genericsName = array[2];
        String genericsExtendsClassName = array[3];

        return new WriteDbData4ClassSignatureGenerics(dbOperWrapper.getSimpleClassName(className), seq, genericsName, genericsExtendsClassName, className);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassSignatureGenerics data) {
        return new Object[]{
                genNextRecordId(),
                data.getSimpleClassName(),
                data.getSeq(),
                data.getGenericsName(),
                data.getGenericsExtendsClassName(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "序号，从0开始",
                "签名中的泛型名称",
                "签名中的泛型的父类类名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类的签名中的泛型信息",
                "例如：“interface CommonMapper<S, T1> extends BaseMapper<T1>”，对应信息中会包含 S、T1"
        };
    }
}
