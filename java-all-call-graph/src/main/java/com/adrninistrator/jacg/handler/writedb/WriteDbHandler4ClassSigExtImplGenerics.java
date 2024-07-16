package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassSigExtImplGenerics;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/7/16
 * @description: 写入数据库，类的签名中继承或实现的泛型关系
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_CLASS_SIG_EXT_IMPL_GENERICS,
        minColumnNum = 7,
        maxColumnNum = 7,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_SIG_EXT_IMPL_GENERICS
)
public class WriteDbHandler4ClassSigExtImplGenerics extends AbstractWriteDbHandler<WriteDbData4ClassSigExtImplGenerics> {

    public WriteDbHandler4ClassSigExtImplGenerics(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ClassSigExtImplGenerics genData(String[] array) {
        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }
        String genericsName = array[1];
        int seq = Integer.parseInt(array[2]);
        String extType = array[3];
        String superItfClassName = array[4];
        String superItfGenericsExtendsClassName = array[5];
        int superItfSeq = Integer.parseInt(array[6]);

        WriteDbData4ClassSigExtImplGenerics writeDbData4ClassSigExtImplGenerics = new WriteDbData4ClassSigExtImplGenerics();
        writeDbData4ClassSigExtImplGenerics.setSimpleClassName(dbOperWrapper.getSimpleClassName(className));
        writeDbData4ClassSigExtImplGenerics.setGenericsName(genericsName);
        writeDbData4ClassSigExtImplGenerics.setSeq(seq);
        writeDbData4ClassSigExtImplGenerics.setExtType(extType);
        writeDbData4ClassSigExtImplGenerics.setSuperItfSimpleClassName(dbOperWrapper.getSimpleClassName(superItfClassName));
        writeDbData4ClassSigExtImplGenerics.setSuperItfGenericsExtendsClassName(superItfGenericsExtendsClassName);
        writeDbData4ClassSigExtImplGenerics.setSuperItfSeq(superItfSeq);
        writeDbData4ClassSigExtImplGenerics.setClassName(className);
        writeDbData4ClassSigExtImplGenerics.setSuperItfClassName(superItfClassName);
        return writeDbData4ClassSigExtImplGenerics;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassSigExtImplGenerics data) {
        return new Object[]{
                genNextRecordId(),
                data.getSimpleClassName(),
                data.getGenericsName(),
                data.getSeq(),
                data.getExtType(),
                data.getSuperItfSimpleClassName(),
                data.getSuperItfGenericsExtendsClassName(),
                data.getSuperItfSeq(),
                data.getClassName(),
                data.getSuperItfClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "签名中的泛型名称",
                "签名中泛型的序号，从0开始",
                "继承或实现类型，e:继承，i:实现",
                "父类或接口的类名",
                "签名中的父类或接口的泛型继承的父类类名",
                "父类或接口的签名中泛型的序号，从0开始"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类的签名中使用了父类或接口相同的泛型相关的信息",
                "例如：“public abstract class AbstractMapper<E, N, T2> implements BaseMapper<T2> ”，对应信息中会包含 T2",
                "例如：“public interface CommonMapper<S, T1> extends BaseMapper<T1>”，对应信息中会包含 T1"
        };
    }
}
