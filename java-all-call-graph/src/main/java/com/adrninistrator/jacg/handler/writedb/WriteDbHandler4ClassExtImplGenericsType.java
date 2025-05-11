package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassExtImplGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/7/16
 * @description: 写入数据库，类的继承或实现的泛型信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_EXT_IMPL_GENERICS_TYPE,
        minColumnNum = 9,
        maxColumnNum = 9,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_EXT_IMPL_GENERICS_TYPE
)
public class WriteDbHandler4ClassExtImplGenericsType extends AbstractWriteDbHandler<WriteDbData4ClassExtImplGenericsType> {

    public WriteDbHandler4ClassExtImplGenericsType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ClassExtImplGenericsType genData(String[] array) {
        String className = readLineData();
        String extType = readLineData();
        int seq = Integer.parseInt(readLineData());
        String superItfClassName = readLineData();
        int genericsSeq = Integer.parseInt(readLineData());
        String genericsTypeNad = readLineData();
        int genericsArrayDimensions = Integer.parseInt(readLineData());
        String typeVariablesName = readLineData();
        String genericsCategory = readLineData();

        WriteDbData4ClassExtImplGenericsType writeDbData4ClassExtImplGenericsType = new WriteDbData4ClassExtImplGenericsType();
        writeDbData4ClassExtImplGenericsType.setRecordId(genNextRecordId());
        writeDbData4ClassExtImplGenericsType.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4ClassExtImplGenericsType.setExtType(extType);
        writeDbData4ClassExtImplGenericsType.setSeq(seq);
        writeDbData4ClassExtImplGenericsType.setSuperItfSimpleClassName(dbOperWrapper.querySimpleClassName(superItfClassName));
        writeDbData4ClassExtImplGenericsType.setGenericsSeq(genericsSeq);
        writeDbData4ClassExtImplGenericsType.setSimpleGenericsTypeNad(dbOperWrapper.querySimpleClassName(genericsTypeNad));
        writeDbData4ClassExtImplGenericsType.setGenericsArrayDimensions(genericsArrayDimensions);
        writeDbData4ClassExtImplGenericsType.setTypeVariablesName(typeVariablesName);
        writeDbData4ClassExtImplGenericsType.setGenericsCategory(genericsCategory);
        writeDbData4ClassExtImplGenericsType.setGenericsTypeNad(genericsTypeNad);
        writeDbData4ClassExtImplGenericsType.setClassName(className);
        writeDbData4ClassExtImplGenericsType.setSuperItfClassName(superItfClassName);
        return writeDbData4ClassExtImplGenericsType;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassExtImplGenericsType data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getExtType(),
                data.getSeq(),
                data.getSuperItfSimpleClassName(),
                data.getGenericsSeq(),
                data.getSimpleGenericsTypeNad(),
                data.getGenericsArrayDimensions(),
                data.getTypeVariablesName(),
                data.getGenericsCategory(),
                data.getGenericsTypeNad(),
                data.getClassName(),
                data.getSuperItfClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "继承或实现类型，e:继承，i:实现",
                "继承或实现的序号，从0开始",
                "父类或接口的类名",
                "类的继承或实现中的泛型类型序号，从0开始",
                "类的继承或实现中的泛型类型类名（不包含数组标志）",
                "类的继承或实现中的泛型数组类型的维度，为0代表不是数组类型",
                "类的继承或实现中的泛型类型变量名称",
                "类的继承或实现中的泛型类型分类，J:JDK中的类型，C:自定义类型"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类在继承父类或实现接口时的泛型信息，包含泛型的类型、数组维度、变量名称等",
                "例如：“GenericClassImplSuper1 implements GenericInterfaceSuper1<ChildClassA2, ChildClassB1>”，对应信息中会包含 ChildClassA2、ChildClassB1",
                "例如：“GenericClassImplSuper2b2 extends GenericAbstractSuper2<String[], byte[]>”，对应信息中会包含 String[]、byte[]",
                "例如：“GenericClassImplSuper2c<E1, E2> extends GenericAbstractSuper2<E1, E2>”，对应信息中会包含 E1、E2",
                "例如：“GenericClassImplSuper2b3 extends GenericAbstractSuper2<List<String[]>, Map<String, Map<Integer, byte[]>>>”" +
                        "，对应信息中会包含 List、String[]、Map、String、Map、Integer、byte"
        };
    }
}
