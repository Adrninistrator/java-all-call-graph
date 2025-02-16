package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ExtendsImpl;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，继承与实现相关信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_EXTENDS_IMPL,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_EXTENDS_IMPL,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_EXTENDS_IMPL_PRE}
)
public class WriteDbHandler4ExtendsImpl extends AbstractWriteDbHandler<WriteDbData4ExtendsImpl> {
    // 父类或接口类名
    private Set<String> superClassOrInterfaceNameSet = new HashSet<>();

    /*
        涉及继承的唯一类名
        key     子类唯一类名
        value   对应的父类唯一类名
     */
    private Map<String, String> extendsSimpleClassNameMap;

    public WriteDbHandler4ExtendsImpl(WriteDbResult writeDbResult) {
        super(writeDbResult);
        initSeqMap();
    }

    @Override
    protected WriteDbData4ExtendsImpl genData(String[] array) {
        String className = array[0];
        int accessFlags = Integer.parseInt(array[1]);
        String type = array[2];
        String upwardClassName = array[3];
        // 生成下一个序号，从0开始
        Integer seq = genNextSeq(className + JavaCG2Constants.FLAG_COLON + type);
        // 判断当前类是否存在子类或子接口
        int existsDownwardClasses = JavaCG2YesNoEnum.parseIntValue(superClassOrInterfaceNameSet.contains(className));

        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String simpleUpwardClassName = dbOperWrapper.querySimpleClassName(upwardClassName);
        if (JavaCG2Constants.FILE_KEY_EXTENDS.equals(type)) {
            // 记录子类及对应的父类唯一类名
            extendsSimpleClassNameMap.put(simpleClassName, simpleUpwardClassName);
        }

        WriteDbData4ExtendsImpl writeDbData4ExtendsImpl = new WriteDbData4ExtendsImpl();
        writeDbData4ExtendsImpl.setRecordId(genNextRecordId());
        writeDbData4ExtendsImpl.setSimpleClassName(simpleClassName);
        writeDbData4ExtendsImpl.setClassName(className);
        writeDbData4ExtendsImpl.setAccessFlags(accessFlags);
        writeDbData4ExtendsImpl.setType(type);
        writeDbData4ExtendsImpl.setSeq(seq);
        writeDbData4ExtendsImpl.setExistsDownwardClasses(existsDownwardClasses);
        writeDbData4ExtendsImpl.setUpwardSimpleClassName(simpleUpwardClassName);
        writeDbData4ExtendsImpl.setUpwardClassName(upwardClassName);
        return writeDbData4ExtendsImpl;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ExtendsImpl data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getClassName(),
                data.getAccessFlags(),
                data.getType(),
                data.getSeq(),
                data.getExistsDownwardClasses(),
                data.getUpwardSimpleClassName(),
                data.getUpwardClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "类的access_flags",
                "类型，e:继承，i:实现",
                "父类或接口的完整类名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类继承类，或类实现接口相关的信息"
        };
    }

    public void setSuperClassOrInterfaceNameSet(Set<String> superClassOrInterfaceNameSet) {
        this.superClassOrInterfaceNameSet = superClassOrInterfaceNameSet;
    }

    public void setExtendsSimpleClassNameMap(Map<String, String> extendsSimpleClassNameMap) {
        this.extendsSimpleClassNameMap = extendsSimpleClassNameMap;
    }
}
