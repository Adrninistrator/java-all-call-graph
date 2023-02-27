package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ExtendsImpl;
import com.adrninistrator.javacg.common.JavaCGConstants;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，继承与实现相关信息
 */
public class WriteDbHandler4ExtendsImpl extends AbstractWriteDbHandler<WriteDbData4ExtendsImpl> {
    // 父类或接口类名
    private Set<String> superClassOrInterfaceNameSet = new HashSet<>();

    public WriteDbHandler4ExtendsImpl() {
        initSeqMap();
    }

    @Override
    protected WriteDbData4ExtendsImpl genData(String line) {
        String[] array = splitEquals(line, 4);

        String className = array[0];
        int accessFlags = Integer.parseInt(array[1]);
        String type = array[2];
        String upwardClassName = array[3];

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className) && !isAllowedClassPrefix(upwardClassName)) {
            return null;
        }

        // 生成下一个序号，从0开始
        Integer seq = genNextSeq(className + JavaCGConstants.FLAG_COLON + type);
        // 判断当前类是否存在子类或子接口
        int existsDownwardClasses = superClassOrInterfaceNameSet.contains(className) ? JACGConstants.YES_1 : JACGConstants.NO_0;

        WriteDbData4ExtendsImpl writeDbData4ExtendsImpl = new WriteDbData4ExtendsImpl();
        writeDbData4ExtendsImpl.setRecordId(genNextRecordId());
        writeDbData4ExtendsImpl.setSimpleClassName(dbOperWrapper.getSimpleClassName(className));
        writeDbData4ExtendsImpl.setClassName(className);
        writeDbData4ExtendsImpl.setAccessFlags(accessFlags);
        writeDbData4ExtendsImpl.setType(type);
        writeDbData4ExtendsImpl.setSeq(seq);
        writeDbData4ExtendsImpl.setExistsDownwardClasses(existsDownwardClasses);
        writeDbData4ExtendsImpl.setUpwardSimpleClassName(dbOperWrapper.getSimpleClassName(upwardClassName));
        writeDbData4ExtendsImpl.setUpwardClassName(upwardClassName);
        return writeDbData4ExtendsImpl;
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_EXTENDS_IMPL;
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

    public void setSuperClassOrInterfaceNameSet(Set<String> superClassOrInterfaceNameSet) {
        this.superClassOrInterfaceNameSet = superClassOrInterfaceNameSet;
    }
}
