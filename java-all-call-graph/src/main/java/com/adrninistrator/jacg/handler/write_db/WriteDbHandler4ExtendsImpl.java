package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ExtendsImpl;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，继承与实现相关信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_EXTENDS_IMPL,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_EXTENDS_IMPL
)
public class WriteDbHandler4ExtendsImpl extends AbstractWriteDbHandler<WriteDbData4ExtendsImpl> {
    // 父类或接口类名
    private Set<String> superClassOrInterfaceNameSet = new HashSet<>();

    public WriteDbHandler4ExtendsImpl() {
        initSeqMap();
    }

    @Override
    protected WriteDbData4ExtendsImpl genData(String[] array) {
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
        int existsDownwardClasses = JavaCGYesNoEnum.parseIntValue(superClassOrInterfaceNameSet.contains(className));

        WriteDbData4ExtendsImpl writeDbData4ExtendsImpl = new WriteDbData4ExtendsImpl();
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
    protected Object[] genObjectArray(WriteDbData4ExtendsImpl data) {
        return new Object[]{
                genNextRecordId(),
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
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "类继承类，或类实现接口相关的信息"
        };
    }

    public void setSuperClassOrInterfaceNameSet(Set<String> superClassOrInterfaceNameSet) {
        this.superClassOrInterfaceNameSet = superClassOrInterfaceNameSet;
    }
}
