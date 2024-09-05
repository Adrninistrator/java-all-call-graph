package com.adrninistrator.jacg.handler.writedb;


import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，类的信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_INFO,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_INFO
)
public class WriteDbHandler4ClassInfo extends AbstractWriteDbHandler<WriteDbData4ClassInfo> {

    // 枚举唯一类名集合
    private Set<String> enumSimpleClassNameSet;

    public WriteDbHandler4ClassInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ClassInfo genData(String[] array) {
        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        int accessFlags = Integer.parseInt(array[1]);
        if (JavaCG2ByteCodeUtil.isEnumFlag(accessFlags)) {
            // 记录枚举唯一类名
            enumSimpleClassNameSet.add(simpleClassName);
        }

        String classFileHash = array[2];
        int jarNum = Integer.parseInt(array[3]);

        WriteDbData4ClassInfo writeDbData4ClassInfo = new WriteDbData4ClassInfo();
        writeDbData4ClassInfo.setRecordId(genNextRecordId());
        writeDbData4ClassInfo.setSimpleClassName(simpleClassName);
        writeDbData4ClassInfo.setAccessFlags(accessFlags);
        writeDbData4ClassInfo.setClassName(className);
        writeDbData4ClassInfo.setClassFileHash(classFileHash);
        writeDbData4ClassInfo.setJarNum(jarNum);

        return writeDbData4ClassInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getAccessFlags(),
                data.getClassName(),
                data.getClassFileHash(),
                data.getJarNum()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "类的access_flags",
                "类文件的HASH值（MD5）",
                "类所在的Jar包序号"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类的信息，包括类名、access_flags、类文件的HASH值（MD5）、类所在的Jar包序号等"
        };
    }

    public void setEnumSimpleClassNameSet(Set<String> enumSimpleClassNameSet) {
        this.enumSimpleClassNameSet = enumSimpleClassNameSet;
    }
}
