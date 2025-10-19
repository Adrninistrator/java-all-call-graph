package com.adrninistrator.jacg.handler.writedb;


import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

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
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_INFO
)
public class WriteDbHandler4ClassInfo extends AbstractWriteDbHandler<WriteDbData4ClassInfo> {

    // 枚举唯一类名集合
    private Set<String> enumSimpleClassNameSet;

    private final boolean isHandler4ClassInfo;

    public WriteDbHandler4ClassInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
        isHandler4ClassInfo = this.getClass().getName().equals(WriteDbHandler4ClassInfo.class.getName());
    }

    @Override
    protected WriteDbData4ClassInfo genData(String[] array) {
        String className = readLineData();
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        int accessFlags = Integer.parseInt(readLineData());
        if (isHandler4ClassInfo && JavaCG2ByteCodeUtil.isEnumFlag(accessFlags) && enumSimpleClassNameSet != null) {
            // 记录枚举唯一类名
            enumSimpleClassNameSet.add(simpleClassName);
        }

        String packageName = JavaCG2ClassMethodUtil.getPackageName(className);
        int packageLevel = JavaCG2ClassMethodUtil.getPackageLevel(packageName);
        String classFileHash = readLineData();
        int jarNum = Integer.parseInt(readLineData());
        String classPathInJar = readLineData();

        WriteDbData4ClassInfo writeDbData4ClassInfo = new WriteDbData4ClassInfo();
        writeDbData4ClassInfo.setRecordId(genNextRecordId());
        writeDbData4ClassInfo.setSimpleClassName(simpleClassName);
        writeDbData4ClassInfo.setAccessFlags(accessFlags);
        writeDbData4ClassInfo.setClassName(className);
        writeDbData4ClassInfo.setPackageName(packageName);
        writeDbData4ClassInfo.setPackageLevel(packageLevel);
        writeDbData4ClassInfo.setClassFileHash(classFileHash);
        writeDbData4ClassInfo.setJarNum(jarNum);
        writeDbData4ClassInfo.setClassPathInJar(classPathInJar);
        return writeDbData4ClassInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getAccessFlags(),
                data.getClassName(),
                data.getPackageName(),
                data.getPackageLevel(),
                data.getClassFileHash(),
                data.getJarNum(),
                data.getClassPathInJar()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "类的access_flags",
                "类文件的HASH值（MD5）",
                "类所在的jar文件序号",
                "类在jar包中的路径"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类的信息，包括类名、access_flags、类文件的HASH值（MD5）、类所在的jar文件序号等"
        };
    }

    public void setEnumSimpleClassNameSet(Set<String> enumSimpleClassNameSet) {
        this.enumSimpleClassNameSet = enumSimpleClassNameSet;
    }
}
