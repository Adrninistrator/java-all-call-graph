package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4PackageInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2025/10/13
 * @description: 写入数据库，包名信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_PACKAGE_INFO,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_PACKAGE_INFO
)
public class WriteDbHandler4PackageInfo extends AbstractWriteDbHandler<WriteDbData4PackageInfo> {

    public WriteDbHandler4PackageInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4PackageInfo genData(String[] array) {
        WriteDbData4PackageInfo methodReturnFieldInfo = new WriteDbData4PackageInfo();
        methodReturnFieldInfo.setRecordId(genNextRecordId());
        methodReturnFieldInfo.setPackageName(readLineData());
        methodReturnFieldInfo.setPackageLevel(Integer.parseInt(readLineData()));
        methodReturnFieldInfo.setJarNum(Integer.parseInt(readLineData()));
        methodReturnFieldInfo.setJarFileName(readLineData());
        return methodReturnFieldInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4PackageInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getPackageName(),
                data.getPackageLevel(),
                data.getJarNum(),
                data.getJarFileName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "包名",
                "包名层级，等于包名中的.数量+1",
                "类所在的jar文件序号",
                "jar文件名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "包名信息，包括包名，对应的jar文件序号、jar文件名"
        };
    }
}
