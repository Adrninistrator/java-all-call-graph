package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringScanPackage;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/6/16
 * @description: 写入数据库，Spring的包扫描路径，抽象父类
 */
public abstract class AbstractWriteDbHandler4SpringScanPackage extends AbstractWriteDbHandler<WriteDbData4SpringScanPackage> {

    public AbstractWriteDbHandler4SpringScanPackage(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    protected Set<String> scanPackageSet;

    @Override
    protected WriteDbData4SpringScanPackage genData(String[] array) {
        String type = readLineData();
        int seq = Integer.parseInt(readLineData());
        String scanPackage = readLineData();
        String defineClassNameXmlPath = readLineData();

        WriteDbData4SpringScanPackage springScanPackage = new WriteDbData4SpringScanPackage();
        springScanPackage.setRecordId(genNextRecordId());
        springScanPackage.setType(type);
        springScanPackage.setSeq(seq);
        springScanPackage.setScanPackage(scanPackage);
        scanPackageSet.add(scanPackage);
        springScanPackage.setDefineClassNameXmlPath(defineClassNameXmlPath);
        return springScanPackage;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringScanPackage data) {
        return new Object[]{
                data.getRecordId(),
                data.getType(),
                data.getSeq(),
                data.getScanPackage(),
                data.getDefineClassNameXmlPath()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "包扫描路径类型，j: 在Java代码中定义，x: 在XML文件中定义；dist: 按范围去重后的包扫描路径",
                "序号，从0开始，大于0代表有多种可能",
                "包扫描路径",
                "在Java代码中定义时所在的类名，或在XML中定义时对应的文件路径"
        };
    }

    public Set<String> getScanPackageSet() {
        return scanPackageSet;
    }

    public void setScanPackageSet(Set<String> scanPackageSet) {
        this.scanPackageSet = scanPackageSet;
    }
}
