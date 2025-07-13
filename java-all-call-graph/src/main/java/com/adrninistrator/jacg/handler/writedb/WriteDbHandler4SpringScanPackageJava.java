package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SpringScanPackage;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/6/22
 * @description: 写入数据库，Spring的包扫描路径，在Java代码中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_SCAN_PACKAGE_JAVA,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_SCAN_PACKAGE
)
public class WriteDbHandler4SpringScanPackageJava extends AbstractWriteDbHandler4SpringScanPackage {
    public WriteDbHandler4SpringScanPackageJava(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring的包扫描路径，在Java代码中定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"包括Spring的包扫描路径、定义Spring包扫描路径的类名"};
    }

    @Override
    public void afterHandle() {
        super.afterHandle();
        // 获得所有的顶层包扫描路径
        Set<String> notRootScanPackageSet = new HashSet<>();
        for (String scanPackage1 : scanPackageSet) {
            for (String scanPackage2 : scanPackageSet) {
                if (scanPackage1.startsWith(scanPackage2) && !scanPackage1.equals(scanPackage2)) {
                    notRootScanPackageSet.add(scanPackage1);
                    break;
                }
            }
        }
        List<String> rootScanPackageList = new ArrayList<>();
        for (String scanPackage : scanPackageSet) {
            if (!notRootScanPackageSet.contains(scanPackage)) {
                rootScanPackageList.add(scanPackage);
            }
        }
        Collections.sort(rootScanPackageList);

        // 记录顶层包扫描路径
        for (int i = 0; i < rootScanPackageList.size(); i++) {
            WriteDbData4SpringScanPackage springScanPackage = new WriteDbData4SpringScanPackage();
            springScanPackage.setRecordId(genNextRecordId());
            springScanPackage.setType(JavaCG2Constants.FILE_KEY_SPRING_SCAN_PACKAGE_DISTINCT);
            springScanPackage.setSeq(i);
            springScanPackage.setScanPackage(rootScanPackageList.get(i));
            springScanPackage.setDefineClassNameXmlPath("");
            dataList.add(springScanPackage);
        }
        insertDb();
    }
}
