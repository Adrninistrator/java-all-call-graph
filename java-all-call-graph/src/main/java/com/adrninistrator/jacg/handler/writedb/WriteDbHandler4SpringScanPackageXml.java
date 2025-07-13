package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.SpringXmlCodeParser;

/**
 * @author adrninistrator
 * @date 2025/6/22
 * @description: 写入数据库，Spring的包扫描路径，在XML文件中定义
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = SpringXmlCodeParser.FILE_NAME_SPRING_SCAN_PACKAGE_XML,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SPRING_SCAN_PACKAGE
)
public class WriteDbHandler4SpringScanPackageXml extends AbstractWriteDbHandler4SpringScanPackage {
    public WriteDbHandler4SpringScanPackageXml(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "Spring的包扫描路径，在XML文件中定义";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"包括Spring的包扫描路径、XML文件路径"};
    }
}
