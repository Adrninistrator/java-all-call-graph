package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisMySqlInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MySqlTableColumnInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MySqlWhereColumnInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/10/8
 * @description: 从MyBatis的XML文件获取where子句的字段信息（使用MySQL）
 */
public class MyBatisMySqlWhereColumnCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMySqlWhereColumnCodeParser.class);

    public static final String FILE_NAME = "mybatis_ms_where_column";

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }

    // 指定不需要处理文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return null;
    }

    // 不需要处理文件
    @Override
    public boolean parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        return true;
    }

    public void handleMyBatisMySqlInfo(MyBatisMySqlInfo myBatisMySqlInfo, String mybatisXmlFilePath) {
        try {
            String mapperInterfaceName = myBatisMySqlInfo.getMapperInterfaceName();
            Map<String, MySqlTableColumnInfo> mySqlTableColumnInfoMap = myBatisMySqlInfo.getMySqlTableColumnInfoMap();
            List<String> methodNameList = new ArrayList<>(mySqlTableColumnInfoMap.keySet());
            Collections.sort(methodNameList);
            for (String methodName : methodNameList) {
                MySqlTableColumnInfo mySqlTableColumnInfo = mySqlTableColumnInfoMap.get(methodName);
                for (MySqlWhereColumnInfo mySqlWhereColumnInfo : mySqlTableColumnInfo.getMySqlWhereColumnInfoList()) {
                    writeData2File(mapperInterfaceName, methodName, mySqlWhereColumnInfo.getDbTableName(), mySqlWhereColumnInfo.getDbColumnName(),
                            mySqlWhereColumnInfo.getOperation(), mySqlWhereColumnInfo.getParameterName(), mySqlWhereColumnInfo.getParameterType(), mybatisXmlFilePath);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
