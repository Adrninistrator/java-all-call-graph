package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisMySqlInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MySqlSetColumnInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MySqlTableColumnInfo;
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
 * @description: 从MyBatis的XML文件获取update set子句的字段信息（使用MySQL）
 */
public class MyBatisMySqlSetColumnCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMySqlSetColumnCodeParser.class);

    public static final String FILE_NAME = "mybatis_ms_set_column";

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
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath) {
    }

    public void handleMyBatisMySqlInfo(MyBatisMySqlInfo myBatisMySqlInfo, String mybatisXmlFilePath) {
        try {
            String mapperInterfaceName = myBatisMySqlInfo.getMapperInterfaceName();
            Map<String, MySqlTableColumnInfo> mySqlTableColumnInfoMap = myBatisMySqlInfo.getMySqlTableColumnInfoMap();
            List<String> methodNameList = new ArrayList<>(mySqlTableColumnInfoMap.keySet());
            Collections.sort(methodNameList);
            for (String methodName : methodNameList) {
                MySqlTableColumnInfo mySqlTableColumnInfo = mySqlTableColumnInfoMap.get(methodName);
                for (MySqlSetColumnInfo mySqlSetColumnInfo : mySqlTableColumnInfo.getMySqlSetColumnInfoList()) {
                    JavaCG2FileUtil.write2FileWithTab(writer, mapperInterfaceName, methodName, mySqlSetColumnInfo.getDbTableName(), mySqlSetColumnInfo.getDbColumnName(),
                            mySqlSetColumnInfo.getParameterName(), mybatisXmlFilePath);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
