package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisMySqlInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisXmlElement4Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/24
 * @description: 从MyBatis的XML文件获取格式化后的sql文本（使用MySQL）
 */
public class MyBatisMySqlFormatedSqlCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMySqlFormatedSqlCodeParser.class);

    public static final String FILE_NAME = "mybatis_ms_formated_sql";

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
            Map<String, MyBatisXmlElement4Statement> statementMap = myBatisMySqlInfo.getStatementMap();
            List<String> sqlIdList = new ArrayList<>(statementMap.keySet());
            Collections.sort(sqlIdList);
            for (String sqlId : sqlIdList) {
                MyBatisXmlElement4Statement statement = statementMap.get(sqlId);
                String xmlElementName = statement.getXmlElementName();
                int sqlSeq = 0;
                for (String formatedSql : statement.getFullSqlList()) {
                    formatedSql = formatedSql.replace('\r', ' ').replace('\n', ' ');
                    writeData2File(mapperInterfaceName, sqlId, String.valueOf(sqlSeq), xmlElementName, formatedSql, mybatisXmlFilePath, statement.getResultMap());
                    sqlSeq++;
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
