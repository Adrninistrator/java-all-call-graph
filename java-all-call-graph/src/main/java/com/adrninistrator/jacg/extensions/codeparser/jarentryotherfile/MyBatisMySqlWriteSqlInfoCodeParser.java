package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.mybatismysqltableparser.dto.MySqlWriteTableInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 从MyBatis的XML文件获取写操作语句及数据库表名（支持MySQL数据库）
 */
public class MyBatisMySqlWriteSqlInfoCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMySqlWriteSqlInfoCodeParser.class);

    public static final String FILE_NAME = "mybatis_ms_write_table";

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

    public void handleMySQLWriteTableInfo(String mapperInterfaceName, String methodName, MySqlWriteTableInfo mySqlWriteTableInfo, String mybatisXmlFilePath) {
        try {
            writeData2File(mapperInterfaceName, methodName, mySqlWriteTableInfo.getMySqlStatementEnum().getInitials(),
                    mySqlWriteTableInfo.getTableName(), mybatisXmlFilePath);
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
