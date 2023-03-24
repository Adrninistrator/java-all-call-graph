package com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file;

import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.mybatis_mysql_table_parser.dto.MySQLWriteTableInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 从MyBatis的XML文件获取写操作语句及数据库表名（支持MySQL数据库）
 */
public class MyBatisMySqlWriteSqlInfoCodeParser extends MyBatisMySqlSqlInfoCodeParser {
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
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName) {
    }

    // 处理写操作语句及数据库表名
    public void handleMySQLWriteTableInfo(String mapperInterfaceName, String methodName, MySQLWriteTableInfo mySQLWriteTableInfo) {
        try {
            JavaCGFileUtil.write2FileWithTab(writer, mapperInterfaceName, methodName, mySQLWriteTableInfo.getMySqlStatementEnum().getInitials(),
                    mySQLWriteTableInfo.getTableName());
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
