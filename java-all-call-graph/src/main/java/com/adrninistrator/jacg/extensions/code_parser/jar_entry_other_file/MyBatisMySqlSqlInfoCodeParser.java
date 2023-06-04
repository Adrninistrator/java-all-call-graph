package com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.extensions.code_parser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.mybatis_mysql_table_parser.common.enums.MySqlStatementEnum;
import com.adrninistrator.mybatis_mysql_table_parser.dto.MyBatisMySqlInfo;
import com.adrninistrator.mybatis_mysql_table_parser.dto.MySQLWriteTableInfo;
import com.adrninistrator.mybatis_mysql_table_parser.dto.MySqlTableInfo;
import com.adrninistrator.mybatis_mysql_table_parser.entry.Entry4ParseMyBatisMySqlTable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/1
 * @description: 从MyBatis的XML文件获取数据库操作语句及数据库表名（支持MySQL数据库）
 */
public class MyBatisMySqlSqlInfoCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(MyBatisMySqlSqlInfoCodeParser.class);

    public static final String FILE_NAME = "mybatis_ms_table";

    // 用于解析MyBatis XML中涉及的MySQL表名
    private Entry4ParseMyBatisMySqlTable entry4ParseMyBatisMySqlTable;

    // 从MyBatis的XML文件获取写操作语句及数据库表名
    private MyBatisMySqlWriteSqlInfoCodeParser myBatisMySqlWriteSqlInfoCodeParser;

    @Override
    public void initCodeParser() {
        entry4ParseMyBatisMySqlTable = new Entry4ParseMyBatisMySqlTable();
    }

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }

    // 指定需要处理xml文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return JavaCGConstants.FILE_EXT_ARRAY_XML;
    }

    // 处理.xml文件
    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName) {
        try {
            // 尝试解析xml文件
            MyBatisMySqlInfo myBatisMySqlInfo = entry4ParseMyBatisMySqlTable.parseFile(inputStream, jarEntryName);
            if (myBatisMySqlInfo == null) {
                return;
            }
            String mapperInterfaceName = myBatisMySqlInfo.getMapperInterfaceName();
            for (Map.Entry<String, MySqlTableInfo> entry : myBatisMySqlInfo.getMySqlTableInfoMap().entrySet()) {
                String methodName = entry.getKey();
                MySqlTableInfo mySqlTableInfo = entry.getValue();

                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_SELECT.getInitials(), mySqlTableInfo.getSelectTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_SELECT_4_UPDATE.getInitials(), mySqlTableInfo.getSelect4UpdateTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_INSERT.getInitials(), mySqlTableInfo.getInsertTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_INSERT_IGNORE.getInitials(), mySqlTableInfo.getInsertIgnoreTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_INSERT_OR_UPDATE.getInitials(), mySqlTableInfo.getInsertOrUpdateTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_REPLACE.getInitials(), mySqlTableInfo.getReplaceTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_UPDATE.getInitials(), mySqlTableInfo.getUpdateTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_DELETE.getInitials(), mySqlTableInfo.getDeleteTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_ALTER.getInitials(), mySqlTableInfo.getAlterTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_TRUNCATE.getInitials(), mySqlTableInfo.getTruncateTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_CREATE.getInitials(), mySqlTableInfo.getCreateTableList());
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_DROP.getInitials(), mySqlTableInfo.getDropTableList());

                // 处理写操作语句及数据库表名
                MySQLWriteTableInfo mySQLWriteTableInfo = mySqlTableInfo.getMySQLWriteTableInfo();
                if (mySQLWriteTableInfo != null) {
                    myBatisMySqlWriteSqlInfoCodeParser.handleMySQLWriteTableInfo(mapperInterfaceName, methodName, mySQLWriteTableInfo);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    private void writeFile(String mapperInterfaceName, String methodName, String initials, List<String> tableList) throws IOException {
        if (tableList == null) {
            return;
        }
        for (int i = 0; i < tableList.size(); i++) {
            JavaCGFileUtil.write2FileWithTab(writer, mapperInterfaceName, methodName, initials, String.valueOf(i), tableList.get(i));
        }
    }

    public void setMyBatisMySqlWriteSqlInfoCodeParser(MyBatisMySqlWriteSqlInfoCodeParser myBatisMySqlWriteSqlInfoCodeParser) {
        this.myBatisMySqlWriteSqlInfoCodeParser = myBatisMySqlWriteSqlInfoCodeParser;
    }
}
