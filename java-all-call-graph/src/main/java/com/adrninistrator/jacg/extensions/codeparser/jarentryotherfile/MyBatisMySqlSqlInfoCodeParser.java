package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.mybatismysqltableparser.common.enums.MySqlStatementEnum;
import com.adrninistrator.mybatismysqltableparser.dto.MyBatisMySqlInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MySqlTableColumnInfo;
import com.adrninistrator.mybatismysqltableparser.dto.MySqlWriteTableInfo;
import com.adrninistrator.mybatismysqltableparser.entry.Entry4ParseMyBatisMySqlTable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
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

    // 从MyBatis的XML文件获取Entity与数据库字段名
    private MyBatisMySqlColumnInfoCodeParser myBatisMySqlColumnInfoCodeParser;

    // 从MyBatis的XML文件获取Entity与Mapper、表名
    private MyBatisMySqlEntityInfoCodeParser myBatisMySqlEntityInfoCodeParser;

    // 从MyBatis的XML文件获取格式化后的sql文本
    private MyBatisMySqlFormatedSqlCodeParser myBatisMySqlFormatedSqlCodeParser;

    // 从MyBatis的XML文件获取update set子句的字段信息
    private MyBatisMySqlSetColumnCodeParser myBatisMySqlSetColumnCodeParser;

    // 从MyBatis的XML文件获取select的字段信息
    private MyBatisMySqlSelectColumnCodeParser myBatisMySqlSelectColumnCodeParser;

    // 从MyBatis的XML文件获取where子句的字段信息
    private MyBatisMySqlWhereColumnCodeParser myBatisMySqlWhereColumnCodeParser;

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
        return new String[]{JACGConstants.EXT_XML};
    }

    // 处理.xml文件
    @Override
    public boolean parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        try {
            // 尝试解析xml文件
            MyBatisMySqlInfo myBatisMySqlInfo = entry4ParseMyBatisMySqlTable.parseFile(inputStream, jarEntryPath);
            if (myBatisMySqlInfo == null) {
                return true;
            }

            // 处理Entity与Mapper、表名
            myBatisMySqlEntityInfoCodeParser.handleMyBatisMySqlInfo(myBatisMySqlInfo, jarEntryPath);
            // 处理格式化后的sql文本
            myBatisMySqlFormatedSqlCodeParser.handleMyBatisMySqlInfo(myBatisMySqlInfo, jarEntryPath);
            // 处理Entity与数据库字段名
            myBatisMySqlColumnInfoCodeParser.handleMyBatisMySqlInfo(myBatisMySqlInfo, jarEntryPath);
            // 处理update set子句的字段信息
            myBatisMySqlSetColumnCodeParser.handleMyBatisMySqlInfo(myBatisMySqlInfo, jarEntryPath);
            // 处理select的字段信息
            myBatisMySqlSelectColumnCodeParser.handleMyBatisMySqlInfo(myBatisMySqlInfo, jarEntryPath);
            // 处理where子句的字段信息
            myBatisMySqlWhereColumnCodeParser.handleMyBatisMySqlInfo(myBatisMySqlInfo, jarEntryPath);

            String mapperInterfaceName = myBatisMySqlInfo.getMapperInterfaceName();
            Map<String, MySqlTableColumnInfo> mySqlTableColumnInfoMap = myBatisMySqlInfo.getMySqlTableColumnInfoMap();
            List<String> methodNameList = new ArrayList<>(mySqlTableColumnInfoMap.keySet());
            Collections.sort(methodNameList);
            for (String methodName : methodNameList) {
                MySqlTableColumnInfo mySqlTableColumnInfo = mySqlTableColumnInfoMap.get(methodName);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_SELECT.getInitials(), mySqlTableColumnInfo.getSelectTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_SELECT_4_UPDATE.getInitials(), mySqlTableColumnInfo.getSelect4UpdateTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_INSERT.getInitials(), mySqlTableColumnInfo.getInsertTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_INSERT_IGNORE.getInitials(), mySqlTableColumnInfo.getInsertIgnoreTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_INSERT_OR_UPDATE.getInitials(), mySqlTableColumnInfo.getInsertOrUpdateTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_REPLACE.getInitials(), mySqlTableColumnInfo.getReplaceTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_UPDATE.getInitials(), mySqlTableColumnInfo.getUpdateTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_DELETE.getInitials(), mySqlTableColumnInfo.getDeleteTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_ALTER.getInitials(), mySqlTableColumnInfo.getAlterTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_TRUNCATE.getInitials(), mySqlTableColumnInfo.getTruncateTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_CREATE.getInitials(), mySqlTableColumnInfo.getCreateTableList(), jarEntryPath);
                writeFile(mapperInterfaceName, methodName, MySqlStatementEnum.DSSE_DROP.getInitials(), mySqlTableColumnInfo.getDropTableList(), jarEntryPath);
                if (mySqlTableColumnInfo.getAllTableSet().isEmpty()) {
                    // 当前XML元素未获取到数据库表名，写一条记录代表对应表名为空
                    writeFile(mapperInterfaceName, methodName, "", Collections.singletonList(""), jarEntryPath);
                }
                // 处理写操作语句及数据库表名
                MySqlWriteTableInfo mySqlWriteTableInfo = mySqlTableColumnInfo.getMySqlWriteTableInfo();
                if (mySqlWriteTableInfo != null) {
                    myBatisMySqlWriteSqlInfoCodeParser.handleMySQLWriteTableInfo(mapperInterfaceName, methodName, mySqlWriteTableInfo, jarEntryPath);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    private void writeFile(String mapperInterfaceName, String methodName, String initials, List<String> tableList, String mybatisXmlFilePath) throws IOException {
        if (tableList == null) {
            return;
        }
        for (int i = 0; i < tableList.size(); i++) {
            writeData2File(mapperInterfaceName, methodName, initials, String.valueOf(i), tableList.get(i), mybatisXmlFilePath);
        }
    }

    public void setMyBatisMySqlWriteSqlInfoCodeParser(MyBatisMySqlWriteSqlInfoCodeParser myBatisMySqlWriteSqlInfoCodeParser) {
        this.myBatisMySqlWriteSqlInfoCodeParser = myBatisMySqlWriteSqlInfoCodeParser;
    }

    public void setMyBatisMySqlColumnInfoCodeParser(MyBatisMySqlColumnInfoCodeParser myBatisMySqlColumnInfoCodeParser) {
        this.myBatisMySqlColumnInfoCodeParser = myBatisMySqlColumnInfoCodeParser;
    }

    public void setMyBatisMySqlEntityInfoCodeParser(MyBatisMySqlEntityInfoCodeParser myBatisMySqlEntityInfoCodeParser) {
        this.myBatisMySqlEntityInfoCodeParser = myBatisMySqlEntityInfoCodeParser;
    }

    public void setMyBatisMySqlFormatedSqlCodeParser(MyBatisMySqlFormatedSqlCodeParser myBatisMySqlFormatedSqlCodeParser) {
        this.myBatisMySqlFormatedSqlCodeParser = myBatisMySqlFormatedSqlCodeParser;
    }

    public void setMyBatisMySqlSetColumnCodeParser(MyBatisMySqlSetColumnCodeParser myBatisMySqlSetColumnCodeParser) {
        this.myBatisMySqlSetColumnCodeParser = myBatisMySqlSetColumnCodeParser;
    }

    public void setMyBatisMySqlSelectColumnCodeParser(MyBatisMySqlSelectColumnCodeParser myBatisMySqlSelectColumnCodeParser) {
        this.myBatisMySqlSelectColumnCodeParser = myBatisMySqlSelectColumnCodeParser;
    }

    public void setMyBatisMySqlWhereColumnCodeParser(MyBatisMySqlWhereColumnCodeParser myBatisMySqlWhereColumnCodeParser) {
        this.myBatisMySqlWhereColumnCodeParser = myBatisMySqlWhereColumnCodeParser;
    }
}
