package com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.extensions.code_parser.JarEntryOtherFileParser;
import com.adrninistrator.mybatis_mysql_table_parser.dto.MyBatisSqlInfo;
import com.adrninistrator.mybatis_mysql_table_parser.entry.Entry4ParseMyBatisMySqlTable;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/1
 * @description: 从MyBatis的XML文件获取对应的数据库操作语句及被操作的数据库表名（支持MySQL数据库）
 */
public class MyBatisMySqlSqlInfoCodeParser implements JarEntryOtherFileParser {

    /*
        使用静态变量记录当前类的最后一个实例，因为获取相关信息时找不到当前类的实例
        假如在同一个JVM内并发执行时，数据访问可能出现问题
     */
    private static MyBatisMySqlSqlInfoCodeParser LAST_INSTANCE;

    /*
        保存从MyBatis的XML文件获取对应的数据库操作语句及被操作的数据库表名
        key
            mapper接口类名
        value
            从MyBatis的XML文件获取对应的数据库操作语句及被操作的数据库表名
     */
    private Map<String, MyBatisSqlInfo> myBatisSqlInfoMap;

    // 用于解析MyBatis XML中涉及的MySQL表名
    private Entry4ParseMyBatisMySqlTable entry4ParseMyBatisMySqlTable;

    public static MyBatisMySqlSqlInfoCodeParser getLastInstance() {
        return LAST_INSTANCE;
    }

    public MyBatisMySqlSqlInfoCodeParser() {
        LAST_INSTANCE = this;
    }

    /**
     * 根据MyBatis Mapper类名，获取对应MyBatis的sql信息
     *
     * @param mapperInterfaceName MyBatis Mapper类名
     * @return
     */
    public MyBatisSqlInfo getMyBatisSqlInfo(String mapperInterfaceName) {
        return myBatisSqlInfoMap.get(mapperInterfaceName);
    }

    @Override
    public void initCodeParser() {
        myBatisSqlInfoMap = new HashMap<>();
        entry4ParseMyBatisMySqlTable = new Entry4ParseMyBatisMySqlTable();
    }

    // 指定需要处理xml文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return JavaCGConstants.FILE_EXT_ARRAY_XML;
    }

    // 处理.xml文件
    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName) {
        // 尝试解析xml文件
        MyBatisSqlInfo myBatisSqlInfo = entry4ParseMyBatisMySqlTable.parseFile(inputStream, jarEntryName);
        if (myBatisSqlInfo != null) {
            // 处理完MyBatis xml文件后，记录相关信息
            myBatisSqlInfoMap.put(myBatisSqlInfo.getMapperInterfaceName(), myBatisSqlInfo);
        }
    }
}
