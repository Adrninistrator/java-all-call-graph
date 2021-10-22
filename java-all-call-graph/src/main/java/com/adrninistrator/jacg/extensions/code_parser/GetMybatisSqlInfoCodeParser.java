package com.adrninistrator.jacg.extensions.code_parser;

import com.adrninistrator.jacg.extensions.dto.DbOperateData;
import com.adrninistrator.jacg.extensions.enums.DbStatementEnum;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.adrninistrator.jacg.util.CommonUtil;
import com.adrninistrator.javacg.extensions.code_parser.AbstractCustomCodeParser;
import com.adrninistrator.javacg.extensions.dto.ExtendedData;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.StringUtils;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.dom4j.tree.DefaultAttribute;
import org.dom4j.tree.DefaultElement;
import org.dom4j.tree.DefaultText;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

import java.io.InputStream;
import java.io.StringBufferInputStream;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * @author adrninistrator
 * @date 2021/8/25
 * @description:
 */
public class GetMybatisSqlInfoCodeParser extends AbstractCustomCodeParser {

    public static final String DATA_TYPE = "MB_SQL";

    // 保存简单处理后的SQL语句
    private Map<String, Map<String, String>> mapperSqlMap = new HashMap<>(200);

    // 保存保存经过解析后的SQL语句对应的数据库操作，及对应的数据库表
    private Map<String, Map<String, DbOperateData>> mapperDbOperateMap = new HashMap<>(200);

    @Override
    public void init() {
        extendedDataList = new ArrayList<>(1000);
    }

    @Override
    public void handleJarEntryFile(JarFile jarFile, JarEntry jarEntry) {
        if (!StringUtils.endsWithIgnoreCase(jarEntry.getName(), ".xml")) {
            return;
        }

        try (InputStream inputStream = jarFile.getInputStream(jarEntry)) {
            // 获取Mybatis的XML中的SQL语句
            getMybatisXmlSql(inputStream, jarEntry.getName());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void handleMethodCall(int callId, String calleeClassName, String calleeMethodName, Type[] arguments, InstructionHandle mcIh, MethodGen methodGen) {
        if (!calleeClassName.contains(".dao.")) {
            return;
        }

        Map<String, DbOperateData> dbOperateDataMap = mapperDbOperateMap.computeIfAbsent(calleeClassName, k -> new HashMap<>());
        DbOperateData dbOperateData = dbOperateDataMap.get(calleeMethodName);
        if (dbOperateData != null) {
            // 当前被调用的Mapper的方法已被记录
            ExtendedData extendedData = ExtendedData.genExtendedData(callId, getDataType(), JsonUtil.getJsonStr(dbOperateData));
            extendedDataList.add(extendedData);
            return;
        }

        // 获取当前被调用的Mapper的方法对应的数据库操作
        Map<String, String> sqlMap = mapperSqlMap.get(calleeClassName);
        if (CommonUtil.isMapEmpty(sqlMap)) {
            System.err.println("### 未查找到对应的mapper " + calleeClassName);
            return;
        }

        String sql = sqlMap.get(calleeMethodName);
        if (sql == null) {
            System.err.println("### 未查找到对应的sql语句 " + calleeClassName + " " + calleeMethodName);
            return;
        }

        DbOperateData dbOperateDataNew = getDbOperateData(sql);
        // 记录当前被调用的Mapper的方法
        dbOperateDataMap.put(calleeMethodName, dbOperateDataNew);

        ExtendedData extendedData = ExtendedData.genExtendedData(callId, getDataType(), JsonUtil.getJsonStr(dbOperateDataNew));
        extendedDataList.add(extendedData);
    }

    @Override
    public String getDataType() {
        return DATA_TYPE;
    }

    // 获取Mybatis的XML中的SQL语句
    private void getMybatisXmlSql(InputStream inputStream, String filePath) throws DocumentException {
        SAXReader reader = new SAXReader();
        // 不读取DTD
        reader.setEntityResolver(new NoOpEntityResolver());

        Document document = reader.read(inputStream);

        Element root = document.getRootElement();
        String rootName = root.getName();
        if (!StringUtils.equals(rootName, "mapper")) {
            System.out.println("跳过非Mybatis xml 1: " + filePath);
            return;
        }
        String namespace = root.attributeValue("namespace");
        if (StringUtils.isBlank(namespace)) {
            System.out.println("跳过非Mybatis xml 2: " + filePath);
            return;
        }
        // 以上用于跳过非Mybatis mapper的XML文件
        System.out.println("开始处理Mybatis xml: " + filePath);

        Map<String, String> sqlMap = new HashMap<>();
        for (Iterator it = root.elementIterator(); it.hasNext(); ) {
            Element element = (Element) it.next();
            if (StringUtils.equalsAny(element.getName(), "select", "insert", "update", "delete")) {
                // 处理SQL语句
                String sqlId = element.attributeValue("id");
                // 获取一个SQL Element中的SQL语句
                StringBuilder stringBuilder = new StringBuilder();
                getElementSql(element, stringBuilder);

                String sql = stringBuilder.toString();

                sqlMap.put(sqlId, sql);
            }
        }

        mapperSqlMap.put(namespace, sqlMap);
    }

    // 获取一个SQL Element中的SQL语句
    private void getElementSql(Element element, StringBuilder stringBuilder) {
        List contentList = element.content();
        for (Object content : contentList) {
            if (content instanceof DefaultText) {
                // 处理一个SQL Element中的文本
                DefaultText defaultText = (DefaultText) content;
                String text = defaultText.getText();
                addData(stringBuilder, text);
            } else if (content instanceof DefaultElement) {
                // 处理一个SQL Element中的Element
                DefaultElement defaultElement = (DefaultElement) content;
                String elementName = defaultElement.getName();
                if (StringUtils.equalsAny(elementName, "foreach", "if")) {
                    getElementSql(defaultElement, stringBuilder);
                } else if (StringUtils.equalsAny(elementName, "where", "set")) {
                    addData(stringBuilder, elementName);
                }

                List attributeList = defaultElement.attributes();
                for (Object attribute : attributeList) {
                    if (attribute instanceof DefaultAttribute) {
                        // 处理一个SQL Element中的Attribute
                        DefaultAttribute defaultAttribute = (DefaultAttribute) attribute;
                        String attributeValue = defaultAttribute.getValue();
                        if (StringUtils.equals(defaultAttribute.getName(), "prefix")) {
                            addData(stringBuilder, attributeValue);
                        }
                    }
                }
            }
        }
    }

    private void addData(StringBuilder stringBuilder, String data) {
        stringBuilder.append(data);
    }

    /**
     * 解析SQL语句，获得对应的操作，及相关数据库表
     *
     * @param sql
     * @return
     */
    private DbOperateData getDbOperateData(String sql) {
        DbOperateData dbOperateData = new DbOperateData();

        // 获取数据库操作语句
        DbStatementEnum dbStatementEnum = getSqlStatement(sql);
        if (dbStatementEnum == null) {
            dbOperateData.setStatement("");
            dbOperateData.setTableList(new ArrayList<>(0));

            return dbOperateData;
        }

        List<String> tableList = null;

        switch (dbStatementEnum) {
            case DSE_SELECT:
                tableList = getTablesFromSql(sql, " from ", new String[]{" where ", " order by ", " group by ", " limit ", ";"});
                break;
            case DSE_INSERT:
            case DSE_REPLACE:
                tableList = getTablesFromSql(sql, " into ", new String[]{"(", " values", ";"});
                break;
            case DSE_UPDATE:
                tableList = getTablesFromSql(sql, "update ", new String[]{" set "});
                break;
            case DSE_DELETE:
                tableList = getTablesFromSql(sql, " from ", new String[]{" where ", " limit ", ";"});
                break;
        }

        dbOperateData.setStatement(dbStatementEnum.getStatement());
        dbOperateData.setTableList(tableList);
        return dbOperateData;
    }

    /**
     * 获取数据库操作语句
     *
     * @param sql
     * @return
     */
    private DbStatementEnum getSqlStatement(String sql) {
        DbStatementEnum minIndexDbStatementEnum = null;
        // 记录各类SQL语句中，出现下标最小的
        int minIndex = -1;

        for (DbStatementEnum dbStatementEnum : DbStatementEnum.values()) {
            int index = StringUtils.indexOfIgnoreCase(sql, dbStatementEnum.getStatement());
            if (index == 0) {
                return dbStatementEnum;
            }

            if (index == -1) {
                continue;
            }

            if (minIndex == -1 || index < minIndex) {
                minIndex = index;
                minIndexDbStatementEnum = dbStatementEnum;
            }
        }

        return minIndexDbStatementEnum;
    }

    // 对sql语句进行格式化
    private String formatSql(String sql) {
        return sql.replaceAll("[\r\n\t]", " ")
                .replaceAll("[ ][ ]*", " ").trim();
    }

    /**
     * 从数据库表名相关的sql语句中获得表名
     *
     * @param sql 示例： "table1"    "table1, table2"    "table1 as t1, table2 as t2"
     * @return
     */
    private List<String> getTablesFromPartSql(String sql) {
        String[] array1 = sql.split(",");
        List<String> tableList = new ArrayList<>(array1.length);
        for (String str1 : array1) {
            String[] array2 = str1.trim().split(" ");
            if (!tableList.contains(array2[0])) {
                tableList.add(array2[0]);
            }
        }

        return tableList;
    }

    /**
     * 从sql语句中获得对应的表名
     *
     * @param sql
     * @param startFlag
     * @param endFlagArray
     * @return
     */
    private List<String> getTablesFromSql(String sql, String startFlag, String[] endFlagArray) {
        // 对sql语句进行格式化
        String formattedSql = formatSql(sql);

        // 查找开始标志下标
        int fromIndex = StringUtils.indexOfIgnoreCase(formattedSql, startFlag);
        if (fromIndex == -1) {
            return new ArrayList<>(0);
        }

        // 查找结束标志下标
        int minEndFlagIndex = -1;
        for (String endFlag : endFlagArray) {
            int endFlagIndex = StringUtils.indexOfIgnoreCase(formattedSql, endFlag, fromIndex + startFlag.length());
            if (endFlagIndex == -1) {
                continue;
            }
            if (minEndFlagIndex == -1) {
                minEndFlagIndex = endFlagIndex;
            } else {
                minEndFlagIndex = Math.min(minEndFlagIndex, endFlagIndex);
            }
        }

        String partSql;
        if (minEndFlagIndex == -1) {
            partSql = formattedSql.substring(fromIndex + startFlag.length()).trim();
        } else {
            partSql = formattedSql.substring(fromIndex + startFlag.length(), minEndFlagIndex).trim();
        }

        // 从数据库表名相关的sql语句中获得表名
        return getTablesFromPartSql(partSql);
    }

    class NoOpEntityResolver implements EntityResolver {
        public InputSource resolveEntity(String publicId, String systemId) {
            return new InputSource(new StringBufferInputStream(""));
        }
    }
}

