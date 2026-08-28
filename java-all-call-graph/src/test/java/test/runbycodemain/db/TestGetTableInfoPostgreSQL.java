package test.runbycodemain.db;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.db.TableColumnInfo;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import test.annotation.JACGExample;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.junit.JUnit4ClassRunnerSortMethod;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2026/6/14
 * @description: 测试 DbOperWrapper 获取表名及COMMENT、建表语句、表字段信息功能（PostgreSQL数据库）
 */
@RunWith(JUnit4ClassRunnerSortMethod.class)
@JACGExample(title = "获取数据库表名及COMMENT、建表语句、表字段信息（PostgreSQL数据库）",
        desc = {})
public class TestGetTableInfoPostgreSQL {

    // 获得数据库表名及COMMENT
    @Test
    public void testGetTableNameAndCommentMap() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, org.postgresql.Driver.class.getName());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:postgresql://127.0.0.1:5432/testdb?currentSchema=jacg&sslmode=disable&useUnicode=true&characterEncoding=UTF-8");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            Map<String, String> tableNameAndCommentMap = dbOperWrapper.getTableNameAndCommentMap();
            Assert.assertNotNull(tableNameAndCommentMap);
            Assert.assertFalse("应存在以appName结尾的表", tableNameAndCommentMap.isEmpty());

            String appName = dbOperator.getAppName();
            for (Map.Entry<String, String> entry : tableNameAndCommentMap.entrySet()) {
                Assert.assertTrue("表名应以appName结尾: " + entry.getKey(),
                        entry.getKey().endsWith(appName));
                System.out.println("表名: [" + entry.getKey() + "] COMMENT: [" + entry.getValue() + "]");
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 获得指定数据库表的建表语句
    @Test
    public void testGetCreateTableSql() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, org.postgresql.Driver.class.getName());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:postgresql://127.0.0.1:5432/testdb?currentSchema=jacg&sslmode=disable&useUnicode=true&characterEncoding=UTF-8");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            Map<String, String> tableNameAndCommentMap = dbOperWrapper.getTableNameAndCommentMap();
            Assert.assertFalse(tableNameAndCommentMap.isEmpty());

            String firstTableName = tableNameAndCommentMap.keySet().iterator().next();
            String createTableSql = dbOperWrapper.getCreateTableSql(firstTableName);
            Assert.assertNotNull("建表语句不应为null", createTableSql);
            Assert.assertTrue("建表语句应包含CREATE TABLE",
                    createTableSql.toUpperCase().contains("CREATE TABLE"));
            Assert.assertTrue("建表语句应包含PRIMARY KEY",
                    createTableSql.toUpperCase().contains("PRIMARY KEY"));
            System.out.println("表 [" + firstTableName + "] 的建表语句:\n" + createTableSql);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 获得不存在的表的建表语句
    @Test
    public void testGetCreateTableSqlNotExists() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, org.postgresql.Driver.class.getName());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:postgresql://127.0.0.1:5432/testdb?currentSchema=jacg&sslmode=disable&useUnicode=true&characterEncoding=UTF-8");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String createTableSql = dbOperWrapper.getCreateTableSql("not_exists_table");
            Assert.assertNull("不存在的表建表语句应为null", createTableSql);
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 获得所有数据库表的建表语句
    @Test
    public void testGetAllCreateTableSqlMap() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, org.postgresql.Driver.class.getName());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:postgresql://127.0.0.1:5432/testdb?currentSchema=jacg&sslmode=disable&useUnicode=true&characterEncoding=UTF-8");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            Map<String, String> createTableSqlMap = dbOperWrapper.getAllCreateTableSqlMap();
            Assert.assertNotNull(createTableSqlMap);
            Assert.assertFalse("应存在以appName结尾的表", createTableSqlMap.isEmpty());

            for (Map.Entry<String, String> entry : createTableSqlMap.entrySet()) {
                Assert.assertNotNull("建表语句不应为null: " + entry.getKey(), entry.getValue());
                Assert.assertTrue("建表语句应包含CREATE TABLE: " + entry.getKey(),
                        entry.getValue().toUpperCase().contains("CREATE TABLE"));
                System.out.println("表 [" + entry.getKey() + "] 建表语句: " + entry.getValue());
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // ---- queryTableColumnInfo 相关测试 ----

    // 准备数据库操作对象（PostgreSQL数据库）
    private DbOperWrapper prepareDbOperWrapper() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, org.postgresql.Driver.class.getName());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:postgresql://127.0.0.1:5432/testdb?currentSchema=jacg&sslmode=disable&useUnicode=true&characterEncoding=UTF-8");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "test");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
        return DbInitializer.genDbOperWrapper(configureWrapper, this);
    }

    // 查询全部表的字段信息
    @Test
    public void testQueryTableColumnInfoAll() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 表名列表为空，查询全部表
            List<TableColumnInfo> columnInfoList = dbOperWrapper.queryTableColumnInfo(null);
            Assert.assertNotNull("查询结果不应为null", columnInfoList);
            Assert.assertFalse("查询结果不应为空", columnInfoList.isEmpty());

            System.out.println("查询全部表的字段信息数量: " + columnInfoList.size());
            for (TableColumnInfo columnInfo : columnInfoList) {
                System.out.println("  表: [" + columnInfo.getTableName() + "] 字段: [" + columnInfo.getColumnName()
                        + "] 类型: [" + columnInfo.getColumnType() + "] 描述: [" + columnInfo.getColumnComment() + "]");
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 查询指定表的字段信息
    @Test
    public void testQueryTableColumnInfoSpecified() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 获取第一个表名
            String firstTableName = dbOperWrapper.getTableNameAndCommentMap().keySet().iterator().next();
            List<String> tableNameList = new ArrayList<>();
            tableNameList.add(firstTableName);

            List<TableColumnInfo> columnInfoList = dbOperWrapper.queryTableColumnInfo(tableNameList);
            Assert.assertNotNull("查询结果不应为null", columnInfoList);
            Assert.assertFalse("查询结果不应为空", columnInfoList.isEmpty());

            // 所有字段信息都应属于指定的表
            for (TableColumnInfo columnInfo : columnInfoList) {
                Assert.assertEquals("字段信息应属于指定表", firstTableName, columnInfo.getTableName());
            }

            System.out.println("查询表 [" + firstTableName + "] 的字段信息数量: " + columnInfoList.size());
            for (TableColumnInfo columnInfo : columnInfoList) {
                System.out.println("  字段: [" + columnInfo.getColumnName() + "] 类型: [" + columnInfo.getColumnType()
                        + "] 描述: [" + columnInfo.getColumnComment() + "]");
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 查询指定表（包含不属于当前项目的表名）- 应过滤掉不属于当前项目的表
    @Test
    public void testQueryTableColumnInfoFilterInvalid() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 获取第一个表名
            String firstTableName = dbOperWrapper.getTableNameAndCommentMap().keySet().iterator().next();
            // 指定表名列表包含一个不属于当前项目的表名
            List<String> tableNameList = new ArrayList<>();
            tableNameList.add(firstTableName);
            tableNameList.add("not_exist_table_123");

            List<TableColumnInfo> columnInfoList = dbOperWrapper.queryTableColumnInfo(tableNameList);
            Assert.assertNotNull("查询结果不应为null", columnInfoList);
            Assert.assertFalse("查询结果不应为空", columnInfoList.isEmpty());

            // 所有字段信息都应属于有效表，不应包含不存在的表
            for (TableColumnInfo columnInfo : columnInfoList) {
                Assert.assertNotEquals("不应包含不存在的表", "not_exist_table_123", columnInfo.getTableName());
            }

            System.out.println("过滤无效表后字段信息数量: " + columnInfoList.size());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 查询指定表（全部为不属于当前项目的表名）- 应返回null
    @Test
    public void testQueryTableColumnInfoAllInvalid() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 指定的表名均不属于当前项目
            List<String> tableNameList = new ArrayList<>();
            tableNameList.add("not_exist_table_123");

            List<TableColumnInfo> columnInfoList = dbOperWrapper.queryTableColumnInfo(tableNameList);
            Assert.assertNull("全部为无效表名时应返回null", columnInfoList);

            System.out.println("全部为无效表名时返回null，符合预期");
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 查询空表名列表 - 应查询全部表
    @Test
    public void testQueryTableColumnInfoEmptyList() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 空表名列表，应查询全部表
            List<TableColumnInfo> columnInfoList = dbOperWrapper.queryTableColumnInfo(Collections.emptyList());
            Assert.assertNotNull("查询结果不应为null", columnInfoList);
            Assert.assertFalse("查询结果不应为空", columnInfoList.isEmpty());

            System.out.println("空表名列表查询全部表字段信息数量: " + columnInfoList.size());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }
}
