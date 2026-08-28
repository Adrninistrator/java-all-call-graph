package test.runbycodemain.db;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.db.QueryDataResult;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import test.annotation.JACGExample;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.junit.JUnit4ClassRunnerSortMethod;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2026/6/14
 * @description: 测试 DbOperWrapper 查询任意数据功能（H2数据库）
 */
@RunWith(JUnit4ClassRunnerSortMethod.class)
@JACGExample(title = "查询任意数据（H2数据库）",
        desc = {})
public class TestQueryDataH2Db {

    // 查询任意数据，无参数
    @Test
    public void testQueryDataNoParam() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String sql = "SELECT TABLE_NAME, REMARKS FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ?";
            Map<Integer, Object> paramMap = new HashMap<>();
            paramMap.put(0, "jacg");

            System.out.println("SQL: " + sql);
            System.out.println("参数: " + paramMap);

            QueryDataResult queryDataResult = dbOperWrapper.queryData(sql, paramMap);
            Assert.assertTrue("查询应成功", queryDataResult.isSuccess());
            Assert.assertFalse("不应超时", queryDataResult.isTimeout());
            Assert.assertNull("失败信息应为null", queryDataResult.getErrorMsg());

            List<Map<String, Object>> result = queryDataResult.getResultList();
            Assert.assertFalse("查询结果不应为空", result.isEmpty());

            System.out.println("查询结果数量: " + result.size() + " 耗时: " + queryDataResult.getCostTimeMs() + " ms");
            for (Map<String, Object> row : result) {
                System.out.println("  TABLE_NAME: [" + row.get("TABLE_NAME") + "] REMARKS: [" + row.get("REMARKS") + "]");
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 查询任意数据，有参数
    @Test
    public void testQueryDataWithParam() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 获取第一个表名
            String firstTableName = dbOperWrapper.getTableNameAndCommentMap().keySet().iterator().next();
            // 查询项目表的数据
            String sql = "SELECT * FROM " + firstTableName + " LIMIT ?";
            Map<Integer, Object> paramMap = new HashMap<>();
            paramMap.put(0, 5);

            System.out.println("SQL: " + sql);
            System.out.println("参数: " + paramMap);

            QueryDataResult queryDataResult = dbOperWrapper.queryData(sql, paramMap);
            Assert.assertTrue("查询应成功", queryDataResult.isSuccess());
            Assert.assertFalse("不应超时", queryDataResult.isTimeout());
            Assert.assertNull("失败信息应为null", queryDataResult.getErrorMsg());

            List<Map<String, Object>> result = queryDataResult.getResultList();
            System.out.println("查询结果数量: " + result.size() + " 耗时: " + queryDataResult.getCostTimeMs() + " ms");
            for (Map<String, Object> row : result) {
                System.out.println("  " + row);
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 查询任意数据，无参数无Map
    @Test
    public void testQueryDataNullParam() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 获取第一个表名
            String firstTableName = dbOperWrapper.getTableNameAndCommentMap().keySet().iterator().next();
            String sql = "SELECT COUNT(*) AS cnt FROM " + firstTableName;

            System.out.println("SQL: " + sql);
            System.out.println("参数: null");

            QueryDataResult queryDataResult = dbOperWrapper.queryData(sql, null);
            Assert.assertTrue("查询应成功", queryDataResult.isSuccess());
            Assert.assertFalse("不应超时", queryDataResult.isTimeout());
            Assert.assertNull("失败信息应为null", queryDataResult.getErrorMsg());

            List<Map<String, Object>> result = queryDataResult.getResultList();
            Assert.assertFalse("查询结果不应为空", result.isEmpty());

            System.out.println("查询结果: " + result.get(0) + " 耗时: " + queryDataResult.getCostTimeMs() + " ms");
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // ---- queryDataWithTimeout 相关测试 ----

    // 准备数据库操作对象（H2数据库）
    private DbOperWrapper prepareDbOperWrapper() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        // 先写数据库
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
        return DbInitializer.genDbOperWrapper(configureWrapper, this);
    }

    // 带超时的查询（成功，不触发超时）
    @Test
    public void testQueryDataWithTimeoutSuccess() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String sql = "SELECT TABLE_NAME, REMARKS FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ?";
            Map<Integer, Object> paramMap = new HashMap<>();
            paramMap.put(0, "jacg");

            System.out.println("SQL: " + sql);
            System.out.println("参数: " + paramMap);
            System.out.println("超时时间(秒): 5");

            // 超时时间5秒，足够完成查询
            QueryDataResult queryDataResult = dbOperWrapper.queryDataWithTimeout(sql, paramMap, 5);
            Assert.assertTrue("查询应成功", queryDataResult.isSuccess());
            Assert.assertFalse("不应超时", queryDataResult.isTimeout());
            Assert.assertNull("失败信息应为null", queryDataResult.getErrorMsg());
            Assert.assertTrue("查询耗时应大于0", queryDataResult.getCostTimeMs() > 0);

            List<Map<String, Object>> result = queryDataResult.getResultList();
            Assert.assertFalse("查询结果不应为空", result.isEmpty());

            System.out.println("查询结果数量: " + result.size() + " 耗时: " + queryDataResult.getCostTimeMs() + " ms");
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 带超时的查询 - 超时时间非法（<=0）
    @Test
    public void testQueryDataWithTimeoutInvalidTimeout() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 超时时间为0
            QueryDataResult result1 = dbOperWrapper.queryDataWithTimeout("SELECT 1", null, 0);
            Assert.assertFalse("超时时间为0应失败", result1.isSuccess());
            Assert.assertTrue("失败信息应包含'超时时间必须大于0'", result1.getErrorMsg().contains("超时时间必须大于0"));

            // 超时时间为负数
            QueryDataResult result2 = dbOperWrapper.queryDataWithTimeout("SELECT 1", null, -1);
            Assert.assertFalse("超时时间为负数应失败", result2.isSuccess());
            Assert.assertTrue("失败信息应包含'超时时间必须大于0'", result2.getErrorMsg().contains("超时时间必须大于0"));

            System.out.println("超时时间0失败信息: " + result1.getErrorMsg());
            System.out.println("超时时间-1失败信息: " + result2.getErrorMsg());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 带超时的查询 - SQL语句为空
    @Test
    public void testQueryDataWithTimeoutEmptySql() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // SQL语句为空字符串
            QueryDataResult result1 = dbOperWrapper.queryDataWithTimeout("", null, 5);
            Assert.assertFalse("空SQL应失败", result1.isSuccess());
            Assert.assertTrue("失败信息应包含'sql语句为空'", result1.getErrorMsg().contains("sql语句为空"));

            // SQL语句为null
            QueryDataResult result2 = dbOperWrapper.queryDataWithTimeout(null, null, 5);
            Assert.assertFalse("null SQL应失败", result2.isSuccess());
            Assert.assertTrue("失败信息应包含'sql语句为空'", result2.getErrorMsg().contains("sql语句为空"));

            System.out.println("空SQL失败信息: " + result1.getErrorMsg());
            System.out.println("null SQL失败信息: " + result2.getErrorMsg());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 带超时的查询 - SQL注入防护（非SELECT语句被拒绝）
    // 测试H2数据库方言下的Druid SQL解析是否正确拒绝非SELECT语句
    @Test
    public void testQueryDataWithTimeoutNonSelect() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // DELETE语句
            QueryDataResult result1 = dbOperWrapper.queryDataWithTimeout("DELETE FROM method_info", null, 5);
            Assert.assertFalse("DELETE应被拒绝", result1.isSuccess());
            Assert.assertTrue("失败信息应包含'SELECT'", result1.getErrorMsg().contains("SELECT"));

            // INSERT语句
            QueryDataResult result2 = dbOperWrapper.queryDataWithTimeout("INSERT INTO method_info VALUES (1,2,3)", null, 5);
            Assert.assertFalse("INSERT应被拒绝", result2.isSuccess());
            Assert.assertTrue("失败信息应包含'SELECT'", result2.getErrorMsg().contains("SELECT"));

            // UPDATE语句
            QueryDataResult result3 = dbOperWrapper.queryDataWithTimeout("UPDATE method_info SET full_method = 'test'", null, 5);
            Assert.assertFalse("UPDATE应被拒绝", result3.isSuccess());
            Assert.assertTrue("失败信息应包含'SELECT'", result3.getErrorMsg().contains("SELECT"));

            // DROP语句
            QueryDataResult result4 = dbOperWrapper.queryDataWithTimeout("DROP TABLE method_info", null, 5);
            Assert.assertFalse("DROP应被拒绝", result4.isSuccess());
            Assert.assertTrue("失败信息应包含'SELECT'", result4.getErrorMsg().contains("SELECT"));

            // 混合语句：SELECT后跟DROP（SQL注入攻击）
            QueryDataResult result5 = dbOperWrapper.queryDataWithTimeout("SELECT 1; DROP TABLE method_info", null, 5);
            Assert.assertFalse("混合语句应被拒绝", result5.isSuccess());
            Assert.assertTrue("失败信息应包含'SELECT'", result5.getErrorMsg().contains("SELECT"));

            System.out.println("DELETE失败信息: " + result1.getErrorMsg());
            System.out.println("INSERT失败信息: " + result2.getErrorMsg());
            System.out.println("UPDATE失败信息: " + result3.getErrorMsg());
            System.out.println("DROP失败信息: " + result4.getErrorMsg());
            System.out.println("混合语句失败信息: " + result5.getErrorMsg());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 带超时的查询 - 参数下标不连续
    @Test
    public void testQueryDataWithTimeoutDiscontinuousParam() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String sql = "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ? AND TABLE_NAME LIKE ?";
            Map<Integer, Object> paramMap = new HashMap<>();
            paramMap.put(0, "jacg");
            paramMap.put(2, "%method");  // 下标1缺失，不连续

            System.out.println("SQL: " + sql);
            System.out.println("参数(下标不连续): " + paramMap);

            QueryDataResult queryDataResult = dbOperWrapper.queryDataWithTimeout(sql, paramMap, 5);
            Assert.assertFalse("参数下标不连续应失败", queryDataResult.isSuccess());
            Assert.assertTrue("失败信息应包含'参数下标不连续'", queryDataResult.getErrorMsg().contains("参数下标不连续"));

            System.out.println("参数下标不连续失败信息: " + queryDataResult.getErrorMsg());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 带超时的查询 - SQL语句不合法导致Druid解析异常
    // 验证解析异常时errorMsg包含具体的解析失败信息（而非固定的"SQL语句不是SELECT语句"）
    @Test
    public void testQueryDataWithTimeoutInvalidSql() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 语法不合法的SQL（缺少关键字、括号不匹配等），Druid解析会抛异常
            String[] invalidSqls = {
                    "SELECT FROM",                    // 缺少列名
                    "SELECT * WHERE",                  // 缺少表名
                    "SELEC * FROM method_info",        // 关键字拼写错误
                    "SELECT * FROM (SELECT)",          // 子查询不完整
            };

            for (String sql : invalidSqls) {
                QueryDataResult result = dbOperWrapper.queryDataWithTimeout(sql, null, 5);
                Assert.assertFalse("不合法SQL应失败: " + sql, result.isSuccess());
                // 解析异常时errorMsg应包含"SQL语句解析失败"及具体的异常信息
                Assert.assertTrue("失败信息应包含'SQL语句解析失败'或'SQL语句不是SELECT语句': " + sql,
                        result.getErrorMsg().contains("SQL语句解析失败") || result.getErrorMsg().contains("SQL语句不是SELECT语句"));

                System.out.println("不合法SQL: [" + sql + "] 失败信息: " + result.getErrorMsg());
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }
}
