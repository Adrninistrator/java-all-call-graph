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
 * @description: 测试 DbOperWrapper 查询任意数据功能（PostgreSQL数据库）
 */
@RunWith(JUnit4ClassRunnerSortMethod.class)
@JACGExample(title = "查询任意数据（PostgreSQL数据库）",
        desc = {})
public class TestQueryDataPostgreSQL {

    // 查询任意数据，有参数
    @Test
    public void testQueryDataWithParam() {
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
            String sql = "SELECT c.relname AS TABLE_NAME, obj_description(c.oid) AS REMARKS " +
                    "FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace " +
                    "WHERE n.nspname = current_schema() AND c.relkind = 'r' AND c.relname LIKE ? LIMIT ?";
            Map<Integer, Object> paramMap = new HashMap<>();
            paramMap.put(0, "%jacg");
            paramMap.put(1, 5);

            System.out.println("SQL: " + sql);
            System.out.println("参数: " + paramMap);

            QueryDataResult queryDataResult = dbOperWrapper.queryData(sql, paramMap);
            Assert.assertTrue("查询应成功", queryDataResult.isSuccess());
            Assert.assertFalse("不应超时", queryDataResult.isTimeout());
            Assert.assertNull("失败信息应为null", queryDataResult.getErrorMsg());

            List<Map<String, Object>> result = queryDataResult.getResultList();
            System.out.println("查询结果数量: " + result.size() + " 耗时: " + queryDataResult.getCostTimeMs() + " ms");
            for (Map<String, Object> row : result) {
                System.out.println("  TABLE_NAME: [" + row.get("TABLE_NAME") + "] REMARKS: [" + row.get("REMARKS") + "]");
            }
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 查询任意数据，无参数
    @Test
    public void testQueryDataNoParam() {
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

    // 带超时的查询（成功，不触发超时）
    @Test
    public void testQueryDataWithTimeoutSuccess() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String sql = "SELECT c.relname AS TABLE_NAME, obj_description(c.oid) AS REMARKS " +
                    "FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace " +
                    "WHERE n.nspname = current_schema() AND c.relkind = 'r' AND c.relname LIKE ? LIMIT ?";
            Map<Integer, Object> paramMap = new HashMap<>();
            paramMap.put(0, "%jacg");
            paramMap.put(1, 5);

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
            System.out.println("查询结果数量: " + result.size() + " 耗时: " + queryDataResult.getCostTimeMs() + " ms");
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 带超时的查询（超时触发）
    // PostgreSQL使用pg_sleep函数制造耗时查询，超时时间1秒，pg_sleep(3)需要3秒完成
    @Test
    public void testQueryDataWithTimeoutTrigger() {
        DbOperWrapper dbOperWrapper = prepareDbOperWrapper();
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // PostgreSQL的pg_sleep(N)函数让查询暂停N秒
            String sql = "SELECT pg_sleep(3)";
            System.out.println("SQL: " + sql);
            System.out.println("超时时间(秒): 1（预期触发超时）");

            // 超时时间1秒，pg_sleep(3)需要3秒完成，应触发超时
            QueryDataResult queryDataResult = dbOperWrapper.queryDataWithTimeout(sql, null, 1);
            Assert.assertFalse("查询应失败", queryDataResult.isSuccess());
            Assert.assertTrue("应超时", queryDataResult.isTimeout());
            Assert.assertNotNull("失败信息不应为null", queryDataResult.getErrorMsg());
            Assert.assertTrue("查询耗时应大于0", queryDataResult.getCostTimeMs() > 0);

            System.out.println("查询耗时: " + queryDataResult.getCostTimeMs() + " ms 超时: " + queryDataResult.isTimeout()
                    + " 失败信息: " + queryDataResult.getErrorMsg());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    // 带超时的查询 - SQL注入防护（非SELECT语句被拒绝）
    // 测试PostgreSQL数据库方言下的Druid SQL解析是否正确拒绝非SELECT语句
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
            System.out.println("混合语句失败信息: " + result5.getErrorMsg());
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }
}
