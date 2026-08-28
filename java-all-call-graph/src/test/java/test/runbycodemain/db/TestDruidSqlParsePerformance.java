package test.runbycodemain.db;

import com.alibaba.druid.DbType;
import com.alibaba.druid.sql.SQLUtils;
import com.alibaba.druid.sql.ast.SQLStatement;
import org.junit.Test;
import org.junit.runner.RunWith;
import test.annotation.JACGExample;
import test.runbycode.junit.JUnit4ClassRunnerSortMethod;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/6/17
 * @description: 测试Druid解析SQL语句的性能开销，确认增加SQL注入防护（validateSelectSql）时对性能的影响
 */
@RunWith(JUnit4ClassRunnerSortMethod.class)
@JACGExample(title = "Druid解析SQL耗时性能测试",
        desc = "循环执行Druid SQL解析，打印执行次数、总耗时、平均耗时，评估SQL注入防护的性能开销")
public class TestDruidSqlParsePerformance {

    // 循环执行次数
    private static final int LOOP_COUNT = 100000;

    // 测试用的SQL语句（模拟queryData实际接收的各类查询）
    private static final String[] TEST_SQLS = {
            // 简单查询
            "SELECT * FROM method_info",
            // 带条件查询
            "SELECT full_method, simple_class_name FROM method_info WHERE simple_class_name = ? AND method_hash = ?",
            // 多表关联查询
            "SELECT a.full_method, b.method_hash FROM method_info a JOIN method_call b ON a.method_hash = b.caller_method_hash WHERE a.simple_class_name = ? LIMIT ?",
            // INFORMATION_SCHEMA查询（H2/MySQL常用）
            "SELECT TABLE_NAME, REMARKS FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = ?",
            // MySQL特有语法
            "SELECT TABLE_NAME, TABLE_COMMENT FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME LIKE ? LIMIT ?",
            // PostgreSQL特有语法
            "SELECT c.relname AS TABLE_NAME, obj_description(c.oid) AS REMARKS FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = current_schema() AND c.relkind = 'r'",
            // 聚合查询
            "SELECT COUNT(*) AS cnt FROM method_info WHERE simple_class_name LIKE ?",
            // 子查询
            "SELECT * FROM method_info WHERE method_hash IN (SELECT callee_method_hash FROM method_call WHERE caller_method_hash = ?)",
    };

    // 测试用的非法SQL（验证非SELECT语句解析开销，仅解析不判断结果）
    private static final String[] TEST_NON_SELECT_SQLS = {
            "DELETE FROM method_info WHERE method_hash = ?",
            "INSERT INTO method_info VALUES (?, ?, ?, ?, ?, ?, ?)",
            "UPDATE method_info SET full_method = ? WHERE method_hash = ?",
            "DROP TABLE method_info",
    };

    /**
     * 测试Druid解析SELECT语句的耗时（MySQL方言）
     */
    @Test
    public void testParseSelectMysql() {
        testParsePerformance("MySQL SELECT语句", TEST_SQLS, DbType.mysql);
    }

    /**
     * 测试Druid解析SELECT语句的耗时（H2方言）
     */
    @Test
    public void testParseSelectH2() {
        testParsePerformance("H2 SELECT语句", TEST_SQLS, DbType.h2);
    }

    /**
     * 测试Druid解析SELECT语句的耗时（PostgreSQL方言）
     */
    @Test
    public void testParseSelectPostgreSQL() {
        testParsePerformance("PostgreSQL SELECT语句", TEST_SQLS, DbType.postgresql);
    }

    /**
     * 测试Druid解析非SELECT语句的耗时（MySQL方言）
     */
    @Test
    public void testParseNonSelectMysql() {
        testParsePerformance("MySQL 非SELECT语句", TEST_NON_SELECT_SQLS, DbType.mysql);
    }

    /**
     * 测试Druid解析混合语句（SELECT + 非SELECT）的耗时（MySQL方言）
     */
    @Test
    public void testParseMixedMysql() {
        String[] mixedSqls = {
                "SELECT 1; DROP TABLE method_info",
                "SELECT * FROM method_info; DELETE FROM method_call",
        };
        testParsePerformance("MySQL 混合语句（注入攻击）", mixedSqls, DbType.mysql);
    }

    /**
     * 通用性能测试方法：循环执行SQLUtils.parseStatements，打印执行次数、总耗时、平均耗时
     *
     * @param label  测试标签（用于打印区分）
     * @param sqls   要测试的SQL语句数组
     * @param dbType Druid方言类型
     */
    private void testParsePerformance(String label, String[] sqls, DbType dbType) {
        System.out.println("===== " + label + " 性能测试 =====");
        System.out.println("方言: " + dbType);
        System.out.println("SQL语句数量: " + sqls.length);
        System.out.println("循环执行次数: " + LOOP_COUNT);
        System.out.println("总解析次数: " + (LOOP_COUNT * sqls.length));

        long startTime = System.currentTimeMillis();
        for (int i = 0; i < LOOP_COUNT; i++) {
            for (String sql : sqls) {
                List<SQLStatement> statementList = SQLUtils.parseStatements(sql, dbType);
                // 模拟validateSelectSql的检查逻辑：遍历解析结果判断是否为SELECT
                for (SQLStatement statement : statementList) {
                    statement.getClass().getName().contains("Select");
                }
            }
        }
        long totalCostTimeMs = System.currentTimeMillis() - startTime;
        long totalParseCount = LOOP_COUNT * sqls.length;
        double avgCostTimeMs = (double) totalCostTimeMs / totalParseCount;

        System.out.println("总耗时: " + totalCostTimeMs + " ms");
        System.out.println("平均耗时: " + String.format("%.4f", avgCostTimeMs) + " ms/次");
        System.out.println("执行次数: " + totalParseCount);
        System.out.println();
    }
}
