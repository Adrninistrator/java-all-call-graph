package test.runbycode.config;

import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2026/6/13
 * @description: 测试 DbOperator 数据库连接可用性检测
 */
public class TestDbOperatorConnectionAvailable extends TestRunByCodeBase {

    // 场景1：正常情况，数据库连接可用
    @Test
    public void test1ConnectionAvailable() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            Assert.assertTrue(dbOperator.isConnectionAvailable());
        } catch (Exception e) {
            Assert.fail("测试数据库连接可用性时出现异常: " + e.getMessage());
        }
    }

    // 场景2：正常情况，连续多次检测连接可用
    @Test
    public void test2ConnectionAvailableMultipleTimes() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            for (int i = 0; i < 5; i++) {
                Assert.assertTrue("第" + (i + 1) + "次检测连接应可用", dbOperator.isConnectionAvailable());
            }
        } catch (Exception e) {
            Assert.fail("测试数据库连接可用性时出现异常: " + e.getMessage());
        }
    }

    // 场景3：关闭数据源后，连接不可用
    @Test
    public void test3ConnectionUnavailableAfterClose() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        DbOperator dbOperator = dbOperWrapper.getDbOperator();
        // 关闭前连接可用
        Assert.assertTrue(dbOperator.isConnectionAvailable());
        // 关闭数据源
        dbOperator.closeDs(this);
        // 关闭后连接不可用
        Assert.assertFalse(dbOperator.isConnectionAvailable());
    }

    // 场景4：关闭数据源后，isClosed()返回true
    @Test
    public void test4ClosedAfterCloseDs() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        DbOperator dbOperator = dbOperWrapper.getDbOperator();
        Assert.assertFalse(dbOperator.isClosed());
        dbOperator.closeDs(this);
        Assert.assertTrue(dbOperator.isClosed());
    }

    // 场景5：使用try-with-resource关闭后，连接不可用
    @Test
    public void test5ConnectionUnavailableAfterTryWithResource() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            Assert.assertTrue(dbOperator.isConnectionAvailable());
        } catch (Exception e) {
            Assert.fail("测试数据库连接可用性时出现异常: " + e.getMessage());
        }
        // try-with-resource关闭后，获取同一个DbOperator检查
        DbOperator dbOperator = dbOperWrapper.getDbOperator();
        Assert.assertFalse(dbOperator.isConnectionAvailable());
    }

    // 场景6：执行数据库操作后，连接仍然可用
    @Test
    public void test6ConnectionAvailableAfterDbOperation() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 执行一次查询操作
            dbOperator.queryListOneColumn("SELECT 1", Integer.class);
            // 查询后连接仍然可用
            Assert.assertTrue(dbOperator.isConnectionAvailable());
        } catch (Exception e) {
            Assert.fail("测试数据库操作后连接可用性时出现异常: " + e.getMessage());
        }
    }
}
