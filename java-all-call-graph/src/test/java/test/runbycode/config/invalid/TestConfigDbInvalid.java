package test.runbycode.config.invalid;

import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.javacg2.exceptions.JavaCG2ConfigException;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

/**
 * @author adrninistrator
 * @date 2026/5/24
 * @description: 验证ConfigDbKeyEnum中配置参数对于H2/非H2数据库场景下合法与非法值的处理是否正确
 * 不仅设置配置参数，还会调用DbInitializer.genDbOperWrapper方法验证数据库配置是否正确
 */
public class TestConfigDbInvalid extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestConfigDbInvalid.class);

    // ===== CDKE_DB_USE_H2 测试 =====

    // 测试CDKE_DB_USE_H2 不允许为null
    @Test
    public void testDbUseH2_Null() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, null));
        logger.error("error ", e);
    }

    // 测试CDKE_DB_USE_H2 不允许为空
    @Test
    public void testDbUseH2_Empty() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, ""));
        logger.error("error ", e);
    }

    // ===== CDKE_DB_H2_FILE_PATH 测试 =====

    // 测试CDKE_DB_H2_FILE_PATH 不允许为null
    @Test
    public void testDbH2FilePath_Null() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, null));
        logger.error("error ", e);
    }

    // 测试CDKE_DB_H2_FILE_PATH 允许为空（notBlank=false）
    @Test
    public void testDbH2FilePath_EmptyAllowed() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "");
    }

    // 测试CDKE_DB_H2_FILE_PATH 合法值-不带后缀
    @Test
    public void testDbH2FilePath_ValidWithoutExt() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/test_db");
    }

    // 测试CDKE_DB_H2_FILE_PATH 合法值-带后缀自动处理
    @Test
    public void testDbH2FilePath_ValidWithExt() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/test_db.mv.db");
    }

    // 测试CDKE_DB_H2_FILE_PATH 使用绝对路径
    @Test
    public void testDbH2FilePath_AbsolutePath() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "D:/build/jacg_h2db");
    }

    // 测试CDKE_DB_H2_FILE_PATH 使用绝对路径带后缀
    @Test
    public void testDbH2FilePath_AbsolutePathWithExt() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "D:/build/jacg_h2db.mv.db");
    }

    // ===== CDKE_DB_DRIVER_NAME 测试 =====

    // 测试CDKE_DB_DRIVER_NAME 不允许为null
    @Test
    public void testDbDriverName_Null() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, null));
        logger.error("error ", e);
    }

    // 测试CDKE_DB_DRIVER_NAME 允许为空（notBlank=false）
    @Test
    public void testDbDriverName_EmptyAllowed() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "");
    }

    // 测试CDKE_DB_DRIVER_NAME 合法值-MySQL
    @Test
    public void testDbDriverName_MySql() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "com.mysql.cj.jdbc.Driver");
    }

    // 测试CDKE_DB_DRIVER_NAME 合法值-PostgreSQL
    @Test
    public void testDbDriverName_PostgreSql() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "org.postgresql.Driver");
    }

    // ===== CDKE_DB_URL 测试 =====

    // 测试CDKE_DB_URL 不允许为null
    @Test
    public void testDbUrl_Null() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL, null));
        logger.error("error ", e);
    }

    // 测试CDKE_DB_URL 允许为空（notBlank=false）
    @Test
    public void testDbUrl_EmptyAllowed() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL, "");
    }

    // 测试CDKE_DB_URL 合法值-MySQL
    @Test
    public void testDbUrl_MySql() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:mysql://127.0.0.1:3306/test?useUnicode=true&characterEncoding=UTF-8&rewriteBatchedStatements=true");
    }

    // 测试CDKE_DB_URL 合法值-PostgreSQL
    @Test
    public void testDbUrl_PostgreSql() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:postgresql://127.0.0.1:5432/test?currentSchema=public");
    }

    // ===== CDKE_DB_USERNAME 测试 =====

    // 测试CDKE_DB_USERNAME 不允许为null
    @Test
    public void testDbUsername_Null() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, null));
        logger.error("error ", e);
    }

    // 测试CDKE_DB_USERNAME 允许为空（notBlank=false）
    @Test
    public void testDbUsername_EmptyAllowed() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "");
    }

    // 测试CDKE_DB_USERNAME 合法值
    @Test
    public void testDbUsername_Valid() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "root");
    }

    // ===== CDKE_DB_PASSWORD 测试 =====

    // 测试CDKE_DB_PASSWORD 不允许为null
    @Test
    public void testDbPassword_Null() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, null));
        logger.error("error ", e);
    }

    // 测试CDKE_DB_PASSWORD 允许为空（notBlank=false）
    @Test
    public void testDbPassword_EmptyAllowed() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "");
    }

    // 测试CDKE_DB_PASSWORD 合法值
    @Test
    public void testDbPassword_Valid() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");
    }

    // ===== CDKE_DB_TABLE_SUFFIX 测试 =====

    // 测试CDKE_DB_TABLE_SUFFIX 不允许为null
    @Test
    public void testDbTableSuffix_Null() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, null));
        logger.error("error ", e);
    }

    // 测试CDKE_DB_TABLE_SUFFIX 允许为空
    @Test
    public void testDbTableSuffix_Empty() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, "");
    }

    // 测试CDKE_DB_TABLE_SUFFIX 合法值
    @Test
    public void testDbTableSuffix_Valid() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_TABLE_SUFFIX, "_test");
    }

    // ===== CDKE_SLOW_QUERY_SWITCH 测试 =====

    // 测试CDKE_SLOW_QUERY_SWITCH 开启
    @Test
    public void testSlowQuerySwitch_True() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_SWITCH, "true");
    }

    // 测试CDKE_SLOW_QUERY_SWITCH 关闭
    @Test
    public void testSlowQuerySwitch_False() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_SWITCH, "false");
    }

    // 测试CDKE_SLOW_QUERY_SWITCH 非法值
    @Test
    public void testSlowQuerySwitch_Invalid() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_SWITCH, "yes"));
        logger.error("error ", e);
    }

    // ===== CDKE_SLOW_QUERY_TIME 测试（Integer类型） =====

    // 测试CDKE_SLOW_QUERY_TIME 合法值
    @Test
    public void testSlowQueryTime_Valid() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_TIME, "200");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_TIME, "1000");
    }

    // 测试CDKE_SLOW_QUERY_TIME 非法值（非数字）
    @Test
    public void testSlowQueryTime_InvalidNotNum() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_TIME, "abc"));
        logger.error("error ", e);
    }

    // ===== CDKE_SLOW_QUERY_ROW_NUM 测试（Integer类型） =====

    // 测试CDKE_SLOW_QUERY_ROW_NUM 合法值
    @Test
    public void testSlowQueryRowNum_Valid() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_ROW_NUM, "5000");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_ROW_NUM, "10000");
    }

    // 测试CDKE_SLOW_QUERY_ROW_NUM 非法值（非数字）
    @Test
    public void testSlowQueryRowNum_InvalidNotNum() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_SLOW_QUERY_ROW_NUM, "xyz"));
        logger.error("error ", e);
    }

    // ===== H2数据库场景：设置参数并调用DbInitializer.genDbOperWrapper验证 =====

    // 使用H2数据库，指定H2文件路径，调用DbInitializer.genDbOperWrapper，预期成功
    @Test
    public void testH2Db_GenDbOperWrapper_Success() {
        TestConfigGenerator.useH2Db(configureWrapper);
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
        Assert.assertNotNull(dbOperWrapper);
    }

    // 使用H2数据库，指定H2文件路径（不带后缀），调用DbInitializer.genDbOperWrapper，预期成功
    @Test
    public void testH2DbPrefix_GenDbOperWrapper_Success() {
        TestConfigGenerator.useH2DbPrefix(configureWrapper);
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
        Assert.assertNotNull(dbOperWrapper);
    }

    // 使用H2数据库时，非H2数据库参数可以为空，调用DbInitializer.genDbOperWrapper，预期成功
    @Test
    public void testH2Db_NonH2ParamsEmpty_GenDbOperWrapper_Success() {
        TestConfigGenerator.useH2Db(configureWrapper);
        // 使用H2时，以下参数允许为空
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL, "");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "");
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
        Assert.assertNotNull(dbOperWrapper);
    }

    // 使用H2数据库时，H2文件路径为空，调用DbInitializer.genDbOperWrapper，预期失败
    @Test
    public void testH2Db_EmptyFilePath_GenDbOperWrapper_Fail() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, "true");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "");
        try {
            DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
            // 若创建成功，关闭数据库连接
            dbOperWrapper.getDbOperator().close();
            Assert.fail("应该抛出异常");
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 使用H2数据库文件，app.name留空（表名固定为 jacg_关键字），调用DbInitializer.genDbOperWrapper
    @Test
    public void testUseFixedAppNameH2Db_GenDbOperWrapper() {
        configureWrapper.useEmptyAppNameH2Db();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
        Assert.assertNotNull(dbOperWrapper);
    }

    // ===== 非H2数据库场景：设置参数并调用DbInitializer.genDbOperWrapper验证 =====

    // 使用非H2数据库时，指定MySQL配置参数，调用DbInitializer.genDbOperWrapper，预期失败（无法连接数据库）
    @Test
    public void testNonH2Db_MySql_GenDbOperWrapper_Fail() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, "false");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "com.mysql.cj.jdbc.Driver");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:mysql://127.0.0.1:3306/test?useUnicode=true&characterEncoding=UTF-8&rewriteBatchedStatements=true");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "root");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");
        try {
            DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
            dbOperWrapper.getDbOperator().close();
            Assert.fail("应该抛出异常");
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 使用非H2数据库时，指定PostgreSQL配置参数，调用DbInitializer.genDbOperWrapper，预期失败（无法连接数据库）
    @Test
    public void testNonH2Db_PostgreSql_GenDbOperWrapper_Fail() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, "false");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "org.postgresql.Driver");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:postgresql://127.0.0.1:5432/test?currentSchema=public");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "postgres");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");
        try {
            DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
            dbOperWrapper.getDbOperator().close();
            Assert.fail("应该抛出异常");
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 使用非H2数据库时，非H2参数全为空，调用DbInitializer.genDbOperWrapper，预期失败
    @Test
    public void testNonH2Db_EmptyParams_GenDbOperWrapper_Fail() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, "false");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL, "");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "");
        try {
            DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
            dbOperWrapper.getDbOperator().close();
            Assert.fail("应该抛出异常");
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 使用非H2数据库时，H2文件路径允许为空，调用DbInitializer.genDbOperWrapper，预期失败（无法连接数据库）
    @Test
    public void testNonH2Db_H2FilePathEmpty_GenDbOperWrapper_Fail() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, "false");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "com.mysql.cj.jdbc.Driver");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:mysql://127.0.0.1:3306/test?useUnicode=true&characterEncoding=UTF-8&rewriteBatchedStatements=true");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "root");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");
        // 使用非H2时，H2文件路径允许为空
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "");
        try {
            DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
            dbOperWrapper.getDbOperator().close();
            Assert.fail("应该抛出异常");
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 使用非H2数据库时，MySQL URL缺少rewriteBatchedStatements，调用DbInitializer.genDbOperWrapper，预期失败
    @Test
    public void testNonH2Db_MySqlWithoutRewriteBatchedStatements_GenDbOperWrapper_Fail() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, "false");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "com.mysql.cj.jdbc.Driver");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:mysql://127.0.0.1:3306/test?useUnicode=true&characterEncoding=UTF-8");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "root");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "123456");
        try {
            DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
            dbOperWrapper.getDbOperator().close();
            Assert.fail("应该抛出异常");
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 使用非H2数据库时，仅指定驱动类名和URL，用户名密码为空，调用DbInitializer.genDbOperWrapper，预期失败
    @Test
    public void testNonH2Db_EmptyUsernamePassword_GenDbOperWrapper_Fail() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, "false");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, "com.mysql.cj.jdbc.Driver");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
                "jdbc:mysql://127.0.0.1:3306/test?useUnicode=true&characterEncoding=UTF-8&rewriteBatchedStatements=true");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "");
        try {
            DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, true, this);
            dbOperWrapper.getDbOperator().close();
            Assert.fail("应该抛出异常");
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // ===== useEmptyAppNameH2Db 测试 =====

    // 测试使用H2数据库文件，app.name留空（表名固定为 jacg_关键字）
    @Test
    public void testUseFixedAppNameH2Db() {
        configureWrapper.useEmptyAppNameH2Db();
    }
}
