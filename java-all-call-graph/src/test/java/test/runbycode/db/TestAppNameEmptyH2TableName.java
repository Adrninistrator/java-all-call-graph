package test.runbycode.db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

import java.util.Map;

/**
 * 验证 app.name 默认为空、显式指定 null、"" 时，生成的 H2 数据库表名是否符合预期（固定表名 jacg_关键字，无 _jacg 后缀）。
 *
 * <p>对应 prompt/数据库表名后缀支持为空 改造：空 app.name → 固定表名。
 */
@JACGExample(title = "验证空app.name生成固定H2表名",
        desc = {"默认（未设置app.name）、显式null、显式\"\" 三种情况",
                "断言生成的H2表名为 jacg_关键字（无 _jacg 后缀）"})
public class TestAppNameEmptyH2TableName extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestAppNameEmptyH2TableName.class);

    // 场景1：默认（未显式设置 app.name）——应使用空默认值，表名固定
    @Test
    public void test1DefaultEmptyAppName() {
        verifyEmptyAppNameTableNames("默认", null, false);
    }

    // 场景2：显式设置 app.name 为 null —— 应被配置层拒绝（setMainConfig 不接受null）
    @Test
    public void test2ExplicitNullAppName() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        try {
            // 显式设置 app.name 为 null，应抛出 JavaCG2ConfigException
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, null);
            Assert.fail("显式设置 app.name=null 应被配置层拒绝（抛出异常）");
        } catch (com.adrninistrator.javacg2.exceptions.JavaCG2ConfigException e) {
            // 预期：配置参数不允许为null
            Assert.assertTrue("应提示不允许为null: " + e.getMessage(),
                    e.getMessage().contains("null"));
            logger.info("显式null被正确拒绝: {}", e.getMessage());
        }
    }

    // 场景3：显式设置 app.name 为 ""
    @Test
    public void test3ExplicitEmptyStringAppName() {
        verifyEmptyAppNameTableNames("显式空串", "", true);
    }

    /**
     * 验证空 app.name 下生成的 H2 表名符合固定表名 jacg_关键字（无 _jacg 后缀）。
     *
     * @param label       场景标签（日志用）
     * @param appName     显式设置的 app.name 值（null 表示不显式设置或设为null）
     * @param explicitSet 是否显式设置 app.name（true=显式set，false=不设置用默认）
     */
    private void verifyEmptyAppNameTableNames(String label, String appName, boolean explicitSet) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TestConfigGenerator.TEST_JAR_PATH);

        ConfigureWrapper configureWrapper = new ConfigureWrapper();
        // 使用H2数据库，独立的库文件（按场景区分，避免互相覆盖）
        String h2DbPath = "./build/jacg_h2db_appname_empty_" + label;
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, h2DbPath);
        if (explicitSet) {
            // 显式设置 app.name（null 或 ""）
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, appName);
        }
        // 不显式设置时，使用 CKE_APP_NAME 默认值（应为空）

        // 先写数据库
        Assert.assertTrue(label + "：写数据库失败", new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());

        // 读取生成的H2库的表名
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            Map<String, String> tableNameAndCommentMap = dbOperWrapper.getTableNameAndCommentMap();
            Assert.assertNotNull(label + "：表名映射不应为null", tableNameAndCommentMap);
            Assert.assertFalse(label + "：应存在生成的表", tableNameAndCommentMap.isEmpty());

            // 抽查若干表，断言表名为固定 jacg_关键字（无 _jacg 后缀）
            String[] checkKeywords = {"method_call", "method_info", "class_name"};
            for (String keyword : checkKeywords) {
                String expectedTableName = "jacg_" + keyword;
                Assert.assertTrue(label + "：应存在固定表名 " + expectedTableName + "，实际表名: " + tableNameAndCommentMap.keySet(),
                        tableNameAndCommentMap.containsKey(expectedTableName));
            }

            // 断言不存在带 _jacg 后缀的旧式表名
            for (String tableName : tableNameAndCommentMap.keySet()) {
                Assert.assertFalse(label + "：不应存在带 _jacg 后缀的表名 " + tableName,
                        tableName.endsWith("_jacg"));
            }

            // 顺带验证 getDbTableInfoEnumByTableName 能按固定表名反查到枚举
            DbTableInfoEnum enumByTableName = dbOperWrapper.getDbTableInfoEnumByTableName("jacg_method_call");
            Assert.assertEquals(label + "：按固定表名反查枚举应为 DTIE_METHOD_CALL",
                    DbTableInfoEnum.DTIE_METHOD_CALL, enumByTableName);

            printMapContent(tableNameAndCommentMap, label + " 生成的表名");
        } catch (Exception e) {
            Assert.fail(label + "：异常 " + e.getMessage());
        }
    }
}
