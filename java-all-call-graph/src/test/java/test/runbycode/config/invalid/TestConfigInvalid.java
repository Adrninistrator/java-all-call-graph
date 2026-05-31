package test.runbycode.config.invalid;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2ConfigException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2026/5/24
 * @description: 验证各配置参数对于合法及非法值赋值的处理是否正确
 */
public class TestConfigInvalid {

    private static final Logger logger = LoggerFactory.getLogger(TestConfigInvalid.class);

    private ConfigureWrapper configureWrapper;

    @Before
    public void init() {
        configureWrapper = new ConfigureWrapper();
    }

    // 测试 CKE_APP_NAME 合法值
    @Test
    public void testAppName_Valid() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, "test_app");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, "TestApp123");
    }

    // 测试 CKE_APP_NAME 非法值-包含特殊字符
    @Test
    public void testAppName_InvalidSpecialChar() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, "test@app"));
        logger.error("error ", e);
    }

    // 测试 CKE_APP_NAME 非法值-包含-
    @Test
    public void testAppName_InvalidDash() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, "test-app"));
        logger.error("error ", e);
    }

    // 测试 CKE_CALL_GRAPH_OUTPUT_DETAIL 合法值
    @Test
    public void testOutputDetail_Valid() {
        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL != outputDetailEnum) {
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
            }
        }
    }

    // 测试 CKE_CALL_GRAPH_OUTPUT_DETAIL 非法值
    @Test
    public void testOutputDetail_Invalid() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, "9"));
        logger.error("error ", e);
    }

    // 测试 CKE_THREAD_NUM 合法值
    @Test
    public void testThreadNum_Valid() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, "1");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, String.valueOf(JACGConstants.MAX_THREAD_NUM));
    }

    // 测试 CKE_THREAD_NUM 非法值-超出范围
    @Test
    public void testThreadNum_InvalidTooLarge() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, String.valueOf(JACGConstants.MAX_THREAD_NUM + 1)));
        logger.error("error ", e);
    }

    // 测试 CKE_THREAD_NUM 非法值-小于1
    @Test
    public void testThreadNum_InvalidZero() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, "0"));
        logger.error("error ", e);
    }

    // 测试 CKE_DB_INSERT_BATCH_SIZE 合法值
    @Test
    public void testDbInsertBatchSize_Valid() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "1");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, String.valueOf(JACGConstants.MAX_DB_INSERT_BATCH_SIZE));
    }

    // 测试 CKE_DB_INSERT_BATCH_SIZE 非法值-超出范围
    @Test
    public void testDbInsertBatchSize_InvalidTooLarge() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, String.valueOf(JACGConstants.MAX_DB_INSERT_BATCH_SIZE + 1)));
        logger.error("error ", e);
    }

    // 测试 CKE_DB_INSERT_BATCH_SIZE 非法值-小于1
    @Test
    public void testDbInsertBatchSize_InvalidZero() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "0"));
        logger.error("error ", e);
    }

    // 测试 CKE_OUTPUT_DIR_NAME 合法值
    @Test
    public void testOutputDirName_Valid() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, "");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, "test_dir");
    }

    // 测试 CKE_OUTPUT_DIR_NAME 非法值-包含目录分隔符
    @Test
    public void testOutputDirName_InvalidSeparator() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, "test/dir"));
        logger.error("error ", e);
    }

    // 测试 CKE_OUTPUT_DIR_FLAG 合法值
    @Test
    public void testOutputDirFlag_Valid() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "test_flag");
    }

    // 测试 CKE_OUTPUT_DIR_FLAG 非法值-包含目录分隔符
    @Test
    public void testOutputDirFlag_InvalidSeparator() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "test\\flag"));
        logger.error("error ", e);
    }

    // 测试 Boolean 类型参数合法值
    @Test
    public void testBooleanValid() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DROP_OR_TRUNCATE_TABLE, "true");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DROP_OR_TRUNCATE_TABLE, "false");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DROP_OR_TRUNCATE_TABLE, "TRUE");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DROP_OR_TRUNCATE_TABLE, "FALSE");
    }

    // 测试 Boolean 类型参数非法值
    @Test
    public void testBooleanInvalid() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DROP_OR_TRUNCATE_TABLE, "yes"));
        logger.error("error ", e);
    }

    // 测试参数不允许为null
    @Test
    public void testNullValue() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, null));
        logger.error("error ", e);
    }

    // 测试不允许为空的参数传入空字符串
    @Test
    public void testNotBlankEmptyValue() {
        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, ""));
        logger.error("error ", e);
    }

    // 测试 CDKE_DB_H2_FILE_PATH 合法值-去掉后缀
    @Test
    public void testDbH2FilePath_Valid() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/test_db");
    }

    // 测试 CDKE_DB_H2_FILE_PATH 合法值-带后缀自动处理
    @Test
    public void testDbH2FilePath_WithExt() {
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/test_db.mv.db");
    }
}
