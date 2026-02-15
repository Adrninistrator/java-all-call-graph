package test.runbycode.analysejacg;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/5/28
 * @description: 检查SqlKeyEnum枚举常量没有被重复使用
 */
public class TestAnalyseJACG1CheckSqlKeyEnum extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestAnalyseJACG1CheckSqlKeyEnum.class);

    @Test
    public void test() throws Exception {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, false, this);
        String sql = "select field_name from (" +
                "select mcsf.field_name as field_name, count(mcsf.field_name) as cf" +
                " from " + DbTableInfoEnum.DTIE_METHOD_CALL_STATIC_FIELD.getTableName() + " as mcsf," +
                " " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() + " as mc" +
                " where mcsf.call_id = mc.call_id" +
                " and mcsf.simple_class_name = ?" +
                " and mc.callee_simple_class_name = ? and mc.callee_method_name = ?" +
                " group by mcsf.field_name" +
                " having cf > 1" +
                ") as t";
        sql = dbOperWrapper.formatSql(sql, true);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            List<String> list1 = dbOperator.queryListOneColumn(sql, String.class, SqlKeyEnum.class.getSimpleName(), DbOperWrapper.class.getSimpleName(), "getCachedSql");
            List<String> list2 = dbOperator.queryListOneColumn(sql, String.class, SqlKeyEnum.class.getSimpleName(), DbOperWrapper.class.getSimpleName(), "cacheSql");
            if (JavaCG2Util.isCollectionEmpty(list1) && JavaCG2Util.isCollectionEmpty(list2)) {
                logger.info("检查通过");
                return;
            }
            Set<String> sqlEnumSet = new HashSet<>();
            sqlEnumSet.addAll(list1);
            sqlEnumSet.addAll(list2);
            List<String> sqlEnumList = new ArrayList<>(sqlEnumSet);
            Collections.sort(sqlEnumList);
            logger.error("存在 {} 枚举使用超过一次 {}", SqlKeyEnum.class.getName(), StringUtils.join(sqlEnumList, " "));
            Assert.fail("检查不通过");
        }
    }
}
