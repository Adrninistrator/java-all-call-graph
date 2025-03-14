package test.runbycode.handler.methodcall.checker;

import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.handler.methodcall.MethodCallStaticFieldHandler;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/3/9
 * @description:
 */
public class TestSqlKeyEnumChecker extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestSqlKeyEnumChecker.class);

    @Test
    public void $test0WriteDb() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "jar_output_dir/jar/run_jacg.jar");
        commonWriteDb();
    }

    @Test
    public void checkDuplicateSqlKey() {
        try (MethodCallStaticFieldHandler methodCallStaticFieldHandler = new MethodCallStaticFieldHandler(configureWrapper)) {
            List<String> fieldNameList = methodCallStaticFieldHandler.queryClassStaticFieldNameList(SqlKeyEnum.class.getName());
            Set<String> fieldNameSet = new HashSet<>();
            Set<String> dupFieldNameSet = new HashSet<>();
            for (String fieldName : fieldNameList) {
                if (!fieldNameSet.add(fieldName)) {
                    dupFieldNameSet.add(fieldName);
                }
            }
            if (dupFieldNameSet.isEmpty()) {
                logger.info("{} 不存在重复使用", SqlKeyEnum.class.getName());
                return;
            }
            logger.error("{} 存在重复使用 {}", SqlKeyEnum.class.getName(), StringUtils.join(dupFieldNameSet, " "));
            Assert.fail();
        }
    }
}
