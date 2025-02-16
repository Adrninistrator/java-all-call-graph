package test.runbycodemain;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/2/16
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入数据库，简单模式",
        desc = {"处理方法调用时不解析被调用对象和参数可能的类型与值",
                "仅解析.class文件，不解析.xml、.properties等其他类型的文件"})
public class Test0RunnerSimpleWriteDb extends TestRunByCodeBase {
    @Test
    public void test() {
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.FALSE.toString());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_PARSE_OTHER_TYPE_FILE, Boolean.FALSE.toString());
        Assert.assertTrue(new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run());
    }
}
