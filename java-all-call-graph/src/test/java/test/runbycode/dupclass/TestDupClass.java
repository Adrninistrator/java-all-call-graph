package test.runbycode.dupclass;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/12/5
 * @description:
 */
@JACGExample(title = "解析包含重复同名类的代码并将结果写入数据库",
        desc = {"需要先执行 unittest.gradle 中的命令，生成用于比较的示例jar包"})
public class TestDupClass extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setAllowAllClasses();
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "build/libs/jar-diff-version-1/jar-diff-1.jar",
                "build/libs/jar-diff-version-2/jar-diff-2.jar");
        Assert.assertTrue(new RunnerWriteDb(configureWrapper).run());
    }
}
