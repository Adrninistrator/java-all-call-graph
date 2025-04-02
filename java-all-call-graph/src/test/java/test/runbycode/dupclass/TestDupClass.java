package test.runbycode.dupclass;

import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/12/5
 * @description:
 */
@JACGExample(title = "解析包含重复同名类的代码并将结果写入数据库",
        desc = {"需要先执行 gradle 命令，生成用于比较的示例jar包",
                "gradlew test_gen_diff_jar -Pexample_flag=1",
                "gradlew test_gen_diff_jar -Pexample_flag=2"})
public class TestDupClass extends TestRunByCodeBase {

    @Test
    public void test() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "build/jar-diff-version-1/jar-diff.jar",
                "build/jar-diff-version-2/jar-diff.jar");
        commonWriteDb();
    }
}
