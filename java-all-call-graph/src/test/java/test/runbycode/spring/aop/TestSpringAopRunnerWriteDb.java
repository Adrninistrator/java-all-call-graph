package test.runbycode.spring.aop;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/6/29
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入数据库，对Spring AOP相关信息进行解析",
        desc = {"需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar",
                "通过代码指定配置参数的主要功能示例"})
public class TestSpringAopRunnerWriteDb extends TestRunByCodeBase {

    @Test
    public void test() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_PARSE_SPRING_AOP_INFO, Boolean.TRUE.toString());
        commonWriteDbForce();
    }
}
