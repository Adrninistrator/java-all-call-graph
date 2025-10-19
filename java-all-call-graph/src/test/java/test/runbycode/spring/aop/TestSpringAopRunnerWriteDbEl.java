package test.runbycode.spring.aop;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/8/25
 * @description:
 */
@JACGExample(title = "解析代码并将结果写入数据库，对Spring AOP相关信息进行解析",
        desc = {"需要先执行 unittest.gradle 中的命令，生成示例jar包： gradlew test_gen_jar",
                "通过代码指定配置参数的主要功能示例",
                "通过EL表达式指定在处理Spring AOP时需要忽略哪些Spring Bean类"})
public class TestSpringAopRunnerWriteDbEl extends TestRunByCodeBase {

    // 对比 TestSpringAopRunnerWriteDb 类执行后 spring_aop_advice_affected_method 表的内容，数据会有差异
    @Test
    public void test() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_PARSE_SPRING_AOP_INFO, Boolean.TRUE.toString());
        configureWrapper.setElConfigText(ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS,
                JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == '" + TestSpringAOPAnnoPointcutUserServiceImpl1.class.getName() + "'");
        commonWriteDbForce();
    }
}
