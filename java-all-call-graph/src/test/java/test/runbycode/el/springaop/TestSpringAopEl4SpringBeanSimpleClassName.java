package test.runbycode.el.springaop;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import org.junit.Test;
import test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/30
 * @description:
 */
public class TestSpringAopEl4SpringBeanSimpleClassName extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " == '" + TestSpringAOPAnnoPointcutUserServiceImpl1.class.getSimpleName() + "'";
    }

    @Override
    protected String chooseTitle() {
        return "解析Spring AOP影响方法判断受影响Bean简单类名";
    }

    @Override
    protected String chooseDesc() {
        return "在解析Spring AOP影响方法时，判断受影响Bean简单类名是否等于指定关键字，忽略匹配的Bean";
    }

    @Test
    public void test() {
        writeDbSupportSpringAop(TestSpringAOPAnnoPointcutUserServiceImpl1.class.getName(), false);
    }
}