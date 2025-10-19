package test.runbycode.el.springaop;

import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import org.junit.Test;
import test.callgraph.spring.aop.annopointcut1.service.TestSpringAOPAnnoPointcutUserServiceImpl1;
import test.runbycode.base.TestElRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/30
 * @description:
 */
public class TestSpringAopElFixedFalse extends TestElRunByCodeBase {
    @Override
    protected ElConfigEnum chooseElConfigEnum() {
        return ElConfigEnum.ECE_SPRING_AOP_IGNORE_SPRING_BEAN_CLASS;
    }

    @Override
    protected String chooseElText() {
        return "false";
    }

    @Override
    protected String chooseTitle() {
        return "解析Spring AOP影响方法固定全部不忽略";
    }

    @Override
    protected String chooseDesc() {
        return "在解析Spring AOP影响方法时，使用固定的表达式，Bean全部不忽略";
    }

    @Test
    public void test() {
        writeDbSupportSpringAop(TestSpringAOPAnnoPointcutUserServiceImpl1.class.getName(), true);
    }
}