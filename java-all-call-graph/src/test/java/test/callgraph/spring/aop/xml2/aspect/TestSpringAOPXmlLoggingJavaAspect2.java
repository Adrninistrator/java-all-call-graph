package test.callgraph.spring.aop.xml2.aspect;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

/**
 * @author adrninistrator
 * @date 2025/6/26
 * @description:
 */
@Aspect
@Component
public class TestSpringAOPXmlLoggingJavaAspect2 {

    @Pointcut("execution(* getUserById(..)) || execution(* updateUser(..)) || execution(* deleteUser(..))")
    public void methodCondition() {
    }

    @Pointcut("execution(* test.callgraph.spring.aop.xml2.service.*.*(..))")
    public void classMethodCondition() {
    }
}
