package test.callgraph.spring.aop.annopointcut1.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class TestSpringAOPAnnoPointcutLoggingAspect1 {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPAnnoPointcutLoggingAspect1.class);

    @Pointcut("	execution(* getUserById(..)) || execution(* updateUser(..)) || execution(* deleteUser(..))")
    public void methodCondition() {
    }

    @Pointcut("test.callgraph.spring.aop.annopointcut2.aspect.TestSpringAOPAnnoPointcutLoggingAspect2.classCondition1()		&&		methodCondition()	")
    public void classMethodCondition() {
    }

    // 前置通知
    @Before("test.callgraph.spring.aop.annopointcut2.aspect.TestSpringAOPAnnoPointcutLoggingAspect2.classCondition1         ()")
    public void logBefore(JoinPoint joinPoint) {
        logger.info("Before method: {}", joinPoint.getSignature().getName());
    }

    // 后置通知
    @After("classMethodCondition(                          	)       &&      methodCondition     ()")
    public void logAfter(JoinPoint joinPoint) {
        logger.info("After method: {}", joinPoint.getSignature().getName());
    }

    // 返回后通知
    @AfterReturning(pointcut = "classMethodCondition               ()", returning = "result")
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        logger.info("AfterReturning method: {}, result: {}", joinPoint.getSignature().getName(), result);
    }

    // 异常通知
    @AfterThrowing(pointcut = "classMethodCondition()", throwing = "error")
    public void logAfterThrowing(JoinPoint joinPoint, Throwable error) {
        logger.error("AfterThrowing in method: {} , exception: {}", joinPoint.getSignature().getName(), error);
    }

    // 环绕通知
    @Around("classMethodCondition()")
    public Object logAround(ProceedingJoinPoint joinPoint) throws Throwable {
        logger.info("Around before method: {}", joinPoint.getSignature().getName());
        Object result = joinPoint.proceed();
        logger.info("Around after method: {}", joinPoint.getSignature().getName());
        return result;
    }
}