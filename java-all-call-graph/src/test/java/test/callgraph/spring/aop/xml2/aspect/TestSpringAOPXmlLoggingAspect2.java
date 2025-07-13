package test.callgraph.spring.aop.xml2.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestSpringAOPXmlLoggingAspect2 {
    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPXmlLoggingAspect2.class);

    // 前置通知
    public void logBefore(JoinPoint joinPoint) {
        logger.info("Before method: {}", joinPoint.getSignature().getName());
    }

    // 后置通知
    public void logAfter(JoinPoint joinPoint) {
        logger.info("After method: {}", joinPoint.getSignature().getName());
    }

    // 返回后通知
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        logger.info("AfterReturning method: {}, result: {}", joinPoint.getSignature().getName(), result);
    }

    // 异常通知
    public void logAfterThrowing(JoinPoint joinPoint, Throwable error) {
        logger.error("AfterThrowing in method: {} , exception: {}", joinPoint.getSignature().getName(), error);
    }

    // 环绕通知
    public Object logAround(ProceedingJoinPoint joinPoint) throws Throwable {
        logger.info("Around before method: {}", joinPoint.getSignature().getName());
        Object result = joinPoint.proceed();
        logger.info("Around after method: {}", joinPoint.getSignature().getName());
        return result;
    }
}