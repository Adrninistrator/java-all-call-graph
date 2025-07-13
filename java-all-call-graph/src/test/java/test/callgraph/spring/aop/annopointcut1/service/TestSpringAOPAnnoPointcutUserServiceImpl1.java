package test.callgraph.spring.aop.annopointcut1.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class TestSpringAOPAnnoPointcutUserServiceImpl1 implements TestSpringAOPAnnoPointcutUserService1 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPAnnoPointcutUserServiceImpl1.class);

    @Override
    public String getUserById(int[][] id) {
        logger.info("getUserById");
        return "User ";
    }

    @Override
    public int[] updateUser(String[] user) {
        // 更新用户逻辑
        logger.info("updateUser");
        return null;
    }

    @Override
    public String[][][] deleteUser(Object... id) {
        // 删除用户逻辑
        logger.info("deleteUser {}", id);
        return null;
    }
}