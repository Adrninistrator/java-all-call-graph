package test.callgraph.spring.aop.annopointcut2.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.spring.aop.annopointcut2.dto.TestAnnoPointcut2Dto;

@Service
public class TestSpringAOPAnnoPointcutUserServiceImpl2 implements TestSpringAOPAnnoPointcutUserService2 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPAnnoPointcutUserServiceImpl2.class);

    @Override
    public String getUserById(int id, TestAnnoPointcut2Dto dto) {
        logger.info("getUserById {}", id);
        return "User " + id;
    }

    @Override
    public TestAnnoPointcut2Dto updateUser(String user) {
        // 更新用户逻辑
        logger.info("updateUser {}", user);
        return null;
    }

    @Override
    public void deleteUser(int id) throws Exception {
        if (id < 0) {
            logger.error("deleteUser error {}", id);
            throw new Exception("Invalid ID");
        }
        // 删除用户逻辑
        logger.info("deleteUser {}", id);
    }
}