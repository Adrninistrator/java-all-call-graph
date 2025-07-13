package test.callgraph.spring.aop.customanno1.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.spring.aop.customanno1.annotation.TestSpringAOPCustomAnnotation1A;
import test.callgraph.spring.aop.customanno1.annotation.TestSpringAOPCustomAnnotation1B;

@TestSpringAOPCustomAnnotation1B
@Service
public class TestSpringAOPCustomAnnoUserServiceImpl1 implements TestSpringAOPCustomAnnoUserService1 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPCustomAnnoUserServiceImpl1.class);

    @TestSpringAOPCustomAnnotation1A
    @Override
    public String getUserById(int id) {
        logger.info("getUserById {}", id);
        return "User " + id;
    }

    @TestSpringAOPCustomAnnotation1A
    @Override
    public void updateUser(String user) {
        // 更新用户逻辑
        logger.info("updateUser {}", user);
    }

    @TestSpringAOPCustomAnnotation1A
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