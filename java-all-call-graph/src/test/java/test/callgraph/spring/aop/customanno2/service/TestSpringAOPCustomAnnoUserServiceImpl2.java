package test.callgraph.spring.aop.customanno2.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.spring.aop.customanno2.annotation.TestSpringAOPCustomAnnotation2A;
import test.callgraph.spring.aop.customanno2.annotation.TestSpringAOPCustomAnnotation2B;

@TestSpringAOPCustomAnnotation2B
@Service
public class TestSpringAOPCustomAnnoUserServiceImpl2 implements TestSpringAOPCustomAnnoUserService2 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPCustomAnnoUserServiceImpl2.class);

    @TestSpringAOPCustomAnnotation2A
    @Override
    public String getUserById(int id) {
        logger.info("getUserById {}", id);
        return "User " + id;
    }

    @TestSpringAOPCustomAnnotation2A
    @Override
    public void updateUser(String user) {
        // 更新用户逻辑
        logger.info("updateUser {}", user);
    }

    @TestSpringAOPCustomAnnotation2A
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