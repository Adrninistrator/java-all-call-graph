package test.callgraph.spring.aop.multi.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.spring.aop.multi.annotation.TestSpringAOPCustomAnnotationMulti2;

@Service
public class TestSpringAOPXmlUserServiceImplMulti2 implements TestSpringAOPXmlUserServiceMulti2 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPXmlUserServiceImplMulti2.class);

    @TestSpringAOPCustomAnnotationMulti2
    @Override
    public String getUserById(int id) {
        logger.info("getUserById {}", id);
        return "User " + id;
    }

    @TestSpringAOPCustomAnnotationMulti2
    @Override
    public void updateUser(String user) {
        // 更新用户逻辑
        logger.info("updateUser {}", user);
    }

    @TestSpringAOPCustomAnnotationMulti2
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