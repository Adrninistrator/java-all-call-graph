package test.callgraph.spring.aop.xml2.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class TestSpringAOPXmlUserServiceImpl2 implements TestSpringAOPXmlUserService2 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPXmlUserServiceImpl2.class);

    @Override
    public String getUserById(int id) {
        logger.info("getUserById {}", id);
        return "User " + id;
    }

    @Override
    public void updateUser(String user) {
        // 更新用户逻辑
        logger.info("updateUser {}", user);
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