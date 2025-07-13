package test.callgraph.spring.aop.xml4.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class TestSpringAOPXmlUserServiceImpl4 implements TestSpringAOPXmlUserService4 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringAOPXmlUserServiceImpl4.class);

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
}