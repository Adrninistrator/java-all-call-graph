package test.callgraph.spring.aop.annopointcut2.service;

import test.callgraph.spring.aop.annopointcut2.dto.TestAnnoPointcut2Dto;

public interface TestSpringAOPAnnoPointcutUserService2 {
    String getUserById(int id, TestAnnoPointcut2Dto dto);

    TestAnnoPointcut2Dto updateUser(String user);

    void deleteUser(int id) throws Exception;
}