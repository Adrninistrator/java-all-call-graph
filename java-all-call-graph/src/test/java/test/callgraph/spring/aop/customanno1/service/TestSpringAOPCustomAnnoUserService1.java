package test.callgraph.spring.aop.customanno1.service;

public interface TestSpringAOPCustomAnnoUserService1 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}