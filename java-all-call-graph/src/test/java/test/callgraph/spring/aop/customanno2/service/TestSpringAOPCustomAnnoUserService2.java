package test.callgraph.spring.aop.customanno2.service;

public interface TestSpringAOPCustomAnnoUserService2 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}