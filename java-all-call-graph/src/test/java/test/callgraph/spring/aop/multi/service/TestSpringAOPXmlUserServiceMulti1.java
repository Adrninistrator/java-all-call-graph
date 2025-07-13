package test.callgraph.spring.aop.multi.service;

public interface TestSpringAOPXmlUserServiceMulti1 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}