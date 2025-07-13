package test.callgraph.spring.aop.xml1.service;

public interface TestSpringAOPXmlUserService1 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}