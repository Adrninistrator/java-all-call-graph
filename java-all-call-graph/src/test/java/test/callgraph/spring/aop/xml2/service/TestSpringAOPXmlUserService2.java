package test.callgraph.spring.aop.xml2.service;

public interface TestSpringAOPXmlUserService2 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}