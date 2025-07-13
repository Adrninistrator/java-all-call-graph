package test.callgraph.spring.aop.xml3.service;

public interface TestSpringAOPXmlUserService3 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}