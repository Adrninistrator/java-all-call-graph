package test.callgraph.spring.aop.annotation1.service;

public interface TestSpringAOPAnnotationUserService1 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}