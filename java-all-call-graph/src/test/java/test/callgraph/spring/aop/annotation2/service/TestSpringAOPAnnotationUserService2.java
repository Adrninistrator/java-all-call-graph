package test.callgraph.spring.aop.annotation2.service;

public interface TestSpringAOPAnnotationUserService2 {
    String getUserById(int id);

    void updateUser(String user);

    void deleteUser(int id) throws Exception;
}