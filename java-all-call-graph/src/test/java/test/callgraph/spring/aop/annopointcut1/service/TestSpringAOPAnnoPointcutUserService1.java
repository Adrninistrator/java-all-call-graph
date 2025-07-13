package test.callgraph.spring.aop.annopointcut1.service;

public interface TestSpringAOPAnnoPointcutUserService1 {
    String getUserById(int[][] id);

    int[] updateUser(String[] user);

    String[][][] deleteUser(Object... id);
}