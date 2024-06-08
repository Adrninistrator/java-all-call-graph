package test.callgraph.extendcomplex2;

/**
 * @author adrninistrator
 * @date 2023/11/22
 * @description:
 */
public class TestUseService {

    private EC2CService ec2CServiceGlobal;

    public void test() {
        EC2BService ec2BService = new EC2BService();
        ec2BService.exec();

        ec2CServiceGlobal.exec();
        ec2CServiceGlobal.exec2();
    }
}
