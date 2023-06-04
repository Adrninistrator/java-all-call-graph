package test.call_graph.extend_complex2;

/**
 * @author adrninistrator
 * @date 2023/6/1
 * @description:
 */
public class EC2CService extends EC2BService {

    public void exec() {
        System.out.println("do something");
        super.exec();
    }

    public void exec2() {
        System.out.println("do something2");
        super.exec();
    }

    protected void get() {
        getC();
    }

    private void getC() {
        System.out.println("c");
    }

    public static void main(String[] args) {
        EC2CService service = new EC2CService();
        service.exec();
        service.exec2();

        EC2BService ec2BService = new EC2BService();
        ec2BService.exec();
    }
}
