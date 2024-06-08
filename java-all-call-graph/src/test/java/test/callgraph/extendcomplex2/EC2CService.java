package test.callgraph.extendcomplex2;

/**
 * @author adrninistrator
 * @date 2023/6/1
 * @description:
 */
public class EC2CService extends EC2BService {

    public String exec() {
        System.out.println("do something");
        return super.exec();
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
}
