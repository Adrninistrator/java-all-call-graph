package test.callgraph.array;

/**
 * @author adrninistrator
 * @date 2025/5/25
 * @description:
 */
public class TestArrayFieldChild1 extends TestArrayFieldSuper1 {

    protected int[] f1;

    protected String f2;

    protected String f3;

    public void test1() {
        super.f1 = this.f1[0];
        this.f1[1] = super.f1;

        this.f2 = String.valueOf(super.f2[0]);
        super.f2[1] = Integer.parseInt(this.f2);

        super.f3 = this.f3;
        this.f3 = super.f3;
    }
}
