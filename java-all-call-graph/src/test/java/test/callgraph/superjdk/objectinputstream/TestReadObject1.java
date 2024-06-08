package test.callgraph.superjdk.objectinputstream;

import java.io.IOException;

/**
 * @author adrninistrator
 * @date 2024/5/20
 * @description:
 */
public class TestReadObject1 {

    public void test1() throws IOException, ClassNotFoundException {
        ObjectInputStreamChild1 objectInputStreamChild1 = new ObjectInputStreamChild1();
        objectInputStreamChild1.readObject();
        objectInputStreamChild1.skip(1L);
        objectInputStreamChild1.reset();
    }

    public void test2() throws IOException, ClassNotFoundException {
        FakeObjectInputStreamChild1 fakeObjectInputStreamChild1 = new FakeObjectInputStreamChild1();
        fakeObjectInputStreamChild1.readObject();
    }
}
