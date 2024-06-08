package test.callgraph.superjdk.objectinputstream;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;

/**
 * @author adrninistrator
 * @date 2024/5/20
 * @description:
 */
public class ObjectInputStreamChild1 extends ObjectInputStream {
    public ObjectInputStreamChild1(InputStream in) throws IOException {
        super(in);
    }

    protected ObjectInputStreamChild1() throws IOException, SecurityException {
    }
}
