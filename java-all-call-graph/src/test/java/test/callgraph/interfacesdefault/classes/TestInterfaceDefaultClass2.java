package test.callgraph.interfacesdefault.classes;

import org.apache.commons.lang3.StringUtils;
import test.callgraph.interfacesdefault.interfaces.TestInterfaceDefault1;

/**
 * @author adrninistrator
 * @date 2024/12/7
 * @description:
 */
public class TestInterfaceDefaultClass2 implements TestInterfaceDefault1 {
    @Override
    public void custom() {
        StringUtils.join();
    }
}
