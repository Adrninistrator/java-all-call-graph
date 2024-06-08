package test.callgraph.fieldrelationships.frc;

/**
 * @author adrninistrator
 * @date 2023/11/2
 * @description:
 */
public interface InterfaceFRC1<T> {

    void save(T data);

    T query(String id);
}
