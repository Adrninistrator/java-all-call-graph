package test.callgraph.methodcallarg.common.enums;

/**
 * @author adrninistrator
 * @date 2025/2/17
 * @description:
 */
public enum MCAEnum1 {
    ENUM_1("key-1", 1, "desc_key-1"),
    ENUM_2("key-2", 2, "desc_key-2"),
    ENUM_3("key-3", 3, "desc_key-3"),
    ;

    private final String key;
    private final int Seq;
    private final String desc;

    MCAEnum1(String key, int seq, String desc) {
        this.key = key;
        Seq = seq;
        this.desc = desc;
    }

    public String getKey() {
        return key;
    }

    public int getSeq() {
        return Seq;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return desc;
    }
}
