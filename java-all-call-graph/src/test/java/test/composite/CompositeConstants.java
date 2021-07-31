package test.composite;

import com.adrninistrator.jacg.common.Constants;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class CompositeConstants {

    public static final Boolean[] BOOLEAN_ARRAY = new Boolean[]{Boolean.FALSE, Boolean.TRUE};

    public static final String[] OUTPUT_DETAIL_ARRAY = new String[]{
            Constants.CONFIG_OUTPUT_DETAIL_1,
            Constants.CONFIG_OUTPUT_DETAIL_2,
            Constants.CONFIG_OUTPUT_DETAIL_3};

    private CompositeConstants() {
        throw new IllegalStateException("illegal");
    }
}
