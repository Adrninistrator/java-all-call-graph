package test.callgraph.fieldrelationships.fre;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2023/10/4
 * @description:
 */
public class FREDtoC {
    @JsonProperty("_freDtoB")
    private FREDtoB freDtoB;

    public FREDtoB getFreDtoB() {
        return freDtoB;
    }

    public void setFreDtoB(FREDtoB freDtoB) {
        this.freDtoB = freDtoB;
    }
}
