package com.adrninistrator.jacg.handler.dto.businessdata;

/**
 * @author adrninistrator
 * @date 2023/9/10
 * @description: Spring Controller 对应的URI
 */
public class MethodBehaviorBySpringController extends MethodBehaviorByBusinessData {

    public static final String TYPE = "SpringController";

    // URI
    private final String uri;

    public MethodBehaviorBySpringController(String uri) {
        super(TYPE);
        this.uri = uri;
    }

    public String getUri() {
        return uri;
    }
}
