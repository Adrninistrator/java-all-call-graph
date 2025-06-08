package com.adrninistrator.jacg.dto.entrymethodinfo;

/**
 * @author adrninistrator
 * @date 2024/3/24
 * @description: 入口方法信息，Spring Controller
 */
public class EntryMethodInfo4SpringController extends BaseEntryMethodInfo {

    public static final String TYPE = "spc";

    private String controllerUri;

    public EntryMethodInfo4SpringController() {
        this.type = TYPE;
    }

    public String getControllerUri() {
        return controllerUri;
    }

    public void setControllerUri(String controllerUri) {
        this.controllerUri = controllerUri;
    }
}
