package com.adrninistrator.jacg.dto.entrymethodinfo;

/**
 * @author adrninistrator
 * @date 2024/3/24
 * @description: 入口方法信息，Spring Controller
 */
public class EntryMethodInfo4SpringController extends BaseEntryMethodInfo {

    public static final String TYPE = "spc";

    private String controllerUti;

    public EntryMethodInfo4SpringController() {
        this.type = TYPE;
    }

    public String getControllerUti() {
        return controllerUti;
    }

    public void setControllerUti(String controllerUti) {
        this.controllerUti = controllerUti;
    }
}
