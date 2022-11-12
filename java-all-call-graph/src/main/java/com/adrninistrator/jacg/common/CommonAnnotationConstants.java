package com.adrninistrator.jacg.common;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description:
 */
public class CommonAnnotationConstants {

    public static final String[] SPRING_MVC_MAPPING_ANNOTATIONS = new String[]{
            "org.springframework.web.bind.annotation.RequestMapping",
            "org.springframework.web.bind.annotation.PatchMapping",
            "org.springframework.web.bind.annotation.DeleteMapping",
            "org.springframework.web.bind.annotation.PutMapping",
            "org.springframework.web.bind.annotation.PostMapping",
            "org.springframework.web.bind.annotation.GetMapping"
    };

    public static final String[] SPRING_MVC_MAPPING_ATTRIBUTE_NAMES = new String[]{
            "value",
            "path"
    };

    public static final String[] SPRING_COMPONENT_ANNOTATIONS = new String[]{
            "org.springframework.stereotype.Component",
            "org.springframework.stereotype.Controller",
            "org.springframework.stereotype.Repository",
            "org.springframework.stereotype.Service",
            "org.springframework.web.bind.annotation.RestController"
    };

    public static final String SPRING_COMPONENT_ATTRIBUTE_NAME = "value";

    private CommonAnnotationConstants() {
        throw new IllegalStateException("illegal");
    }
}
