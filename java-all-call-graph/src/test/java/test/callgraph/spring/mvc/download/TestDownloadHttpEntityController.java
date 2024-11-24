package test.callgraph.spring.mvc.download;

import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import test.callgraph.field.dto.TestFieldDto1;

/**
 * @author adrninistrator
 * @date 2024/11/8
 * @description:
 */
@Controller
@RequestMapping("download_http_entity")
public class TestDownloadHttpEntityController {

    @GetMapping("/dl1")
    public HttpEntity<Resource> downloadFile1(TestFieldDto1 testFieldDto1) {
        System.getProperty("");
        return new HttpEntity<>(null, null);
    }

    @GetMapping("/dl2")
    public HttpEntity<InputStreamResource> downloadFile2(String fileInfo) {
        System.getProperty("");
        return new HttpEntity<>(null, null);
    }

    @GetMapping("/dl3")
    public HttpEntity<byte[]> downloadFile3() {
        System.getProperty("");
        return new HttpEntity<>(null, null);
    }

    @GetMapping("/dl4")
    public HttpEntity<ByteArrayResource> downloadFile4() {
        System.getProperty("");
        return new HttpEntity<>(null, null);
    }
}
