package test.callgraph.spring.mvc.download;

import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;
import test.callgraph.field.TestField1;
import test.callgraph.field.TestField2;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/11/8
 * @description:
 */
@Controller
@RequestMapping("download_response_entity")
public class TestDownloadResponseEntityController {

    @GetMapping("/dl1")
    public ResponseEntity<Resource> downloadFile1(TestField1 testField1) {
        System.getProperty("");
        return new ResponseEntity<>(null, null, HttpStatus.OK);
    }

    @GetMapping("/dl2")
    public ResponseEntity<InputStreamResource> downloadFile2(List<TestField2> testField2List) {
        System.getProperty("");
        return new ResponseEntity<>(null, null, HttpStatus.OK);
    }

    @GetMapping("/dl3")
    public ResponseEntity<byte[]> downloadFile3() {
        System.getProperty("");
        return new ResponseEntity<>(null, null, HttpStatus.OK);
    }

    @GetMapping("/dl4")
    public ResponseEntity<ByteArrayResource> downloadFile4() {
        System.getProperty("");
        return new ResponseEntity<>(null, null, HttpStatus.OK);
    }

    @GetMapping("/dl5")
    public ResponseEntity<StreamingResponseBody> downloadFile5() {
        System.getProperty("");
        return new ResponseEntity<>(null, null, HttpStatus.OK);
    }
}
