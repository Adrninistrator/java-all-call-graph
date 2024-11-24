package test.callgraph.spring.mvc.upload;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author adrninistrator
 * @date 2024/11/19
 * @description:
 */
@Controller
@RequestMapping("test_upload")
public class TestUploadController {

    @PostMapping(value = "/upload")
    public void upload(@RequestParam(value = "uploadInfo") String uploadInfo,
                       @RequestParam(value = "uploadFile") MultipartFile uploadFile) {
        System.getProperty(uploadFile.getName());
    }

    @PostMapping(value = "/upload2_not_used")
    public void upload2(@RequestParam(value = "uploadInfo") String uploadInfo,
                        @RequestParam(value = "uploadFile") MultipartFile uploadFile) {
    }
}
