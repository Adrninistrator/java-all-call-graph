package test.callgraph.spring.mvc.download;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.servlet.http.HttpServletResponse;


/**
 * @author adrninistrator
 * @date 2024/11/8
 * @description:
 */
@Controller
@RequestMapping("download_http_servlet_response")
public class TestDownloadHttpServletResponseController {

    @GetMapping("/dl1")
    public void downloadFile1(HttpServletResponse response) {
        System.getProperty(String.valueOf(response.getStatus()));
    }

    @GetMapping("/dl2_not_used")
    public void downloadFil2NotUsed1(HttpServletResponse response) {
    }
}
