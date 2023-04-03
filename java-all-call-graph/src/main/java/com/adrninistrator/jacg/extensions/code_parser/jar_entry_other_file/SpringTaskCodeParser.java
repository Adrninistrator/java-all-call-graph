package com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file;

import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.extensions.code_parser.SaveData2FileParser;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 获取Spring定时任务信息
 */
public class SpringTaskCodeParser extends SaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(SpringTaskCodeParser.class);

    public static final String FILE_NAME = "spring_task";

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }

    // 指定需要处理xml文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return JavaCGConstants.FILE_EXT_ARRAY_XML;
    }

    // 处理.xml文件
    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName) {
        try {
            Element root = JACGXmlUtil.parseXmlRootElement(inputStream);
            if (!"beans".equals(root.getName())) {
                logger.debug("跳过非Spring XML 1: {}", jarEntryName);
                return;
            }

            logger.info("处理Spring XML文件 {}", jarEntryName);
            for (Element element : root.getChildren()) {
                if (!"task:scheduled-tasks".equals(element.getQualifiedName())) {
                    continue;
                }

                for (Element element2 : element.getChildren()) {
                    if (!"task:scheduled".equals(element2.getQualifiedName())) {
                        continue;
                    }

                    String beanName = element2.getAttributeValue("ref");
                    String methodName = element2.getAttributeValue("method");
                    JavaCGFileUtil.write2FileWithTab(writer, beanName, methodName);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
