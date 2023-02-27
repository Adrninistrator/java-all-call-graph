package com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file;

import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringTask;
import com.adrninistrator.javacg.extensions.code_parser.JarEntryOtherFileParser;
import com.adrninistrator.mybatis_mysql_table_parser.xml.NoOpEntityResolver;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.input.SAXBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 获取Spring定时任务信息
 */
public class SpringTaskCodeParser implements JarEntryOtherFileParser {
    private static final Logger logger = LoggerFactory.getLogger(SpringTaskCodeParser.class);

    private static final String[] FILE_EXT_ARRAY = new String[]{"xml"};

    private static SpringTaskCodeParser LAST_INSTANCE;

    private List<WriteDbData4SpringTask> springTaskList;

    public static SpringTaskCodeParser getLastInstance() {
        return LAST_INSTANCE;
    }

    public SpringTaskCodeParser() {
        LAST_INSTANCE = this;
    }

    public List<WriteDbData4SpringTask> getSpringTaskList() {
        return springTaskList;
    }

    @Override
    public void initCodeParser() {
        springTaskList = new ArrayList<>();
    }

    // 指定需要处理xml文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return FILE_EXT_ARRAY;
    }

    // 处理.xml文件
    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName) {
        try {
            SAXBuilder saxBuilder = new SAXBuilder();
            saxBuilder.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false);
            saxBuilder.setFeature("http://xml.org/sax/features/external-general-entities", false);
            saxBuilder.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            // 不读取DTD
            saxBuilder.setEntityResolver(new NoOpEntityResolver());

            Document document = saxBuilder.build(inputStream);

            Element root = document.getRootElement();
            if (!"beans".equals(root.getName())) {
                logger.info("跳过非Spring XML 1: {}", jarEntryName);
                return;
            }

            // 以上用于跳过非Spring的XML文件
            logger.info("开始处理Spring XML: {}", jarEntryName);
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

                    WriteDbData4SpringTask writeDbData4SpringTask = new WriteDbData4SpringTask();
                    writeDbData4SpringTask.setSpringBeanName(beanName);
                    writeDbData4SpringTask.setMethodName(methodName);

                    springTaskList.add(writeDbData4SpringTask);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
