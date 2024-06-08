package com.adrninistrator.jacg.util;

import com.adrninistrator.mybatismysqltableparser.xml.NoOpEntityResolver;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.input.SAXBuilder;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public class JACGXmlUtil {

    /**
     * 根据InputStream解析XML，获取根元素
     *
     * @param inputStream
     * @return
     */
    public static Element parseXmlRootElement(InputStream inputStream) throws IOException, JDOMException {
        SAXBuilder saxBuilder = new SAXBuilder();
        saxBuilder.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false);
        saxBuilder.setFeature("http://xml.org/sax/features/external-general-entities", false);
        saxBuilder.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
        // 不读取DTD
        saxBuilder.setEntityResolver(new NoOpEntityResolver());

        Document document = saxBuilder.build(inputStream);
        return document.getRootElement();
    }

    private JACGXmlUtil() {
        throw new IllegalStateException("illegal");
    }
}
