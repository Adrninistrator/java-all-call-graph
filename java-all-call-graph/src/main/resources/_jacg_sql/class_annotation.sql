CREATE TABLE if not exists class_annotation_{appName} (
  full_class_name varchar(255) NOT NULL COMMENT '完整类名',
  annotation_name varchar(255) NOT NULL COMMENT '注解类名',
  attribute_name varchar(255) NOT NULL COMMENT '注解属性名称，空字符串代表无注解属性',
  attribute_value text NULL COMMENT '注解属性值',
  PRIMARY KEY (full_class_name, annotation_name, attribute_name),
  INDEX idx_ca_an_{appName}(annotation_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类上的注解信息表';