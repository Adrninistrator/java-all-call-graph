CREATE TABLE if not exists jacg_field_annotation_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  field_name varchar(200) NOT NULL COMMENT '字段名称',
  annotation_name varchar(255) NOT NULL COMMENT '注解类名',
  attribute_name varchar(200) NOT NULL COMMENT '注解属性名称，空字符串代表无注解属性',
  attribute_type varchar(5) DEFAULT NULL COMMENT '注解属性类型，参考AnnotationAttributesTypeEnum类',
  attribute_value text DEFAULT NULL COMMENT '注解属性值',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_fa_1_{appName}(simple_class_name, field_name, annotation_name),
  INDEX idx_fa_aaa_{appName}(annotation_name, attribute_name, attribute_value(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='字段上的注解信息表';