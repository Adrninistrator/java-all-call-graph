CREATE TABLE if not exists jacg_method_annotation_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  annotation_name varchar(255) NOT NULL COMMENT '注解类名',
  attribute_name varchar(200) NOT NULL COMMENT '注解属性名称，空字符串代表无注解属性',
  attribute_type varchar(5) NULL COMMENT '注解属性类型，参考AnnotationAttributesTypeEnum类',
  attribute_value text NULL COMMENT '注解属性值',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  PRIMARY KEY (record_id),
  INDEX idx_ma_1_{appName}(method_hash, annotation_name),
  INDEX idx_ma_aaa_{appName}(annotation_name, attribute_name, attribute_value(255)),
  INDEX idx_ma_scn_{appName}(simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法上的注解信息表';