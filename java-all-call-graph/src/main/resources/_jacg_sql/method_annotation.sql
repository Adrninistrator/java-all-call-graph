CREATE TABLE if not exists jacg_method_annotation_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  annotation_name varchar(500) NOT NULL COMMENT '注解类名',
  attribute_name varchar(300) NOT NULL COMMENT '注解属性名称，空字符串代表无注解属性',
  attribute_type varchar(5) NULL COMMENT '注解属性类型，s:字符串；bs:包含回车换行的字符串；m:JSON字符串，Map；ls:JSON字符串，List+String；lm:JSON字符串，List+Map',
  attribute_value text NULL COMMENT '注解属性值',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  simple_class_name varchar(500) NOT NULL COMMENT '唯一类名',
  spring_mapping_annotation tinyint NOT NULL COMMENT '是否为Spring Controller RequestMapping相关注解，0:否，1:是',
  PRIMARY KEY (record_id),
  INDEX idx_ma_1_{appName}(method_hash, annotation_name(255)),
  INDEX idx_ma_an_{appName}(annotation_name(255)),
  INDEX idx_ma_scn_{appName}(simple_class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法上的注解信息表';