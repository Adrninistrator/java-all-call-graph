CREATE TABLE if not exists method_annotation_{appName} (
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  annotation_name varchar(255) NOT NULL COMMENT '注解名',
  annotation_attributes text NOT NULL COMMENT '注解属性，JSON字符串格式',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  PRIMARY KEY (method_hash, annotation_name),
  INDEX idx_ma_annotation_name(annotation_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法上的注解信息表';