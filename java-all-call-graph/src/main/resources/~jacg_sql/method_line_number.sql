CREATE TABLE if not exists method_line_number_{appName} (
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  min_line_number int NOT NULL COMMENT '起始代码行号',
  max_line_number int NOT NULL COMMENT '结束代码行号',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  PRIMARY KEY (method_hash),
  INDEX idx_cn_class_line(simple_class_name, min_line_number, max_line_number)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法代码行号信息表';