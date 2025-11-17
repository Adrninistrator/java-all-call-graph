CREATE TABLE IF NOT EXISTS jacg_method_line_number_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  simple_class_name varchar(300) NOT NULL COMMENT '唯一类名',
  method_name varchar(300) NOT NULL COMMENT '方法名',
  min_line_number int NOT NULL COMMENT '起始代码行号',
  max_line_number int NOT NULL COMMENT '结束代码行号',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_mln_mh_{appName}(method_hash),
  INDEX idx_mln_scl_{appName}(simple_class_name(255), min_line_number, max_line_number),
  INDEX idx_mln_scm_{appName}(simple_class_name(255), method_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法代码行号信息表';