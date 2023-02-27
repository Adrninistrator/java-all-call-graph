CREATE TABLE if not exists jacg_class_info_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  simple_class_name varchar(500) NOT NULL COMMENT '唯一类名',
  access_flags int NOT NULL COMMENT '类的access_flags',
  class_name varchar(500) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_ci_scn_{appName} (simple_class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的信息表';