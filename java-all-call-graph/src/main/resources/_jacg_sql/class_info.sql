CREATE TABLE if not exists jacg_class_info_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  access_flags int NOT NULL COMMENT '类的access_flags',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  class_file_hash varchar(32) NOT NULL COMMENT '类文件的HASH值（MD5）',
  jar_num int NOT NULL COMMENT '类所在的Jar包序号',
  PRIMARY KEY (record_id),
  INDEX idx_ci_scn_{appName}(simple_class_name),
  INDEX idx_ci_cn_{appName}(class_name),
  INDEX idx_ci_jn_{appName}(jar_num)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的信息表';