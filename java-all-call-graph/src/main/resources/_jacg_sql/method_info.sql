CREATE TABLE if not exists jacg_method_info_{appName} (
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  access_flags int NOT NULL COMMENT '方法的access_flags',
  method_name varchar(200) NOT NULL COMMENT '方法名',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  simple_return_type varchar(255) NOT NULL COMMENT '返回类型唯一类名',
  return_type varchar(255) NOT NULL COMMENT '返回类型类名',
  method_instructions_hash varchar(32) NOT NULL COMMENT '方法指令的HASH值（MD5），可能为空字符串',
  jar_num int NOT NULL COMMENT '方法所在的Jar包序号',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (method_hash),
  INDEX idx_mi_cm_{appName}(simple_class_name, method_name),
  INDEX idx_mi_cn_{appName}(class_name),
  INDEX idx_mi_srt_{appName}(simple_return_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法的信息表';