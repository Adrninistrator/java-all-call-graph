CREATE TABLE IF NOT EXISTS jacg_spring_controller_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  show_uri varchar(250) NOT NULL COMMENT '用于显示的URI',
  class_path varchar(250) NOT NULL COMMENT '类上的注解path属性原始值',
  method_path varchar(250) NOT NULL COMMENT '方法上的注解path属性原始值',
  annotation_name varchar(255) NOT NULL COMMENT '注解类名',
  simple_class_name varchar(300) NOT NULL COMMENT '唯一类名',
  jar_num int NOT NULL COMMENT '方法所在的jar文件序号',
  maybe_file_upload tinyint NOT NULL COMMENT '方法可能用于文件上传，1:是，0:否',
  maybe_file_download tinyint NOT NULL COMMENT '方法可能用于文件下载，1:是，0:否',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_spc_mhs_{appName}(method_hash, seq),
  INDEX idx_spc_su_{appName}(show_uri),
  INDEX idx_spc_scn_{appName}(simple_class_name(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring Controller信息表';