CREATE TABLE if not exists jacg_class_reference_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  class_name varchar(255) NOT NULL COMMENT '引用的完整类名',
  simple_class_name varchar(255) NOT NULL COMMENT '引用的唯一类名',
  referenced_class_name varchar(255) NOT NULL COMMENT '被引用的完整类名',
  referenced_simple_class_name varchar(255) NOT NULL COMMENT '被引用的唯一类名',
  jar_num int NOT NULL COMMENT '类所在的jar文件序号',
  PRIMARY KEY (record_id),
  INDEX idx_cr_scn_{appName}(simple_class_name),
  INDEX idx_cr_rscn_{appName}(referenced_simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的引用关系表';