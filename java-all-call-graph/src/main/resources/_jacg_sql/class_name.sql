CREATE TABLE if not exists jacg_class_name_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  class_name varchar(500) NOT NULL COMMENT '完整类名',
  simple_class_name varchar(500) NOT NULL COMMENT '唯一类名',
  duplicate_class tinyint NOT NULL COMMENT '是否存在同名类，1:是，0:否',
  PRIMARY KEY (record_id),
  INDEX idx_cn_cn_{appName}(class_name(255)),
  INDEX idx_cn_scn_{appName}(simple_class_name(255)),
  INDEX idx_cn_dc_{appName}(duplicate_class)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类名信息表';