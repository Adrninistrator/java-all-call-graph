CREATE TABLE if not exists jacg_allowed_class_prefix_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  class_prefix varchar(500) NOT NULL COMMENT '类名或包名前缀',
  PRIMARY KEY (record_id),
  INDEX idx_acp_cp_{appName} (class_prefix(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='允许处理的类名或包名前缀';