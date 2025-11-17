CREATE TABLE IF NOT EXISTS jacg_package_info_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  package_name varchar(255) NOT NULL COMMENT '包名',
  package_level int NOT NULL COMMENT '包名层级，等于包名中的.数量+1',
  jar_num int NOT NULL COMMENT '类所在的jar文件序号',
  jar_file_name varchar(255) NOT NULL COMMENT 'jar文件名',
  PRIMARY KEY (record_id),
  UNIQUE INDEX uni_pi_pnjn_{appName}(package_name, jar_num),
  INDEX idx_pi_plpn_{appName}(package_level, package_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='包名信息表';