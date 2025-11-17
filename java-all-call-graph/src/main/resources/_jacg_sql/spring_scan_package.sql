CREATE TABLE IF NOT EXISTS jacg_spring_scan_package_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  type varchar(10) NOT NULL COMMENT '包扫描路径类型，j: 在Java代码中定义，x: 在XML文件中定义；dist: 按范围去重后的包扫描路径',
  seq int NOT NULL COMMENT '序号，从0开始，大于0代表有多种可能',
  scan_package varchar(255) NOT NULL COMMENT '包扫描路径',
  define_class_name_xml_path varchar(255) NOT NULL COMMENT '在Java代码中定义时所在的类名，或在XML中定义时对应的文件路径',
  PRIMARY KEY (record_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring的包扫描路径';