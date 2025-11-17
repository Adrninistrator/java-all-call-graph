CREATE TABLE IF NOT EXISTS jacg_jar_info_{appName} (
  jar_num int NOT NULL COMMENT 'jar文件序号',
  jar_type varchar(5) NOT NULL COMMENT 'jar文件类型，J: jar/war文件，D: 目录，JIJ: jar/war文件中的jar，R: 解析结果文件保存目录',
  jar_path_hash varchar(32) NOT NULL COMMENT '外层jar文件路径HASH+字节数',
  jar_full_path text NOT NULL COMMENT '外层jar文件完整路径',
  jar_file_name varchar(255) NOT NULL COMMENT '外层jar文件名',
  jar_file_name_head varchar(255) NOT NULL COMMENT '外层jar文件名，不包含版本号及文件后缀名',
  jar_file_name_ext varchar(255) NOT NULL COMMENT '外层jar文件名的后缀名，以.开头',
  last_modified_time varchar(20) NOT NULL COMMENT '外层jar文件上次修改时间（精度到秒）',
  jar_file_hash varchar(32) NOT NULL COMMENT '外层jar文件HASH',
  inner_jar_path text NOT NULL COMMENT 'jar/war文件中的jar文件路径',
  inner_jar_file_name varchar(255) NOT NULL COMMENT 'jar/war文件中的jar文件名',
  import_time datetime(3) NOT NULL COMMENT '导入时间',
  PRIMARY KEY (jar_num),
  INDEX idx_ji_jph_{appName}(jar_path_hash),
  INDEX idx_ji_jfn_{appName}(jar_file_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='jar文件信息表';