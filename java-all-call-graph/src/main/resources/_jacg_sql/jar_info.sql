CREATE TABLE if not exists jacg_jar_info_{appName} (
  jar_num int NOT NULL COMMENT 'Jar包序号',
  jar_type varchar(5) NOT NULL COMMENT 'Jar包类型，J: jar包，D: 目录，R: 解析结果文件保存目录',
  jar_path_hash varchar(30) NOT NULL COMMENT 'Jar包路径HASH+字节数',
  jar_full_path text NOT NULL COMMENT 'Jar包完整路径',
  jar_file_name varchar(255) NOT NULL COMMENT 'Jar包文件名',
  jar_file_name_head varchar(255) NOT NULL COMMENT 'Jar包文件名，不包含版本号及文件后缀名',
  jar_file_name_ext varchar(255) NOT NULL COMMENT 'Jar包文件名的后缀名',
  last_modified_time varchar(20) NOT NULL COMMENT 'Jar包上次修改时间（精度到秒）',
  jar_file_hash varchar(32) NOT NULL COMMENT 'Jar包文件HASH',
  import_time datetime(3) NOT NULL COMMENT '导入时间',
  PRIMARY KEY (jar_num),
  UNIQUE INDEX idx_ji_jph_{appName}(jar_path_hash),
  UNIQUE INDEX idx_ji_jfn_{appName}(jar_file_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='jar包信息表';