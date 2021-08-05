# 执行当前脚本前，需要将对应应用的jar/war包解压，以下find_dir参数为保存Mybatis的XML文件的目录
array_string="mapper1 mapper2"

find_dir=/test/dir1/

array_mapper=($array_string)
for i in ${!array_mapper[@]}
do 
mapper=${array_mapper[i]}
xml_file=`find $find_dir -name \*.xml | xargs grep 'mapper [ ]*namespace' | grep "\.$mapper\"" | head -1 | awk -F ':' '{print $1}'`
xml_tmp_file="$mapper"_tmp.txt
xml_tmp_tables_file="$mapper"_#tmp_tables.txt
xml_tables_file="$mapper"_#tables.txt

cat $xml_file | tr '\r' ' ' | tr '\n' ' ' | tr '\t' ' ' | sed 's#[ ][ ]*# #g' | sed 's#<select#\n<select#g' | sed 's#<update#\n<update#g' | sed 's#<delete#\n<delete#g' | sed 's#<insert#\n<insert#g' | sed 's# select # select #i' | sed 's#</select# where #g' | sed 's# from # from #i' | sed 's# where # where #i' | sed 's#<where# where #i' | sed 's# order by # where #i' | sed 's# group by # where #i' | sed 's# limit # where #i' | sed 's# update # update #i' | sed 's#on duplicate key update#on_duplicate_key_update#i' | sed 's# set # set #i' | sed 's#<set[ ]*># set #g' | sed 's# delete # delete #i' | sed 's# insert into # insert into #i' | sed 's#<trim prefix[ ]*=[ ]*"##i' | sed 's#where"#where #i' | sed 's#("#(#i' | sed 's#from[ ]*(#_f_r_o_m_(_#i' | sed 's#[ ][ ]*# #g' > $xml_tmp_file

cat $xml_tmp_file | grep ' insert into ' | awk -F ' insert into ' '{print $2}' | awk -F '(' '{print $1}' | sed 's# ##g' > $xml_tmp_tables_file

cat $xml_tmp_file | grep ' select ' | awk -F ' from ' '{print $2}' | awk -F ' where ' '{print $1}' | sed 's#,#\n#g' >> $xml_tmp_tables_file

cat $xml_tmp_file | grep ' update ' | awk -F ' update ' '{print $2}' | awk -F ' set ' '{print $1}' | sed 's#,#\n#g' >> $xml_tmp_tables_file

cat $xml_tmp_file | grep ' delete ' | awk -F ' from ' '{print $2}' | awk -F ' where ' '{print $1}' | sed 's#,#\n#g' >> $xml_tmp_tables_file

cat $xml_tmp_tables_file | grep -v '^${' | awk '{print $1}' | sort | uniq | sort > $xml_tables_file

done

cat *#tables.txt | sort | uniq | sort > all_talbes.txt
