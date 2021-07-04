# 以下'\.dao\.'为Mapper所在的包名关键字
cat \~all-4caller.txt | grep '\.dao\.' | awk -F '#' '{print $2}' | sed 's# ##g' | awk -F ':' '{print $1}' | sort | uniq | sort