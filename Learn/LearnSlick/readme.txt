sudo tail -f -n 30 /var/lib/mysql/vspivak-h-u.log

mysql -uvictor -pvadim vicTest
set GLOBAL general_log = 'ON';

~runMain svl.slick.HelloSlick
