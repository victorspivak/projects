import re

email = 'victor.spivak@gmail.com'
match = re.match('(.*)@(.*)', email)
if(match):
    print("Domain:", match.group(2))

filename = 'Oi.Mamochki.09.avi'
pattern =  '.*?(\d\d?[.]).*'
match = re.match(pattern, filename)
if(match):
    print("Domain:", match.group(1))

