import re

email = 'victor.spivak@gmail.com'
match = re.match('(.*)@(.*)', email)
if(match):
    print("Domain:", match.group(2))

