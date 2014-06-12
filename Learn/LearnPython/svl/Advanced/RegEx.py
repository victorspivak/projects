import re

email = 'victor.spivak@gmail.com'
match = re.match('(.*)@(.*)', email)
if(match):
    print("Domain:", match.group(2))

filename = 'Oi.Mamochki.09_10.avi'
pattern =  '.*?(\d\d?)_(\d\d?).*'
match = re.match(pattern, filename)
if(match):
    groups = [int(group) for group in match.groups()]
    print("Filename: {0} {1}".format(*groups))

