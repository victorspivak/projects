import urllib.request
import re

password_mgr = urllib.request.HTTPPasswordMgrWithDefaultRealm()

username = 'admin'
password = 'vadim83!!'

top_level_url = 'http://192.168.1.1/'
a_url = 'http://192.168.1.1/bwm-ipt-daily.asp'

password_mgr.add_password(None, top_level_url, username, password)

handler = urllib.request.HTTPBasicAuthHandler(password_mgr)

opener = urllib.request.build_opener(handler)

# # Install the opener.
# # Now all calls to urllib.request.urlopen use our opener.
# urllib.request.install_opener(opener)

pattern = re.compile('daily_history = \[\n(.+)\n\];', re.M)
# pattern = re.compile('<co.*sty', re.S | re.M)
with opener.open(a_url) as response:
    html = response.read().decode('utf-8')
    # print(html)
    # hist = pattern.search(html, re.MULTILINE, re.DOTALL)
    hist = pattern.search(html)
    # hist = re.search('<co.*sty', html, re.S | re.M)
    print(hist.group(1))






