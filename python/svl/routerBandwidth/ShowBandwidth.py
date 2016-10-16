import urllib.request
import re
from datetime import date
from datetime import datetime
from collections import defaultdict
import pygal
from pygal.style import Style
import socket
import argparse


def fetch_data():
    password_mgr = urllib.request.HTTPPasswordMgrWithDefaultRealm()

    username = 'admin'
    password = 'vadim83!!'

    top_level_url = 'http://192.168.1.1/'
    a_url = 'http://192.168.1.1/bwm-ipt-daily.asp'

    password_mgr.add_password(None, top_level_url, username, password)

    handler = urllib.request.HTTPBasicAuthHandler(password_mgr)

    opener = urllib.request.build_opener(handler)

    pattern = re.compile('daily_history = \[\n(.+)\n\];', re.M)
    with opener.open(a_url) as response:
        html = response.read().decode('utf-8')
        hist = pattern.search(html)
        return hist.group(1)


def input_date():
    today = datetime.today()
    def_value = date(today.year, today.month, 1)

    inp = input('Enter starting date yyyy/mm/dd (%s) --> ' % def_value)
    if inp == '':
        return def_value
    elif inp.lower() == 'all':
        return date(2000, 1, 1)
    else:
        date_parts = inp.split('/')
        return date(int(date_parts[0]), int(date_parts[1]), int(date_parts[2]))


class DayBandwidth:
    def __init__(self, when, download, upload):
        self.when = when
        self.download = download
        self.upload = upload

    def when(self):
        return self.when

    def download(self):
        return self.download

    def upload(self):
        return self.upload

    def __str__(self):
        return 'DayBandwidth: %s %f %f' % (str(self.when), self.download, self.upload)


class BandwidthData:
    def __init__(self, ip, bandwidth):
        self.ip = ip
        self.bandwidth = bandwidth

    def ip(self):
        return self.ip

    def bandwidth(self):
        return self.bandwidth

    def __str__(self):
        return 'BandwidthData: %s %s' % (self.ip,  str(self.bandwidth))


class IpData:
    def __init__(self, ip, bandwidth):
        self.ip = ip
        self.bandwidth = bandwidth
        self.total_download = sum(map(lambda e: e.download, bandwidth))
        self.total_upload = sum(map(lambda e: e.upload, bandwidth))
        self.total = self.total_download + self.total_upload

    def ip(self):
        return self.ip

    def bandwidth(self):
        return self.bandwidth

    def __str__(self):
        return 'Ip Data: %s %f  details: %s' % (self.ip, self.total, ','.join(map(str, self.bandwidth)))


def parse_line(l):
    parts = l.split(',')

    encoded_date = int(parts[0], 16)

    when = date(day=encoded_date & 0xFF,
                month=((encoded_date >> 8) & 0xFF) + 1,
                year=((encoded_date >> 16) & 0xFF) + 1900)
    bandwidth = DayBandwidth(when,  int(parts[2]) / 1000000, int(parts[3]) / 1000000)

    ip = parts[1][1:-1]
    return BandwidthData(ip, bandwidth)


def parse_data(after):
    pattern = re.compile('\[([^]]+)\]')

    all_bandwidth_data = pattern.findall(fetch_data())

    raw_data = list(filter(lambda b: b.bandwidth.when >= after, list(parse_line(line) for line in all_bandwidth_data)))

    data_by_ip = defaultdict(list)

    for entry in raw_data:
        data_by_ip[entry.ip].append(entry.bandwidth)

    return list([IpData(ip, data_by_ip[ip]) for ip in data_by_ip.keys()])


def add_chart(chart, ip_data: IpData):
    bandwidth = ip_data.bandwidth
    bandwidth.sort(key=lambda entry: entry.when)

    ip1 = ip_data.ip
    hostname = hostname_lookup(ip1)

    chart.add('D %s' % hostname, list(map(lambda ipdata: ipdata.download, bandwidth)))
    chart.add('U %s' % hostname, list(map(lambda ipdata: ipdata.upload, bandwidth)))


def hostname_lookup(ip_address):
    try:
        host_info = socket.gethostbyaddr(ip_address)
        hostname = host_info[0]
    except socket.herror:
        hostname = ip_address
    return hostname


def determine_date(inp):
    if inp == '':
        today = datetime.today()
        return date(today.year, today.month, 1)
    elif inp.lower() == 'all':
        return date(2000, 1, 1)
    else:
        date_parts = inp.split('/')
        return date(int(date_parts[0]), int(date_parts[1]), int(date_parts[2]))


def extract_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--starting', required=False, default='')
    parser.add_argument('-m', '--mode', required=False, default='top')
    return parser.parse_args()


def filter_data_to_show(mode):
    if mode.lower() == 'top':
        data.sort(key=lambda ipdata: ipdata.total, reverse=True)
        top_data = data[1:6]
        return top_data
    else:
        return list(filter(lambda ipdata: ipdata.ip == mode, data))


def create_chart(top_ip_bandwidth, after):
    top_ip_bandwidth.sort(key=lambda entry: entry.when)
    custom_style = Style(
        legend_font_size=8,
        label_font_size=10,
        title_font_size=12
    )
    chart = pygal.StackedBar(legend_at_bottom=True, style=custom_style)
    chart.title = 'Bandwidth After %s' % after
    chart.x_labels = list(map(lambda ipdata: str(ipdata.when.day), top_ip_bandwidth))
    return chart


args = extract_args()
starting = determine_date(args.starting.lstrip('\''))

data = parse_data(starting)
data_to_show = filter_data_to_show(args.mode)

bandwidth_chart = create_chart(data_to_show[0].bandwidth, starting)

for ip_entry in data_to_show:
    add_chart(bandwidth_chart, ip_entry)

bandwidth_chart.render_in_browser()
