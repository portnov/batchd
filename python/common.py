
from datetime import datetime

def format_time(value):
    if value:
        d = datetime.strptime(value, "%Y-%m-%dT%H:%M:%S.%fZ")
        return unicode(datetime.strftime(d, "%c"), "utf-8")
    else:
        return "<undefined>"
