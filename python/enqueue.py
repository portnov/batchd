#!/usr/bin/python

import sys
import requests
import json

URL='http://localhost:9681/queue'

def do_enqueue(qname, typename, params):
    rq = dict(queue = qname, type=typename, params=params)
    url = URL + "/" + qname
    print(url)
    print(rq)
    rs = requests.put(url, data=json.dumps(rq))
    print(rs.text)

def parse_params(strs):
    result = {}
    for string in strs:
        name, value = string.split("=")
        result[name] = value
    return result

def main():
    print(sys.argv)
    qname = sys.argv[1]
    typename = sys.argv[2]
    params = sys.argv[3:]
    params = parse_params(params)

    do_enqueue(qname, typename, params)

if __name__ == "__main__":
    main()

