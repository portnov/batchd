
import os
from os.path import isfile, join, dirname
import requests
import json

try:
    import yaml
    YAML_AVAILABLE=True
except ImportError:
    YAML_AVAILABLE=False

class InsufficientRightsException(Exception):
    pass

class Client(object):
    def __init__(self, manager_url = None, username=None, password=None):
        self._manager_url = manager_url
        self.username = username
        self.password = password
        self.key = None
        self.certificate = None
        self.ca_certificate = None

    @classmethod
    def from_config(cls, config=None):
        if config is None:
            config = cls.load_config()

        settings = Client()
        settings.certificate = config.get('certificate', None)
        settings.key = config.get('key', None)
        settings.ca_certificate = config.get('ca_certificate', None)
        settings.config = config
        return settings

    @classmethod
    def load_config(cls):
        if not YAML_AVAILABLE:
            raise RuntimeError("YAML python module is not available, can't load batchd client config from file")
        home = os.environ['HOME']
        homecfg = join(home, ".config", "batchd", "client.yaml")
        cfgfile = None
        if isfile(homecfg):
            cfgfile = open(homecfg, 'r')
        else:
            etc = join("/etc", "batchd", "client.yaml")
            if isfile(etc):
                cfgfile = open(etc, 'r')
        if cfgfile:
            return yaml.load(cfgfile)

    @classmethod
    def obtain_manager_url(cls):
        env = os.environ.get('BATCH_MANAGER_URL', None)
        if env:
            return env
        cfg = cls.load_config()
        url = cfg['manager_url']
        if url:
            return url
        return 'http://localhost:9681'
    
    @property
    def manager_url(self):
        if self._manager_url is None:
            self._manager_url = self.obtain_manager_url()
        return self._manager_url

    @property
    def need_password(self):
        if self.key and self.certificate:
            return False
        else:
            return True

    @property
    def credentials(self):
        return (self.username, self.password)

    @property
    def client_certificate(self):
        if self.key and self.certificate:
            return (self.certificate, self.key)
        else:
            return None

    @property
    def verify(self):
        if self.ca_certificate:
            return self.ca_certificate
        else:
            return False

    def _handle_status(self, rs):
        if rs.status_code in (401, 403):
            raise InsufficientRightsException(rs.text)
        if rs.status_code != 200:
            raise Exception(rs.text)

    def get_job_types(self):
        rs = requests.get(self.manager_url + "/type", auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        return json.loads(rs.text)

    def get_queues(self):
        rs = requests.get(self.manager_url + "/queue", auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        return json.loads(rs.text)

    def do_enqueue(self, qname, typename, hostname, params):
        rq = dict(queue = qname, type=typename, params=params, host=hostname)
        rs = requests.post(self.manager_url+ "/queue/" + qname, data=json.dumps(rq), auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        print(rs.text)

    def get_queue_stats(self, qname):
        rs = requests.get(self.manager_url + "/stats/" + qname, auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        return json.loads(rs.text)

    def get_jobs(self, qname):
        rs = requests.get(self.manager_url + "/queue/" + qname + "/jobs?status=all", auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        return json.loads(rs.text)

    def get_job_results(self, jobid):
        rs = requests.get(self.manager_url + "/job/" + str(jobid) + "/results", auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        return json.loads(rs.text)

    def delete_job(self, jobid):
        rs = requests.delete(self.manager_url + "/job/" + str(jobid), auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        print(rs)

    def get_schedules(self):
        rs = requests.get(self.manager_url + "/schedule", auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        return json.loads(rs.text)

    def new_queue(self, queue):
        rs = requests.post(self.manager_url + "/queue", data=json.dumps(queue), auth=self.credentials, verify=self.verify, cert=self.client_certificate)
        self._handle_status(rs)
        print(rs.text)


