batchd README
=============

The batchd is a toolset for batch processing for Linux/Unix. It enables one to:

* Create and manage queues of tasks (batch jobs);
* Specify time periods (schedules) when jobs from each queue can be executed;
* Run batch jobs on localhost or on several machines one-by-one or in parallel.

The main concern of batchd are batch jobs, which are meant to take some time to
execute (minutes to days) and consume a lot of computational power (probably
whole power of the machine). Examples of such jobs are:

* Scientific calculations (physical modelling or numeric experiments on
  differential equations, for example);
* Building large software products from source code;
* Running integration test suites;
* Rendering complex 3D scenes or animations;
* Executing complex reports on large databases;
* Backups;
* and so on.

For such tasks it becomes inconvinient to run them just when you want. For
example, if you are a 3D artist, and you just completed your scene, you want to
render it, but the rendering process will take the whole computational power of
your machine, while you are going to work on the next scene. It would be good
to run the rendering as a night job. Then you create the next scene and want it
to be rendered next after the first. So you have a queue of batch jobs.

batchd supports simple host management for several kinds of virtual hosts:

* LibVirt-supported hypervisors (KVM, QEMU, Xen, Virtuozzo, VMWare ESX, LXC,
  BHyve and more);
* Docker containers;
* AWS EC2 instances;
* Linode.com instances.

batchd can automatically start such hosts when there are jobs to be executed on
them, and automatically stop hosts when there is no need for them. 

Architecture
------------
The batchd suite consists of the following components:

* Client utilities. These allow you to create queues, put jobs into queues and
  so on. The following clients are available for now:
  * Command-line utility, `batch`. This is for now the most complete client.
  * Python3+Qt5 GUI client. This allows to view and edit queues, view, create
    and edit jobs.
  * Blender python client. This is simple addon for Blender, which allows to
    put rendering jobs to batchd queue from Blender's UI.
  * Web client. This allows to view queues, create and view jobs. It is mainly
    intended for job creation and monitoring.
* Batchd manager. It is a daemon process which provides REST API for clients
  for jobs and queues management.
* Batchd dispatcher. It is a daemon process which takes jobs from queues,
  decides whether it is possible to run them now and runs them. The dispatcher
  can execute jobs on the localhost or it can connect to other machines via SSH
  and execute jobs there. Jobs can be executed in one or more threads
  (workers). Several threads are useful when you have several machines to run
  your jobs on or your jobs consume only part of computational power of one
  machine.  Batchd manager and batchd dispatcher can be run on the same machine
  in one OS thread for simple use cases.
* Database. This can be Sqlite database or other SQL database server. For now,
  PostgreSQL is supported. In the simplest case, it is possible to use Sqlite
  in-memory database, so for simple use cases you do not have to install any
  SQL database server or even use database file. Note that if you use in-memory
  database, then there will be no persistency: for example, if you reboot your
  computer, or your scheduled jobs will be forgot.

There is also a command-line utility called batchd-admin, for administrative needs.
For now it allows to create superuser (root), since it is not possible to create
first user via REST API.

See also REST API description in the REST.API file.

Access control
--------------
batchd uses relatively simple access control model, based on users and permissions.
Each user can have one of predefined permissions. Permissions can be assigned
with relation to:

  * Specific queue or any queue
  * Specific job type or any type (only for creating jobs)
  * Specific host or any host (only for creating jobs)

The following permissions are supported:

  * SuperUser - user that have this permission is superuser, it can do anything.
  * CreateJobs - a permission to create jobs. Can be granted with relation to
    queue and job type.
  * ViewJobs - a permission to view job details and results.
  * ManageJobs - a permission to edit and delete jobs.
  * ViewQueues - a permission to view queue details.
  * ManageQueues - a permission to create, edit and delete queues.
  * ViewSchedules - a permission to view schedules. This cannot be granted with
    relation to queue or job type.
  * ManageSchedules - a permission to create, edit or delete schedules. This
    cannot be granted with relation to queue or job type.

Only superuser can manage users and their permissions.

The following options are available for user authentication for access to REST API:

* Basic HTTP authentication. In this case password are sent in clear text, so
  this is secure only if channel is secured with HTTPS. This can be implemented
  with external tool, for example nginx.
* Unconditional authentication of user specified in X-Auth-User: HTTP header.
  This is intended for cases when authentication is done by external system.
  For example, this is usable when nginx checks client HTTPS certificates.
* Disable authentication. In this mode all users are treated as superusers.
  This can be usable when running on localhost.

Configuration
-------------
All configuration files are in YAML format and stored under /etc/batchd/ or
under ~/.config/batchd/. The config files are:

* batchd/batchd.yaml. This file contains global options: database connection,
  worker threads count and so on.
* batchd/client.yaml. This file contains default settings for `batch'
  command-line client.
* batchd/hosts/$hostname.yaml. Such files describe remote hosts (which are
  available through the SSH protocol with public key auth).
* batchd/jobtypes/$type.yaml. Such files describe job types - types of jobs
  which can be run. See also the description below.

For examples, see sample configs under sample-configs/ directory.

The `batch' command-line client also supports the following environment
variables:

* BATCH_MANAGER_URL
* BATCH_QUEUE
* BATCH_TYPE
* BATCH_HOST
* BATCH_USERNAME
* BATCH_PASSWORD

The priority for these options is as follows:
* Options specified explicitly in command line have the highest priority.
* Next environment variables are checked.
* Then client.yaml config file is checked.
* If there is no client.yaml or the option is not specified in it, then default
  value is used:
  * Default manager url is http://localhost:9681.
  * Default queue is "default".
  * Default job type is "command".
  * Default host name is "undefined", which means use local host.
  * CLI and Python clients use current OS user name as batchd user name by default.

Job types
---------
The job type describes how to execute jobs of specific kind:

* Shell command to be run. ${}-syntax can be used for parameters substitution.
* Types of job parameters. The following types are supported for now:
  * String - just a string.
  * Int - an integer number.
  * InputFile - the parameter represents a path to file which should be used as
    input. If the job is run on the remote host, then before command execution
    all files specified in InputFile-parameters are copied from the host where
    batchd dispatcher is running to the remote host (via SCP protocol).
  * OutputFile - the parameter represents a path to file which should be used
    as output. If the job is run on the remote host, then after command
    execution all files specified in OutputFile-parameters are copied from the
    remote host to the host where batchd dispatcher is running (via SCP protocol).
* on_fail option specifies what should dispatcher do if command execution
  failed. The following options are available:
  * continue - the job will be marked as failed and dispatcher will just
    proceed with other jobs.
  * retry:
    * {when: now, count: n}: the job will be marked as new and will be left in
      the beginning of the queue; so dispatcher will retry execution of this
      job in short time. Maximum number of retries is specified in `count'
      parameter; default maximum is 1.
    * {when: later, count: n}: simillar to previous, but the job will be moved
      to the end of the queue; so dispatcher will retry execution of this job
      after all previous jobs are finished.

Schedules
---------
The schedule describes time periods when jobs can be executed. There are two
options of how to specify periods:

* Specify time of day period; for example, time: [{"begin": "08:00:00", "end":
  "19:00:00"}]. It is possible to specify several periods. If no periods are
  specified, it is treated as "any time of day".
* Specify week days; for example, weekdays: ["Saturday", "Sunday"]. If no week
  days are specified, it is treated as "any week day".

Installation
------------

    $ sudo apt-get install stack
    $ cd batchd/
    $ stack install --flag batchd:docker --flag batchd:libvirt --flag batchd:ec2
    $ vi .config/batchd/batchd.yaml # Please refer to sample-configs/ directory
    $ batchd-admin upgrade-db
    $ batchd-admin create-superuser

