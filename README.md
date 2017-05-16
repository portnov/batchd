batchd README
=============

The batchd is a toolset for batch processing. It enables one to:

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

Architecture
------------
The batchd suite consists of the following components:

* Client utilities. These allow you to create queues, put jobs into queues and
  so on. For now, there are only command-line utilities; it is planned to
  develop more or less advanced GUI client.
* Batchd manager. It is a daemon process which provides REST API for clients
  for jobs and queues management.
* Batchd dispatcher. It is a daemon process which takes jobs from queues,
  decides whether it is possible to run them now and runs them. The dispatcher
  can execute jobs on the localhost or it can connect to other machines via SSH
  and execute jobs there. Jobs can be executed in one or more threads. Several
  threads are useful when you have several machines to run your jobs on or your
  jobs consume only part of computational power of one machine.
  Batchd manager and batchd dispatcher can be run on the same machine in one OS
  threads for simple use cases.
* Database. This can be Sqlite database or other SQL database server. For now,
  PostgreSQL is supported. In the simplest case, it is possible to use Sqlite
  in-memory database, so for simple use cases you do not have to install any
  SQL database server or even use database file. Note that if you use in-memory
  database, then there will be no persistency: for example, if you reboot your
  computer, or your scheduled jobs will be forgot.

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


Job types
---------
The job type describes how to execute jobs of specific kind:

* Shell command to be run. ${}-syntax can be used for parameters substitution.
* Types of job parameters. The following types are supported for now:
  * String - just a string.
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

    $ sudo apt-get install haskell-platform
    $ cd batchd/
    $ cabal update
    $ cabal sandbox init
    $ cabal install

