# procps

## About

procps is a package to manage processes. Currently in its early stages
of development, the purpose is to enable people to use this library like
[psutil](https://psutil.readthedocs.io/en/latest) in Python.

It should be convenient, easy to use.

## API

Being currently in its early stages of development, the API is subject
to changes. It supports Linux only currently. I may add a support for
BSD OSes, but I will likely support Windows in the future but with
tests only conducted in a virtual machine.


## Command line interface

Several actions are available via the command line:
- Process Management visualization tool.
To visually manage processes, you can use start the web interface:

``` haskell
procps start-server
```
It will start a web server to show currently monitored
processes. Processes are monitored through the API or the CLI. Just do
ctrl-c to stop it.

Through the CLI, you can also show a process you created:
``` haskell
procps show "process id"
```

Or list all processes currently being monitored:
``` haskell
procps list-all
```

- Monitored processes creation
``` haskell
procps create "process id" "process command"
```
To create a monitored process through the API or the CLI.

- Search utilities to find currently working processes.
``` haskell
procps find "process name"
```

## Usage examples

How you can get a process and some basic information about it:
``` haskell
Prelude> import Process.Manage as PM
Prelude P> PM.findProcess PM.PName "thunderbird"
[Process {pname = "thunderbird", pid = "3654", command = "/usr/lib/thunderbird/thunderbird\NUL"}]
```

See information about the current directory of a process and the environment variables used for it:
``` haskell
Prelude> import Process.Information as PI
Prelude PI> PI.seeCwd "1873"
"/home/alex"
```

## TODO

- [ ] Improve documentation
- [ ] Add tests
- [ ] Ease the use of function by accepting not only string but the
      Process data type
- [x] Refactor System functions related to processes and function to
      manage processes in different modules
- [x] Add skeleton for BSD support and abstract directory names access.
- [x] Improve abstraction of platform dependent functions
