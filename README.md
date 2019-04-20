# procps

## About

procps is a package to manage processes. Currently in its early stages
of development, the purpose is to enable people to use this library like
[psutil](https://psutil.readthedocs.io/en/latest) in Python.

It should be convenient, easy to use.

## API

Being currently in its early stages of development, the API is subject
to changes. It supports Linux only currently. I may add a support for
BSD OSes, but I will unlikely support Windows, unless someone want to
provide patches for it.


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
