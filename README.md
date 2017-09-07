# MockShell

Small, fast command line utility for mocking command line calls.  Acts like middleware between shell
and your script.

## Usage

It's your script's responsibility to wrap commands with MockShell.

```
MockShell -p -m mocks.json -c 'command'

-p            -- Should enable profiling for this call
-m mocks.json -- Well formatted JSON array of mocks to match (see example.json)
-c 'command'  -- The command you may or may not be mocking
```

## Installation

* Make sure you have slack installed
* Checkout repository
* `slack install` to install it globally.

## Mock JSON File

Included in the repository is a sample mock.json file which simply maps a specific echo statement to
a new output.

## TODO

* Regex based matching
* Testing
