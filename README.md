# cl-oclapi

[![Build Status](https://circleci.com/gh/gos-k/cl-oclapi.svg?style=shield)](https://circleci.com/gh/gos-k/cl-oclapi)
[![TravisCI Status](https://travis-ci.org/gos-k/cl-oclapi.svg?branch=master)](https://travis-ci.org/gos-k/cl-oclapi)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](http://opensource.org/licenses/mit-license.php)
[![Quicklisp](http://quickdocs.org/badge/cl-oclapi.svg)](http://quickdocs.org/cl-oclapi/)

cl-oclapi is a Common Lisp binding for [OpenCL](https://www.khronos.org/opencl/) API.

## Usage

## Installation

cl-oclapi is now available on [Quicklisp](https://www.quicklisp.org).

```
(ql:quickload :cl-oclapi)
```

## Test

### REPL

```
(ql:quickload :cl-oclapi-test)
(prove:run :cl-oclapi-test)
```

### Command line

Install prove.

```
ros install prove
```

Run test.

```
run-prove cl-oclapi-test.asd
```

## Specifications and platforms

* Specifications
  * OpenCL 1.2
* Platforms
  * pocl 0.12
  * OpenCL 1.2 CUDA 8.0.20
  * Intel(R) OpenCL

### Support functions

| Status | APIs |
|:------:|:-----|
| Yes    | Platform APIs |
| Yes    | Device APIs |
| Yes    | Context APIs |
| Yes    | Command Queue APIs |
| Yes    | Memory Object APIs |
| Yes    | Sampler APIs |
| Yes    | Program Object APIs |
| Yes    | Kernel Object APIs |
| Yes    | Event Object APIs |
| Yes    | Profiling APIs |
| Yes    | Flush and Finish APIs |
| Yes    | Enqueued Commands APIs |

## Author

* gos-k (mag4.elan@gmail.com)

## Copyright

Copyright (c) 2015 gos-k (mag4.elan@gmail.com)

## License

Licensed under the MIT License.
