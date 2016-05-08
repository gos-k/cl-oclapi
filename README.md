# cl-oclapi

[![Build Status](https://circleci.com/gh/gos-k/cl-oclapi.svg?style=shield)](https://circleci.com/gh/gos-k/cl-oclapi)
[![TravisCI Status](https://travis-ci.org/gos-k/cl-oclapi.svg?branch=master)](https://travis-ci.org/gos-k/cl-oclapi)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](http://opensource.org/licenses/mit-license.php)
[![Quicklisp](http://quickdocs.org/badge/cl-oclapi.svg)](http://quickdocs.org/cl-oclapi/)

Yet another [OpenCL](https://www.khronos.org/opencl/) API bindings for Common Lisp.

## Usage

## Installation

### Ubuntu

If you use Roswell then

```
git clone https://github.com/gos-k/cl-oclapi.git ~/.roswell/local-projects
```

## Test

```
(ql:quickload :cl-oclapi-test)
(prove:run :cl-oclapi-test)
```

## Specifications and platforms

* Specifications
  * OpenCL 1.2
* Platforms
  * pocl 0.12

### Support functions

| Status | APIs |
|:------:|:-----|
| Yes    | Platform APIs |
| Yes    | Device APIs |
| Yes    | Context APIs |
| Yes    | Command Queue APIs |
| Yes    | Memory Object APIs |
| No     | Sampler APIs |
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
