# Changelog

## 0.0.x

This version is a experimental section to try various features and interfaces of Flip. 


### [0.0.3](https://github.com/xxxnell/flip/tree/v0.0.3)

> 2018-03-26

We refine its usabilities and inner workings. Especially, the functions of `Dist` including `Sketch` doesn't consider the empty structure anuymore. Thus, many functions do not return options, making them easier to use. 

* Many functions of `Dist` doesn't return `Option` [#1](https://github.com/xxxnell/flip/issues/1) [#3](https://github.com/xxxnell/flip/issues/3)
* Implement `sample` for Sketch [#25](https://github.com/xxxnell/flip/issues/25)
* Support for-comprehension [#26](https://github.com/xxxnell/flip/issues/26)
* Fix minor bugs [#16](https://github.com/xxxnell/flip/issues/16) [#17](https://github.com/xxxnell/flip/issues/17) [#20](https://github.com/xxxnell/flip/issues/20)


### [0.0.2](https://github.com/xxxnell/flip/tree/v0.0.2)

> 2018-02-18

In this version, we have implemented the basic functionality of the `Sketch` algorithm. `Sketch` summarize real-valued random variable stream in order to estimate its probability density in a quick way, and to store and retrieve it only using the sublinear space. 

* Implement of various distribution categories including `Sketch`
* Implement several similarity measurement methods
* Apply code style formatter [#12](https://github.com/xxxnell/flip/issues/12)
* Automate version bumping and releaseing to [bintray](https://bintray.com/xxxnell/oss-maven/flip/0.0.2) [#11](https://github.com/xxxnell/flip/issues/11)