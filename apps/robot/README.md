robot
=====

An OTP application

Run
-----
```shell
$ ./gamectl robot
```
then
```erlang
(robot@127.0.0.1)1> application:start(robot).
(robot@127.0.0.1)2> robot_ctl:command({do_robot, 1}).
```
