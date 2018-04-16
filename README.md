otpbp: Parse transformer for use new OTP functions in old Erlang/OTP releases (R15, R16, 17, 18, 19, 20)
==================================

[![Build Status](https://secure.travis-ci.org/Ledest/otpbp.png)](http://travis-ci.org/Ledest/otpbp)

**For what you need this parse transformer?**

Example: cowboy released new version for erlang 22, but your project supports only erlang 16.
You can convert new cowboy release to erlang 16. Cool, isn't in?

### Download repo
```
git clone git://github.com/Ledest/otpbp.git
```
### Install rebar2
**Ubuntu**

```sh
$ apt install rebar
```

**Gentoo**

```sh
$ emerge --ask rebar2 
```

**Arch**

```sh
$ pacman -Sy rebar
```

**Fedora**

```
$ yum install erlang-rebar
```

or **Build from source**

```sh
$ git clone git://github.com/rebar/rebar.git
$ cd rebar
$ ./bootstrap
Recompile: src/getopt
...
Recompile: src/rebar_utils
==> rebar (compile)
Congratulations! You now have a self-contained script called "rebar" in
your current working directory. Place this script anywhere in your path
and you can use rebar to build OTP-compliant apps.
```

Open otpbp folder and compile project

```sh
$ rebar compile
```

