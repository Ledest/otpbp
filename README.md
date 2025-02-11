OTP Backports (otpbp)
=====================
[![Build Status](https://github.com/Ledest/otpbp/actions/workflows/erlang.yml/badge.svg)](https://github.com/Ledest/otpbp/actions/workflows/erlang.yml/badge.svg)

Parse transformer that implements some new OTP functions in old Erlang/OTP releases.

Current version of Erlang/OTP: 27.2.

Supported versions of Erlang/OTP: 19.3, 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.0, 26.1, 26.2, 27.0, 27.1, 27.2.

## Usage

Add `otpbp` to your `rebar.config` deps:

```erlang
{deps, [otpbp]}.
```

or

```erlang
{deps, [otpbp, "~> 7.6"]}.
```

Add `{parse_transform, otpbp_pt}` to `rebar.config` erl_opts

or

```erlang
-compile({parse_transform, otpbp_pt}).
```

or

```erlang
-include_lib("otpbp/include/otpbp_pt.hrl").
```

to separate modules.
