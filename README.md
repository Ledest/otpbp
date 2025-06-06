OTP Backports (otpbp)
=====================
[![Build Status](https://github.com/Ledest/otpbp/actions/workflows/erlang.yml/badge.svg)](https://github.com/Ledest/otpbp/actions/workflows/erlang.yml/badge.svg)

Parse transformer that implements some new OTP functions in old Erlang/OTP releases.

Current version of Erlang/OTP: 28.1.

Supported versions of Erlang/OTP: 21.x, 22.x, 23.x, 24.x, 25.x, 26.x, 27.x, 28.0, 28.1.

## Usage

Add `otpbp` to your `rebar.config` deps:

```erlang
{deps, [otpbp]}.
```

or

```erlang
{deps, [otpbp, "~> 8.0"]}.
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
