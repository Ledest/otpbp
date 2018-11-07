-module(kernel_tests).

-compile({parse_transform, otpbp_pt}).

-include_lib("eunit/include/eunit.hrl").

application_test() ->
    Default = 'TEST',
    ?assertEqual(application:get_env(stdlib, test, Default), Default).
